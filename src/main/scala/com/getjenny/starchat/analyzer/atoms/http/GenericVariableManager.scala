package com.getjenny.starchat.analyzer.atoms.http

import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshaller
import akka.stream.Materializer
import com.getjenny.starchat.analyzer.atoms.http.AuthorizationType.AuthorizationType
import scalaz.Scalaz._
import scalaz.{Failure, NonEmptyList, Success, Validation}
import Validation.FlatMap._
import com.getjenny.starchat.analyzer.atoms.http.StoreOption.StoreOption
import AtomVariableReader._
import HttpRequestAtomicConstants.ParameterName._
import scala.concurrent.{ExecutionContext, Future}

trait GenericVariableManager extends VariableManager {

  def extractInput[T](parameter: String,
                      configMap: VariableConfiguration,
                      findProperty: String => Option[String])(buildInput: String => T): AtomValidation[T] = {
    as[String](parameter).run(configMap)
      .flatMap(t => substituteTemplate(t, findProperty))
      .map(buildInput)
  }

  def urlConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomUrlConf] = {
    (as[String](url) |@| as[HttpMethod](httpMethod) |@| as[ContentType](inputContentType)) {
      (url, method, ct) => {
        val formattedUrl = url
          .flatMap(u => substituteTemplate(u, findProperty))

        val contentType = (method, ct) match {
          case (Success(m), Failure(_)) if m.toString === HttpMethods.GET.toString =>
            Success(ContentTypes.NoContentType)
          case _ => ct
        }
        (formattedUrl |@| method |@| contentType) (HttpAtomUrlConf)
      }
    }.run(configMap)
  }

  def authenticationConf(configMap: VariableConfiguration): AtomValidation[Option[HttpAtomAuthConf]] = {
    if (configMap.get(authorizationType).nonEmpty) {
      as[AuthorizationType](authorizationType).run(configMap) match {
        case Success(AuthorizationType.BASIC) =>
          (as[String](username) |@| as[String](password)) ((u, p) => (u |@| p) (BasicAuth))
            .run(configMap)
            .map(_.some)
        case Success(AuthorizationType.BEARER) =>
          as[String](token).run(configMap)
            .map(t => BearerAuth(t))
            .map(_.some)
        case Success(AuthorizationType.API_KEY) =>
          (as[String](key) |@| as[String](token) |@| as[StoreOption](storeTo)) ((k, t, s) => (k |@| t |@| s) (ApiKeyAuth))
            .run(configMap)
            .map(_.some)
        case Failure(f) => Failure(f)
        case t: Any => Failure(NonEmptyList(s"Undefined authorization type $t"))
      }

    } else {
      None.successNel[String]
    }
  }

  def inputConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomInputConf] = {
    val isQueryString = configMap.get(inputQueryTemplate).nonEmpty
    val isJson = configMap.get(inputJson).nonEmpty
    (isQueryString, isJson) match {
      case (true, true) => Failure(NonEmptyList("Both json and query string configuration enabled"))
      case (true, false) => extractInput[QueryStringConf](inputQueryTemplate,
        configMap,
        findProperty) {
        QueryStringConf
      }
      case (false, true) =>
        extractInput[JsonConf](inputJson,
          configMap,
          findProperty) {
          JsonConf
        }
      case _ => Failure(NonEmptyList("Unable to find input configuration"))
    }

  }

  def outputConf(configuration: VariableConfiguration): AtomValidation[HttpAtomOutputConf] = {
    (as[String](outputContentType) |@|
      as[String](outputStatus) |@|
      as[String](outputData) |@|
      as[String](outputScore)) {
      (ct, os, od, score) => (ct |@| os |@| od |@| score) (GenericHttpOutputConf)
    }.run(configuration)
  }

}

case class GenericHttpOutputConf(contentType: String, statusCode: String, data: String, override val score: String)
  extends HttpAtomOutputConf {
  override def responseExtraction(response: HttpResponse)(implicit ec: ExecutionContext, materializer: Materializer): Future[Map[String, String]] = {
    Unmarshaller.stringUnmarshaller(response.entity)
      .map(body =>
        Map(
          contentType -> response.entity.contentType.toString(),
          statusCode -> response.status.toString,
          data -> body,
          score -> "1"
        )
      )
  }
}
