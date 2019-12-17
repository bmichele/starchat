package com.getjenny.starchat.analyzer.atoms.http

import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.unmarshalling.Unmarshaller
import akka.stream.Materializer
import scalaz.Scalaz._
import scalaz.{Failure, NonEmptyList, Success}

import scala.concurrent.{ExecutionContext, Future}

class GenericVariableManager extends VariableManager {

  import AtomVariableReader._
  import HttpRequestAtomicConstants.ParameterName._
  import HttpRequestAtomicConstants.Regex._

  def dictionary: Map[String, String] = {
    Map(
      Url -> UrlRegex,
      HttpMethod -> HttpMethodRegex,
      InputContentType -> InputContentTypeRegex,
      AuthorizationType -> AuthorizationTypeRegex,
      Username -> UsernameRegex,
      Password -> PasswordRegex,
      InputQueryTemplate -> InputQueryTemplateRegex,
      OutputContentType -> OutputContentTypeRegex,
      OutputStatus -> OutputStatusRegex,
      OutputData -> OutputDataRegex
    )
  }

  def queryString(configMap: Map[String, List[String]], findProperty: String => Option[String]): AtomValidation[String] = {
    val template = as[String](InputQueryTemplate)
      .run(configMap)

    template.map(t => evaluateTemplate(t, findProperty)) match {
      case Success(queryString) =>
        if(queryString.contains('{')){
        Failure(NonEmptyList(s"Unable to found substitution in template: $queryString"))
      }else {
        Success(queryString)
      }
      case err: Failure[_] => err
    }
  }

  private[this] def evaluateTemplate(template: String, findProperty: String => Option[String]): String = {
    GenericVariableNameRegex.findAllIn(template)
      .foldLeft(template) { case (acc, variable) =>
        findProperty(variable).map(v => acc.replaceAll(s"\\{$variable\\}", v)).getOrElse(acc)
      }
  }

  def urlConf(configMap: Map[String, List[String]], findProperty: String => Option[String]): AtomValidation[UrlConf] = {
    (as[String](Url) |@| as[String](HttpMethod) |@| as[String](InputContentType)) {
      (url, method, ct) => {
        val formattedUrl = url match {
          case Success(u) =>
            val formatted = evaluateTemplate(u, findProperty)
            if(formatted.contains('{')) {
              Failure(NonEmptyList(s"Unable to found substitution in template: $u"))
            } else {
              Success(formatted)
            }
          case e => e
        }

        val contentType = (method, ct) match {
          case (Success(m), Failure(_)) if m === "GET" => Success("")
          case _ => ct
        }
        (formattedUrl |@| method |@| contentType) (UrlConf)
      }
    }.run(configMap)
  }

  def authenticationConf(configMap: Map[String, List[String]]): AtomValidation[Option[HttpAtomAuthentication]] = {
    if (configMap.getOrElse(AuthorizationType, List.empty).nonEmpty) {
      (as[String](Username) |@| as[String](Password)) ((u, p) => (u |@| p) (BasicAuth))
        .run(configMap)
        .map(Some(_))
    } else {
      None.successNel[String]
    }
  }

  def inputConf(configMap: Map[String, List[String]], findProperty: String => Option[String]): AtomValidation[String] = {
    if (configMap.getOrElse(InputQueryTemplate, List.empty).isEmpty) {
      "".successNel[String]
    } else {
      queryString(configMap, findProperty)
    }
  }

  def outputConf(configMap: Map[String, List[String]]): AtomValidation[HttpAtomOutputConf] = {
    (as[String](OutputContentType) |@| as[String](OutputStatus) |@| as[String](OutputData)) {
      (ct, os, od) => (ct |@| os |@| od) (GenericHttpOutputConf)
    }.run(configMap)
  }

}

case class GenericHttpOutputConf(contentType: String, statusCode: String, data: String) extends HttpAtomOutputConf {
  override def toMap(response: HttpResponse)(implicit ec: ExecutionContext, materializer: Materializer): Future[Map[String, String]] = {
    Unmarshaller.stringUnmarshaller(response.entity)
      .map(body =>
        Map(
          contentType -> response.entity.contentType.toString(),
          statusCode -> response.status.toString,
          data -> body
        )
      )
  }
}
