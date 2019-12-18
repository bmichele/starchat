package com.getjenny.starchat.analyzer.atoms.http

import akka.http.scaladsl.model.{ContentType, HttpMethod, HttpMethods, HttpResponse}
import akka.stream.Materializer
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.VariableConfiguration
import com.getjenny.starchat.analyzer.atoms.http.AuthorizationType.AuthorizationType
import com.getjenny.starchat.analyzer.atoms.http.HttpRequestAtomicConstants.Regex.genericVariableNameRegex
import com.getjenny.starchat.analyzer.atoms.http.HttpRequestAtomicConstants.{ParameterName, queryStringTemplateDelimiterPrefix, queryStringTemplateDelimiterSuffix}
import scalaz.Scalaz._
import scalaz.{Failure, NonEmptyList, Reader, Success, Validation, ValidationNel}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

trait VariableManager {

  type AtomValidation[T] = ValidationNel[String, T]

  def urlConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[UrlConf]

  def authenticationConf(configuration: VariableConfiguration): AtomValidation[Option[HttpAtomAuthentication]]

  def inputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[InputConf]

  def outputConf(configMap: VariableConfiguration): AtomValidation[HttpAtomOutputConf]

  def additionalArguments: List[String] = Nil

  def validateAndBuild(arguments: List[String],
                       restrictedArgs: Map[String, String],
                       analyzerData: Map[String, Any]): AtomValidation[HttpRequestAtomicConfiguration] = {
    val findProperty: String => Option[String] = findIn(systemConfiguration = restrictedArgs, analyzerData)
    val allArguments = arguments ++ additionalArguments
    val configuration = configurationFromArguments(allArguments, findProperty)

    (urlConf(configuration, findProperty) |@|
      authenticationConf(configuration) |@|
      inputConf(configuration, findProperty) |@|
      outputConf(configuration)
      ) {
      (url, auth, qs, out) => HttpRequestAtomicConfiguration(url, auth, qs, out)
    }
  }

  def configurationFromArguments(arguments: List[String],
                                 findProperty: String => Option[String]): VariableConfiguration = {
    arguments
      .map(variable => variable.substring(variable.lastIndexOf('.') + 1, variable.length) -> findProperty(variable))
      .collect { case (key, Some(value)) => (key, value) }
      .toMap
  }

  protected final def clearVariableName(variableName: String): String = {
    variableName
      .replace(ParameterName.system, "")
  }

  protected final def findIn(systemConfiguration: Map[String, String], analyzerData: Map[String, Any])
                            (key: String): Option[String] = {
    if (key.contains(ParameterName.system)) {
      systemConfiguration.get(clearVariableName(key))
    } else {
      analyzerData.get(clearVariableName(key)).map(_.toString)
    }
  }

  private[this] def evaluateTemplate(template: String,
                                       findProperty: String => Option[String],
                                       delimiterPrefix: String,
                                       delimiterSuffix: String
                                      ): String = {
    genericVariableNameRegex.findAllIn(template)
      .foldLeft(template) { case (acc, variable) =>
        findProperty(variable)
          .map { v =>
            val regex = s"\\$delimiterPrefix$variable\\$delimiterSuffix"
            acc.replaceAll(regex, v)
          }
          .getOrElse(acc)
      }
  }

  protected def substituteTemplate(template: String,
                         findProperty: String => Option[String],
                         delimiterPrefix: String = queryStringTemplateDelimiterPrefix,
                         delimiterSuffix: String = queryStringTemplateDelimiterSuffix
                        ): AtomValidation[String] = {
    val formatted = evaluateTemplate(template, findProperty, delimiterPrefix, delimiterSuffix)
    if (formatted.contains(delimiterPrefix)) {
      Failure(NonEmptyList(s"Unable to found substitution in template: $template"))
    } else {
      Success(formatted)
    }
  }


}

case class HttpRequestAtomicConfiguration(urlConf: UrlConf,
                                          auth: Option[HttpAtomAuthentication],
                                          inputConf: InputConf,
                                          outputConf: HttpAtomOutputConf)

case class UrlConf(url: String, method: HttpMethod, contentType: ContentType)

object AuthorizationType extends Enumeration {
  type AuthorizationType = Value
  val BASIC: AuthorizationType = Value("Basic")
  val BEARER: AuthorizationType = Value("Bearer")
  val API_KEY: AuthorizationType = Value("ApiKey")

  def fromName(s: String): Validation[Throwable, AuthorizationType] = Try(withName(s)).toValidation
}

trait HttpAtomAuthentication

case class BasicAuth(username: String, password: String) extends HttpAtomAuthentication

case class BearerAuth(token: String) extends HttpAtomAuthentication

trait InputConf

case class QueryStringConf(queryString: String) extends InputConf

case class JsonConf(json: String) extends InputConf

trait HttpAtomOutputConf {
  def toMap(response: HttpResponse)
           (implicit ec: ExecutionContext, materializer: Materializer): Future[Map[String, String]]
}

object AtomVariableReader {
  import Validation.FlatMap._
  import scalaz.syntax.std.option._

  type VariableConfiguration = Map[String, String]

  type VariableReader[T] = Reader[VariableConfiguration, ValidationNel[String, T]]

  def keyFromMap(configMap: VariableConfiguration, key: String): Validation[String, String] = {
    configMap.get(key)
      .map(Success(_))
      .getOrElse(Failure(s"$key not found in configuration"))
  }

  def as[T](key: String)(implicit to: String => VariableReader[T]): VariableReader[T] = {
    to(key)
  }

  implicit def asString(key: String): VariableReader[String] = {
    Reader((request: VariableConfiguration) =>
      keyFromMap(request, key)
        .toValidationNel)
  }

  implicit def asAuthorizationType(key: String): VariableReader[AuthorizationType] = {
    Reader((request: VariableConfiguration) =>
      keyFromMap(request, key)
        .flatMap({AuthorizationType.fromName(_) |> toMessage[AuthorizationType](key) })
        .toValidationNel)
  }

  implicit def asHttpMethod(key: String): VariableReader[HttpMethod] = {
    Reader((request: VariableConfiguration) =>
      keyFromMap(request, key)
        .flatMap(x => {HttpMethods.getForKey(x)
          .toSuccess(s"Error while extracting key <$key>: $x is an invalid method")
        })
        .toValidationNel)
  }

  private[this] def getContentType(contentType: String): Validation[String, ContentType] = {
      ContentType.parse(contentType).leftMap(_.map(_.summary).mkString(";")).validation
  }

  implicit def asContentType(key: String): VariableReader[ContentType] = {
    Reader((request: VariableConfiguration) =>
      keyFromMap(request, key)
        .flatMap(getContentType)
        .toValidationNel)
  }

  def toMessage[S](key: String)(v: Validation[Throwable, S]): Validation[String, S] = {
    ((f: Throwable) => s"Error while extracting key <$key>: ${f.getMessage}")  <-: v
  }
}



