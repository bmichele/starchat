package com.getjenny.starchat.analyzer.atoms.http

import akka.http.scaladsl.model.{ContentType, HttpMethod, HttpMethods, HttpResponse}
import akka.http.scaladsl.unmarshalling.Unmarshaller
import akka.stream.Materializer
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.VariableConfiguration
import com.getjenny.starchat.analyzer.atoms.http.AuthorizationType.AuthorizationType
import com.getjenny.starchat.analyzer.atoms.http.HttpRequestAtomicConstants.Regex.templateRegex
import com.getjenny.starchat.analyzer.atoms.http.HttpRequestAtomicConstants._
import com.getjenny.starchat.analyzer.atoms.http.StoreOption.StoreOption
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._
import scalaz.syntax.std.option.ToOptionOpsFromOption
import scalaz.{Failure, NonEmptyList, Reader, Success, Validation, ValidationNel}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.Try

trait VariableManager {

  type AtomValidation[T] = ValidationNel[String, T]

  def urlConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomUrlConf]

  def authenticationConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[Option[HttpAtomAuthConf]]

  def inputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomInputConf]

  def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf]

  def additionalArguments: List[String] = Nil

  def validateAndBuild(arguments: List[String],
                       restrictedArgs: Map[String, String],
                       extractedVariables: Map[String, String]): AtomValidation[HttpRequestAtomicConfiguration] = {
    val allArguments = arguments ++ additionalArguments
    val keyValueSeparator = "="
    val additionalConfiguration = allArguments
      .filter(_.contains(keyValueSeparator))
      .map { x =>
        val keyValue = x.split(keyValueSeparator)
        keyValue(0) -> keyValue(1)
      }.toMap
    val runtimeConfiguration = additionalConfiguration ++ extractedVariables
    val findProperty: String => Option[String] = findIn(systemConfiguration = restrictedArgs, runtimeConfiguration)
    val configuration = configurationFromArguments(arguments.map(_.split(keyValueSeparator)(0)), findProperty)

    (urlConf(configuration, findProperty) |@|
      authenticationConf(configuration, findProperty) |@|
      inputConf(configuration, findProperty) |@|
      outputConf(configuration, findProperty)
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

  protected final def findIn(systemConfiguration: Map[String, String], runtimeConfiguration: Map[String, String])
                            (key: String): Option[String] = {
    systemConfiguration.get(key)
      .orElse(runtimeConfiguration.get(key))
  }

  private[this] def evaluateTemplate(template: String,
                                     findProperty: String => Option[String]): String = {
    templateRegex.findAllIn(template)
      .foldLeft(template) { case (acc, variable) =>
        findProperty(variable)
          .map { v =>
            val regex = s"\\$templateDelimiterPrefix$variable\\$templateDelimiterSuffix"
            acc.replaceAll(regex, v)
          }
          .getOrElse(acc)
      }
  }

  protected def substituteTemplate(template: String,
                                   findProperty: String => Option[String]): AtomValidation[String] = {
    val formatted = evaluateTemplate(template, findProperty)
    if (formatted.contains(templateDelimiterPrefix)) {
      Failure(NonEmptyList(s"Unable to found substitution in template: $template"))
    } else {
      Success(formatted)
    }
  }


}

object AtomVariableReader {
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
        .flatMap({
          AuthorizationType.fromName(_) |> toMessage[AuthorizationType](key)
        })
        .toValidationNel)
  }

  implicit def asHttpMethod(key: String): VariableReader[HttpMethod] = {
    Reader((request: VariableConfiguration) =>
      keyFromMap(request, key)
        .flatMap(x => {
          HttpMethods.getForKey(x)
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

  implicit def asStoreOption(key: String): VariableReader[StoreOption] = {
    Reader((request: VariableConfiguration) =>
      keyFromMap(request, key)
        .flatMap({
          StoreOption.fromName(_) |> toMessage[StoreOption](key)
        })
        .toValidationNel)
  }

  def toMessage[S](key: String)(v: Validation[Throwable, S]): Validation[String, S] = {
    ((f: Throwable) => s"Error while extracting key <$key>: ${f.getMessage}") <-: v
  }
}

case class HttpRequestAtomicConfiguration(urlConf: HttpAtomUrlConf,
                                          auth: Option[HttpAtomAuthConf],
                                          inputConf: HttpAtomInputConf,
                                          outputConf: HttpAtomOutputConf)

case class HttpAtomUrlConf(url: String, method: HttpMethod, contentType: ContentType)

object AuthorizationType extends Enumeration {
  type AuthorizationType = Value
  val BASIC: AuthorizationType = Value("basic")
  val BEARER: AuthorizationType = Value("bearer")
  val API_KEY: AuthorizationType = Value("apiKey")

  def fromName(s: String): Validation[Throwable, AuthorizationType] = Try(withName(s)).toValidation
}

object StoreOption extends Enumeration {
  type StoreOption = Value
  val HEADER: StoreOption = Value("header")
  val QUERY: StoreOption = Value("query")

  def fromName(s: String): Validation[Throwable, StoreOption] = Try(withName(s)).toValidation
}

trait HttpAtomAuthConf

case class BasicAuth(username: String, password: String) extends HttpAtomAuthConf

case class BearerAuth(token: String) extends HttpAtomAuthConf

case class ApiKeyAuth(key: String, token: String, storeTo: StoreOption) extends HttpAtomAuthConf

trait HttpAtomInputConf

case class QueryStringConf(queryString: String) extends HttpAtomInputConf

case class JsonConf(json: String) extends HttpAtomInputConf

trait HttpAtomOutputConf {
  val score: String

  def responseExtraction(response: HttpResponse)
                        (implicit ec: ExecutionContext, materializer: Materializer): Future[Map[String, String]] = {
    Unmarshaller.stringUnmarshaller(response.entity)
      .map(bodyParser(_, response.entity.contentType.toString(), response.status.toString))
  }

  def bodyParser(body: String, contentType: String, status: String): Map[String, String]

  def exists(extractedVariables: Map[String, String]): Boolean = {
    extractedVariables.contains(score)
  }

  def getScoreValue(extractedVariables: Map[String, String]): String = {
    extractedVariables.getOrElse(score, "0")
  }
}




