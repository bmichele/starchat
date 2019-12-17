package com.getjenny.starchat.analyzer.atoms.http

import akka.http.scaladsl.model.HttpResponse
import akka.stream.Materializer
import com.getjenny.starchat.analyzer.atoms.http.HttpRequestAtomicConstants.ParameterName.{System, Temp}
import scalaz.Scalaz._
import scalaz.{Failure, Reader, Success, Validation, ValidationNel}

import scala.concurrent.{ExecutionContext, Future}

trait VariableManager {
  type AtomValidation[T] = ValidationNel[String, T]

  def dictionary: Map[String, String]

  def urlConf(configuration: Map[String, List[String]], findProperty: String => Option[String]): AtomValidation[UrlConf]

  def authenticationConf(configuration: Map[String, List[String]]): AtomValidation[Option[HttpAtomAuthentication]]

  def inputConf(configuration: Map[String, List[String]], findProperty: String => Option[String]): AtomValidation[String]

  def outputConf(configMap: Map[String, List[String]]): AtomValidation[HttpAtomOutputConf]

  def validateAndBuild(arguments: List[String],
                       restrictedArgs: Map[String, String],
                       analyzerData: Map[String, Any]): AtomValidation[HttpRequestAtomicConfiguration] = {
    val findProperty: String => Option[String] = findIn(systemConfiguration = restrictedArgs, analyzerData)
    val configuration = configurationFromArguments(arguments, findProperty)

    (urlConf(configuration, findProperty) |@|
      authenticationConf(configuration) |@|
      inputConf(configuration, findProperty) |@|
      outputConf(configuration)
      ) {
      (url, auth, qs, out) => HttpRequestAtomicConfiguration(url, auth, qs, out)
    }
  }

  def configurationFromArguments(arguments: List[String],
                                 findProperty: String => Option[String]): Map[String, List[String]] = {
    dictionary
      .mapValues(regex => arguments.filter(_.matches(regex)))
      .mapValues(variableName => variableName.flatMap(x => findProperty(x)))
  }

  protected final def clearVariableName(variableName: String): String = {
    variableName
      .replace(System, "")
  }

  protected final def findIn(systemConfiguration: Map[String, String], analyzerData: Map[String, Any])
                            (key: String): Option[String] = {
    if (key.contains(System)) {
      systemConfiguration.get(clearVariableName(key))
    } else {
      analyzerData.get(clearVariableName(key)).map(_.toString)
    }
  }

}

case class HttpRequestAtomicConfiguration(urlConf: UrlConf,
                                          auth: Option[HttpAtomAuthentication],
                                          queryString: String,
                                          outputConf: HttpAtomOutputConf)

case class UrlConf(url: String, method: String, contentType: String)

trait HttpAtomAuthentication

case class BasicAuth(username: String, password: String) extends HttpAtomAuthentication

trait HttpAtomOutputConf {
  def toMap(response: HttpResponse)
           (implicit ec: ExecutionContext, materializer: Materializer): Future[Map[String, String]]
}

object AtomVariableReader {
  type VariableReader[T] = Reader[Map[String, List[String]], ValidationNel[String, T]]

  type VariableConfiguration = Map[String, List[String]]

  def keyFromMap(configMap: Map[String, List[String]], key: String): Validation[String, List[String]] = {
    configMap.get(key)
      .map(x => if (x.isEmpty) Failure(s"$key not found in configuration") else Success(x))
      .getOrElse(Failure(s"$key not found in configuration"))
  }

  def as[T](key: String)(implicit to: String => VariableReader[T]): VariableReader[T] = {
    to(key)
  }

  implicit def asString(key: String): VariableReader[String] = {
    Reader((request: VariableConfiguration) =>
      keyFromMap(request, key)
        .map(list => list.head)
        .toValidationNel)
  }

  implicit def asStringList(key: String): VariableReader[List[String]] = {
    Reader((request: VariableConfiguration) =>
      keyFromMap(request, key)
        .map(list => list)
        .toValidationNel)
  }
}



