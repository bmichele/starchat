package com.getjenny.starchat.analyzer.atoms

import HttpRequestAtomicValidation.as
import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.unmarshalling.Unmarshaller
import akka.http.scaladsl.{Http, HttpExt}
import akka.stream.ActorMaterializer
import com.getjenny.analyzer.atoms.AbstractAtomic
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import com.getjenny.starchat.SCActorSystem
import scalaz.{Failure, NonEmptyList, Reader, Success, Validation, ValidationNel}
import scalaz.Validation.FlatMap._
import scalaz.Scalaz._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.util.matching.Regex

case class ServiceResponse(status: String, data: String, contentType: String)

class HttpRequestAtomic(arguments: List[String], restrictedArgs: Map[String, String]) extends AbstractAtomic {

  import HttpRequestAtomic._

  val atomName: String = "HttpRequest"
  private[this] implicit val system: ActorSystem = SCActorSystem.system
  private[this] implicit val materializer: ActorMaterializer = ActorMaterializer()
  private[this] implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  private[this] val http: HttpExt = Http()
  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass)


  val configuration: HttpRequestAtomicConfiguration = {
    HttpRequestAtomicValidation(arguments, restrictedArgs) match {
      case Success(conf) => conf
      case Failure(errors) => throw new IllegalArgumentException(s"Error in parameter list: ${errors.toList.mkString("; ")}")
    }
  }

  override def evaluate(query: String, data: AnalyzersDataInternal): Result = {
    Result(0)
  }

  private[this] def serviceCall(url: String): Option[ServiceResponse] = {
    val request = http.singleRequest(HttpRequest(uri = "url"))
      .map { response =>
        val status = response.status
        Unmarshaller.stringUnmarshaller(response.entity)
          .map(body => ServiceResponse(status.toString(), body, response.entity.contentType.toString()))
      }.flatten
      .map(_.some)
      .recover { case error =>
        log.error(error, "Error during api call")
        none
      }
    Await.result(request, 10 seconds)
  }

  /*protected[this] def responseExtraction(response: HttpResponse): Map[String, String] = {

  }*/
  override val isEvaluateNormalized: Boolean = true
}

object HttpRequestAtomic {

  val baseRegex = "(?:A__temp__.)?(?:system.)?http-atom.[a-z0-9]{1,40}."
  val urlRegex = s"($baseRegex.url)"
  val httpMethodRegex = s"($baseRegex.http-method)"
  val inputKeyRegexString = s"($baseRegex.input-key-([0-9]{1,2}))"
  val inputKeyRegex: Regex = inputKeyRegexString.r
  val inputValueRegexString = s"($baseRegex.input-value-([0-9]{1,2}))"
  val inputValueRegex: Regex = inputValueRegexString.r
  val authorizationTypeRegex = s"($baseRegex.authorization-type)"
  val usernameRegex = s"($baseRegex.username)"
  val passwordRegex = s"($baseRegex.password)"

  val configurationMap = Map("url" -> urlRegex,
    "http-method" -> httpMethodRegex,
    "username" -> usernameRegex,
    "password" -> passwordRegex,
    "input-keys" -> inputKeyRegexString,
    "input-values" -> inputValueRegexString
  )

}

object HttpRequestAtomicValidation {

  import HttpRequestAtomic._
  import scalaz.syntax.std.option._

  type VariableReader[T] = Reader[Map[String, List[String]], ValidationNel[String, T]]

  type VariableConfiguration = Map[String, List[String]]

  def keyFromMap(configMap: Map[String, List[String]], key: String): Validation[String, List[String]] = {
    configMap.get(key)
      .map(x => if (x.isEmpty) Failure(s"$key Not found in configuration") else Success(x))
      .getOrElse(Failure(s"$key Not found in configuration"))
  }

  def as[T](key: String)(implicit to: (String) => VariableReader[T]): VariableReader[T] = {
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

  def queryString(configMap: Map[String, List[String]]): ValidationNel[String, String] = {
    val (inputKeys, inputValues) = (as[List[String]]("input-keys") |@| as[List[String]]("input-values"))
      .tupled.run(configMap)

    (inputKeys |@| inputValues).apply((a, b) =>
      a.zip(b)
        .map { case (k, v) => s"$k=$v" }.mkString("&"))

  }

  private def clearVariableName(variableName: String): String = {
    variableName
      .replace("A__temp__.", "")
      .replace("system.", "")
  }

  def createConfigurationMap(arguments: List[String], restrictedArgs: Map[String, String]): ValidationNel[String, Map[String, List[String]]] = {
    val inputKey = arguments.filter(_.matches(inputKeyRegexString)).map { case inputKeyRegex(_, idx) => idx.toInt }.sorted
    val inputValue = arguments.filter(_.matches(inputValueRegexString)).map { case inputValueRegex(_, idx) => idx.toInt }.sorted

    val differentKeyValues = inputKey.zip(inputValue).filter { case (k, v) => k =/= v }
    if (differentKeyValues.nonEmpty) {
      "Argument list contains an invalid input keys".failureNel
    } else {
      configurationMap
        .mapValues(regex => arguments.filter(_.matches(regex)).map(clearVariableName))
        .mapValues(variableName => variableName.flatMap(x => restrictedArgs.get(x))).successNel[String]
    }
  }

  def apply(arguments: List[String], restrictedArgs: Map[String, String]): ValidationNel[String, HttpRequestAtomicConfiguration] = {

    val configMapNel: ValidationNel[String, Map[String, List[String]]] = createConfigurationMap(arguments, restrictedArgs)

    val res = configMapNel.map { configMap =>
      val urlInfo: ValidationNel[String, UrlInfo] = (as[String]("url") |@|
        as[String]("http-method")
        ) ((a, b) => (a |@| b)(UrlInfo)).run(configMap)

      val authInfo: ValidationNel[String, Authentication] = (as[String]("username") |@|
        as[String]("password")
        ) ((a, b) => (a |@| b) (BasicAuth)).run(configMap)

      val parsedQueryString: ValidationNel[String, String] = if (configMap.getOrElse("input-keys", List.empty).isEmpty) {
        "".successNel[String]
      } else {
        queryString(configMap)
      }


      (urlInfo |@| authInfo |@| parsedQueryString).apply((a, b, q) => HttpRequestAtomicConfiguration(a, b, q))
    }


    res.flatMap(identity)
  }
}

case class HttpRequestAtomicConfiguration(url: UrlInfo, auth: Authentication, queryString: String)

case class UrlInfo(url: String, method: String)

sealed trait Authentication

case class BasicAuth(username: String, password: String) extends Authentication