package com.getjenny.starchat.analyzer.atoms.http

import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials, OAuth2BearerToken, RawHeader}
import akka.http.scaladsl.model.{HttpRequest, _}
import akka.http.scaladsl.{Http, HttpExt}
import akka.stream.ActorMaterializer
import akka.util.ByteString
import com.getjenny.analyzer.atoms.AbstractAtomic
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import com.getjenny.starchat.SCActorSystem
import scalaz.Scalaz._
import scalaz.{Failure, Success}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.util.Try
import scala.language.postfixOps

class HttpRequestAtomic(arguments: List[String], restrictedArgs: Map[String, String]) extends AbstractAtomic {
  this: VariableManager =>

  val atomName: String = "HttpRequest"
  private[this] implicit val system: ActorSystem = SCActorSystem.system
  private[this] implicit val materializer: ActorMaterializer = ActorMaterializer()
  private[this] implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  private[this] val http: HttpExt = Http()
  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass)
  private[this] val timeout: FiniteDuration = restrictedArgs.getOrElse("http-atom.default-timeout", "10").toInt seconds


  override def evaluate(query: String, analyzerData: AnalyzersDataInternal): Result = {
    validateAndBuild(arguments, restrictedArgs, analyzerData.data) match {
      case Success(conf) =>
        serviceCall(query, conf)
          .map { outputData =>
            Result(1, analyzerData.copy(extractedVariables = analyzerData.extractedVariables ++ outputData))
          }.getOrElse(Result(0, analyzerData))
      case Failure(errors) =>
        log.error(s"Error in parameter list: ${errors.toList.mkString("; ")}")
        Result(0, analyzerData)
    }
  }

  private[this] def serviceCall(query: String, configuration: HttpRequestAtomicConfiguration): Option[Map[String, String]] = Try {
    val request = createRequest(configuration)

    val futureOutput = (http.singleRequest(request) flatMap configuration.outputConf.toMap)
      .map(_.some)
      .recover {
        case error => log.error(error, "http error: ")
          None
      }

    Await.result(futureOutput, timeout)
  }.toValidation match {
    case Success(out) => out
    case Failure(error) => log.error(error, "Error during service call: ")
      None
  }

  protected[this] def createRequest(configuration: HttpRequestAtomicConfiguration): HttpRequest = {
    val (authHeader, authQueryParam) = configuration.auth match {
      case Some(BasicAuth(username, password)) =>
        (Authorization(BasicHttpCredentials(s"$username $password")) :: Nil) -> None
      case Some(BearerAuth(token)) =>
        (Authorization(OAuth2BearerToken(token)) :: Nil) -> None
      case Some(ApiKeyAuth(key, token, storeTo)) if storeTo == StoreOption.HEADER =>
        (RawHeader(key, token) :: Nil) -> None
      case Some(ApiKeyAuth(key, token, storeTo)) if storeTo == StoreOption.QUERY => Nil -> s"$key=$token".some
      case _ => Nil -> None
    }

    val url = authQueryParam.map(auth => s"${configuration.urlConf.url}?$auth").getOrElse(configuration.urlConf.url)

    val (finalUrl, httpBody) = configuration.inputConf match {
      case QueryStringConf(queryString) =>
        if (configuration.urlConf.contentType.equals(ContentTypes.`application/x-www-form-urlencoded`)) {
          url -> HttpEntity(configuration.urlConf.contentType, ByteString(queryString))
        } else {
          val querySeparator = if(authQueryParam.isDefined) "&" else "?"
          s"${configuration.urlConf.url}$querySeparator$queryString" -> HttpEntity.Empty
        }
      case JsonConf(json) => url -> HttpEntity(ContentTypes.`application/json`, ByteString(json))
    }

    HttpRequest(method = configuration.urlConf.method,
      uri = finalUrl,
      headers = authHeader,
      entity = httpBody)
  }

  override val isEvaluateNormalized: Boolean = true
}