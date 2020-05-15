package com.getjenny.starchat.analyzer.atoms.http

import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials, OAuth2BearerToken, RawHeader}
import akka.http.scaladsl.model.{HttpRequest, _}
import akka.http.scaladsl.{Http, HttpExt}
import akka.util.ByteString
import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import com.getjenny.starchat.SCActorSystem
import scalaz.Scalaz._
import scalaz.{Failure, Success}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.language.postfixOps
import scala.util.Try
import org.apache.commons.lang3.StringUtils.stripAccents

class HttpRequestAtomic(arguments: List[String], restrictedArgs: Map[String, String]) extends AbstractAtomic {
  this: VariableManager =>

  val atomName: String = "HttpRequest"
  private[this] implicit val system: ActorSystem = SCActorSystem.system
  private[this] implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  private[this] val http: HttpExt = Http()
  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass)
  private[this] val timeout: FiniteDuration = restrictedArgs.getOrElse("http-atom.default-timeout", "10").toInt seconds

  private val errorScore = "0"
  private val successScore = "1"


  override def evaluate(query: String, analyzerData: AnalyzersDataInternal): Result = {
    val extractedVariables = analyzerData.extractedVariables


    validateAndBuild(arguments, restrictedArgs, extractedVariables, query) match {
      case Success(conf) =>
          serviceCall(query, conf)
            .map { outputData =>
              //if there haven't been previous invocations, the score will be 0
              val previousInvocationScore = conf.outputConf.getScoreValue(extractedVariables)
              val score = outputData.getOrElse(conf.outputConf.score, errorScore).toInt
              if(previousInvocationScore === successScore.toInt && score === errorScore.toInt) {
                Result(score, data = analyzerData)
              } else {
                Result(score, analyzerData.copy(extractedVariables = analyzerData.extractedVariables ++ outputData))
              }
            }.getOrElse(Result(errorScore.toInt, analyzerData
            .copy(extractedVariables = analyzerData.extractedVariables + (conf.outputConf.score -> errorScore))
          ))
      case Failure(errors) =>
        log.error(s"Error in parameter list: ${errors.toList.mkString("; ")}")
        throw ExceptionAtomic(s"Error in atom configuration: $errors")
    }
  }

  private[this] def serviceCall(query: String,
                                configuration: HttpRequestAtomicConfiguration): Option[Map[String, String]] = Try {
    val request = createRequest(configuration)

    val futureOutput = (http.singleRequest(request) flatMap configuration.outputConf.responseExtraction)
      .map(_.some)
      .recoverWith {
        case error: Throwable =>
          log.error(error, "http error: ")
          Future{None}
        case _ =>
          log.error("unknown http call error")
          Future{None}
      }

    Await.result(futureOutput, timeout)
  }.toValidation match {
    case Success(out) => out
    case Failure(error) => log.error(error, "Error during service call: ")
      None
  }

  protected[this] def cleanUrl(url: String): String = stripAccents(url).replace(" ", "+")

  protected[this] def createRequest(configuration: HttpRequestAtomicConfiguration): HttpRequest = {
    val (authHeader, authQueryParam) = configuration.auth match {
      case Some(BasicAuth(username, password)) =>
        (Authorization(BasicHttpCredentials(s"$username $password")) :: Nil) -> None
      case Some(BearerAuth(token)) =>
        (Authorization(OAuth2BearerToken(token)) :: Nil) -> None
      case Some(ApiKeyAuth(key, token, storeTo)) if storeTo.toString === StoreOption.HEADER.toString =>
        (RawHeader(key, token) :: Nil) -> None
      case Some(ApiKeyAuth(key, token, storeTo)) if storeTo.toString === StoreOption.QUERY.toString =>
        Nil -> s"$key=$token".some
      case _ => Nil -> None
    }
    val urlClean = cleanUrl(configuration.urlConf.url)
    val urlAuth = authQueryParam.map(auth => s"$urlClean?$auth").getOrElse(urlClean)

    val (finalUrl, httpBody) = configuration.inputConf match {
      case Some(QueryStringConf(queryString)) =>
        if (configuration.urlConf.contentType.equals(ContentTypes.`application/x-www-form-urlencoded`)) {
          urlAuth -> HttpEntity(configuration.urlConf.contentType, ByteString(queryString))
        } else {
          val querySeparator = authQueryParam match {
            case Some(_) => "&"
            case _ => "?"
          }
          s"$urlAuth$querySeparator$queryString" -> HttpEntity.Empty
        }
      case Some(JsonConf(json)) => urlAuth -> HttpEntity(ContentTypes.`application/json`, ByteString(json))
      case _ => urlAuth -> HttpEntity.Empty
    }

    HttpRequest(method = configuration.urlConf.method,
      uri = finalUrl,
      headers = authHeader,
      entity = httpBody)
  }

  override val isEvaluateNormalized: Boolean = true
}