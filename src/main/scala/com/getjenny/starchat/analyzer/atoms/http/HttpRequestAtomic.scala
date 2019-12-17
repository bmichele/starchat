package com.getjenny.starchat.analyzer.atoms.http

import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials}
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


class HttpRequestAtomic(arguments: List[String], restrictedArgs: Map[String, String]) extends AbstractAtomic {
  this: VariableManager =>

  val atomName: String = "HttpRequest"
  private[this] implicit val system: ActorSystem = SCActorSystem.system
  private[this] implicit val materializer: ActorMaterializer = ActorMaterializer()
  private[this] implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  private[this] val http: HttpExt = Http()
  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass)


  override def evaluate(query: String, analyzerData: AnalyzersDataInternal): Result = {
    serviceCall(query, analyzerData.data).map { outputData =>
      Result(1, analyzerData.copy(extractedVariables = analyzerData.extractedVariables ++ outputData))
    }.getOrElse(Result(0, analyzerData))
  }

  private[this] def serviceCall(query: String, data: Map[String, Any]): Option[Map[String, String]] = {
    val configuration: HttpRequestAtomicConfiguration = validateAndBuild(arguments, restrictedArgs, data) match {
        case Success(conf) => conf
        case Failure(errors) =>
          throw new IllegalArgumentException(s"Error in parameter list: ${errors.toList.mkString("; ")}")
      }
    val request = createRequest(configuration)
    val response = http.singleRequest(request)
      .map { response => configuration.outputConf.toMap(response) }
      .flatten
      .map(_.some)
      .recover { case error =>
        log.error(error, "Error during api call: ")
        None
      }
    Await.result(response, 10 seconds)
  }

  protected[this] def createRequest(configuration: HttpRequestAtomicConfiguration): HttpRequest = {
    import HttpCharsets._
    import HttpMethods._
    import MediaTypes._

    val authHeader = configuration.auth match {
      case Some(BasicAuth(username, password)) => Authorization(BasicHttpCredentials(s"$username $password")) :: Nil
      case None => Nil
    }

    val body = if (configuration.urlConf.contentType === "application/x-www-form-urlencoded") {
      HttpEntity(`application/x-www-form-urlencoded`, ByteString(configuration.queryString))
    } else {
      HttpEntity(`text/plain` withCharset `UTF-8`, ByteString(""))
    }

    HttpRequest(method = HttpMethods.getForKey(configuration.urlConf.method).getOrElse(GET),
      uri = configuration.urlConf.url,
      headers = authHeader,
      entity = body)
  }

  override val isEvaluateNormalized: Boolean = true
}