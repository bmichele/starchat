package com.getjenny.command

/**
 * Created by angelo on 07/05/20.
 */

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.unmarshalling.Unmarshal
import com.getjenny.starchat.entities.io.QADocumentUpdate
import com.getjenny.starchat.entities.persistents.{QADocument, QADocumentAnnotations}
import com.getjenny.starchat.serializers.JsonSupport
import scopt.OptionParser

import scala.collection.immutable
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.io.Source
import scala.util.{Failure, Success}

case class RescaleFeedbackRangeException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

object RescaleFeedbackRange extends JsonSupport {
  private[this] case class Params(
                                   host: String = "http://localhost:8888",
                                   indexName: String = "index_getjenny_english_0",
                                   path: String = "/conversation_logs",
                                   maxTimestamp: Long = 1E200.toLong,
                                   refMaxScore: Double = 10.0d,
                                   minScore: Double = 1.0d,
                                   inputfile: Option[String] = None: Option[String],
                                   timeout: Int = 60,
                                   dryrun: Boolean = false,
                                   headerKv: Seq[String] = Seq.empty[String]
                                 )

  private[this] def execute(params: Params) {
    implicit val system: ActorSystem = ActorSystem()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    val baseUrl = params.host + "/" + params.indexName + params.path

    val inputFile = params.inputfile match {
      case Some(path) => path
      case _ => throw RescaleFeedbackRangeException("inputfile cannot be empty")
    }

    val httpHeader: immutable.Seq[HttpHeader] = if(params.headerKv.nonEmpty) {
      val headers: Seq[RawHeader] = params.headerKv.map(x => {
        val header_opt = x.split(":")
        val key = header_opt(0)
        val value = header_opt(1)
        RawHeader(key, value)
      }) ++ Seq(RawHeader("application", "json"))
      headers.to[immutable.Seq]
    } else {
      immutable.Seq(RawHeader("application", "json"))
    }

    val timeout = Duration(params.timeout, "s")

    val source = Source.fromFile(name=inputFile)
    val documents = source.getLines.toList.map{ case json: String =>
      Await.ready(Unmarshal(json).to[QADocument], 30.seconds).value match {
        case Some(documentOpt) => documentOpt match {
          case Success(document) => Some(document)
          case Failure(e) =>
            println("Error: " + e)
            None
        }
        case _ =>
          println(s"Error: unmarshaling document => $json")
          None
      }
    }.filter(_.nonEmpty).map(_.get).filter(_.annotations.nonEmpty)
      .filter(_.annotations.get.feedbackConvScore.nonEmpty)
        .filter(_.timestamp.getOrElse(0L) <= params.maxTimestamp)
    source.close()

    val scoreList = documents.map(d => {
      val score = d.annotations.get.feedbackConvScore.get
      (d.id, score, d.timestamp.getOrElse(0))
    })

    val updateDoc = scoreList.filter(s => s._2 > params.minScore).map { s =>
      val newScore = s._2 / params.refMaxScore
      QADocumentUpdate(id = List(s._1),
        annotations = Some(QADocumentAnnotations(feedbackConvScore = Some(newScore))))
    }

    if(params.dryrun) {
      updateDoc.foreach(println)
    } else {
      updateDoc.foreach { d =>
        val entityFuture = Marshal(d).to[MessageEntity]
        val entity = Await.result(entityFuture, 10.second)
        val responseFuture: Future[HttpResponse] =
          Http().singleRequest(HttpRequest(
            method = HttpMethods.PUT,
            uri = baseUrl,
            headers = httpHeader,
            entity = entity))
        val result = Await.result(responseFuture, timeout)
        result.status match {
          case StatusCodes.Created | StatusCodes.OK => println("updated: " + d.id +
            " text(" + d + ")")
          case _ =>
            println("failed indexing entry(" + d + ") Message(" + result.toString() +
              ") StatusCode(" + result.status + ")")
        }
      }
    }

    Await.ready(system.terminate(), Duration.Inf)
  }

  def main(args: Array[String]) {
    val defaultParams = Params()
    val parser = new OptionParser[Params]("RescaleFeedbackRange") {
      head("Update feedback range on QA")
      help("help").text("prints this usage text")
      opt[String]("inputfile")
        .text(s"path of the file input file, a document per line, eventually base64 encoded" +
          s"  default: ${defaultParams.inputfile}")
        .action((x, c) => c.copy(inputfile = Option(x)))
      opt[String]("host")
        .text(s"*Chat base url" +
          s"  default: ${defaultParams.host}")
        .action((x, c) => c.copy(host = x))
      opt[String]("index_name")
        .text(s"the index_name, e.g. index_english_XXX" +
          s"  default: ${defaultParams.indexName}")
        .action((x, c) => c.copy(indexName = x))
      opt[String]("path")
        .text(s"the service path" +
          s"  default: ${defaultParams.path}")
        .action((x, c) => c.copy(path = x))
      opt[Int]("timeout")
        .text(s"the timeout in seconds of each insert operation" +
          s"  default: ${defaultParams.timeout}")
        .action((x, c) => c.copy(timeout = x))
      opt[Long]("maxTimestamp")
        .text(s"consider only the items with a timestamp earlier than maxTimestamp" +
          s"  default: ${defaultParams.maxTimestamp}")
        .action((x, c) => c.copy(maxTimestamp = x))
      opt[Double]("minScore")
        .text(s"consider only the score higher than minScore" +
          s"  default: ${defaultParams.minScore}")
        .action((x, c) => c.copy(minScore = x))
      opt[Double]("refMaxScore")
        .text(s"maxScore to be used for normalization" +
          s"  default: ${defaultParams.refMaxScore}")
        .action((x, c) => c.copy(refMaxScore = x))
      opt[Seq[String]]("header_kv")
        .text(s"header key-value pair, as key1:value1,key2:value2" +
          s"  default: ${defaultParams.headerKv}")
        .action((x, c) => c.copy(headerKv = x))
      opt[Boolean]("dryrun")
        .text(s"if true print the new documents without sendind them to starchat" +
          s"  default: ${defaultParams.dryrun}")
        .action((x, c) => c.copy(dryrun = x))
    }

    parser.parse(args, defaultParams) match {
      case Some(params) =>
        execute(params)
      case _ =>
        sys.exit(1)
    }
  }
}
