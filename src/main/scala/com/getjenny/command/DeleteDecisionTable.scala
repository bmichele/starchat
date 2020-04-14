package com.getjenny.command

/**
  * Created by angelo on 29/03/17.
  */

import java.net.URLEncoder

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpRequest, _}
import com.getjenny.starchat.serializers.JsonSupport
import scopt.OptionParser

import scala.collection.immutable
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.io.Source

object DeleteDecisionTable extends JsonSupport {

  private[this] case class Params(
                             host: String = "http://localhost:8888",
                             indexName: String = "index_getjenny_english_0",
                             path: String = "/decisiontable",
                             inputfile: String = "decision_table.csv",
                             skiplines: Int = 1,
                             timeout: Int = 60,
                             headerKv: Seq[String] = Seq.empty[String]
                           )

  private[this] def execute(params: Params) {
    implicit val system: ActorSystem = ActorSystem()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    val skiplines: Int = params.skiplines

    val baseUrl = params.host + "/" + params.indexName + params.path
    lazy val termTextEntries = Source.fromFile(params.inputfile).getLines

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

    termTextEntries.drop(skiplines).foreach(entry => {
      val cols = entry.split(",").map(_.trim)
      val state = cols(0)
      val url = baseUrl + "/" + URLEncoder.encode(state, "UTF-8")

      val response_future: Future[HttpResponse] =
        Http().singleRequest(HttpRequest(
          method = HttpMethods.DELETE,
          uri = url,
          headers = httpHeader))
      val result = Await.result(response_future, timeout)
      result.status match {
        case StatusCodes.OK => println("deleted: " + state)
        case _ =>
          println("failed delete state(" + state + ") Row(" + entry + ") Message(" + result.toString() + ")")
      }

    })
    Await.ready(system.terminate(), Duration.Inf)
  }

  def main(args: Array[String]) {
    val defaultParams = Params()
    val parser = new OptionParser[Params]("DeleteDecisionTable") {
      head("Delete decision table items")
      help("help").text("prints this usage text")
      opt[String]("inputfile")
        .text(s"the csv file with the list of states" +
          s"  default: ${defaultParams.inputfile}")
        .action((x, c) => c.copy(inputfile = x))
      opt[String]("host")
        .text(s"*Chat base url" +
          s"  default: ${defaultParams.host}")
        .action((x, c) => c.copy(host = x))
      opt[String]("index_name")
        .text(s"the index_name, e.g. index_XXX" +
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
      opt[Int]("skiplines")
        .text(s"skip the first N lines from the csv file" +
          s"  default (skip csv header): ${defaultParams.skiplines}")
        .action((x, c) => c.copy(skiplines = x))
      opt[Seq[String]]("header_kv")
        .text(s"header key-value pair, as key1:value1,key2:value2" +
          s"  default: ${defaultParams.headerKv}")
        .action((x, c) => c.copy(headerKv = x))
    }

    parser.parse(args, defaultParams) match {
      case Some(params) =>
        execute(params)
      case _ =>
        sys.exit(1)
    }
  }
}
