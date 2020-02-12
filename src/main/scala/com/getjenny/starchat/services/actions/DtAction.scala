package com.getjenny.starchat.services.actions

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 26/04/19.
 */

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.analyzer.expressions.AnalyzersDataInternal
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.analyzer.analyzers.StarChatAnalyzer
import com.getjenny.starchat.entities.io.DtActionResult
import com.getjenny.starchat.services.AnalyzerService.log
import com.getjenny.starchat.utils.SystemConfiguration
import scalaz.Scalaz._
import spray.json.DefaultJsonProtocol._
import spray.json.JsObject

import scala.util.{Failure, Success, Try}

case class DtActionException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

trait DtAction {
  protected val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)

  def apply(indexName: String, stateName: String, params: Seq[JsObject]): DtActionResult
}

object DtAction {
  val actionPrefix = "com.getjenny.starchat.actions."
  val analyzerActionPrefix = "com.getjenny.analyzer.analyzers.DefaultParser "

  def apply(indexName: String, stateName: String, action: String, params: Seq[JsObject], query: String): DtActionResult = {
    action match {
      case "com.getjenny.starchat.actions.SendEmailSmtp" => SendEmailSmtp(indexName, stateName, params)
      case "com.getjenny.starchat.actions.SendEmailGJ" => SendEmailGJ(indexName, stateName, params)
      case action if action.startsWith(analyzerActionPrefix) => DtActionAtomAdapter(indexName, stateName, action, params, query)
      case _ => throw DtActionException("Action not implemented: " + action)
    }
  }
}

object DtActionAtomAdapter {
  private[this] val atomConfigurationBasePath = "starchat.atom-values"
  private[this] val restrictedArgs: Map[String, String] = SystemConfiguration
    .createMapFromPath(atomConfigurationBasePath)

  def apply(indexName: String, stateName: String, action: String, params: Seq[JsObject], query: String): DtActionResult = {
    val command = action.stripPrefix(DtAction.analyzerActionPrefix)

    val allParams = if(params.nonEmpty)
      params.map(_.convertTo[Map[String, String]]).reduce(_ ++ _)
    else
      Map.empty[String, String]

    //FIXME use context to pass index_name
    val wrappedCommand = "band(" + command + ")"
    val starchatAnalyzer = Try(new StarChatAnalyzer(wrappedCommand, restrictedArgs + ("index_name" -> indexName))) match {
      case Success(analyzerObject) =>
        log.debug(s"Analyzer successfully built index($indexName) state($stateName)")
        Some(analyzerObject)
      case Failure(e) =>
        log.error(e, s"Error building analyzer index($indexName) state($stateName)")
        None
    }

    starchatAnalyzer.map { analyzer =>
      val result = analyzer.evaluate(query, AnalyzersDataInternal(extractedVariables = allParams))
      DtActionResult(result.score === 1,
        if (result.score === 0) 1 else 0,
        result.data.extractedVariables
      )
    }.getOrElse(DtActionResult(code = 1))
  }
}
