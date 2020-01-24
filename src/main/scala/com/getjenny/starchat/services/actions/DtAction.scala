package com.getjenny.starchat.services.actions

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 26/04/19.
 */

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.analyzer.expressions.{AnalyzersData, AnalyzersDataInternal}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.analyzer.atoms.http.HttpRequestAtomic
import com.getjenny.starchat.analyzer.atoms.http.custom.WeatherVariableManager
import com.getjenny.starchat.entities.io.DtActionResult
import com.getjenny.starchat.services.AnalyzerService.atomConfigurationBasePath
import com.getjenny.starchat.utils.SystemConfiguration

case class DtActionException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

trait DtAction {
  protected val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)
  def apply(indexName: String, stateName: String, params: Map[String, String]): DtActionResult
}

object DtAction {
  val actionPrefix = "com.getjenny.starchat.actions."
  def apply(indexName: String, stateName: String, action: String, params: Map[String, String]): DtActionResult = {
    action match {
      case "com.getjenny.starchat.actions.SendEmailSmtp"  => SendEmailSmtp(indexName, stateName, params)
      case "com.getjenny.starchat.actions.SendEmailGJ"  => SendEmailGJ(indexName, stateName, params)
      case _ => throw DtActionException("Action not implemented: " + action)
    }
  }
}

object DtActionAtomAdapter{
  private[this] val atomConfigurationBasePath = "starchat.atom-values"
  val systemConfiguration = SystemConfiguration
    .createMapFromPath(atomConfigurationBasePath)
  val atoms = Map("weather" -> new HttpRequestAtomic(List.empty, systemConfiguration) with WeatherVariableManager)
  def apply(indexName: String, stateName: String, action: String, params: Seq[Map[String, String]]): DtActionResult = {
    val result = atoms.get(action).get.evaluate("", AnalyzersDataInternal(extractedVariables = params.head))
    DtActionResult(result.score == 1,
      if(result.score == 0) 1 else 0,
      result.data.extractedVariables
    )
  }
}
