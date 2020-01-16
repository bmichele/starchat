package com.getjenny.starchat.services.actions

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 26/04/19.
 */

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.io.DtActionResult

case class DtActionException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

trait DtAction {
  protected val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)
  def apply(indexName: String, stateName: String, actionInput: Seq[Map[String, String]]): DtActionResult

  def actionInputToMap(actionInput: Seq[Map[String, String]]): Map[String, String] = {
    actionInput.map{ keyValuePair =>
      val key: String = keyValuePair.get("key") match {
        case Some(v) => v
        case _ => throw DtActionException(s"Missing key field in parameter specification: ${keyValuePair}")
      }
      val value: String = keyValuePair.get("value") match {
        case Some(v) => v
        case _ => throw DtActionException(s"Missing value field in parameter specification: ${keyValuePair}")
      }
      (key, value)
    }.toMap
  }
}

object DtAction {
  val actionPrefix = "com.getjenny.starchat.actions."
  def apply(indexName: String, stateName: String, action: String, actionInput: Seq[Map[String, String]]): DtActionResult = {
    action match {
      case "com.getjenny.starchat.actions.SendEmailSmtp"  => SendEmailSmtp(indexName, stateName, actionInput)
      case "com.getjenny.starchat.actions.SendEmailGJ"  => SendEmailGJ(indexName, stateName, actionInput)
      case _ => throw DtActionException("Action not implemented: " + action)
    }
  }
}
