package com.getjenny.starchat.entities.persistents

import com.getjenny.starchat.entities.io.DTDocumentStatus
import spray.json.JsObject

import scala.collection.immutable.{List, Map}

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 01/07/16.
 */

trait DTDocumentBase{
  val state: String
}

case class DTDocument(override val state: String,
                      executionOrder: Int,
                      maxStateCount: Int,
                      analyzer: String,
                      queries: List[String],
                      bubble: String,
                      action: String,
                      actionInput: Seq[JsObject],
                      stateData: Map[String, String],
                      successValue: String,
                      failureValue: String,
                      evaluationClass: Option[String] = Some("default"),
                      version: Option[Long] = Some(0L),
                      status: Option[DTDocumentStatus.Value] = Some(DTDocumentStatus.VALID),
                      timestamp: Long = 0L
                     ) extends DTDocumentBase

case class DTDocumentUpdate(override val state: String,
                            executionOrder: Option[Int],
                            maxStateCount: Option[Int],
                            analyzer: Option[String],
                            queries: Option[List[String]],
                            bubble: Option[String],
                            action: Option[String],
                            actionInput: Option[Seq[JsObject]],
                            stateData: Option[Map[String, String]],
                            successValue: Option[String],
                            failureValue: Option[String],
                            evaluationClass: Option[String],
                            status: Option[DTDocumentStatus.Value]
                           ) extends DTDocumentBase

