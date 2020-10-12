package com.getjenny.starchat.entities.persistents

import java.util

import com.getjenny.starchat.entities.io.DTDocumentStatus
import com.getjenny.starchat.utils.Base64
import org.elasticsearch.action.get.GetResponse
import org.elasticsearch.action.search.SearchResponse
import org.elasticsearch.search.SearchHit
import spray.json.{JsObject, _}

import scala.collection.JavaConverters._
import scala.collection.immutable.{List, Map}

case class DecisionTableItem(state: String,
                             analyzerDeclaration: String,
                             executionOrder: Int = 0,
                             maxStateCounter: Int = -1,
                             evaluationClass: String = "default",
                             status: DTDocumentStatus.Value = DTDocumentStatus.VALID,
                             timestamp: Long = -1L,
                             version: Long = -1L,
                             queries: List[String] = List.empty[String],
                             bubble: String,
                             action: String,
                             actionInput: Seq[JsObject] = Seq.empty[JsObject],
                             stateData: Map[String, String] = Map.empty[String, String],
                             successValue: String,
                             failureValue: String
                            )

object DecisionTableEntityManager extends ReadEntityManager[DecisionTableItem] {
  override def fromSearchResponse(response: SearchResponse): List[DecisionTableItem] = {
    response.getHits.getHits.toList.map {
      item: SearchHit =>
        val source: Map[String, Any] = item.getSourceAsMap.asScala.toMap
        fromSearch(source, item.getId, item.getVersion)
    }
  }

  private[this] def fromSearch(source: Map[String, Any], id: String, version: Long): DecisionTableItem = {

    val analyzerDeclaration: String = source.get("analyzer") match {
      case Some(t) => Base64.decode(t.asInstanceOf[String])
      case _ => ""
    }

    val executionOrder: Int = source.get("execution_order") match {
      case Some(t) => t.asInstanceOf[Int]
      case _ => 0
    }

    val maxStateCounter: Int = source.get("max_state_counter") match {
      case Some(t) => t.asInstanceOf[Int]
      case _ => 0
    }

    val evaluationClass: String = source.get("evaluation_class") match {
      case Some(t) => t.asInstanceOf[String]
      case _ => "default"
    }

    val queries: List[String] = source.get("queries") match {
      case Some(t) =>
        t.asInstanceOf[util.ArrayList[util.HashMap[String, String]]].asScala.toList
          .map(q_e => q_e.get("query"))
      case None => List[String]()
    }

    val bubble: String = source.get("bubble") match {
      case Some(t) => t.asInstanceOf[String]
      case None => ""
    }

    val action: String = source.get("action") match {
      case Some(t) => t.asInstanceOf[String]
      case None => ""
    }

    val actionInput: Seq[JsObject] = source.get("action_input") match {
      case Some(t) => t.asInstanceOf[util.List[util.HashMap[String, String]]]
        .asScala.map{ v => v.getOrDefault("json", "").parseJson.asJsObject }
      case Some(null) | None => Seq[JsObject]()
    }

    val stateData: Map[String, String] = source.get("state_data") match {
      case Some(null) => Map[String, String]()
      case Some(t) => t.asInstanceOf[util.HashMap[String, String]].asScala.toMap
      case None => Map[String, String]()
    }

    val successValue: String = source.get("success_value") match {
      case Some(t) => t.asInstanceOf[String]
      case None => ""
    }

    val failureValue: String = source.get("failure_value") match {
      case Some(t) => t.asInstanceOf[String]
      case None => ""
    }

    val status: DTDocumentStatus.Value = source.get("status") match {
      case Some(t) => DTDocumentStatus.value(t.asInstanceOf[String])
      case None => DTDocumentStatus.VALID
    }

    val timestamp: Long = source.get("timestamp") match {
      case Some(t) => t.asInstanceOf[Long]
      case None => System.currentTimeMillis()
    }

    val state = extractId(id)

    val decisionTableRuntimeItem: DecisionTableItem =
      DecisionTableItem(state = state,
        analyzerDeclaration = analyzerDeclaration,
        executionOrder = executionOrder,
        maxStateCounter = maxStateCounter,
        queries = queries,
        evaluationClass = evaluationClass,
        bubble = bubble,
        action = action,
        actionInput = actionInput,
        stateData = stateData,
        successValue = successValue,
        failureValue = failureValue,
        status = status,
        timestamp = timestamp,
        version = version)
    decisionTableRuntimeItem
  }

  override def fromGetResponse(response: List[GetResponse]): List[DecisionTableItem] = {
    response
      .filter(p => p.isExists)
      .map { item: GetResponse =>
        val source: Map[String, Any] = item.getSourceAsMap.asScala.toMap
        fromSearch(source, item.getId, item.getVersion)
      }
  }
}