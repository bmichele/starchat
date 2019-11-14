package com.getjenny.starchat.services

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 23/08/17.
 */

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.{DtReloadTimestamp, IndexManagementResponse}
import com.getjenny.starchat.services.esclient.SystemIndexManagementElasticClient
import com.getjenny.starchat.services.esclient.crud.EsCrudBase
import com.getjenny.starchat.utils.Index
import org.elasticsearch.action.update.UpdateResponse
import org.elasticsearch.common.xcontent.XContentBuilder
import org.elasticsearch.common.xcontent.XContentFactory._
import org.elasticsearch.index.query.{BoolQueryBuilder, QueryBuilders}
import org.elasticsearch.search.SearchHit
import org.elasticsearch.search.sort.{FieldSortBuilder, SortOrder}
import scalaz.Scalaz._
import org.elasticsearch.common.Strings
import scala.collection.JavaConverters._
import scala.collection.concurrent.TrieMap
import scala.collection.immutable.Map
import scala.util.{Failure, Success, Try}

case class InstanceRegistryDocument(timestamp: Option[Long] = None,
                                    enabled: Option[Boolean] = None,
                                    delete: Option[Boolean] = None,
                                    deleted: Option[Boolean] = None) {
  def builder: XContentBuilder = {
    val builder = jsonBuilder().startObject()
    timestamp.foreach(t => builder.field("timestamp", t))
    enabled.foreach(e => builder.field("enabled", e))
    delete.foreach(d => builder.field("delete", d))
    deleted.foreach(d => builder.field("deleted", d))
    builder.endObject()
  }

  def isEmpty: Boolean = {
    deleted.getOrElse(false) || timestamp.isEmpty && enabled.isEmpty && delete.isEmpty
  }
}

object InstanceRegistryDocument {
  val InstanceRegistryTimestampDefault: Long = 0

  def apply(source: Map[String, Any]): InstanceRegistryDocument = {
    val timestamp = source.get("timestamp") match {
      case Some(value: Long) => value
      case Some(value: Int) => value.toLong
      case _ => InstanceRegistryTimestampDefault
    }
    val enabled = source.get("enabled").map(_.asInstanceOf[Boolean])
    val delete = source.get("delete").map(_.asInstanceOf[Boolean])
    val isDeleted = source.get("deleted").map(_.asInstanceOf[Boolean])
    InstanceRegistryDocument(Option(timestamp), enabled, delete, isDeleted)
  }

  def empty: InstanceRegistryDocument = {
    InstanceRegistryDocument()
  }
}

object InstanceRegistryService extends AbstractDataService {
  override val elasticClient: SystemIndexManagementElasticClient.type = SystemIndexManagementElasticClient
  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)
  private[this] val InstanceRegistryIndex: String = Index.indexName(elasticClient.indexName,
    elasticClient.systemInstanceRegistrySuffix)
  val esCrudBase = new EsCrudBase(elasticClient, InstanceRegistryIndex)

  val cache = new TrieMap[String, InstanceRegistryDocument]()

  def updateTimestamp(dtIndexName: String, timestamp: Long = InstanceRegistryDocument.InstanceRegistryTimestampDefault,
                      refresh: Int = 0): Option[DtReloadTimestamp] = {
    val ts: Long = if (timestamp === InstanceRegistryDocument.InstanceRegistryTimestampDefault) System.currentTimeMillis else timestamp

    val response = updateInstance(dtIndexName, timestamp = Option(ts), enabled = None, delete = None, deleted = None)

    log.debug("dt reload timestamp response status: {}", response.status())

    if (refresh =/= 0) {
      val refreshIndex = esCrudBase.refresh()
      if (refreshIndex.failedShardsN > 0) {
        throw new Exception("System: index refresh failed: (" + InstanceRegistryIndex + ", " + dtIndexName + ")")
      }
    }
    Option {
      DtReloadTimestamp(indexName = InstanceRegistryIndex, timestamp = ts)
    }
  }

  def addInstance(indexName: String): IndexManagementResponse = {
    if(!isValidIndexName(indexName))
      throw new IllegalArgumentException(s"Index name $indexName is not a valid index to be used with instanceRegistry")

    val document = InstanceRegistryDocument(
      timestamp = Option(InstanceRegistryDocument.InstanceRegistryTimestampDefault),
      enabled = Option(true)
    )
    val response = esCrudBase.create(indexName, document.builder)
    cache.put(indexName, document)
    IndexManagementResponse(s"Created instance $indexName, operation status: ${response.status}", check = true)
  }

  def getInstance(indexName: String): InstanceRegistryDocument = {
    if(!isValidIndexName(indexName))
      throw new IllegalArgumentException(s"Index name $indexName is not a valid index to be used with instanceRegistry")

    cache.getOrElseUpdate(indexName, findEsLanguageIndex(indexName))
  }

  def checkInstance(indexName: String): IndexManagementResponse = {
    esCrudBase.refresh()
    val document = findEsLanguageIndex(indexName)
    IndexManagementResponse(message = s"IndexCheck for entry: $indexName", !document.isEmpty)
  }

  def disableInstance(indexName: String): IndexManagementResponse = {
    val response = updateInstance(indexName, timestamp = None, enabled = Option(false), delete = None, deleted = None)
    IndexManagementResponse(s"Disabled instance $indexName, operation status: ${response.status}", check = true)
  }

  def markDeleteInstance(indexName: String): IndexManagementResponse = {
    val response = updateInstance(indexName, timestamp = None, enabled = Option(false), delete = Option(true), None)
    IndexManagementResponse(s"Mark Delete instance $indexName, operation status: ${response.status}", check = true)
  }

  private[this] def updateInstance(indexName: String, timestamp: Option[Long],
                                   enabled: Option[Boolean],
                                   delete: Option[Boolean],
                                   deleted: Option[Boolean]): UpdateResponse = {
    if(!isValidIndexName(indexName))
      throw new IllegalArgumentException(s"Index name $indexName is not a valid index to be used with instanceRegistry")

    val toBeUpdated = InstanceRegistryDocument(timestamp =  timestamp , enabled = enabled,
      delete = delete,
      deleted = deleted)
    val response = esCrudBase.update(indexName, toBeUpdated.builder)
    esCrudBase.refresh()

    val updatedDocument = findEsLanguageIndex(indexName)
    log.debug("Updated instance {} with document: {}", indexName, updatedDocument)
    cache.put(indexName, updatedDocument)
    response
  }

  private[this] def findEsLanguageIndex(dtIndexName: String): InstanceRegistryDocument = Try {
    esCrudBase.read(dtIndexName)
  } match {
    case Success(response) => if (!response.isExists || response.isSourceEmpty) {
      InstanceRegistryDocument.empty
    } else {
      val document = InstanceRegistryDocument(response.getSource.asScala.toMap)
      document.deleted match {
        case Some(true) => InstanceRegistryDocument.empty
        case _ => document
      }
    }
    case Failure(e) =>
      log.error(e, "Error while reading from instance-registry - id: {}", dtIndexName)
      InstanceRegistryDocument.empty
  }

  def instanceTimestamp(dtIndexName: String): DtReloadTimestamp = {
    val document = getInstance(dtIndexName)

    DtReloadTimestamp(indexName = dtIndexName, timestamp = document.timestamp
      .getOrElse(InstanceRegistryDocument.InstanceRegistryTimestampDefault))
  }

  def deleteEntry(ids: List[String]): Unit = {
    ids.foreach(entry => updateInstance(entry, None, None, None, deleted = Option(true)))
    ids.foreach(cache.remove)
  }

  def getAll: List[(String, InstanceRegistryDocument)] = {
    val response = esCrudBase.read(QueryBuilders.matchAllQuery())

    response.getHits.getHits.map { x =>
      x.getId -> InstanceRegistryDocument(x.getSourceAsMap.asScala.toMap)
    }.toList
  }

  def allInstanceTimestamp(minTimestamp: Option[Long] = None,
                           maxItems: Option[Long] = None): List[DtReloadTimestamp] = {
    val boolQueryBuilder: BoolQueryBuilder = QueryBuilders.boolQuery()
    minTimestamp match {
      case Some(minTs) => boolQueryBuilder.filter(
        QueryBuilders.rangeQuery(elasticClient.dtReloadTimestampFieldName).gt(minTs))
      case _ => ;
    }

    val scrollResp = esCrudBase.read(boolQueryBuilder,
      maxItems = maxItems.orElse(Option(100L)).map(_.toInt),
      version = Option(true),
      sort = List(new FieldSortBuilder(elasticClient.dtReloadTimestampFieldName).order(SortOrder.DESC))
    )

    val dtReloadTimestamps: List[DtReloadTimestamp] = scrollResp.getHits.getHits.toList
      .map { timestampEntry =>
        val item: SearchHit = timestampEntry
        val docId: String = item.getId // the id is the index name

        val document = InstanceRegistryDocument(item.getSourceAsMap.asScala.toMap)
        DtReloadTimestamp(docId, document.timestamp
          .getOrElse(InstanceRegistryDocument.InstanceRegistryTimestampDefault))
      }
    dtReloadTimestamps
  }

  def isValidIndexName(indexName: String): Boolean = indexName match {
      case Index.fullInstanceIndex(_) => true
      case _ => false
  }

}
