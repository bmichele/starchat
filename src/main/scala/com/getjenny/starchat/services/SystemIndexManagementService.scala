package com.getjenny.starchat.services

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 22/11/17.
 */

import java.io._

import com.getjenny.starchat.entities.io.{IndexManagementResponse, RefreshIndexResult, RefreshIndexResults}
import com.getjenny.starchat.services.esclient._
import com.getjenny.starchat.utils.Index
import com.typesafe.config.{Config, ConfigFactory}
import org.elasticsearch.action.admin.cluster.health.ClusterHealthRequest
import org.elasticsearch.action.admin.indices.delete.DeleteIndexRequest
import org.elasticsearch.action.support.master.AcknowledgedResponse
import org.elasticsearch.client.RequestOptions
import org.elasticsearch.client.indices._
import org.elasticsearch.common.settings._
import org.elasticsearch.common.xcontent.XContentType

import scala.collection.JavaConverters._
import scala.io.Source

case class SystemIndexManagementServiceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

/**
 * Implements functions, eventually used by IndexManagementResource, for ES index management
 */
object SystemIndexManagementService {
  val config: Config = ConfigFactory.load()

  private[this] def analyzerJsonIs(analyzerJsonPath: String): Option[InputStream] = Option {
    getClass.getResourceAsStream(analyzerJsonPath)
  }
  private[this] def analyzerJson(analyzerJsonPath: String): String = analyzerJsonIs(analyzerJsonPath: String) match {
    case Some(stream) => Source.fromInputStream(stream, "utf-8").mkString
    case _ =>
      val message = "Check the file: (" + analyzerJsonPath + ")"
      throw new FileNotFoundException(message)
  }

  private[this] val esClients: List[SystemElasticClient] =
    List(
      BayesOperatorCacheElasticClient,
      ClusterNodesElasticClient,
      InstanceRegistryElasticClient,
      NodeDtLoadingStatusElasticClient,
      UserElasticClient,
      CloneDtElasticClient
    )

  private[this] def filteredClients(indexSuffixes: Set[String]): List[SystemElasticClient] = {
    if (indexSuffixes.nonEmpty) {
      esClients.filter { item =>
        indexSuffixes.contains(item.indexSuffix)
      }
    } else {
      esClients
    }
  }

  def create(indexSuffix: Set[String] = Set.empty[String]): IndexManagementResponse = {
    val operationsMessage: List[(String, Boolean)] = filteredClients(indexSuffix).map(item => {
      val jsonInStream: Option[InputStream] = Option {
        getClass.getResourceAsStream(item.mappingPath)
      }

      val schemaJson = jsonInStream match {
        case Some(stream) => Source.fromInputStream(stream, "utf-8").mkString
        case _ =>
          val message = "Check the file: (" + item.mappingPath + ")"
          throw new FileNotFoundException(message)
      }

      val fullIndexName = Index.indexName(item.indexName, item.indexSuffix)

      val createIndexReq = new CreateIndexRequest(fullIndexName).settings(
        Settings.builder().loadFromSource(analyzerJson(item.analyzerJsonPath), XContentType.JSON)
          .put("index.number_of_shards", item.numberOfShards)
          .put("index.number_of_replicas", item.numberOfReplicas)
      ).source(schemaJson, XContentType.JSON)

      var createIndexRes: CreateIndexResponse = null
      try {
        createIndexRes = item.httpClient.indices.create(createIndexReq, RequestOptions.DEFAULT)
      } catch {
        case e: Throwable => e.printStackTrace()
      }
      (item.indexSuffix + "(" + fullIndexName + ", " + createIndexRes.isAcknowledged + ")",
        createIndexRes.isAcknowledged)
    })

    val message = "IndexCreation: " + operationsMessage.map { case (msg, _) => msg }.mkString(" ")
    IndexManagementResponse(message = message, check = operationsMessage.forall { case (_, ck) => ck })
  }

  def remove(indexSuffix: Set[String] = Set.empty[String]): IndexManagementResponse = {
    val operationsMessage: List[(String, Boolean)] = filteredClients(indexSuffix).map(item => {
      if (!item.enableDelete) {
        val message: String = "operation is not allowed: delete index is forbidden, contact system administrator"
        throw SystemIndexManagementServiceException(message)
      }

      val fullIndexName = Index.indexName(item.indexName, item.indexSuffix)

      val deleteIndexReq = new DeleteIndexRequest(fullIndexName)

      val deleteIndexRes: AcknowledgedResponse = item.httpClient.indices.delete(deleteIndexReq, RequestOptions.DEFAULT)

      (item.indexSuffix + "(" + fullIndexName + ", " + deleteIndexRes.isAcknowledged + ")",
        deleteIndexRes.isAcknowledged)
    })

    val message = "IndexDeletion: " + operationsMessage.map { case (msg, _) => msg }.mkString(" ")
    IndexManagementResponse(message = message, check = operationsMessage.forall { case (_, ck) => ck })
  }

  def check(indexSuffix: Set[String] = Set.empty[String]): IndexManagementResponse = {
    val operationsMessage: List[(String, Boolean)] = filteredClients(indexSuffix).map(item => {
      val fullIndexName = Index.indexName(item.indexName, item.indexSuffix)
      val getMappingsReq: GetMappingsRequest = new GetMappingsRequest()
        .indices(fullIndexName)

      val getMappingsRes: GetMappingsResponse =
        item.httpClient.indices.getMapping(getMappingsReq, RequestOptions.DEFAULT)

      val check = getMappingsRes.mappings.containsKey(fullIndexName)
      (item.indexSuffix + "(" + fullIndexName + ", " + check + ")", check)
    })

    val message = "IndexCheck: " + operationsMessage.map { case (msg, _) => msg }.mkString(" ")
    IndexManagementResponse(message = message, check = operationsMessage.forall { case (_, ck) => ck })
  }

  def update(indexSuffix: Set[String] = Set.empty[String]): IndexManagementResponse = {
    val operationsMessage: List[(String, Boolean)] = filteredClients(indexSuffix).map { item =>
      val jsonInStream: Option[InputStream] = Option {
        getClass.getResourceAsStream(item.updateMappingPath)
      }
      val schemaJson: String = jsonInStream match {
        case Some(stream) => Source.fromInputStream(stream, "utf-8").mkString
        case _ =>
          throw new FileNotFoundException(s"Check the file: (${item.updateMappingPath})")
      }

      val fullIndexName = Index.indexName(item.indexName, item.indexSuffix)

      val putMappingReq = new PutMappingRequest(fullIndexName)
        .source(schemaJson, XContentType.JSON)

      val putMappingRes: AcknowledgedResponse = item.httpClient.indices
        .putMapping(putMappingReq, RequestOptions.DEFAULT)

      val check = putMappingRes.isAcknowledged
      (item.indexSuffix + "(" + fullIndexName + ", " + check + ")", check)
    }

    val message = "IndexUpdate: " + operationsMessage.map { case (msg, _) => msg }.mkString(" ")
    IndexManagementResponse(message = message, check = operationsMessage.forall { case (_, ck) => ck })
  }

  def refresh(indexSuffix: Set[String] = Set.empty[String]): Option[RefreshIndexResults] = {
    val operationsResults: List[RefreshIndexResult] = filteredClients(indexSuffix).map(item => {
      val fullIndexName = Index.indexName(item.indexName, item.indexSuffix)
      val refreshIndexRes: RefreshIndexResult = item.refresh(fullIndexName)
      if (refreshIndexRes.failedShardsN > 0) {
        val indexRefreshMessage = item.indexSuffix + "(" + fullIndexName + ", " + refreshIndexRes.failedShardsN + ")"
        throw SystemIndexManagementServiceException(indexRefreshMessage)
      }

      refreshIndexRes
    })

    Option {
      RefreshIndexResults(results = operationsResults)
    }
  }

  def indices: List[String] = {
    val clusterHealthReq = new ClusterHealthRequest()
    clusterHealthReq.level(ClusterHealthRequest.Level.INDICES)
    val clusterHealthRes =
      ClusterNodesService.elasticClient.httpClient.cluster().health(clusterHealthReq, RequestOptions.DEFAULT)
    clusterHealthRes.getIndices.asScala.map { case (k, _) => k }.toList
  }

}
