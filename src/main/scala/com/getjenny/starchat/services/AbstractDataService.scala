package com.getjenny.starchat.services

import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.io._
import com.getjenny.starchat.services.esclient.ElasticClient
import com.getjenny.starchat.utils.Index
import org.elasticsearch.action.bulk.{BulkRequest, BulkResponse}
import org.elasticsearch.action.delete.DeleteRequest
import org.elasticsearch.client.{RequestOptions, RestHighLevelClient}
import org.elasticsearch.index.query.QueryBuilders
import org.elasticsearch.index.reindex.DeleteByQueryRequest
import org.elasticsearch.rest.RestStatus
import scalaz.Scalaz._

import scala.collection.immutable.List
import scala.concurrent.ExecutionContext

case class DeleteDataServiceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

trait AbstractDataService {
  implicit def executionContext: ExecutionContext = SCActorSystem.system.dispatchers.lookup("starchat.dispatcher")

  protected[this] val elasticClient: ElasticClient

  /** delete all the terms in a table
   *
   * @param indexName index name
   * @param refreshPolicy whether to call an index update on ElasticSearch or not
   * @return a DeleteDocumentsResult with the status of the delete operation
   */
  def deleteAll(indexName: String, refreshPolicy: RefreshPolicy.Value): DeleteDocumentsSummaryResult = {
    val client: RestHighLevelClient = elasticClient.httpClient
    val esLanguageSpecificIndexName =
      Index.esLanguageFromIndexName(indexName, elasticClient.indexSuffix) //FIXME: refactor class hierarchy
    val request: DeleteByQueryRequest =
      new DeleteByQueryRequest(esLanguageSpecificIndexName)
    request.setConflicts("proceed")
    request.setQuery(QueryBuilders.matchAllQuery)

    refreshPolicy match {
      case RefreshPolicy.`1` | RefreshPolicy.`wait_for` | RefreshPolicy.`true` => request.setRefresh("true".toBoolean)
      case _ => request.setRefresh("false".toBoolean)
    }

    val bulkResponse = client.deleteByQuery(request, RequestOptions.DEFAULT)

    DeleteDocumentsSummaryResult(message = "delete", deleted = bulkResponse.getTotal)
  }

  /** delete one or more terms
   *
   * @param indexName index name
   * @param ids the list of term ids to delete
   * @param refreshPolicy whether to call an index update on ElasticSearch or not
   * @return DeleteDocumentListResult with the result of term delete operations
   */
  def delete(indexName: String, ids: List[String], refreshPolicy: RefreshPolicy.Value): DeleteDocumentsResult = {
    val client: RestHighLevelClient = elasticClient.httpClient
    val bulkReq: BulkRequest = new BulkRequest()

    ids.foreach( id => {
      val deleteReq = new DeleteRequest()
        .index(indexName)
        .id(id)
      bulkReq.add(deleteReq)
    })

    val bulkRes: BulkResponse = client.bulk(bulkReq, RequestOptions.DEFAULT)

    refreshPolicy match {
      case RefreshPolicy.`0` => bulkReq.setRefreshPolicy("false")
      case RefreshPolicy.`1` => bulkReq.setRefreshPolicy("true")
      case _ => bulkReq.setRefreshPolicy(refreshPolicy.toString)
    }

    val listOfDocRes: List[DeleteDocumentResult] = bulkRes.getItems.map(x => {
      DeleteDocumentResult(x.getIndex, x.getId, x.getVersion, x.status =/= RestStatus.NOT_FOUND)
    }).toList

    DeleteDocumentsResult(data=listOfDocRes)
  }

  protected[this] def refresh: RefreshIndexResult = {
    elasticClient.refresh(Index.indexName(elasticClient.indexName, elasticClient.indexSuffix))
  }

  protected[this] def refresh(refreshPolicy: RefreshPolicy.Value): RefreshIndexResult = {
    refreshPolicy match {
      case RefreshPolicy.`1` | RefreshPolicy.`wait_for` | RefreshPolicy.`true` =>
        refresh
      case _ =>
    }
    elasticClient.refresh(Index.indexName(elasticClient.indexName, elasticClient.indexSuffix))
  }
}
