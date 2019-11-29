package com.getjenny.starchat.services

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Context}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.analyzer.analyzers.StarChatAnalyzer
import com.getjenny.starchat.analyzer.operators.BayesOperator
import com.getjenny.starchat.entities.io.BayesOperatorCacheServiceResponse
import com.getjenny.starchat.services.esclient.crud.EsCrudBase
import com.getjenny.starchat.services.esclient.{BayesOperatorCacheElasticClient, ElasticClient}
import com.getjenny.starchat.utils.Index
import org.elasticsearch.action.get.GetResponse
import org.elasticsearch.common.xcontent.XContentBuilder
import org.elasticsearch.common.xcontent.XContentFactory._
import org.elasticsearch.index.query.QueryBuilders
import scalaz.Scalaz._
import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class BayesOperatorCacheDocument(key: String, value: Option[Double]) {
  def toBuilder: XContentBuilder = {
    jsonBuilder()
      .startObject()
      .field("value", value.getOrElse(0d))
      .endObject()
  }
}

object BayesOperatorCacheDocument {
  def apply(response: GetResponse): BayesOperatorCacheDocument = {
    new BayesOperatorCacheDocument(
      response.getId,
      response.getSourceAsMap.asScala.get("value").map(_.asInstanceOf[Double])
    )
  }
}

object BayesOperatorCacheService extends AbstractDataService {
  override protected[this] val elasticClient: ElasticClient = BayesOperatorCacheElasticClient
  private[this] val indexName = Index.indexName(elasticClient.indexName, elasticClient.indexSuffix)
  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)
  private[this] val esCrudBase = new EsCrudBase(elasticClient, indexName)
  private[this] val bayesOperator = "BayesOperator"

  def loadAsync(indexName: String): BayesOperatorCacheServiceResponse = {
    Future(performLoad(indexName))
      .onComplete {
        handleLoadResponse(indexName, _)
      }
    BayesOperatorCacheServiceResponse(indexName, status = true, "Loading async")
  }

  def load(indexName: String): BayesOperatorCacheServiceResponse = {
    handleLoadResponse(indexName, Try {
      performLoad(indexName)
    })
  }

  private[this] def handleLoadResponse(indexName: String, response: Try[Int]): BayesOperatorCacheServiceResponse = response match {
    case Failure(exception) =>
      log.error(exception, "Error during load bayes operator cache:")
      BayesOperatorCacheServiceResponse(indexName, status = false, s"Error while loading cache ${exception.getMessage}")
    case Success(value) =>
      BayesOperatorCacheServiceResponse(indexName, status = true, s"Loaded $value items in cache")
  }

  private[this] def performLoad(indexName: String): Int = {
    val analyzerMap = AnalyzerService.analyzersMap
      .getOrElse(indexName, throw new IllegalArgumentException(s"No analyzer map found for index: $indexName"))
      .analyzerMap
      .toMap

    val analyzers = analyzerMap
      .map { case (state, decisionTableRuntimeItem) => state -> decisionTableRuntimeItem.analyzer.analyzer }
      .collect { case (state, Some(analyzer)) => state -> analyzer }
      .toMap

    val allQueries = analyzerMap.values
      .flatMap { x => x.queries }

    val allBayesOperators = analyzers
      .map { case (state, analyzer: StarChatAnalyzer) =>
        state -> analyzer.firstOccurrenceOfOperator(bayesOperator)
      }.collect { case (state, Some(bayes)) => state -> bayes }

    val scores = allQueries.par
      .flatMap(q =>
        allBayesOperators
          .map { case (state, analyzer) =>
            (indexName, state) -> analyzer.evaluate(q, AnalyzersDataInternal(Context(indexName, state))).score
          }
          .filter { case (_, analyzer) => analyzer =/= 0 }
      ).toMap
      .toList

    scores.map { case ((indexName, state), analyzer) => (indexName, state, analyzer) }
      .grouped(1000)
      .foreach(x => bulkPut(x))

    log.info("Calculated {} scores", scores.length)
    scores.length
  }

  def put(index: String, state: String, value: Double): Unit = {
    val key = createKey(index, state)
    val document = BayesOperatorCacheDocument(key, Some(value))
    val response = esCrudBase.update(document.key, document.toBuilder, upsert = true)
    log.info("BayesOperatorCache put - key: {}, value: {}, operation status: {}", key, value, response.status())
  }

  def bulkPut(scoresList: List[(String, String, Double)]): Unit = {
    val elements = scoresList.map { case (index, state, v) => val key = createKey(index, state)
      key -> BayesOperatorCacheDocument(key, Some(v)).toBuilder
    }
    val response = esCrudBase.bulkUpdate(elements, upsert = true)
    log.info("BayesOperatorCache bulk put {} elements, operation status: {}", response.getItems.length, response.status())
  }

  def get(index: String, state: String): Option[Double] = {
    val key = createKey(index, state)
    val response = esCrudBase.read(key)

    if (response.isExists) {
      BayesOperatorCacheDocument(response).value
    } else {
      None
    }
  }

  def getOrElseUpdate(index: String, state: String)(updateFunction: () => Double): Double = {
    this.get(index, state).getOrElse {
      val value = updateFunction()
      this.put(index, state, value)
      value
    }
  }

  private[this] def createKey(index: String, state: String) = s"$index-$state"

  def refresh(): Unit = {
    esCrudBase.refresh()
  }

  def clear(): Unit = {
    val response = esCrudBase.delete(QueryBuilders.matchAllQuery())
    log.info("BayesOperatorCache cleared {} entries", response.getDeleted)
  }

}
