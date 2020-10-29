package com.getjenny.starchat.services

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 01/07/16.
 */

import java.util.concurrent.ConcurrentHashMap

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.analyzer.expressions.{AnalyzersData, AnalyzersDataInternal, Context}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.analyzer.analyzers.StarChatAnalyzer
import com.getjenny.starchat.entities.io._
import com.getjenny.starchat.entities.persistents.{DecisionTableEntityManager, TextTerms}
import com.getjenny.starchat.services.esclient.DecisionTableElasticClient
import com.getjenny.starchat.services.esclient.crud.IndexLanguageCrud
import com.getjenny.starchat.utils.SystemConfiguration
import org.elasticsearch.index.query.{BoolQueryBuilder, QueryBuilders}
import scalaz.Scalaz._
import spray.json.JsObject

import scala.collection.JavaConverters._
import scala.collection.immutable.{List, Map}
import scala.collection.{concurrent, mutable}
import scala.util.{Failure, Success, Try}

case class AnalyzerServiceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

case class AnalyzerItem(declaration: String,
                        analyzer: Option[StarChatAnalyzer] = None,
                        build: Boolean,
                        message: String)

case class DecisionTableRuntimeItem(
                                     executionOrder: Int = -1,
                                     maxStateCounter: Int = -1,
                                     analyzer: AnalyzerItem = AnalyzerItem(
                                       declaration = "", analyzer = None, build = false, message = ""
                                     ),

                                     queries: List[String] = List.empty[String],
                                     queriesTerms: List[TextTerms] = List.empty[TextTerms],
                                     bubble: String = "",

                                     action: String = "",
                                     actionInput: Seq[JsObject] = Seq.empty[JsObject],
                                     stateData: Map[String, String] = Map.empty[String, String],
                                     successValue: String = "",
                                     failureValue: String = "",

                                     evaluationClass: String = "default",
                                     status: DTDocumentStatus.Value = DTDocumentStatus.VALID,
                                     timestamp: Long = -1L,
                                     version: Long = -1L
                                   )

case class ActiveAnalyzers(
                            var analyzerMap: mutable.LinkedHashMap[String, DecisionTableRuntimeItem],
                            var lastEvaluationTimestamp: Long = 0L,
                            var lastReloadingTimestamp: Long = 0L
                          )

object AnalyzerService extends AbstractDataService {
  override val elasticClient: DecisionTableElasticClient.type = DecisionTableElasticClient
  var analyzersMap: concurrent.Map[String, ActiveAnalyzers] = new ConcurrentHashMap[String, ActiveAnalyzers]().asScala
  val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)
  private[this] val termService: TermService.type = TermService
  private[this] val decisionTableService: DecisionTableService.type = DecisionTableService
  private[this] val instanceRegistryService: InstanceRegistryService.type = InstanceRegistryService
  private[this] val nodeDtLoadingStatusService: NodeDtLoadingStatusService.type = NodeDtLoadingStatusService
  val dtMaxTables: Long = elasticClient.config.getLong("es.dt_max_tables")
  private[this] val atomConfigurationBasePath = "starchat.atom-values"

  def getAnalyzers(indexName: String, complete: Boolean = false): mutable.LinkedHashMap[String, DecisionTableRuntimeItem] = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)
    val lastReloadingTimestamp = analyzersMap.get(indexName) match {
      case Some(am) => am.lastReloadingTimestamp
      case _ => 0L
    }
    val query: BoolQueryBuilder = QueryBuilders.boolQuery()

    if(! complete) { // if complete is false, take only the items modified more recently.
      query.must(QueryBuilders.rangeQuery("timestamp").gte(lastReloadingTimestamp))
    }

    val decisionTableItems = indexLanguageCrud.read(query,
      maxItems = Option(10000),
      version = Option(true),
      fetchSource = Option(Array("state", "execution_order", "max_state_counter",
        "analyzer", "queries", "bubble", "evaluation_class", "action", "action_input",
        "state_data", "success_value", "failure_value", "status", "timestamp")),
      scroll = true,
      entityManager = DecisionTableEntityManager
    )

    val analyzersData: List[(String, DecisionTableRuntimeItem)] = decisionTableItems.map {
      item =>
        val queriesTerms: List[TextTerms] = item.queries.map(q => {
          val queryTerms = termService.textToVectors(indexName, q)
          queryTerms
        }).filter(_.terms.terms.nonEmpty)

        val decisionTableRuntimeItem: DecisionTableRuntimeItem =
          DecisionTableRuntimeItem(
            executionOrder = item.executionOrder,
            maxStateCounter = item.maxStateCounter,
            analyzer = AnalyzerItem(declaration = item.analyzerDeclaration, build = false,
              message = "Analyzer index(" + indexName + ") state(" + item.state + ") not built"),
            queries = item.queries,
            bubble = item.bubble,
            action = item.action,
            actionInput = item.actionInput,
            queriesTerms = queriesTerms,
            successValue = item.successValue,
            failureValue = item.failureValue,
            evaluationClass = item.evaluationClass,
            status = item.status,
            timestamp = item.timestamp,
            version = item.version)
        (item.state, decisionTableRuntimeItem)
    }.sortWith {
      case ((_, decisionTableRuntimeItem1), (_, decisionTableRuntimeItem2)) =>
        decisionTableRuntimeItem1.executionOrder < decisionTableRuntimeItem2.executionOrder
    }

    //get a map of stateId -> AnalyzerItem (only if there is smt in the field "analyzer")
    mutable.LinkedHashMap(analyzersData: _*)
  }

  private[this] case class BuildAnalyzerResult(analyzer: Option[StarChatAnalyzer], version: Long,
                                               build: Boolean,
                                               message: String)

  def buildAnalyzers(indexName: String,
                     persistentAnalyzersMap: mutable.LinkedHashMap[String, DecisionTableRuntimeItem],
                     incremental: Boolean = true):
  mutable.LinkedHashMap[String, DecisionTableRuntimeItem] = {
    val newInPlaceIndexAnalyzers = AnalyzerService.analyzersMap.getOrElse(indexName,
      ActiveAnalyzers(mutable.LinkedHashMap.empty[String, DecisionTableRuntimeItem]))
    val newInPlaceAnalyzersMap = if(incremental) {
      newInPlaceIndexAnalyzers.analyzerMap.clone()
    } else {
      mutable.LinkedHashMap.empty[String, DecisionTableRuntimeItem]
    }
    persistentAnalyzersMap.foreach { case (stateId, runtimeItem) =>
      val analyzerDeclaration = runtimeItem.analyzer.declaration
      runtimeItem.status match {
        case DTDocumentStatus.DELETED =>
          newInPlaceAnalyzersMap.remove(stateId)
        case DTDocumentStatus.VALID =>
          val currentState: DecisionTableRuntimeItem =
            newInPlaceAnalyzersMap.getOrElse(stateId, DecisionTableRuntimeItem())
          val requireReloading: Boolean = currentState.version < 0 ||
            currentState.version =/= runtimeItem.version ||
            currentState.timestamp < runtimeItem.timestamp
          if(requireReloading) {
            val analyzerItem = if(incremental && analyzerDeclaration =/= "" &&
              currentState.analyzer.declaration === runtimeItem.analyzer.declaration) {
              log.debug("Analyzer already built index({}) state({}) version({}:{})",
                indexName, stateId, runtimeItem.version, currentState.version)
              currentState.analyzer
            } else if(analyzerDeclaration =/= "") {
              val restrictedArgs: Map[String, String] = SystemConfiguration
                .createMapFromPath(atomConfigurationBasePath) + ("index_name" -> indexName)
              Try(new StarChatAnalyzer(analyzerDeclaration, restrictedArgs)) match {
                case Success(analyzerObject) =>
                  val msg = "Analyzer successfully built index(" + indexName + ") state(" + stateId +
                    ") version(" + runtimeItem.version + ")"
                  log.debug(msg)
                  AnalyzerItem(declaration = analyzerDeclaration,
                    analyzer = Some(analyzerObject), build = true, message = msg)
                case Failure(e) =>
                  val msg = "Error building analyzer index(" + indexName + ") state(" + stateId +
                    ") declaration(" + analyzerDeclaration +
                    ") version(" + runtimeItem.version + ") : " + e.getMessage
                  log.error(msg)
                  AnalyzerItem(declaration = analyzerDeclaration, analyzer = None, build = false, message = msg)
              }
            } else {
              val msg = "index(" + indexName + ") : state(" + stateId + ") : analyzer declaration is empty"
              log.debug(msg)
              AnalyzerItem(declaration = analyzerDeclaration,
                analyzer = None, message = msg, build = true)
            }

            if(analyzerItem.build) {
              val decisionTableRuntimeItem = runtimeItem.copy(
                analyzer = analyzerItem
              )
              newInPlaceAnalyzersMap.put(stateId, decisionTableRuntimeItem)
            }
          } // else no reloading required
        case _ => throw AnalyzerServiceException(s"invalid Status: ${runtimeItem.status}")
      }
    }
    newInPlaceAnalyzersMap
  }

  def loadAnalyzers(indexName: String, incremental: Boolean = true, propagate: Boolean = false): DTAnalyzerLoad = {
    val persistentsAnalyzers = getAnalyzers(indexName = indexName, complete = ! incremental)

    val analyzerMap = buildAnalyzers(indexName = indexName,
      persistentAnalyzersMap = persistentsAnalyzers,
      incremental = incremental)

    val   dtAnalyzerLoad = DTAnalyzerLoad(numOfEntries = analyzerMap.size)
    val activeAnalyzers: ActiveAnalyzers = ActiveAnalyzers(analyzerMap = analyzerMap,
      lastReloadingTimestamp = System.currentTimeMillis)
    if (AnalyzerService.analyzersMap.contains(indexName)) {
      AnalyzerService.analyzersMap.replace(indexName, activeAnalyzers)
    } else {
      AnalyzerService.analyzersMap.putIfAbsent(indexName, activeAnalyzers)
    }

    val nodeDtLoadingTimestamp = System.currentTimeMillis()
    if (propagate) {
      Try(instanceRegistryService.updateTimestamp(indexName, nodeDtLoadingTimestamp, incremental = incremental)) match {
        case Success(dtReloadTimestamp) =>
          val ts = dtReloadTimestamp
            .getOrElse(DtReloadTimestamp(indexName, InstanceRegistryDocument.InstanceRegistryTimestampDefault))
          log.debug("setting dt reload timestamp to: {}", ts.timestamp)
          activeAnalyzers.lastReloadingTimestamp = ts.timestamp
        case Failure(e) =>
          val message = "unable to set dt reload timestamp" + e.getMessage
          log.error(message)
          throw AnalyzerServiceException(message)
      }
    }
    log.debug("updating: {} : {}", nodeDtLoadingStatusService.clusterNodesService.uuid, nodeDtLoadingTimestamp)
    nodeDtLoadingStatusService.update(dtNodeStatus =
      NodeDtLoadingStatus(index = indexName, timestamp = Some {
        nodeDtLoadingTimestamp
      }), refreshPolicy = RefreshPolicy.`wait_for`)

    dtAnalyzerLoad
  }

  //TODO: extend DTAnalyzerItem to include more informations like action, action input and version
  def getDTAnalyzerMap(indexName: String): DTAnalyzerMap = {
    DTAnalyzerMap(AnalyzerService.analyzersMap(indexName).analyzerMap
      .map {
        case (stateName, dtRuntimeItem) =>
          val dtAnalyzer =
            DTAnalyzerItem(
              dtRuntimeItem.analyzer.declaration,
              dtRuntimeItem.analyzer.build,
              dtRuntimeItem.executionOrder,
              dtRuntimeItem.evaluationClass
            )
          (stateName, dtAnalyzer)
      }.toMap)
  }

  def evaluateAnalyzer(indexName: String, analyzerRequest: AnalyzerEvaluateRequest): Option[AnalyzerEvaluateResponse] = {
    val restrictedArgs: Map[String, String] = SystemConfiguration
      .createMapFromPath(atomConfigurationBasePath) + ("index_name" -> indexName)

    Try(new StarChatAnalyzer(analyzerRequest.analyzer, restrictedArgs)) match {
      case Failure(exception) =>
        log.error("error during evaluation of analyzer: " + exception.getMessage)
        throw exception
      case Success(result) =>
        analyzerRequest.data match {
          case Some(data) =>
            // prepare search result for search analyzer
            val searchRes = decisionTableService.searchDtQueries(indexName, analyzerRequest)
            val analyzersInternalData = decisionTableService.resultsToMap(searchRes)
            val dataInternal = AnalyzersDataInternal(
              context = Context(indexName = indexName, stateName = analyzerRequest.stateName.getOrElse("playground")),
              traversedStates = data.traversedStates,
              extractedVariables = data.extractedVariables, data = analyzersInternalData)
            val evalRes = result.evaluate(analyzerRequest.query, dataInternal)
            val returnData = if (evalRes.data.extractedVariables.nonEmpty || evalRes.data.traversedStates.nonEmpty) {
              val dataInternal = evalRes.data
              Some(AnalyzersData(traversedStates = dataInternal.traversedStates,
                extractedVariables = dataInternal.extractedVariables))
            } else {
              Option.empty[AnalyzersData]
            }
            Some(AnalyzerEvaluateResponse(build = true,
              value = evalRes.score, data = returnData, buildMessage = "success"))

          case _ =>
            Some(AnalyzerEvaluateResponse(build = true,
              value = 0.0, data = Option.empty[AnalyzersData], buildMessage = "success"))
        }
    }
  }
}

