package com.getjenny.starchat.services

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 01/07/16.
 */

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.analyzer.analyzers._
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Context, Result}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.io._
import com.getjenny.starchat.entities.persistents.DTDocumentCreate
import com.getjenny.starchat.services.actions._
import com.getjenny.starchat.services.esclient.DecisionTableElasticClient
import scalaz.Scalaz._

import scala.collection.immutable
import scala.collection.immutable.Map
import scala.util.{Failure, Success, Try}

case class ResponseServiceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

case class ResponseServiceDocumentNotFoundException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

case class ResponseServiceNoResponseException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

case class ResponseServiceDTNotLoadedException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

/**
 * Implements response functionalities
 */
object ResponseService extends AbstractDataService {
  override val elasticClient: DecisionTableElasticClient.type = DecisionTableElasticClient
  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)
  private[this] val decisionTableService: DecisionTableService.type = DecisionTableService

  private[this] def executeAction(indexName: String, document: ResponseRequestOut, query: String, request: ResponseRequestIn): List[ResponseRequestOut] = {
    val isAnalyzerInAction = document.action.startsWith(DtAction.analyzerActionPrefix)
    if (document.action.startsWith(DtAction.actionPrefix) || isAnalyzerInAction) {
      val res = DtAction(indexName, document.state, document.action, document.actionInput, query)

      // error in execution and negative results lead both to failureValue
      val state = if (res.success && res.code === 0)
        document.successValue
      else
        document.failureValue

      if(state.isEmpty || state === document.state) { // avoiding recursive state fetch.
        List(document)
      } else {
        getNextResponse(indexName,
          request.copy(
            traversedStates = Some(document.traversedStates),
            userInput = None,
            data = Option(res.data),
            threshold = Option(0D),
            evaluationClass = None,
            maxResults = None,
            state = Some(List(state))
          )
        ).responseRequestOut.getOrElse(List.empty)
      }
    } else {
      List(document)
    }
  }

  def getNextResponse(indexName: String, request: ResponseRequestIn): ResponseRequestOutOperationResult = {

    val evaluationClass = request.evaluationClass match {
      case Some(c) => c
      case _ => "default"
    }

    if (!AnalyzerService.analyzersMap.contains(indexName)) {
      val message = "Decision table not ready for index(" + indexName + "), please retry later"
      log.debug(message)
      throw ResponseServiceDTNotLoadedException(message)
    }

    val userText: String = request.userInput match {
      case Some(t) =>
        t.text.getOrElse("")
      case _ => ""
    }

    val conversationId: String = request.conversationId

    val variables: Map[String, String] = request.data.getOrElse(Map.empty[String, String])

    val traversedStates: Vector[String] = request.traversedStates.getOrElse(Vector.empty[String])
    val traversedStatesCount: Map[String, Int] =
      traversedStates.foldLeft(Map.empty[String, Int])((map, word) => map + (word -> (map.getOrElse(word, 0) + 1)))

    // refresh last_used timestamp
    Try(AnalyzerService.analyzersMap(indexName).lastEvaluationTimestamp = System.currentTimeMillis) match {
      case Success(_) => ;
      case Failure(e) =>
        val message = "could not update the last evaluation timestamp for: " + indexName
        log.error(message)
        throw ResponseServiceException(message, e)
    }

    // prepare search result for search analyzer
    val analyzerEvaluateRequest = AnalyzerEvaluateRequest(analyzer = "", query = userText, data = None,
      searchAlgorithm = request.searchAlgorithm, evaluationClass = request.evaluationClass)
    val searchResult = decisionTableService.searchDtQueries(indexName, analyzerEvaluateRequest)
    val analyzersInternalData = decisionTableService.resultsToMap(searchResult)
    val searchResAnalyzers = AnalyzersDataInternal(
      context = Context(indexName = indexName, stateName = ""), // stateName is not important here
      extractedVariables = variables,
      traversedStates = traversedStates,
      data = analyzersInternalData)


    val (evaluationList, reqState) = request.state match {
      case Some(states) =>
        (
          states.map { state =>
            (state,
              AnalyzerService.analyzersMap(indexName).analyzerMap.get(state) match {
                case Some(v) => v
                case _ =>
                  throw ResponseServiceDocumentNotFoundException("Requested state not found: state(" + state + ")")
              }
            )
          }, true
        )
      case _ =>
        (AnalyzerService.analyzersMap(indexName).analyzerMap.toList, false)
    }
    val maxResults: Int = request.maxResults.getOrElse(2)
    val threshold: Double = request.threshold.getOrElse(elasticClient.getNextResponseThreshold)

    // attach search res analyzer result to the states
    val analyzersEvalData: Map[String, Result] = evaluationList
      .filter { case (_, runtimeAnalyzerItem) => runtimeAnalyzerItem.evaluationClass === evaluationClass }
      .filter { case (_, runtimeAnalyzerItem) => runtimeAnalyzerItem.analyzer.build === true }
      .filter { case (stateName, runtimeAnalyzerItem) =>
        val traversedStateCount = traversedStatesCount.getOrElse(stateName, 0)
        val maxStateCount = runtimeAnalyzerItem.maxStateCounter
        maxStateCount === 0 ||
          traversedStateCount < maxStateCount // skip states already evaluated too much times
      }.map { case (stateName, runtimeAnalyzerItem) =>
      val analyzerEvaluation = runtimeAnalyzerItem.analyzer.analyzer match {
        case Some(starchatAnalyzer) =>
          val analyzersDataInternal: AnalyzersDataInternal = searchResAnalyzers.copy(
            context = Context(indexName = indexName, stateName = stateName)
          )
          Try(starchatAnalyzer.evaluate(userText, data = analyzersDataInternal)) match {
            case Success(evalRes: Result) =>
              log.debug("ResponseService: Evaluation of State({}) Query({}) Score({})",
                stateName, userText, evalRes.toString)
              evalRes
            case Failure(e) =>
              val message = "ResponseService: Evaluation of State(" + stateName + ") : " + e.getMessage
              log.error(message)
              throw AnalyzerEvaluationException(message, e)
          }
        case _ =>
          log.debug("ResponseService: analyzer is None ({})", stateName)
          val score = if (reqState) threshold else 0.0 // if requested the threshold is matched by default
          Result(score = score, data = searchResAnalyzers)
      }
      val stateId = stateName
      (stateId, analyzerEvaluation)
    }.filter { case (_, analyzerEvaluation) => analyzerEvaluation.score >= threshold }
      .sortWith {
        case ((_, analyzerEvaluation1), (_, analyzerEvaluation2)) =>
          analyzerEvaluation1.score > analyzerEvaluation2.score
      }.take(maxResults).toMap

    if (analyzersEvalData.isEmpty) {
      throw ResponseServiceNoResponseException(
        "The analyzers evaluation list is empty, threshold could be too high")
    }

    val docResults = decisionTableService.read(indexName, analyzersEvalData.keys.toList)
    val dtDocumentsList = docResults.hits.par.map { item =>
      val doc: DTDocumentCreate = item.document
      val state = doc.state
      val evaluationRes: Result = analyzersEvalData(state)
      val maxStateCount: Int = doc.maxStateCount
      val analyzer: String = doc.analyzer
      val action: String = doc.action
      val stateData: Map[String, String] = doc.stateData

      val merged = searchResAnalyzers.extractedVariables ++ evaluationRes.data.extractedVariables
      val bubble = replaceBubble(doc.bubble, merged)
      val actionInput = replaceActionInput(doc.actionInput, merged)
      val cleanedData = merged.filter { case (key, _) => !(key matches "\\A__temp__.*") }

      val traversedStatesUpdated: Vector[String] = traversedStates ++ Vector(state)
      val responseItem = ResponseRequestOut(conversationId = conversationId,
        state = state,
        maxStateCount = maxStateCount,
        traversedStates = traversedStatesUpdated,
        analyzer = analyzer,
        bubble = bubble,
        action = action,
        data = cleanedData,
        actionInput = actionInput,
        stateData = stateData,
        successValue = doc.successValue,
        failureValue = doc.failureValue,
        score = evaluationRes.score)
      responseItem
    }.toList
      .sortWith(_.score > _.score)
      .flatMap { document =>
        executeAction(indexName, document = document, userText, request)
      }

    if (dtDocumentsList.isEmpty) {
      throw ResponseServiceNoResponseException(
        "The analyzers evaluation list is empty, it is possible that no states were found for the evaluationClass")
    }

    ResponseRequestOutOperationResult(ReturnMessageData(200, ""),
      Option {
        dtDocumentsList
      })

  }

  private[this] def replaceBubble(inputBubble: String, values: Map[String, String]): String = {
    values.foldLeft(randomizeBubble(inputBubble)) {
      case (b, (k, v)) => b.replaceAll("%" + k + "%", v)
    }
  }

  private[this] def randomizeBubble(bubble: String): String = {
    val splittedBubble = bubble.split('|')
    if (splittedBubble.length > 1) {
      val r = new scala.util.Random
      val randomIdx = r.nextInt(splittedBubble.length)
      splittedBubble(randomIdx)
    } else {
      bubble
    }
  }

  private[this] def replaceActionInput(actionInput: Map[String, String], values: Map[String, String]): Map[String, String] = {
    values.foldLeft(actionInput) {
      case (acc, (key, value)) => acc.mapValues(_.replaceAll("%" + key + "%", value))
    }
  }

}
