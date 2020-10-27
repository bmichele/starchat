package com.getjenny.starchat

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 27/06/16.
 */

import akka.http.scaladsl.server.Route
import com.getjenny.starchat.resources._
import com.getjenny.starchat.services.{CronCleanDeletedDecisionTableStatesService, _}

import scala.concurrent.ExecutionContext

trait RestInterface extends RootAPIResource
  with SystemIndexManagementResource with IndexManagementResource
  with LanguageGuesserResource
  with TermResource with TokenizersResource
  with DecisionTableResource with AnalyzersPlaygroundResource with TermsExtractionResource
  with SpellcheckResource
  with SpellcheckResource2
  with KnowledgeBaseResource with ConversationLogsResource with PriorDataResource
  with UserResource with NodeDtLoadingStatusResource with ClusterNodesResource with LanguageIndexManagementResource {

  implicit def executionContext: ExecutionContext

  lazy val decisionTableService: DecisionTableService.type = DecisionTableService
  lazy val indexManagementService: LanguageIndexManagementService.type = LanguageIndexManagementService
  lazy val systemIndexManagementService: SystemIndexManagementService.type = SystemIndexManagementService
  lazy val languageGuesserService: LanguageGuesserService.type = LanguageGuesserService
  lazy val termService: TermService.type = TermService
  lazy val responseService: ResponseService.type = ResponseService
  lazy val analyzerService: AnalyzerService.type = AnalyzerService
  lazy val userService: UserService.type = UserService
  lazy val spellcheckService: SpellcheckService.type = SpellcheckService
  lazy val spellcheckService2: SpellcheckService2.type = SpellcheckService2
  lazy val clusterNodesServices: ClusterNodesService.type = ClusterNodesService
  lazy val nodeDtLoadingStatusService: NodeDtLoadingStatusService.type = NodeDtLoadingStatusService
  lazy val cronReloadDTService: CronReloadDTService.type = CronReloadDTService
  lazy val cronCleanDTService: CronCleanInMemoryDTService.type = CronCleanInMemoryDTService
  lazy val cronCleanDeadNodesService: CronCleanDeadNodesService.type = CronCleanDeadNodesService
  lazy val cronNodeAliveSignalService: CronNodeAliveSignalService.type = CronNodeAliveSignalService
  lazy val cronCleanDtLoadingRecordsService: CronCleanDtLoadingRecordsService.type = CronCleanDtLoadingRecordsService
  lazy val cronCleanDeletedDecisionTableStatesService: CronCleanDeletedDecisionTableStatesService.type = CronCleanDeletedDecisionTableStatesService
  lazy val cronCleanQuestionFieldOnStalesLogsService: CronCleanQuestionFieldOnStalesLogsService.type = CronCleanQuestionFieldOnStalesLogsService
  lazy val cronDeleteInstanceService: CronDeleteInstanceService.type = CronDeleteInstanceService
  lazy val cronInitializeSystemIndicesService: CronInitializeSystemIndicesService.type = CronInitializeSystemIndicesService
  lazy val systemService: InstanceRegistryService.type = InstanceRegistryService
  lazy val knowledgeBaseService: KnowledgeBaseService.type = KnowledgeBaseService
  lazy val conversationLogsService: ConversationLogsService.type = ConversationLogsService
  lazy val priorDataService: PriorDataService.type = PriorDataService

  val routes: Route = rootAPIsRoutes ~
    LoggingEntities.logRequestAndResult(analyzersPlaygroundRoutes) ~
    LoggingEntities.logRequestAndResult(delUserRoutes) ~
    LoggingEntities.logRequestAndResult(genUserRoutes) ~
    LoggingEntities.logRequestAndResult(getUserRoutes) ~
    LoggingEntities.logRequestAndResult(languageGuesserRoutes) ~
    LoggingEntities.logRequestAndResult(postUserRoutes) ~
    LoggingEntities.logRequestAndResult(putUserRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clAnnotationsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clAnonymConfigRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clCountersCacheSizeRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clDictSizeRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clQuestionAnswerAnalyticsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clQuestionAnswerConversationsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clQuestionAnswerRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clQuestionAnswerSearchRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clQuestionAnswerStreamRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clTermsCountRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clTotalTermsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clUpdateRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clUpdateTermsRoutes)~
    LoggingEntities.logRequestAndResultReduced(clusterNodesRoutes) ~
    LoggingEntities.logRequestAndResultReduced(decisionTableAnalyzerRoutes) ~
    LoggingEntities.logRequestAndResultReduced(decisionTableAsyncReloadRoutes) ~
    LoggingEntities.logRequestAndResultReduced(decisionTableBulkCreateRoutes) ~
    LoggingEntities.logRequestAndResultReduced(decisionTableBulkUploadAndMultiCreateRoutes) ~
    LoggingEntities.logRequestAndResultReduced(decisionTableCacheLoad) ~
    LoggingEntities.logRequestAndResultReduced(decisionTableCloneIndexRoutes) ~
    LoggingEntities.logRequestAndResultReduced(decisionTableResponseRequestRoutes) ~
    LoggingEntities.logRequestAndResultReduced(decisionTableRoutes) ~
    LoggingEntities.logRequestAndResultReduced(decisionTableRoutesAllRoutes) ~
    LoggingEntities.logRequestAndResultReduced(decisionTableRoutesBulkDeleteRoutes) ~
    LoggingEntities.logRequestAndResultReduced(decisionTableSearchRoutes) ~
    LoggingEntities.logRequestAndResultReduced(decisionTableUploadFilesRoutes) ~
    LoggingEntities.logRequestAndResultReduced(esTokenizersRoutes) ~
    LoggingEntities.logRequestAndResultReduced(freqExtractionRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbAnnotationsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbAnonymConfigRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbCountersCacheSizeRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbDictSizeRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbQuestionAnswerAnalyticsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbQuestionAnswerConversationsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbQuestionAnswerRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbQuestionAnswerSearchRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbQuestionAnswerStreamRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbTermsCountRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbTotalTermsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbUpdateRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbUpdateTermsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(languageIndexManagementRoutes) ~
    LoggingEntities.logRequestAndResultReduced(nodeDtLoadingStatusRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdAnnotationsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdAnonymConfigRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdCountersCacheSizeRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdDictSizeRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdQuestionAnswerAnalyticsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdQuestionAnswerConversationsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdQuestionAnswerRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdQuestionAnswerSearchRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdQuestionAnswerStreamRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdTermsCountRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdTotalTermsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdUpdateRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdUpdateTermsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(postIndexManagementCreateRoutes) ~
    LoggingEntities.logRequestAndResultReduced(spellcheckRoutes) ~
    LoggingEntities.logRequestAndResultReduced(spellcheckRoutes2) ~
    LoggingEntities.logRequestAndResultReduced(synExtractionRoutes) ~
    LoggingEntities.logRequestAndResultReduced(systemGetIndexesRoutes) ~
    LoggingEntities.logRequestAndResultReduced(systemIndexManagementRoutes) ~
    LoggingEntities.logRequestAndResultReduced(termRoutes) ~
    LoggingEntities.logRequestAndResultReduced(termStreamRoutes) ~
    LoggingEntities.logRequestAndResultReduced(termsExtractionRoutes)
}
