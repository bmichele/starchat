package com.getjenny.starchat.resources

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 27/06/16.
  */

import akka.http.scaladsl.server.Route
import com.getjenny.starchat.routing._
import com.getjenny.starchat.services.{PriorDataService, QuestionAnswerService}

trait PriorDataResource extends StarChatResource {
  private[this] val questionAnswerService: QuestionAnswerService = PriorDataService
  private[this] val routeName: String = "prior_data"
  private[this] val qaResource = new QAResource(questionAnswerService, routeName)

  def pdTermsCountRoutes: Route = qaResource.termsCountRoutes

  def pdDictSizeRoutes: Route = qaResource.dictSizeRoutes

  def pdTotalTermsRoutes: Route = qaResource.totalTermsRoutes

  def pdQuestionAnswerStreamRoutes: Route = qaResource.questionAnswerStreamRoutes

  def pdQuestionAnswerRoutes: Route = qaResource.questionAnswerRoutes

  def pdQuestionAnswerSearchRoutes: Route = qaResource.questionAnswerSearchRoutes

  def pdUpdateTermsRoutes: Route = qaResource.updateTermsRoutes

  def pdCountersCacheSizeRoutes: Route = qaResource.countersCacheSizeRoutes

  def pdQuestionAnswerConversationsRoutes: Route = qaResource.questionAnswerConversationsRoutes

  def pdQuestionAnswerAnalyticsRoutes: Route = qaResource.questionAnswerAnalyticsRoutes

  def pdAnnotationsRoutes: Route = qaResource.annotationsRoutes
}
