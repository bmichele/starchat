package com.getjenny.starchat.services

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 07/10/20.
 */

import akka.actor.{Actor, Props}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.io.{QADocumentSearch, RefreshPolicy}
import com.getjenny.starchat.entities.persistents.QADocumentCore
import com.getjenny.starchat.services.esclient.ConversationLogsElasticClient
import scalaz.Scalaz._

import scala.concurrent.duration._
import scala.language.postfixOps

object CronCleanQuestionFieldOnStalesLogsService extends CronService {
  class CleanQuestionFieldsTickActor extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case `tickMessage` =>
        instanceRegistryService.getAll.foreach { case (instance, entry) =>
          entry.staleAge match {
            case Some(age) =>
              if (age =/= 0L &&
                ! entry.delete.getOrElse(true) &&
                entry.enabled.getOrElse(false)) {
                val staleAge = System.currentTimeMillis() - (age * 1000)
                val result = conversationLogsService.clearQuestionField(indexName = instance,
                  documentSearch = QADocumentSearch(
                    coreData = QADocumentCore(question = "*".some).some,
                    timestampLte = Some(staleAge)
                  ),
                  refreshPolicy = RefreshPolicy.`true`
                )
                log.info(s"Deleted questions fields for $instance: updated(${result.updatedDocs}) total(${result.totalDocs})")
              }
            case _ =>
          }
        }
      case _ =>
        log.error("Unknown error cleaning cluster nodes tables")
    }
  }

  def scheduleAction(): Unit = {
    val actorRef =
      SCActorSystem.system.actorOf(Props(new CleanQuestionFieldsTickActor))
    SCActorSystem.system.scheduler.scheduleWithFixedDelay(
      0 seconds,
      conversationLogsService.elasticClient
        .asInstanceOf[ConversationLogsElasticClient.type].cleanVisitorMessageCronInterval second,
      actorRef,
      tickMessage)
  }
}
