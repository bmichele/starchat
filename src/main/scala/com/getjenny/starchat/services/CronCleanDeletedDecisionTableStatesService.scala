package com.getjenny.starchat.services

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 07/10/20.
 */

import akka.actor.{Actor, Props}
import com.getjenny.starchat.SCActorSystem

import scala.concurrent.duration._
import scala.language.postfixOps

object CronCleanDeletedDecisionTableStatesService extends CronService {
  class CleanDeletedDTStatesTickActor extends Actor {
    def receive: PartialFunction[Any, Unit] = {
      case `tickMessage` =>
        languageIndexManagementService.indices.filter(_.endsWith(".state")).distinct
          .foreach(indexName => {
            val delete = decisionTableService.cleanStaleDeletedStates(indexName)
            log.debug(s"Index: $indexName, message=${delete.deleted}")
          }
        )
      case _ =>
        log.error("Unknown error cleaning cluster nodes tables")
    }
  }

  def scheduleAction(): Unit = {
    val actorRef =
      SCActorSystem.system.actorOf(Props(new CleanDeletedDTStatesTickActor))
    SCActorSystem.system.scheduler.scheduleWithFixedDelay(
      0 seconds,
      decisionTableService.elasticClient.cleanDeletedDTStatesProcessInterval millisecond,
      actorRef,
      tickMessage)
  }
}
