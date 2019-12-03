package com.getjenny.starchat.services

import akka.actor.{Actor, Props}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.services.esclient.crud.EsCrudBase
import com.getjenny.starchat.utils.Index
import org.elasticsearch.index.query.QueryBuilders
import scalaz.Scalaz._

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Try

object CronDeleteInstanceService extends CronService {

  case class DeleteInstanceResponse(indexName: String, instance: String, documentsDeleted: Long)

  class DeleteInstanceActor extends Actor {
    override def receive: Receive = {
      case `tickMessage` =>
        instanceRegistryService.getAllMarkedAsDeleted
          .flatMap { case (registryEntryId, _) =>
            val esLanguageSpecificIndexName = Index.esLanguageFromIndexName(registryEntryId, "")
            systemIndexManagementService.indices
              .filter(_.startsWith(esLanguageSpecificIndexName))
              .map(registryEntryId -> _)
          }.map { case (id, indexName) =>
          (id, delete(id, indexName).right.get.documentsDeleted)
        }.groupBy(_._1).mapValues(_.map(_._2).sum)
          .filter(_._2 === 0).map { case(id, _) =>
          instanceRegistryService.markAsDeleted(List(id))
        }
      case m => log.error("Unexpected message in  DeleteInstanceActor :{}", m)
    }

    private[this] def delete(registryEntryId: String, indexName: String): Either[String, DeleteInstanceResponse] = {
      val res = Try {
        val instance = Index.instanceName(registryEntryId)
        val crud = new EsCrudBase(systemIndexManagementService.elasticClient, indexName)
        val delete = crud.delete(QueryBuilders.matchQuery("instance", instance))
        log.info("Deleting data from instance: {} index: {} - doc deleted: {}", instance,
          indexName, delete.getDeleted)
        DeleteInstanceResponse(indexName, instance, delete.getDeleted)
      }.toEither
        .left.map{ _ =>
        log.error("Error during delete registry entry {} for in index {}", registryEntryId, indexName)
        s"Error during delete registry entry $registryEntryId for in index $indexName"
      }
      res
    }

  }

  def scheduleAction(): Unit = {
    if (systemIndexManagementService.elasticClient.instanceRegistryDeleteFrequency > 0) {
      val reloadDecisionTableActorRef =
        SCActorSystem.system.actorOf(Props(new DeleteInstanceActor))
      SCActorSystem.system.scheduler.schedule(
        0 seconds,
        systemIndexManagementService.elasticClient.instanceRegistryDeleteFrequency seconds,
        reloadDecisionTableActorRef,
        tickMessage)
    }
  }
}
