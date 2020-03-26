package com.getjenny.starchat.services

import akka.actor.{Actor, ActorRef, Props}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.io.RefreshPolicy
import com.getjenny.starchat.services.esclient.crud.EsCrudBase
import com.getjenny.starchat.utils.Index
import org.elasticsearch.index.query.QueryBuilders
import scalaz.Scalaz._

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Try

object CronDeleteInstanceService extends CronService {

  case class DeleteInstanceResponse(indexName: String, instance: String, documentsDeleted: Long)

  class DeleteInstanceActor(probeActor: Option[ActorRef] = None) extends Actor {
    override def receive: Receive = {
      case `tickMessage` =>

        val deleteResponse = instanceRegistryService.getAllMarkedToDelete
          .flatMap { case (registryEntryId, _) =>
            val esLanguageSpecificIndexName = Index.esLanguageFromIndexName(registryEntryId, "")
            systemIndexManagementService.indices
              .filter(_.startsWith(esLanguageSpecificIndexName))
              .map(registryEntryId -> _)
          }.flatMap { case (id, indexName) => delete(id, indexName).map(response => id -> response) }

        val instancesToDeleteFromRegistry = deleteResponse.groupBy(_._1)
          .mapValues(_.map(_._2.documentsDeleted).sum)
          .filter(_._2 === 0)

        instancesToDeleteFromRegistry.foreach { case (id, _) =>
          instanceRegistryService.markAsDeleted(List(id))
        }

        probeActor.foreach(ref => ref ! instancesToDeleteFromRegistry)
      case m => log.error("Unexpected message in  DeleteInstanceActor :{}", m)
    }

    private[this] def delete(registryEntryId: String, indexName: String): Option[DeleteInstanceResponse] = {
      val res = Try {
        val instance = Index.instanceName(registryEntryId)
        val crud = new EsCrudBase(clusterNodesService.elasticClient, indexName)
        val delete = crud.delete(QueryBuilders.matchQuery("instance", instance), RefreshPolicy.`wait_for`)
        log.info("Deleting data from instance: {} index: {} - doc deleted: {}", instance,
          indexName, delete.getDeleted)
        DeleteInstanceResponse(indexName, instance, delete.getDeleted)
      }.toEither
        .left.map { e =>
        log.error(e, "Error during delete registry entry {} for in index {}", registryEntryId, indexName)
      }
      res.toOption
    }

  }

  def scheduleAction(): Unit = {
    if (instanceRegistryService.elasticClient.instanceRegistryDeleteFrequency > 0) {
      val reloadDecisionTableActorRef =
        SCActorSystem.system.actorOf(Props(new DeleteInstanceActor))
      SCActorSystem.system.scheduler.schedule(
        0 seconds,
        instanceRegistryService.elasticClient.instanceRegistryDeleteFrequency seconds,
        reloadDecisionTableActorRef,
        tickMessage)
    }
  }

  object DeleteInstanceActor {
    def props: Props = Props(new DeleteInstanceActor())

    def props(actorProbe: ActorRef): Props = Props(new DeleteInstanceActor(Some(actorProbe)))
  }

}
