package com.getjenny.starchat.services

import akka.http.scaladsl.model.StatusCodes
import akka.testkit.{ImplicitSender, TestKitBase, TestProbe}
import com.getjenny.starchat.TestEnglishBase
import com.getjenny.starchat.entities.io.UpdateDocumentsResult
import com.getjenny.starchat.entities.persistents.{Term, Terms}
import com.getjenny.starchat.services.CronDeleteInstanceService.DeleteInstanceActor
import com.getjenny.starchat.services.esclient.IndexManagementElasticClient
import com.getjenny.starchat.services.esclient.crud.EsCrudBase
import com.getjenny.starchat.utils.Index
import org.elasticsearch.index.query.QueryBuilders

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.language.postfixOps

class CronDeleteInstancesTest extends TestEnglishBase with TestKitBase with ImplicitSender {

  private[this] val instanceRegistryService = InstanceRegistryService
  private[this] val indexName = "index_getjenny_english_0"
  private[this] val client = IndexManagementElasticClient
  private[this] val systemIndexManagementService: SystemIndexManagementService.type = SystemIndexManagementService
  private[this] val instance = Index.instanceName(indexName)
  private[this] val testProbe = TestProbe()
  private[this] val deleteInstanceActor = system.actorOf(DeleteInstanceActor.props(testProbe.ref))

  "StarChat" should {

    "return an HTTP code 201 when creating a new document" in {
      val terms = Terms(
        terms = List(Term(term = "term1"), Term(term = "term2"))
      )
      Post("/index_getjenny_english_0/term/index?refresh=1", terms) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[UpdateDocumentsResult]
        response.data.length should be(terms.terms.length)
      }
    }


    "delete instance when cron job is triggered" in {
      val crud = new EsCrudBase(client, "index_english.term")
      crud.refresh()

      instanceRegistryService.markDeleteInstance(indexName)

      deleteInstanceActor ! "tick"

      val messages = testProbe.expectMsgClass(10 seconds, classOf[Map[String, Long]])

      messages shouldBe empty

      val esLanguageSpecificIndexName = Index.esLanguageFromIndexName(indexName, "")
      systemIndexManagementService
        .indices
        .filter(_.startsWith(esLanguageSpecificIndexName))
        .foreach(fullIndexName => {
          val indexLanguageCrud = new EsCrudBase(client, fullIndexName)
          indexLanguageCrud.refresh()
          val read = indexLanguageCrud.read(QueryBuilders.matchQuery("instance", instance))
          assert(read.getHits.getHits.flatMap(_.getSourceAsMap.asScala).isEmpty)
        })

      deleteInstanceActor ! "tick"
      val completedDelete = testProbe.expectMsgClass(10 seconds, classOf[Map[String, Long]])
      completedDelete
        .filterNot{ case (_, v) => v === 0L} shouldBe empty

      //Only when elasticsearch has deleted 0 elements we are sure that instance is completely deleted
      //so we can safely delete the instance from the registry
      val registryEntryDeleted = instanceRegistryService.getInstance(indexName)
      registryEntryDeleted.isEmpty shouldBe true
    }

  }
}
