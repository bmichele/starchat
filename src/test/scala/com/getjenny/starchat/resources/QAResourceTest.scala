package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.TestEnglishBase
import com.getjenny.starchat.entities.io._
import com.getjenny.starchat.entities.persistents._
import shapeless.PolyDefns.~>

class QAResourceTest extends TestEnglishBase {

  val qaRoutes = List("conversation_logs", "knowledgebase", "prior_data")
  val documents = List(
    QADocument(
      id = "id1",
      conversation = "conv_id_1",
      indexInConversation = 1,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
      ))
    ),
    QADocument(
      id = "id2",
      conversation = "conv_id_2",
      indexInConversation = 1,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY)
      ))
    ),
    QADocument(
      id = "id3",
      conversation = "conv_id_3",
      indexInConversation = 1,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY)
      ))
    ),
    QADocument(
      id = "id4",
      conversation = "conv_id_4",
      indexInConversation = 1,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY)
      ))
    ),
    QADocument(
      id = "id5",
      conversation = "conv_id_1",
      indexInConversation = 2,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY)
      )),
      coreData = Some(QADocumentCore(
        question = Some("what is the capital of finland"),
        questionScoredTerms = Some(List(
          ("jokeri", 1.2345)
        )),
        answer = Some("Helsinki")
      ))
    ),
    QADocument(
      id = "idToDeleteByQuery",
      conversation = "delete_by_query_conv",
      indexInConversation = 1,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY)
      ))
    ))

  for (qaRoute <- qaRoutes) {
    "StarChat" should {
      s"return an HTTP code 201 when inserting a QA document to the $qaRoute" in {
        for (document <- documents) {
          Post(s"/index_getjenny_english_0/$qaRoute?refresh=`true`", document) ~> addCredentials(testUserCredentials) ~> routes ~> check {
            status shouldEqual StatusCodes.Created
            val response = responseAs[IndexDocumentResult]
            response.version should be(1)
            response.created should be(true)
          }
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 201 when updating documents in bulk in the $qaRoute" in {
        val request = QADocumentUpdate(
          id = List("id1", "id2", "id3", "id123"),
          coreData = Some(QADocumentCore(
            question = Some("I forgot my password"),
            answer = Some("did you try hunter2?"),
            verified = Some(true)
          )),
          annotations = Some(QADocumentAnnotations(
            feedbackConvScore = Some(1.0),
            algorithmConvScore = Some(1.0),
            feedbackAnswerScore = Some(1.0),
            algorithmAnswerScore = Some(1.0)
          ))
        )
        Put(s"/index_getjenny_english_0/$qaRoute?refresh=`wait_for`", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.Created
          val response = responseAs[IndexDocumentListResult]
          response.data.map(_.version) should be(List(3, 2, 2, -1)) // the insert of items in conversation increment the counter on the first conv. item
          response.data.map(_.id) should be(List("id1", "id2", "id3", "id123"))
          response.data.map(_.created) should contain only false
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when searching documents from the $qaRoute" in {
        val request = QADocumentSearch(
          coreData = Some(QADocumentCore(question = Some("I forgot my password"))),
          size = Some(3),
          minScore = Some(0.0f)
        )
        val request2 = QADocumentSearch(
          size = Some(0),
          minScore = Some(0.0f)
        )
        val request3 = QADocumentSearch(
          conversation = Some(List("conv_id_1", "conv_id_2", "conv_id_3", "conv_id_123")),
          minScore = Some(0.0f)
        )
        Post(s"/index_getjenny_english_0/$qaRoute/search", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[SearchQADocumentsResults]
          response.totalHits should be(3)
          response.hitsCount should be(3)
          response.hits.size should be(3)
          response.hits.map(_.document
            .coreData.getOrElse(fail)
            .question.getOrElse(fail)) should contain only "I forgot my password"
        }
        Post(s"/index_getjenny_english_0/$qaRoute/search", request2) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[SearchQADocumentsResults]
          response.totalHits should be(6)
          response.hits should be(List.empty)
        }
        Post(s"/index_getjenny_english_0/$qaRoute/search", request3) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[SearchQADocumentsResults]
          response.totalHits should be(4)
          response.hits.map(_.document.conversation) should contain only("conv_id_1", "conv_id_2", "conv_id_3")
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when retrieving documents from the $qaRoute" in {
        Get(s"/index_getjenny_english_0/$qaRoute?id=id1&id=id2&id=id3&id=id4&id=id5&id=id6") ~> addCredentials(testUserCredentials) ~> routes ~> check {
          val response = responseAs[SearchQADocumentsResults]
          response.totalHits should be(5)
          response.hits.size should be(5)
          response.hits.map(_.document.id) should contain only("id1", "id2", "id3", "id4", "id5")
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when retrieving conversations from the $qaRoute" in {
        val request = ListOfDocumentId(ids = List("conv_id_1", "conv_id_2", "conv_id_3", "conv_id_123"))
        Post(s"/index_getjenny_english_0/$qaRoute/conversations", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[Conversations]
          response.total should be(3)
          response.conversations.size should be(3)
          response.conversations.map(_.count) should contain only(1, 2)
          response.conversations.map(_.docs.map(_.conversation)) should contain only
            (List("conv_id_1", "conv_id_1"), List("conv_id_2"), List("conv_id_3"))
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when counting terms from the $qaRoute" in {
        Get(s"/index_getjenny_english_0/term_count/$qaRoute?term=password") ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[TermCount]
          response.count should be(3)
          response.numDocs should be(3)
        }
        Get(s"/index_getjenny_english_0/term_count/$qaRoute?term=capital&") ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[TermCount]
          response.numDocs should be(1)
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when getting dictionary size from the $qaRoute" in {
        Get(s"/index_getjenny_english_0/dict_size/$qaRoute") ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[DictSize]
          response.numDocs should be(6)
          response.answer should be(5)
          response.question should be(10)
          response.total should be(15)
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when getting total term count from the $qaRoute" in {
        Get(s"/index_getjenny_english_0/total_terms/$qaRoute") ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[TotalTerms]
          response.numDocs should be(6)
          response.answer should be(13)
          response.question should be(18)
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when updating the terms in a document in the $qaRoute" in {
        val request = UpdateQATermsRequest(
          id = "id1"
        )

        Put(s"/index_getjenny_english_0/updateTerms/$qaRoute", request) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[List[UpdateDocumentResult]]
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when updating all terms $qaRoute" in {
        val request = UpdateQATermsRequest(
          id = ""
        )

        Post(s"/index_getjenny_english_0/updateTerms/$qaRoute", request) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val responses = responseAs[String].split('\n')
          responses.length shouldEqual 6
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when streaming all documents in the $qaRoute" in {
        Get(s"/index_getjenny_english_0/stream/$qaRoute") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[String].split('\n')
          response.length shouldEqual 6
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when update by query conversations from the $qaRoute" in {
        val search = QADocumentSearch(
          coreData = Some(QADocumentCore(question = Some("I forgot my password"))),
          size = Some(2)
        )
        val update = QADocumentUpdate(
          id = List("id1", "id2"),
          coreData = Some(QADocumentCore(
            question = Some("I think I forgot my password"),
            answer = Some("did you try hunter3?"),
            verified = Some(true)
          )),
          annotations = Some(QADocumentAnnotations(
            feedbackConvScore = Some(1.0),
            algorithmConvScore = Some(1.0),
            feedbackAnswerScore = Some(1.0),
            algorithmAnswerScore = Some(1.0)
          ))
        )
        val request = UpdateQAByQueryReq(search, update)
        Put(s"/index_getjenny_english_0/$qaRoute/conversations?refresh=wait_for", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          responseAs[UpdateDocumentsResult]
        }

        val searchRequest = QADocumentSearch(
          coreData = Some(QADocumentCore(question = Some("I think I forgot my password"))),
          size = Some(2)
        )
        Post(s"/index_getjenny_english_0/$qaRoute/search", searchRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[SearchQADocumentsResults]
          response.totalHits should be(3)
          response.hitsCount should be(2)
          response.hits.size should be(2)
          response.hits.map(_.document
            .coreData.getOrElse(fail)
            .question.getOrElse(fail)) should contain only "I think I forgot my password"
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when getting analytics from the $qaRoute" in {
        val request = QAAggregatedAnalyticsRequest(
          aggregations = Some(QAAggregationsTypes.values.toList.filter(
            v => v != QAAggregationsTypes.UNKNOWN
          ))
        )
        Post(s"/index_getjenny_english_0/analytics/$qaRoute", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[QAAggregatedAnalytics]
          response.avgAlgorithmAnswerScore should not be None
          response.avgAlgorithmConvScore should not be None
          response.avgFeedbackAnswerScore should not be None
          response.avgFeedbackConvScore should not be None
          response.labelCountHistograms should not be None
          response.countOverTimeHistograms should not be None
          response.scoreHistograms should not be None
          response.scoresOverTime should not be None
          response.totalConversations should be(1)
          response.totalDocuments should be(2)
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"delete document by query in the $qaRoute" in {
        val request = QADocumentSearch(conversation = Option(List("delete_by_query_conv")))
        Post(s"/index_getjenny_english_0/$qaRoute/delete_by_query", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[DeleteDocumentsSummaryResult]
          response.deleted shouldEqual 1
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when deleting documents in bulk from the $qaRoute" in {
        val request = DocsIds(ids = List("id1", "id2"))
        Delete(s"/index_getjenny_english_0/$qaRoute?refresh=wait_for", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[DeleteDocumentsResult]
          response.data.size should be(2)
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"should have lower term count after deleting documents from the $qaRoute" in {
        Get(s"/index_getjenny_english_0/term_count/$qaRoute?term=password") ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[TermCount]
          response.count should be(1)
          response.numDocs should be(1)
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"should not find any deleted documents by get from the $qaRoute" in {
        Get(s"/index_getjenny_english_0/$qaRoute?id=id1&id=id2&id=id3&id=id4&id=id5") ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[SearchQADocumentsResults]
          response.totalHits should be(3)
          response.hitsCount should be(3)
          response.hits.map(_.document.id) should contain only("id3", "id4", "id5")
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"should not find any deleted documents by search from the $qaRoute" in {
        val request = QADocumentSearch()
        Post(s"/index_getjenny_english_0/$qaRoute/search", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[SearchQADocumentsResults]
          response.totalHits should be(3)
          response.hitsCount should be(3)
          response.hits.map(_.document.id) should contain only("id3", "id4", "id5")
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when deleting all documents from the $qaRoute" in {
        val request = DocsIds(ids = Nil)
        Delete(s"/index_getjenny_english_0/$qaRoute?refresh=wait_for", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[DeleteDocumentsSummaryResult]
          response.deleted should be(3)
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"should not find any documents by get in the $qaRoute" in {
        Get(s"/index_getjenny_english_0/$qaRoute?id=id1&id=id2&id=id3&id=id4&id=id5") ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[SearchQADocumentsResults]
          response.totalHits should be(0)
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    val document = QADocument(
      id = "id94",
      conversation = "conv_id_1",
      indexInConversation = 1,
      coreData = Some(QADocumentCore(
        question = Some(""),
        answer = Some("")
      )),
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
      ))
    )
    it should {
      s"should not store empty strings in question or answer fields in the $qaRoute" in {
        Post(s"/index_getjenny_english_0/$qaRoute?refresh=wait_for", document) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.Created
          val response = responseAs[IndexDocumentResult]
          response.id
        }.andThen(id => {
          Get(s"/index_getjenny_english_0/$qaRoute?id=$id") ~> addCredentials(testUserCredentials) ~> routes ~> check {
            status shouldEqual StatusCodes.OK
            val response = responseAs[SearchQADocumentsResults]
            val document = response.hits.headOption.getOrElse(fail("document not found")).document
            val coreData = document.coreData.getOrElse(fail("no coreData"))
            coreData.answer shouldEqual None
            coreData.question shouldEqual None
          }
        })
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"setup counter cache parameters in the $qaRoute" in {
        val request = CountersCacheParameters(Some(1), Some(1), Some(1), Some(1))
        Post(s"/index_getjenny_english_0/cache/$qaRoute", request) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[CountersCacheParameters]
          response.totalTermsCacheMaxSize should be(Option(1))
          response.dictSizeCacheMaxSize should be(Option(1))
          response.countTermCacheMaxSize should be(Option(1))
          response.cacheStealTimeMillis should be(Option(1))
        }
      }
    }

    it should {
      s"delete counter cache parameters in the $qaRoute" in {
        Delete(s"/index_getjenny_english_0/cache/$qaRoute") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val (cacheParameters, _) = responseAs[(CountersCacheParameters, CountersCacheSize)]
          cacheParameters.totalTermsCacheMaxSize should be(Option(1))
          cacheParameters.dictSizeCacheMaxSize should be(Option(1))
          cacheParameters.countTermCacheMaxSize should be(Option(1))
          cacheParameters.cacheStealTimeMillis should be(Option(1))
        }
      }
    }

    it should {
      s"get counter cache parameters in the $qaRoute" in {
        Get(s"/index_getjenny_english_0/cache/$qaRoute") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val (cacheParameters, cacheSize) = responseAs[(CountersCacheParameters, CountersCacheSize)]
          cacheParameters.totalTermsCacheMaxSize should be(Option(1))
          cacheParameters.dictSizeCacheMaxSize should be(Option(1))
          cacheParameters.countTermCacheMaxSize should be(Option(1))
          cacheParameters.cacheStealTimeMillis should be(Option(1))
          cacheSize.dictSizeCacheSize shouldEqual 0
          cacheSize.totalTermsCacheSize shouldEqual 0
          cacheSize.countTermCacheSize shouldEqual 0
        }
      }
    }
  }

}
