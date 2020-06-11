package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.TestEnglishBase
import com.getjenny.starchat.entities.io._
import com.getjenny.starchat.entities.persistents._
import com.getjenny.starchat.services.SpellcheckService2

import scala.collection.immutable.List

class SpellcheckResourceTest2 extends TestEnglishBase {

  "SpellcheckService2.rightContextList" should {
    "generate right contexts" in {
      val token1 = SpellcheckToken("how",0,3,List())
      val token2 = SpellcheckToken("are",4,3,List())
      val token3 = SpellcheckToken("you",8,3,List())
      val token4 = SpellcheckToken("doing",12,5,List())
      val token5 = SpellcheckToken("today",18,5,List())
      val tokens = List(
        token1, token2, token3, token4, token5
      )
      val service = SpellcheckService2
      val rightContexts = service.rightContextList(tokens)
      rightContexts.size should be (tokens.size)
      rightContexts.head should be (List(token2, token3))
      rightContexts(1) should be (List(token3, token4))
      rightContexts(2) should be (List(token4, token5))
      rightContexts(3) should be (List(token5))
      rightContexts(4) should be (List())
    }
  }

  "SpellcheckService2.leftContextList" should {
    "generate left contexts" in {
      val token1 = SpellcheckToken("how",0,3,List())
      val token2 = SpellcheckToken("are",4,3,List())
      val token3 = SpellcheckToken("you",8,3,List())
      val token4 = SpellcheckToken("doing",12,5,List())
      val token5 = SpellcheckToken("today",18,5,List())
      val tokens = List(
        token1, token2, token3, token4, token5
      )
      val service = SpellcheckService2
      val leftContexts = service.leftContextList(tokens)
      leftContexts.size should be (tokens.size)
      leftContexts.head should be (List())
      leftContexts(1) should be (List(token1))
      leftContexts(2) should be (List(token1, token2))
      leftContexts(3) should be (List(token2, token3))
      leftContexts(4) should be (List(token3, token4))
    }
  }

  "SpellcheckService2.rightProperContextList" should {
    "generate proper right contexts" in {
      val token1 = SpellcheckToken("how",0,3,List())
      val token2 = SpellcheckToken("are",4,3,List())
      val token3 = SpellcheckToken("yoy",8,3,List(SpellcheckTokenSuggestions(0.6666666269302368,23.0,"you")))
      val token4 = SpellcheckToken("doing",12,5,List())
      val token5 = SpellcheckToken("today",18,5,List())
      val tokens = List(
        token1, token2, token3, token4, token5
      )
      val service = SpellcheckService2
      val rightContexts = service.rightContextList(tokens)
      val rightProperContexts = service.rightProperContextList(rightContexts)
      rightProperContexts.head should be (List(token2))
      rightProperContexts(1) should be (List())
      rightProperContexts(2) should be (List(token4, token5))
      rightProperContexts(3) should be (List(token5))
      rightProperContexts(4) should be (List())
    }
  }

  "SpellcheckService2.leftProperContextList" should {
    "generate proper left contexts" in {
      val token1 = SpellcheckToken("how",0,3,List())
      val token2 = SpellcheckToken("are",4,3,List())
      val token3 = SpellcheckToken("yoy",8,3,List(SpellcheckTokenSuggestions(0.6666666269302368,23.0,"you")))
      val token4 = SpellcheckToken("doing",12,5,List())
      val token5 = SpellcheckToken("today",18,5,List())
      val tokens = List(
        token1, token2, token3, token4, token5
      )
      val service = SpellcheckService2
      val leftContexts = service.leftContextList(tokens)
      val leftProperContexts = service.leftProperContextList(leftContexts)
      leftProperContexts.head should be (List())
      leftProperContexts(1) should be (List(token1))
      leftProperContexts(2) should be (List(token1, token2))
      leftProperContexts(3) should be (List())
      leftProperContexts(4) should be (List(token4))
    }
  }

  "StarChat" should {
    "return an HTTP code 201 when populating conversation logs" in {
      val conversationLog1: QADocument = QADocument(
        id = "0",
        conversation = "id:1000",
        indexInConversation = 1,
        coreData = Some(QADocumentCore(
          question = Some("how are you doing today?"),
          answer = Some("pretty good.")
        )),
        annotations = Some(QADocumentAnnotations(
          doctype = Some(Doctypes.NORMAL),
          agent = Some(Agent.STARCHAT),
          escalated = Some(Escalated.UNSPECIFIED),
          answered = Some(Answered.UNSPECIFIED),
          triggered = Some(Triggered.UNSPECIFIED),
          followup = Some(Followup.UNSPECIFIED)
        ))
      )
      val conversationLog2: QADocument = QADocument(
        id = "1",
        conversation = "id:1000",
        indexInConversation = 2,
        coreData = Some(QADocumentCore(
          question = Some("are you serious?"),
          answer = Some("sure")
        )),
        annotations = Some(QADocumentAnnotations(
          doctype = Some(Doctypes.NORMAL),
          agent = Some(Agent.STARCHAT),
          escalated = Some(Escalated.UNSPECIFIED),
          answered = Some(Answered.UNSPECIFIED),
          triggered = Some(Triggered.UNSPECIFIED),
          followup = Some(Followup.UNSPECIFIED)
        ))
      )
      val conversationLog3: QADocument = QADocument(
        id = "2",
        conversation = "id:1000",
        indexInConversation = 3,
        coreData = Some(QADocumentCore(
          question = Some("what are they doing?"),
          answer = Some("i do not know.")
        )),
        annotations = Some(QADocumentAnnotations(
          doctype = Some(Doctypes.NORMAL),
          agent = Some(Agent.STARCHAT),
          escalated = Some(Escalated.UNSPECIFIED),
          answered = Some(Answered.UNSPECIFIED),
          triggered = Some(Triggered.UNSPECIFIED),
          followup = Some(Followup.UNSPECIFIED)
        ))
      )
      Post(s"/index_getjenny_english_0/conversation_logs?refresh=wait_for", conversationLog1) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexDocumentResult]
        response.created should be (true)
        response.id should be ("0")
        response.index should be ("index_english.logs_data")
      }
      Post(s"/index_getjenny_english_0/conversation_logs?refresh=wait_for", conversationLog2) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexDocumentResult]
        response.created should be (true)
        response.id should be ("1")
        response.index should be ("index_english.logs_data")
      }
      Post(s"/index_getjenny_english_0/conversation_logs?refresh=wait_for", conversationLog3) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexDocumentResult]
        response.created should be (true)
        response.id should be ("2")
        response.index should be ("index_english.logs_data")
      }
    }
  }

  "SpellcheckService2.getStats" should {
    "get ngram counts right" in {
      val index = "index_getjenny_english_0"
      val service = SpellcheckService2
      val candidates = List("you", "they")
      val leftContext = List("how", "are")
      val rightContext = List("doing", "today")
      val (stats1, stats2) = service.getStats(index, candidates, leftContext, rightContext)
      stats1("you")("t") should be (2)
      stats1("you")("lt") should be (2)
      stats1("you")("llt") should be (1)
      stats1("you")("tr") should be (1)
      stats1("you")("trr") should be (1)
      stats1("you")("ltr") should be (1)
      stats1("they")("t") should be (1)
      stats1("they")("lt") should be (1)
      stats1("they")("llt") should be (0)
      stats1("they")("tr") should be (1)
      stats1("they")("trr") should be (0)
      stats1("they")("ltr") should be (1)
      stats2("unigrams") should be (12)
      stats2("bigrams") should be (9)
      stats2("trigrams") should be (6)
    }
  }

  "StarChat" should {
    val spellcheckRequest: SpellcheckTermsRequest2 = SpellcheckTermsRequest2(
      text = "how are you doig today?",
      minDocFreq = 0
    )

    s"return an HTTP code 200 when spellchecking" in {
      Post(s"/index_getjenny_english_0/spellcheck2/terms", spellcheckRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[SpellcheckTermsResponse]
        response.tokens.map(_.text) should contain only ("how", "are", "you", "doig", "today")
        response.tokens.find(_.text === "doig").getOrElse(fail).options match {
          case SpellcheckTokenSuggestions(_, _, text) :: Nil => text should be ("doing")
          case _ => fail("Spellcheck didn't correct misspelling")
        }
      }
    }
  }

  it should {
    val spellcheckRequest: SpellcheckTermsRequest2 = SpellcheckTermsRequest2(
      text = "is this text missplelled",
      minDocFreq = -1
    )

    s"return an HTTP code 400 when minDocFreq is negative" in {
      Post(s"/index_getjenny_english_0/spellcheck2/terms", spellcheckRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
        response.code should be (100)
        response.message should be("minDocFreq must be positive")
      }
    }
  }

  it should {
    val spellcheckRequest: SpellcheckTermsRequest2 = SpellcheckTermsRequest2(
      text = "is this text missplelled",
      prefixLength = -1
    )
    s"return an HTTP code 400 when prefixLength is negative" in {

      Post(s"/index_getjenny_english_0/spellcheck2/terms", spellcheckRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
        response.code should be (100)
        response.message should be("prefixLength must be positive")
      }
    }
  }

  it should {
    val spellcheckRequest: SpellcheckTermsRequest2 = SpellcheckTermsRequest2(
      text = "is this text missplelled",
      maxEdit = 0
    )
    val spellcheckRequest2 = spellcheckRequest.copy(maxEdit = 3)

    s"return an HTTP code 400 when maxEdit is not between 1 and 2" in {
      Post(s"/index_getjenny_english_0/spellcheck2/terms", spellcheckRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
        response.code should be (100)
        response.message should be ("maxEdits must be between 1 and 2")
      }
      Post(s"/index_getjenny_english_0/spellcheck2/terms", spellcheckRequest2) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
        response.code should be (100)
        response.message should be ("maxEdits must be between 1 and 2")
      }
    }
  }

  it should {
    val spellcheckRequest: SpellcheckTermsRequest2 = SpellcheckTermsRequest2(
      text = "is this text missplelled",
      minWordLength = -1
    )
    val spellcheckRequest2 = spellcheckRequest.copy(minWordLength = 0)

    s"return an HTTP code 400 when minWordLength is less than one" in {
      Post(s"/index_getjenny_english_0/spellcheck2/terms", spellcheckRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
        response.code should be (100)
        response.message should be ("minWordLength must be greater or equal to 1")
      }
      Post(s"/index_getjenny_english_0/spellcheck2/terms", spellcheckRequest2) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
        response.code should be (100)
        response.message should be ("minWordLength must be greater or equal to 1")
      }
    }
  }

  it should {
    val deleteRequest1: ListOfDocumentId = ListOfDocumentId(ids = List("0"))
    val deleteRequest2: ListOfDocumentId = ListOfDocumentId(ids = List("1"))
    val deleteRequest3: ListOfDocumentId = ListOfDocumentId(ids = List("2"))
    "return an HTTP code 200 when deleting a document from conversation logs" in {
      Delete(s"/index_getjenny_english_0/conversation_logs", deleteRequest1) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentsResult]
        response.data.size should be (1)
        response.data.headOption match {
          case Some(result) => result.id should be ("0")
          case None => fail
        }
      }
      Delete(s"/index_getjenny_english_0/conversation_logs", deleteRequest2) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentsResult]
        response.data.size should be (1)
        response.data.headOption match {
          case Some(result) => result.id should be ("1")
          case None => fail
        }
      }
      Delete(s"/index_getjenny_english_0/conversation_logs", deleteRequest3) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentsResult]
        response.data.size should be (1)
        response.data.headOption match {
          case Some(result) => result.id should be ("2")
          case None => fail
        }
      }
    }
  }
}
