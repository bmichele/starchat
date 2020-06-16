package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.TestEnglishBase
import com.getjenny.starchat.entities.io._
import com.getjenny.starchat.entities.persistents._
import com.getjenny.starchat.services.SpellcheckService2

import scala.collection.immutable.List

class SpellcheckResourceTest2 extends TestEnglishBase {

  val token1: SpellcheckToken = SpellcheckToken("how",0,3,List())
  val token2: SpellcheckToken = SpellcheckToken("are",4,3,List())
  val token3: SpellcheckToken = SpellcheckToken("you",8,3,List())
  val token4: SpellcheckToken = SpellcheckToken("doing",12,5,List())
  val token5: SpellcheckToken = SpellcheckToken("today",18,5,List())
  val tokens: List[SpellcheckToken] = List(
    token1, token2, token3, token4, token5
  )

  "SpellcheckService2" should {

    val service = SpellcheckService2

    "generate right contexts" in {
      val rightContexts = service.rightContextList(tokens)
      rightContexts.size should be (tokens.size)
      rightContexts(0) should be (List(token2, token3))
      rightContexts(1) should be (List(token3, token4))
      rightContexts(2) should be (List(token4, token5))
      rightContexts(3) should be (List(token5))
      rightContexts(4) should be (List())
    }

    "generate left contexts" in {
      val leftContexts = service.leftContextList(tokens)
      leftContexts.size should be (tokens.size)
      leftContexts(0) should be (List())
      leftContexts(1) should be (List(token1))
      leftContexts(2) should be (List(token1, token2))
      leftContexts(3) should be (List(token2, token3))
      leftContexts(4) should be (List(token3, token4))
    }

    val token3Misspell: SpellcheckToken = SpellcheckToken("yoy",8,3,List(SpellcheckTokenSuggestions(0.6666666269302368,23.0,token3.text)))
    val tokensMisspell: List[SpellcheckToken] = List(token1, token2, token3Misspell, token4, token5)

    "generate proper right contexts" in {
      val rightContexts = service.rightContextList(tokensMisspell)
      val rightProperContexts = service.rightProperContextList(rightContexts)
      rightProperContexts(0) should be(List(token2))
      rightProperContexts(1) should be(List())
      rightProperContexts(2) should be(List(token4, token5))
      rightProperContexts(3) should be(List(token5))
      rightProperContexts(4) should be(List())
    }

    "generate proper left contexts" in {
      val leftContexts = service.leftContextList(tokensMisspell)
      val leftProperContexts = service.leftProperContextList(leftContexts)
      leftProperContexts(0) should be(List())
      leftProperContexts(1) should be(List(token1))
      leftProperContexts(2) should be(List(token1, token2))
      leftProperContexts(3) should be(List())
      leftProperContexts(4) should be(List(token4))
    }
  }

  val documentId0: String = "0"
  val documentId1: String = "1"
  val documentId2: String = "2"
  val conversationId: String = "id:1000"
  val starchatIndex: String = "index_getjenny_english_0"
  val slashChar: String = "/"
  val conversationLogsUrl: String = slashChar + starchatIndex + "/conversation_logs"
  val spellcheckService2Url: String = slashChar + starchatIndex + "/spellcheck2/terms"

  "StarChat - populating conversation logs" should {
    "return an HTTP code 201" in {
      val conversationLog1: QADocument = QADocument(
        id = documentId0,
        conversation = conversationId,
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
        id = documentId1,
        conversation = conversationId,
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
        id = documentId2,
        conversation = conversationId,
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
      val indexLogsLanguage = "index_english.logs_data"
      val requestUrlOptions = "?refresh=wait_for"
      Post(conversationLogsUrl + requestUrlOptions, conversationLog1) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexDocumentResult]
        response.created should be (true)
        response.id should be (documentId0)
        response.index should be (indexLogsLanguage)
      }
      Post(conversationLogsUrl + requestUrlOptions, conversationLog2) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexDocumentResult]
        response.created should be (true)
        response.id should be (documentId1)
        response.index should be (indexLogsLanguage)
      }
      Post(conversationLogsUrl + requestUrlOptions, conversationLog3) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexDocumentResult]
        response.created should be (true)
        response.id should be (documentId2)
        response.index should be (indexLogsLanguage)
      }
    }
  }

  "SpellcheckService2.ngramCounts" should {
    "get ngram counts right" in {
      val service = SpellcheckService2
      val candidate1 = "you"
      val candidate2 = "they"
      val candidates = List(candidate1, candidate2)
      val leftContext = List(token1.text, token2.text)
      val rightContext = List(token4.text, token5.text)
      val (stats1, stats2) = service.ngramCounts(starchatIndex, candidates, leftContext, rightContext)
      stats1(candidate1)(service.labelT) should be (2)
      stats1(candidate1)(service.labelLT) should be (2)
      stats1(candidate1)(service.labelLLT) should be (1)
      stats1(candidate1)(service.labelTR) should be (1)
      stats1(candidate1)(service.labelTRR) should be (1)
      stats1(candidate1)(service.labelLTR) should be (1)
      stats1(candidate2)(service.labelT) should be (1)
      stats1(candidate2)(service.labelLT) should be (1)
      stats1(candidate2)(service.labelLLT) should be (0)
      stats1(candidate2)(service.labelTR) should be (1)
      stats1(candidate2)(service.labelTRR) should be (0)
      stats1(candidate2)(service.labelLTR) should be (1)
      stats2(service.labelUnigrams) should be (12)
      stats2(service.labelBigrams) should be (9)
      stats2(service.labelTrigrams) should be (6)
    }
  }

  "StarChat" should {
    val tokenMisspelled = "doig"
    val tokenCorrect = "doing"
    val spellcheckRequest: SpellcheckTermsRequest2 = SpellcheckTermsRequest2(
      text = "how are you " + tokenMisspelled + " today?",
      minDocFreq = 0
    )

    s"return an HTTP code 200 when spellchecking" in {
      Post(spellcheckService2Url, spellcheckRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[SpellcheckTermsResponse]
        response.tokens.map(_.text) should contain only ("how", "are", "you", tokenMisspelled, "today")
        response.tokens.find(_.text === tokenMisspelled).getOrElse(fail).options match {
          case SpellcheckTokenSuggestions(_, _, text) :: Nil => text should be (tokenCorrect)
          case _ => fail("Spellcheck didn't correct misspelling")
        }
      }
    }
  }

  it should {
    val spellcheckRequest: SpellcheckTermsRequest2 = SpellcheckTermsRequest2(
      text = "",
      minDocFreq = 0
    )
    s"return empty list when text is empty" in {
      Post(spellcheckService2Url, spellcheckRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[SpellcheckTermsResponse]
        response.tokens should be (List())
      }
    }

  }

  val misspelledSentence = "is this text missplelled"

  it should {
    val spellcheckRequest: SpellcheckTermsRequest2 = SpellcheckTermsRequest2(
      text = misspelledSentence,
      minDocFreq = -1
    )

    s"return an HTTP code 400 when minDocFreq is negative" in {
      Post(spellcheckService2Url, spellcheckRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
        response.code should be (100)
        response.message should be("minDocFreq must be positive")
      }
    }
  }

  it should {
    val spellcheckRequest: SpellcheckTermsRequest2 = SpellcheckTermsRequest2(
      text = misspelledSentence,
      prefixLength = -1
    )
    s"return an HTTP code 400 when prefixLength is negative" in {

      Post(spellcheckService2Url, spellcheckRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
        response.code should be (100)
        response.message should be("prefixLength must be positive")
      }
    }
  }

  it should {
    val spellcheckRequest: SpellcheckTermsRequest2 = SpellcheckTermsRequest2(
      text = misspelledSentence,
      maxEdit = 0
    )
    val spellcheckRequest2 = spellcheckRequest.copy(maxEdit = 3)
    val responseMessageMinMaxEdits = "maxEdits must be between 1 and 2"

    s"return an HTTP code 400 when maxEdit is not between 1 and 2" in {
      Post(spellcheckService2Url, spellcheckRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
        response.code should be (100)
        response.message should be (responseMessageMinMaxEdits)
      }
      Post(spellcheckService2Url, spellcheckRequest2) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
        response.code should be (100)
        response.message should be (responseMessageMinMaxEdits)
      }
    }
  }

  it should {
    val spellcheckRequest: SpellcheckTermsRequest2 = SpellcheckTermsRequest2(
      text = misspelledSentence,
      minWordLength = -1
    )
    val spellcheckRequest2 = spellcheckRequest.copy(minWordLength = 0)
    val responseMessageMinWordLength = "minWordLength must be greater or equal to 1"
    s"return an HTTP code 400 when minWordLength is less than one" in {
      Post(spellcheckService2Url, spellcheckRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
        response.code should be (100)
        response.message should be (responseMessageMinWordLength)
      }
      Post(spellcheckService2Url, spellcheckRequest2) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
        response.code should be (100)
        response.message should be (responseMessageMinWordLength)
      }
    }
  }

  it should {
    val deleteRequest1: ListOfDocumentId = ListOfDocumentId(ids = List(documentId0))
    val deleteRequest2: ListOfDocumentId = ListOfDocumentId(ids = List(documentId1))
    val deleteRequest3: ListOfDocumentId = ListOfDocumentId(ids = List(documentId2))
    "return an HTTP code 200 when deleting a document from conversation logs" in {
      Delete(conversationLogsUrl, deleteRequest1) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentsResult]
        response.data.size should be (1)
        response.data.headOption match {
          case Some(result) => result.id should be (documentId0)
          case None => fail
        }
      }
      Delete(conversationLogsUrl, deleteRequest2) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentsResult]
        response.data.size should be (1)
        response.data.headOption match {
          case Some(result) => result.id should be (documentId1)
          case None => fail
        }
      }
      Delete(conversationLogsUrl, deleteRequest3) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentsResult]
        response.data.size should be (1)
        response.data.headOption match {
          case Some(result) => result.id should be (documentId2)
          case None => fail
        }
      }
    }
  }
}
