package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.TestEnglishBase
import com.getjenny.starchat.entities.io._
import com.getjenny.starchat.entities.persistents._

class SpellcheckResourceTest2 extends TestEnglishBase {

  "StarChat" should {
    "return an HTTP code 201 when populating conversation logs" in {
      val conversationLogsRequest: QADocument = QADocument(
        id = "0",
        conversation = "id:1000",
        indexInConversation = 1,
        coreData = Some(QADocumentCore(
          question = Some("is this text misspelled?"),
          answer = Some("it might be")
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
      Post(s"/index_getjenny_english_0/conversation_logs?refresh=wait_for", conversationLogsRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexDocumentResult]
        response.created should be (true)
        response.id should be ("0")
        response.index should be ("index_english.logs_data")
      }
    }
  }

  it should {
    val spellcheckRequest: SpellcheckTermsRequest2 = SpellcheckTermsRequest2(
      text = "is this text missplelled",
      minDocFreq = 0
    )

    s"return an HTTP code 200 when spellchecking" in {
      Post(s"/index_getjenny_english_0/spellcheck2/terms", spellcheckRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[SpellcheckTermsResponse]
        response.tokens.map(_.text) should contain only ("is", "this", "text", "missplelled")
        response.tokens.find(_.text === "missplelled").getOrElse(fail).options match {
          case SpellcheckTokenSuggestions(_, _, text) :: Nil => text should be ("misspelled")
          case _ => fail("Spellcheck didn't correct missplelled")
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
    val deleteRequest: ListOfDocumentId = ListOfDocumentId(ids = List("0"))
    "return an HTTP code 200 when deleting a document from conversation logs" in {
      Delete(s"/index_getjenny_english_0/conversation_logs", deleteRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentsResult]
        response.data.size should be (1)
        response.data.headOption match {
          case Some(result) => result.id should be ("0")
          case None => fail
        }
      }
    }
  }
}
