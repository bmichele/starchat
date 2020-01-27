package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.TestEnglishBase
import com.getjenny.starchat.entities.io._

class TokenizersResourceTest extends TestEnglishBase {

  "StarChat" should {
    "return an HTTP code 200 when listing all tokenizers" in {
      Get(s"/index_getjenny_english_0/tokenizers") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[Map[String, String]]
      }
    }
  }

  it should {
    "return an HTTP code 200 when extracting tokens from text" in {
      val text = "a quick brown fox"
      val tokenizer = "base_stem"
      val request = TokenizerQueryRequest(tokenizer = tokenizer, text = text)
      Post(s"/index_getjenny_english_0/tokenizers", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[TokenizerResponse]
        response.tokens.length shouldBe 4
        response.tokens.map(_.token) shouldEqual text.split(" ").toSeq
      }
    }

    "return an HTTP code 400 when tokenizer not supported" in {
      val text = "a quick brown fox"
      val tokenizer = "unknown"
      val request = TokenizerQueryRequest(tokenizer = tokenizer, text = text)
      Post(s"/index_getjenny_english_0/tokenizers", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
      }
    }
  }

}
