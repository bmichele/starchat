package com.getjenny.starchat.resources

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.analyzer.expressions.AnalyzersData
import com.getjenny.starchat.TestEnglishBase
import com.getjenny.starchat.entities.io.{AnalyzerEvaluateRequest, AnalyzerEvaluateResponse, Permissions, User}

class CheckDateAtomicResourceTest extends TestEnglishBase {

  "CheckDate Atomic" should {
    "return 1.0 when evaluating condition 1st december 2019 is after 1st november 2019" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "user query unused",
          analyzer = """band(checkDate("2019-12-01T00:00:00","Greater","P0D", "EET","2019-11-01T00:00:00"))""",
          data = Option {
            AnalyzersData()
          }
        )

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.build should be(true)
        response.buildMessage should be("success")
        response.value should be(1.0)
      }
    }
  }

  "CheckDate Atomic" should {
    "return 0.0 when evaluating condition 1st december 2019 is after 1st november 2019 plus 60 days" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "user query unused",
          analyzer = """band(checkDate("2019-12-01T00:00:00","Greater","P+60D", "EET","2019-11-01T00:00:00"))""",
          data = Option {
            AnalyzersData()
          }
        )

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.build should be(true)
        response.buildMessage should be("success")
        response.value should be(0.0)
      }
    }
  }

  "CheckDate Atomic" should {
    "return 1.0 when evaluating condition now from empty string is after yesterday" in {
      val nowString = LocalDateTime.now.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "user query unused",
          analyzer = """band(checkDate("""" + nowString + """","Greater","P-1D", "EET", ""))""",
          data = Option {
            AnalyzersData()
          }
        )

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.build should be(true)
        response.buildMessage should be("success")
        response.value should be(1.0)
      }
    }
  }

  "CheckDate Atomic" should {
    "return 1.0 when evaluating condition now from missing argument is after yesterday" in {
      val nowString = LocalDateTime.now.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "user query unused",
          analyzer = """band(checkDate("""" + nowString + """","Greater","P-1D", "EET"))""",
          data = Option {
            AnalyzersData()
          }
        )

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.build should be(true)
        response.buildMessage should be("success")
        response.value should be(1.0)
      }
    }
  }

}
