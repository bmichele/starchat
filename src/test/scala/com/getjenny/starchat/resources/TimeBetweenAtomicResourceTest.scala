package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.analyzer.expressions.AnalyzersData
import com.getjenny.analyzer.util.Time
import com.getjenny.starchat.TestEnglishBase
import com.getjenny.starchat.entities.io.{AnalyzerEvaluateRequest, AnalyzerEvaluateResponse, Permissions, User}
import scalaz.Scalaz._

class TimeBetweenAtomicResourceTest extends TestEnglishBase {

  "StarChat" should {
    "return an HTTP code 201 when creating a new user" in {
      val user = User(
        id = "test_user",
        password = "3c98bf19cb962ac4cd0227142b3495ab1be46534061919f792254b80c0f3e566f7819cae73bdc616af0ff555f7460ac96d88d56338d659ebd93e2be858ce1cf9",
        salt = "salt",
        permissions = Map[String, Set[Permissions.Value]]("index_getjenny_english_0" -> Set(Permissions.read, Permissions.write))
      )
      Post(s"/user", user) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
      }
    }
  }

  "TimeBetween Atomic" should {
    "return 1.0 when evaluating condition from midnight to midnight" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "user query unused",
          analyzer = """band(timeBetween("00:00", "23:59", "Europe/Helsinki"))""",
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

  "TimeBetween Atomic" should {
    "return 1.0 when evaluating a compareTime between opening times" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "user query unused",
          // Timezone is not used, but it's required
          analyzer = """band(timeBetween("00:00", "23:59", "Europe/Helsinki", "08:44"))""",
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

  "Two TimeBetween Atomics" should {
    "return 1.0 when evaluated in XOR and have TimeZone in a 12h interval" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "user query unused",
          analyzer = """bor(band(timeBetween("00:00", "11:59", "UTC-6"), bnot(timeBetween("00:00", "11:59", "UTC+6"))), band(timeBetween("00:00", "11:59", "UTC+6"), bnot(timeBetween("00:00", "11:59", "UTC-6"))))""",
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