package com.getjenny.starchat.resources
import akka.http.scaladsl.model.StatusCodes
import com.getjenny.analyzer.expressions.AnalyzersData
import com.getjenny.analyzer.util.Time
import com.getjenny.starchat.TestEnglishBase
import com.getjenny.starchat.entities.io.{AnalyzerEvaluateRequest, AnalyzerEvaluateResponse, Permissions, User}
import scalaz.Scalaz._

class CheckMultipleDaysOfWeekAtomicResourceTest extends TestEnglishBase {

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


  "CheckMultipleDaysOfWeek Atomic" should {
    "return 1.0 when evaluating condition all the weekdays are included in the daylist" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "user query unused",
          analyzer = """band(checkMultipleDaysOfWeek("[1,2,3,4,5,6,7]","CET"))""",
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

  "CheckMultipleDaysOfWeek Atomic" should {
    "fail when daylist argument is empty" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "user query unused",
          analyzer = """band(checkMultipleDaysOfWeek("[]","CET"))""",
          data = Option {
            AnalyzersData()
          }
        )

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
    }
  }

  "CheckMultipleDaysOfWeek Atomic" should {
    "fail when daylist is not well formatted" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "user query unused",
          analyzer = """band(checkMultipleDaysOfWeek("[1,A]","CET"))""",
          data = Option {
            AnalyzersData()
          }
        )

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
    }
  }

  "CheckMultipleDaysOfWeek Atomic" should {
    "fail when timezone is not valid" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "user query unused",
          analyzer = """band(checkMultipleDaysOfWeek("[1,A]","CETAD"))""",
          data = Option {
            AnalyzersData()
          }
        )

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
    }
  }

  "CheckMultipleDaysOfWeek Atomic" should {
    "return 0.0 when the current weekday is not included in the daylist" in {
      val allWeekDays: List[Int] = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil
      // remove current weekday from the list
      val allWeekDaysExcludedToday = allWeekDays.filter(day => day =/= Time.dayOfWeekInt("CET"))
      val analyzer = """band(checkMultipleDaysOfWeek("[""" + allWeekDaysExcludedToday.mkString(",") + """]","CET"))"""

      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "user query unused",
          analyzer = analyzer,
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

  "CheckMultipleDaysOfWeek Atomic" should {
    "return 1.0 when the current weekday is in the daylist" in {
      val analyzer = """band(checkMultipleDaysOfWeek("[""" + Time.dayOfWeekInt("CET") + """]","CET"))"""
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "user query unused",
          analyzer = analyzer,
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
