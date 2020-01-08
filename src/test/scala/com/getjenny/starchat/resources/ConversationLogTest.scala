package com.getjenny.starchat.resources

import java.sql.Timestamp
import java.text.SimpleDateFormat
import java.time.{ZoneId, ZoneOffset}
import java.util.{Date, TimeZone}

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.TestEnglishBase
import com.getjenny.starchat.entities.io.{IndexDocumentResult, QAAggregatedAnalyticsRequest, QAAggregationsTypes}
import com.getjenny.starchat.entities.persistents.{Agent, Doctypes, QAAggregatedAnalytics, QADocument, QADocumentAnnotations, QADocumentCore}
import com.getjenny.starchat.services.{ConversationLogsService, QuestionAnswerServiceException}

import scala.collection.immutable.List

class ConversationLogTest extends TestEnglishBase {

  val formatter = new SimpleDateFormat("dd/MM/yyyy hh:mm:sss");

  val documents = List(
    QADocument(
      id = "id1",
      conversation = "conv_id_1",
      indexInConversation = 1,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.2)
      )),
      timestamp = Some(new Timestamp(formatter.parse("09/12/2019 00:00:00").getTime).getTime)
    ),
    QADocument(
      id = "id2",
      conversation = "conv_id_2",
      indexInConversation = 1,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.5)
      )),
      timestamp = Some(new Timestamp(formatter.parse("15/12/2019 06:00:00").getTime).getTime)
    ),
    QADocument(
      id = "id3",
      conversation = "conv_id_3",
      indexInConversation = 1,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.1)
      )),
      timestamp = Some(new Timestamp(formatter.parse("20/12/2019 06:00:00").getTime).getTime)
    ),
    QADocument(
      id = "id4",
      conversation = "conv_id_4",
      indexInConversation = 1,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.2)
      )),
      timestamp = Some(new Timestamp(formatter.parse("21/12/2019 06:00:00").getTime).getTime)
    ),
    QADocument(
      id = "id5",
      conversation = "conv_id_1",
      indexInConversation = 2,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.5)
      )),
      coreData = Some(QADocumentCore(
        question = Some("what is the capital of finland"),
        questionScoredTerms = Some(List(
          ("jokeri", 1.2345)
        )),
        answerScoredTerms = Some(List(("AA", 1.2))),
        answer = Some("Helsinki")
      )),
      timestamp = Some(new Timestamp(formatter.parse("27/12/2019 06:00:00").getTime).getTime)
    ),
    QADocument(
      id = "id6",
      conversation = "conv_id_1",
      indexInConversation = 2,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.5),
      )),
      timestamp = Some(new Timestamp(formatter.parse("01/01/2020 06:00:00").getTime).getTime)
    ),
    QADocument(
      id = "id7",
      conversation = "conv_id_2",
      indexInConversation = 2,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.5)
      )),
      timestamp = Some(new Timestamp(formatter.parse("02/01/2020 06:00:00").getTime).getTime)
    ),
    QADocument(
      id = "id8",
      conversation = "conv_id_3",
      indexInConversation = 2,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.5)
      )),
      timestamp = Some(new Timestamp(formatter.parse("03/01/2020 06:00:00").getTime).getTime)
    ),
    QADocument(
      id = "id9",
      conversation = "conv_id_4",
      indexInConversation = 2,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.5)
      )),
      timestamp = Some(new Timestamp(formatter.parse("04/01/2020 06:00:00").getTime).getTime)
    ),
    QADocument(
      id = "id10",
      conversation = "conv_id_1",
      indexInConversation = 3,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.5)
      )),
      coreData = Some(QADocumentCore(
        question = Some("what is the capital of finland"),
        questionScoredTerms = Some(List(
          ("jokeri", 1.2345)
        )),
        answer = Some("Helsinki")
      )),
      timestamp = Some(new Timestamp(formatter.parse("04/01/2020 06:00:00").getTime).getTime)
    )
  )

  val documents2 = List(
    QADocument(
      id = "id1",
      conversation = "conv_id_1",
      indexInConversation = 1,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.2),
      )),
      timestamp = Some(new Timestamp(formatter.parse("09/12/2019 00:00:00").getTime).getTime)
    ),
    QADocument(
      id = "id2",
      conversation = "conv_id_2",
      indexInConversation = 1,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.5)
      )),
      timestamp = Some(new Timestamp(formatter.parse("15/12/2019 06:00:00").getTime).getTime)
    ),
    QADocument(
      id = "id3",
      conversation = "conv_id_3",
      indexInConversation = 1,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.5)
      )),
      timestamp = Some(new Timestamp(formatter.parse("20/12/2019 06:00:00").getTime).getTime)
    ),
    QADocument(
      id = "id4",
      conversation = "conv_id_4",
      indexInConversation = 1,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.5)
      )),
      timestamp = Some(new Timestamp(formatter.parse("21/12/2019 06:00:00").getTime).getTime)
    ),
    QADocument(
      id = "id5",
      conversation = "conv_id_1",
      indexInConversation = 2,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.5)
      )),
      coreData = Some(QADocumentCore(
        question = Some("what is the capital of finland"),
        questionScoredTerms = Some(List(
          ("jokeri", 1.2345)
        )),
        answer = Some("Helsinki")
      )),
      timestamp = Some(new Timestamp(formatter.parse("27/12/2019 06:00:00").getTime).getTime)
    ),
    QADocument(
      id = "id6",
      conversation = "conv_id_1",
      indexInConversation = 2,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.5),
      )),
      timestamp = Some(new Timestamp(formatter.parse("27/12/2019 06:00:00").getTime).getTime)
    ),
    QADocument(
      id = "id7",
      conversation = "conv_id_2",
      indexInConversation = 2,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.5)
      )),
      timestamp = Some(new Timestamp(formatter.parse("28/12/2019 06:00:00").getTime).getTime)
    ),
    QADocument(
      id = "id8",
      conversation = "conv_id_3",
      indexInConversation = 2,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.5)
      )),
      timestamp = Some(new Timestamp(formatter.parse("03/01/2020 06:00:00").getTime).getTime)
    ),
    QADocument(
      id = "id9",
      conversation = "conv_id_4",
      indexInConversation = 2,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.5)
      )),
      timestamp = Some(new Timestamp(formatter.parse("04/01/2020 06:00:00").getTime).getTime)
    ),
    QADocument(
      id = "id10",
      conversation = "conv_id_1",
      indexInConversation = 3,
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
        feedbackConvScore = Some(0.5)
      )),
      coreData = Some(QADocumentCore(
        question = Some("what is the capital of finland"),
        questionScoredTerms = Some(List(
          ("jokeri", 1.2345)
        )),
        answer = Some("Helsinki")
      )),
      timestamp = Some(new Timestamp(formatter.parse("04/01/2020 06:00:00").getTime).getTime)
    )
  )

  "StarChat" should {


    s"return an HTTP code 201 when inserting a QA document to the conversationLog" in {
      for (document <- documents) {
        Post(s"/index_getjenny_english_0/conversation_logs?refresh=1", document) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.Created
          val response = responseAs[IndexDocumentResult]
          response.version should be(1)
          response.created should be(true)
        }
      }
      for (doc <- documents2){
        Post(s"/index_getjenny_english_common_0/conversation_logs?refresh=1", doc) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.Created
          val response = responseAs[IndexDocumentResult]
        }
      }
    }

    s"test conversation_logs" in {
      val request = QAAggregatedAnalyticsRequest(
        minDocInBuckets = Option(0),
        interval = Option("1w"),
        timestampGte = Some(new Timestamp(formatter.parse("01/12/2019 06:00:00").getTime).getTime),
        timestampLte = Some(new Timestamp(formatter.parse("07/01/2020 06:00:00").getTime).getTime),
        aggregations = Some(List(
          QAAggregationsTypes.avgFeedbackConvScoreOverTime
        )),
        timezone = Option("+01:00")
      )
      val requestNoTimezone = QAAggregatedAnalyticsRequest(
        minDocInBuckets = Option(0),
        interval = Option("1w"),
        timestampGte = Some(new Timestamp(formatter.parse("01/12/2019 06:00:00").getTime).getTime),
        timestampLte = Some(new Timestamp(formatter.parse("07/01/2020 06:00:00").getTime).getTime),
        aggregations = Some(List(
          QAAggregationsTypes.avgFeedbackConvScoreOverTime
        ))
      )
      Post(s"/index_getjenny_english_0/analytics/conversation_logs", request) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[String]
        println(response)
      }
      Post(s"/index_getjenny_english_common_0/analytics/conversation_logs", requestNoTimezone) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[String]
        println(response)
      }
    }

  }
}
