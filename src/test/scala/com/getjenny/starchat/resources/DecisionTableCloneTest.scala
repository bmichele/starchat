package com.getjenny.starchat.resources

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, Multipart, StatusCodes}
import com.getjenny.starchat.TestEnglishBase
import com.getjenny.starchat.entities.io._

class DecisionTableCloneTest extends TestEnglishBase {

  "StarChat" should {

    "return an HTTP code 200 creating a preview instance" in {
      Post("/index_getjenny_english_0_preview/index_management/create") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
        response.message shouldEqual "Created instance index_getjenny_english_0_preview, operation status: CREATED"
      }
    }

    "return an HTTP 200 and a check is true when an instance is enabled" in {
      Post("/index_getjenny_english_0_preview/index_management/enable") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
        response.message shouldEqual "Enabled instance index_getjenny_english_0_preview, operation status: OK"
        response.check shouldEqual true
      }
    }

    "return an HTTP code 201 when indexing a decision table from csv file" in {
      val input_file = getClass.getResourceAsStream("/doc/decision_table_starchat_doc.csv")
      val input_data = scala.io.Source.fromInputStream(input_file).mkString
      val multipartForm =
        Multipart.FormData(
          Multipart.FormData.BodyPart.Strict(
            "csv",
            HttpEntity(ContentTypes.`text/plain(UTF-8)`, input_data),
            Map("filename" -> "data.csv")))

      Post(s"/index_getjenny_english_0_preview/decisiontable/upload/csv", multipartForm) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexDocumentListResult]
        response.data.length should be (22)
      }
    }
  }

  "return an HTTP code 200 cloning data from the preview index" in {
    Post("/index_getjenny_english_0_preview/decisiontable/clone/index_getjenny_english_0?refresh=wait_for") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      status shouldEqual StatusCodes.OK
      val response = responseAs[ReindexResult]
      response.total should be (22)
    }
  }

  "return an HTTP code 200 when deleting all documents from preview" in {
    Delete("/index_getjenny_english_0_preview/decisiontable/all") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      status shouldEqual StatusCodes.OK
      val response = responseAs[DeleteDocumentsSummaryResult]
      response.deleted should be (22)
    }
  }

  "return an HTTP code 200 when deleting all documents from the cloned index" in {
    Delete("/index_getjenny_english_0/decisiontable/all") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      status shouldEqual StatusCodes.OK
      val response = responseAs[DeleteDocumentsSummaryResult]
      response.deleted should be (22)
    }
  }

}


