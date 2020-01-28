package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.TestEnglishBase
import com.getjenny.starchat.entities.io._

class NodeDtLoadingStatusResourceTest extends TestEnglishBase {

  "StarChat" should {
    "return an HTTP code 200 when getting node status for index" in {
      val strict = true
      Get(s"/index_getjenny_english_0/node_dt_update?strict=$strict") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[ClusterLoadingDtStatusIndex]
        print(responseEntity)
      }
    }
  }

  it should {
    "return an HTTP code 200 when getting node status for all indices" in {
      val strict = true
      val verbose = true
      Get(s"/node_dt_update?strict=$strict&verbose=$verbose") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[NodeLoadingAllDtStatus]
        print(responseEntity)
      }
    }
  }

  it should {
    "return an HTTP code 200 when node status for an index" in {
      val request = NodeDtLoadingStatus(index = "index_getjenny_english_0")
      Post(s"/node_dt_update", request) ~>  addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
      }
    }
  }

  it should {
    "return an HTTP code 200 when deleting decision table loading records for dead nodes" in {
      Delete(s"/node_dt_update") ~>  addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentsSummaryResult]
        println(response)
      }
    }
  }
}
