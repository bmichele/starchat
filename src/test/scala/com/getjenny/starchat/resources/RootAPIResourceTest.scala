package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.TestBase

class RootAPIResourceTest extends TestBase {

  "StarChat" should {
    "return a 200 if the service responds (Health Check)" in {
      Get(s"/") ~> routes ~> check {
        status shouldEqual StatusCodes.OK
      }
    }

    "return a 200 with build info" in {
      Post(s"/") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[String] shouldEqual com.getjenny.starchat.autogen.utils.BuildInfo.toJson
      }
    }
  }
}

