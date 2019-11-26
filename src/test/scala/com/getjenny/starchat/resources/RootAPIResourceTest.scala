package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes

class RootAPIResourceTest extends TestBase {

  "StarChat" should {
    "return a 200 if the service responds (Health Check)" in {
      Get(s"/") ~> routes ~> check {
        status shouldEqual StatusCodes.OK
      }
    }
  }
}

