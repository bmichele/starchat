package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.entities.io.{CreateLanguageIndexRequest, IndexManagementResponse, IndexManagementStatusResponse}
import com.getjenny.starchat.services.{InstanceRegistryService, InstanceRegistryStatus}

class IndexManagementResourceTest extends TestBase {

  val createEnglishRequest = CreateLanguageIndexRequest(List("english"))

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val instance = InstanceRegistryService.getInstance("index_getjenny_english_0")
    Post("/language_index_management", createEnglishRequest) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
  }

  "StarChat" should {

    "return an HTTP code 400 trying to enable a non existent instance" in {
      Post("/index_getjenny_english_0/index_management/enable") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
    }

    "return an HTTP code 400 trying to disable a non existent instance" in {
      Post("/index_getjenny_english_0/index_management/disable") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
    }

    "return an HTTP code 400 when mark delete a non existent instance" in {
      Delete(s"/index_getjenny_english_0/index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
    }

    "return an HTTP code 200 when add instance" in {
      Post("/index_getjenny_english_0/index_management/create") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
        response.message shouldEqual "Created instance index_getjenny_english_0, operation status: CREATED"
      }
    }

    "return an HTTP 200 and a check is true when an instance is enabled" in {
      Post("/index_getjenny_english_0/index_management/enable") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
        response.message shouldEqual "Enabled instance index_getjenny_english_0, operation status: OK"
        response.check shouldEqual true
      }
    }

    "return an HTTP 200 and status" in {
      Get(s"/index_getjenny_english_0/index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementStatusResponse]
        response.status shouldEqual InstanceRegistryStatus.Enabled.toString
      }
    }

    "return an HTTP code 200 when disable instance" in {
      Post(s"/index_getjenny_english_0/index_management/disable") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
        response.message shouldEqual "Disabled instance index_getjenny_english_0, operation status: OK"
        response.check shouldEqual true
      }
    }

    "return an HTTP code 200 when mark delete instance" in {
      Delete(s"/index_getjenny_english_0/index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
        response.message shouldEqual "Mark Delete instance index_getjenny_english_0, operation status: OK"
      }
    }

    "return an HTTP code 401 when trying to access to a service when instance is disabled" in {
      Get("/index_getjenny_english_0/decisiontable?id=forgot_password&id=call_operator") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Unauthorized
      }
    }

    "return an HTTP 200 and status Missing if instance is not yet in the registry" in {
      InstanceRegistryService.deleteEntry(List("index_getjenny_english_0"))

      Get(s"/index_getjenny_english_0/index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementStatusResponse]
        response.status shouldEqual InstanceRegistryStatus.Missing.toString
      }
    }


    "return an HTTP code 200 when create an instance with the same name as a deleted instance" in {
      Post("/index_getjenny_english_0/index_management/create") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
        response.message shouldEqual "Created instance index_getjenny_english_0, operation status: OK"
      }

      Get(s"/index_getjenny_english_0/index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementStatusResponse]
        response.status shouldEqual InstanceRegistryStatus.Disabled.toString
      }
    }

  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    Delete("/language_index_management?index=index_english") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
  }

}
