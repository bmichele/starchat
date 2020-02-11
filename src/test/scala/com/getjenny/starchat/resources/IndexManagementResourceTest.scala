package com.getjenny.starchat.resources

import akka.http.scaladsl.server.AuthorizationFailedRejection
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.BasicHttpCredentials
import com.getjenny.starchat.TestBase
import com.getjenny.starchat.entities.io.Permissions.Permission
import com.getjenny.starchat.entities.io._
import com.getjenny.starchat.services.{InstanceRegistryService, InstanceRegistryStatus}

class IndexManagementResourceTest extends TestBase {

  val createEnglishRequest = CreateLanguageIndexRequest(List("english"))

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Post("/language_index_management", createEnglishRequest) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
    val user = User(
      id = "test_user",
      password = "3c98bf19cb962ac4cd0227142b3495ab1be46534061919f792254b80c0f3e566f7819cae73bdc616af0ff555f7460ac96d88d56338d659ebd93e2be858ce1cf9",
      salt = "salt",
      permissions = Map[String, Set[Permissions.Value]](
        "index_getjenny_english_0" -> Set(Permissions.read, Permissions.write),
        "index_getjenny_english_common_0" -> Set(Permissions.read, Permissions.write))
    )
    Post(s"/user", user) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      val s = status
      s shouldEqual StatusCodes.Created
    }
  }

  "StarChat" should {

    "return an HTTP code 400 trying to create an instance that has no language index " in {
      Post("/index_getjenny_arabic_0/index_management/create") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
    }

    "return an HTTP code 400 trying to enable an instance that has no language index " in {
      Post("/index_getjenny_arabic_0/index_management/enable") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
    }

    "return an HTTP code 400 trying to disable an instance that has no language index " in {
      Post("/index_getjenny_arabic_0/index_management/disable") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
    }

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

    "return an HTTP code 200 and permission set to disabled when instance is disabled" in {
      Post(s"/user/get", UserId("test_user")) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[User]
        response.permissions
          .getOrElse("index_getjenny_english_0", Set.empty[Permission]) contains Permissions.disabled
      }
    }

    "return an HTTP code 200 when mark delete instance" in {
      Delete(s"/index_getjenny_english_0/index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
        response.message shouldEqual "Mark Delete instance index_getjenny_english_0, operation status: OK"
      }
    }

    "return an HTTP code 401 when trying to access to a service and user does not exists" in {
      Get("/index_getjenny_english_0/decisiontable?id=forgot_password&id=call_operator") ~> addCredentials(BasicHttpCredentials("aaaa", "p4ssw0rd")) ~> routes ~> check {
        status shouldEqual StatusCodes.Unauthorized
      }
    }

    "reject call with a 403 when trying to access a service when instance is disabled" in {
      Get("/index_getjenny_english_0/decisiontable?id=forgot_password&id=call_operator") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        rejection shouldBe a [AuthorizationFailedRejection.type]
      }
    }

    "return an HTTP 200 and status Missing if instance is not yet in the registry" in {
      InstanceRegistryService.markAsDeleted(List("index_getjenny_english_0"))

      Get(s"/index_getjenny_english_0/index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementStatusResponse]
        response.status shouldEqual InstanceRegistryStatus.Missing.toString
      }
    }

    /*
    Commented because it can fail during tests due to the execution of the cron delete instance service job
    that can change state between the deletion of an instance and it's recreation
    This behaviour is not expected in production as admin user will check if an instance is deleted before trying to recreate it

    "return an HTTP code 200 when create an instance with the same name as a deleted instance" in {
      Post("/index_getjenny_english_0/index_management/create") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
        response.message shouldEqual "Created instance index_getjenny_english_0, operation status: OK"
      }

      Get(s"/index_getjenny_english_0/index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementStatusResponse]
        response.status shouldEqual InstanceRegistryStatus.Enabled.toString
      }
    }*/

  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    Delete("/language_index_management?index=index_english") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
  }

}
