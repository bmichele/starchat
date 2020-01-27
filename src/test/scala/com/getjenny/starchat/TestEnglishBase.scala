package com.getjenny.starchat

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.entities.io.{CreateLanguageIndexRequest, Permissions, User}
import com.getjenny.starchat.services.InstanceRegistryService

trait TestEnglishBase extends TestBase {
  val createEnglishRequest = CreateLanguageIndexRequest(List("english"))

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Post("/language_index_management", createEnglishRequest) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
    Post(s"/index_getjenny_english_0/index_management/create") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
    Post(s"/index_getjenny_english_0/index_management/enable") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
    Post(s"/index_getjenny_english_common_0/index_management/create") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
    Post(s"/index_getjenny_english_common_0/index_management/enable") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
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

  override protected def afterAll(): Unit = {
    Delete(s"/language_index_management?index_name=index_english") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
    InstanceRegistryService.markAsDeleted(List("index_getjenny_english_0", "index_getjenny_english_common_0"))
    super.afterAll()
  }

}
