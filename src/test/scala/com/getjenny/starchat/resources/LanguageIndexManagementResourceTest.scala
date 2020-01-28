package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.TestEnglishBase
import com.getjenny.starchat.entities.io._
import com.getjenny.starchat.utils.Index

class LanguageIndexManagementResourceTest extends TestEnglishBase {

  "StarChat" should {
    "return an HTTP code 400 when trying to create again the same index" in {
      Post("/language_index_management", createEnglishRequest) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[IndexManagementResponse]
        response.message should fullyMatch regex "Elasticsearch exception \\[type=resource_already_exists_exception, reason=index \\[index_.*\\] already exists\\]"
      }
    }
  }

  it should {
    "return all language index" in {
      Get("/language_index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[List[String]]
        response.foreach(println)
        response.size shouldEqual 5
      }
    }
  }

  it should {
    "return an http 200 when close index" in {
      Post("/language_index_management/close?index_name=index_english") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[List[OpenCloseIndex]]
        response.foreach(println)
      }
    }
  }

  it should {
    "return an HTTP code 200 when update index settings" in {
      Put("/language_index_management/settings?index_name=index_english") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
      }
    }
  }

  it should {
    "return an HTTP code 200 when update index mappings" in {
      Put("/language_index_management/mappings?index_name=index_english&indexSuffix=analyzer") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
      }
    }
  }

  it should {
    "return an http 200 when open index" in {
      Post("/language_index_management/open?index_name=index_english") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[List[OpenCloseIndex]]
        response.foreach(println)
      }
    }
  }

  it should {
    "return an HTTP code 200 when calling elasticsearch index refresh" in {
      Post("/language_index_management/refresh?index_name=index_english") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
      }
    }
  }

  it should {
    "return an HTTP code 200 when getting informations from the index" in {
      Get("/language_index_management/check?index_name=index_english") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
        response.message should fullyMatch regex "IndexCheck: " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.languageIndexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.languageIndexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.languageIndexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.languageIndexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.languageIndexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\)".r
      }
    }
  }


  /*it should {
    "return an HTTP code 200 updating the index" in {
      Put(s"/index_getjenny_english_0/english/index_management") ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
      }
    }
  }*/


  it should {
    "return an HTTP code 200 when deleting an existing index" in {
      Delete(s"/language_index_management?index_name=index_english") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
      }
    }
  }

  it should {
    "return an HTTP code 400 when deleting a non existing index" in {
      Delete(s"/language_index_management?index_name=index_english") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[IndexManagementResponse]
      }
    }
  }

}


