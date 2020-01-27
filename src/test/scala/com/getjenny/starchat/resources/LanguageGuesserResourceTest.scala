package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.BasicHttpCredentials
import com.getjenny.starchat.TestEnglishBase
import com.getjenny.starchat.entities.io._

class LanguageGuesserResourceTest extends TestEnglishBase {

  val languageIterator: Iterator[(String, String)] = {
    val input_file = getClass.getResourceAsStream("/test_data/language_guesser_test_parameters.csv")
    val input_data = scala.io.Source.fromInputStream(input_file, "UTF-8").getLines
    input_data.next() //skip column names
    new Iterator[(String, String)] {
      override def hasNext: Boolean = input_data.hasNext
      override def next(): (String, String) = {
        val line = input_data.next().split(",")
        (line(0), line(1))
      }
    }
  }

  for((language, sentence) <- languageIterator) {
    "StarChat" should {
      s"return an HTTP code 200 when checking if '$language' is supported" in {
        Get(s"/index_getjenny_english_0/language_guesser/$language") ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[LanguageGuesserInformations]
          response.supportedLanguages should (contain key "languages" and contain value Map(language -> true))
        }
      }
    }

    it should {
      val languageGuesserRequestIn: LanguageGuesserRequestIn = LanguageGuesserRequestIn(
        inputText = sentence
      )

      s"return an HTTP code 200 when guessing '$language' language" in {
        Post(s"/index_getjenny_english_0/language_guesser", languageGuesserRequestIn) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[LanguageGuesserRequestOut]
          response.language should be(language)
        }
      }
    }
  }

  it should {
    val languageGuesserRequestIn: LanguageGuesserRequestIn = LanguageGuesserRequestIn(
      inputText = "I am unauthorized"
    )
    val unauthorizedUserCredentials = BasicHttpCredentials("jack", "sparrow")

    s"return an HTTP code 401 when unauthorized" in {
      Post(s"/index_getjenny_english_0/language_guesser", languageGuesserRequestIn) ~> addCredentials(unauthorizedUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Unauthorized
      }
    }
  }

}
