package com.getjenny.starchat.analyzer.atoms

import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.getjenny.starchat.serializers.JsonSupport
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

class HttpRequestAtomicTest extends WordSpec with Matchers with ScalatestRouteTest with JsonSupport with BeforeAndAfterAll {


  "HttpRequestAtomic" should {

    "fail if no arguments are present in argument list" in {
      intercept[IllegalArgumentException] {
        new HttpRequestAtomic(List.empty[String], Map.empty[String, String])
      }

    }

    "fail if no url is present in argument list" in {
      val exception = intercept[IllegalArgumentException] {
        new HttpRequestAtomic(List("http-atom.test.http-method"), Map.empty[String, String])
      }
      exception.getMessage contains "Url parameter not found"
    }

    "fail if no http-method is in argument list" in {
      val exception = intercept[IllegalArgumentException] {
        new HttpRequestAtomic(List("http-atom.test.url"), Map.empty[String, String])
      }
      exception.getMessage contains "Http method parameter not found"
    }

    "fail if no input variables are in argument list" in {
      val exception = intercept[IllegalArgumentException] {
        new HttpRequestAtomic(List("http-atom.test.url", "http-atom.test.http-method"), Map.empty[String, String])
      }
      exception.getMessage contains "Input Key parameter not found"
    }

    "fail if input variables are present but not in a couple key value" in {
      val arguments = List("A__temp__.http-atom.test.url",
        "system.http-atom.test.http-method",
        "http-atom.test.username",
        "A__temp__.system.http-atom.test.password",
        "A__temp__.system.http-atom.test.input-key-1",
      "http-atom.test.input-value-2"
      )
      val configuration =  Map("http-atom.test.url" -> "www.google.it",
        "http-atom.test.http-method" -> "GET",
        "http-atom.test.username" -> "user",
        "http-atom.test.password" -> "pwd",
        "http-atom.test.input-key-1" -> "a",
        "http-atom.test.input-value-2" -> "2")
      val exception = intercept[IllegalArgumentException] {
        new HttpRequestAtomic(arguments, configuration)
      }

      exception.getMessage shouldEqual "Error in parameter list: Argument list contains an invalid input keys"
    }

    "fail if there is no configuration for variable" in {
      val arguments = List("A__temp__.http-atom.test.url",
        "system.http-atom.test.http-method",
        "http-atom.test.input-value-1",
        "A__temp__.system.http-atom.test.input-key-1"
      )
      intercept[IllegalArgumentException] {
        new HttpRequestAtomic(arguments, Map.empty)
      }
    }

    "create atom if configuration is correct" in {
      val arguments = List("A__temp__.http-atom.test.url",
        "system.http-atom.test.http-method",
        "http-atom.test.username",
        "A__temp__.system.http-atom.test.password",
        "http-atom.test.input-value-1",
        "A__temp__.system.http-atom.test.input-key-1"
      )
      val configuration =  Map("http-atom.test.url" -> "www.google.it",
        "http-atom.test.http-method" -> "GET",
        "http-atom.test.username" -> "user",
        "http-atom.test.password" -> "pwd",
        "http-atom.test.input-key-1" -> "a",
        "http-atom.test.input-value-1" -> "2")

      new HttpRequestAtomic(arguments, configuration)
    }

  }

}
