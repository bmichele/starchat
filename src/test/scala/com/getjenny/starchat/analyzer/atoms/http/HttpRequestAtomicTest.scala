package com.getjenny.starchat.analyzer.atoms.http

import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.getjenny.starchat.serializers.JsonSupport
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import scalaz.Scalaz._
import scalaz.{Failure, Success}

class HttpRequestAtomicTest extends WordSpec with Matchers with ScalatestRouteTest with JsonSupport with BeforeAndAfterAll {


  "HttpRequestAtomic" should {

    val variableManager = new GenericVariableManager()
    "fail if no arguments are present in argument list" in {

      val validation = variableManager.validateAndBuild(List.empty, Map.empty, Map.empty)
      validation.isFailure shouldBe true

    }

    "fail if no url is present in argument list" in {

      val validation = variableManager.validateAndBuild(List("system.http-atom.test.http-method"), Map.empty, Map.empty)
      validation.isFailure shouldBe true
      validation.fold(_.toSet, _ => Set.empty[String]) should contain ("url not found in configuration")
    }

    "fail if no http-method is in argument list" in {
      val validation = variableManager.validateAndBuild(List("system.http-atom.test.url"), Map.empty, Map.empty)
      validation.isFailure shouldBe true
      validation.fold(_.toSet, _ => Set.empty[String]) should contain ("http-method not found in configuration")
    }

    "fail if input variables are present but not in a couple key value" in {
      val arguments = List("system.http-atom.test.url",
        "system.http-atom.test.http-method",
        "system.http-atom.test.username",
        "system.http-atom.test.password",
        "system.http-atom.test.input-query-template"
      )
      val configuration = Map("http-atom.test.url" -> "www.google.it",
        "http-atom.test.http-method" -> "GET",
        "http-atom.test.username" -> "user",
        "http-atom.test.password" -> "pwd",
        "http-atom.test.input-query-template" -> "{http-atom.test.query-param}={http-atom.test.query-value}",
        "http-atom.test.query-param" -> "a")

      val validation = variableManager.validateAndBuild(arguments, configuration, Map.empty)
      validation.isFailure shouldBe true
      validation.fold(_.toSet, _ => Set.empty[String]) should contain
      ("Unable to found substitution in template: {http-atom.test.query-param}={http-atom.test.query-value}")
    }

    "fail if basic auth type present but not username and password" in {
      val arguments = List("system.http-atom.test.url",
        "system.http-atom.test.http-method",
        "system.http-atom.test.authorization-type"
      )
      val configuration = Map("http-atom.test.url" -> "www.google.it",
        "http-atom.test.http-method" -> "GET",
        "http-atom.test.authorization-type" -> "Basic")

      val validation = variableManager.validateAndBuild(arguments, configuration, Map.empty)
      validation shouldBe a [Failure[_]]
      validation.fold(_.toSet, _ => Set.empty[String]) should contain allOf
        ("username not found in configuration",
        "password not found in configuration")
    }

    "fail if there is no configuration for variable" in {
      val arguments = List("system.http-atom.test.url",
        "system.http-atom.test.http-method",
        "system.http-atom.test.input-value-1",
        "system.http-atom.test.input-key-1"
      )
      val validation = variableManager.validateAndBuild(arguments,  Map.empty, Map.empty)
      validation shouldBe a [Failure[_]]
    }

    val arguments = List("system.http-atom.test.url",
      "system.http-atom.test.http-method",
      "system.http-atom.test.username",
      "system.http-atom.test.password",
      "system.http-atom.test.input-value-1",
      "system.http-atom.test.input-key-1",
      "system.http-atom.test.output-content-type",
      "system.http-atom.test.output-status",
      "system.http-atom.test.output-data"
    )
    val configuration = Map("http-atom.test.url" -> "www.google.it",
      "http-atom.test.http-method" -> "GET",
      "http-atom.test.username" -> "user",
      "http-atom.test.password" -> "pwd",
      "http-atom.test.input-key-1" -> "a",
      "http-atom.test.input-value-1" -> "2",
      "http-atom.test.output-content-type" -> "test.content-type",
      "http-atom.test.output-status" -> "test.status",
      "http-atom.test.output-data" -> "test.data"
    )

    "validate atom if configuration is correct" in {
      val validation = variableManager.validateAndBuild(arguments, configuration, Map.empty)
      validation shouldBe a [Success[HttpRequestAtomicConfiguration]]
    }

    "fail if http-method post and no content-type" in {
      val brokenConf = configuration + ("http-atom.test.http-method" -> "POST")

      val validation = variableManager.validateAndBuild(arguments, brokenConf, Map.empty)
      validation shouldBe a [Failure[String]]
      validation.fold(_.toSet, _ => Set.empty[String]) should contain ("input-content-type not found in configuration")
    }

    "format templated url" in {
      val newConf = configuration + ("http-atom.test.url" -> "www.google.it/{system.http-atom.test.url-parameter}",
        "http-atom.test.url-parameter" -> "aaaa")

     val conf: HttpRequestAtomicConfiguration = variableManager.validateAndBuild(arguments, newConf, Map.empty) match {
        case Success(conf) => conf
        case Failure(e) => throw new IllegalArgumentException(e.toList.mkString("; "))
      }

      conf.urlConf.url shouldEqual "www.google.it/aaaa"
    }

    "find conf in both system and analyzer data" in {
      val arguments = List("system.http-atom.test.url",
        "system.http-atom.test.http-method",
        "system.http-atom.test.username",
        "system.http-atom.test.password",
        "system.http-atom.test.input-value-1",
        "system.http-atom.test.input-key-1",
        "http-atom.test.output-content-type",
        "http-atom.test.output-status",
        "http-atom.test.output-data"
      )

      val systemConf = Map("http-atom.test.url" -> "www.google.it",
        "http-atom.test.http-method" -> "GET",
        "http-atom.test.username" -> "user",
        "http-atom.test.password" -> "pwd",
        "http-atom.test.input-key-1" -> "a",
        "http-atom.test.input-value-1" -> "2"
      )
      val analyzerData = Map( "http-atom.test.output-content-type" -> "test.content-type",
        "http-atom.test.output-status" -> "test.status",
        "http-atom.test.output-data" -> "test.data")
      val validation = variableManager.validateAndBuild(arguments, systemConf, analyzerData)
      validation shouldBe a [Success[HttpRequestAtomicConfiguration]]
    }

    /*"test call to hubspot" in {
      val portalId = "4040542"
      val formGuid = "c1b44d8c-629f-49fe-9036-d32af2bfdc21"

      val arguments = List("http-atom.test.url",
        "http-atom.test.http-method",
        "http-atom.test.input-content-type",
        "http-atom.test.input-value-1",
        "http-atom.test.input-key-1",
        "http-atom.test.output-content-type",
        "http-atom.test.output-status",
        "http-atom.test.output-data"
      )
      val configuration = Map("http-atom.test.url" -> "https://forms.hubspot.com/uploads/form/v2/%http-atom.test.portal-id%/%http-atom.test.form-guid%",
        "http-atom.test.http-method" -> "POST",
        "http-atom.test.input-content-type" -> "application/x-www-form-urlencoded",
        "http-atom.test.input-key-1" -> "email",
        "http-atom.test.input-value-1" -> "emanuele@getjenny.com",
        "http-atom.test.output-content-type" -> "test.content-type",
        "http-atom.test.output-status" -> "test.status",
        "http-atom.test.output-data" -> "test.data",
        "http-atom.test.portal-id" -> portalId,
        "http-atom.test.form-guid" -> formGuid
      )

      val atom = new HttpRequestAtomic(arguments, configuration)

      val result = atom.evaluate("")
      result.data.extractedVariables.foreach(println)
    }*/
  }

}
