package com.getjenny.starchat.analyzer.atoms.http

import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.getjenny.analyzer.expressions.AnalyzersDataInternal
import com.getjenny.starchat.analyzer.atoms.http.custom.WeatherVariableManager
import com.getjenny.starchat.utils.SystemConfiguration
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scalaz.Scalaz._
import scalaz.{Failure, Success}

class HttpRequestAtomicTest extends AnyWordSpec with Matchers with ScalatestRouteTest {


  "HttpRequestAtomic" should {

    val variableManager = new GenericVariableManager {}
    "fail if no arguments are present in argument list" in {

      val validation = variableManager.validateAndBuild(List.empty, Map.empty, Map.empty, "")
      validation.isFailure shouldBe true
      validation.fold(_.toSet, _ => Set.empty[String]) foreach println
    }

    "fail if no url is present in argument list" in {
      val validation = variableManager.validateAndBuild(List("http-atom.test.http-method"), Map.empty, Map.empty, "")
      validation.isFailure shouldBe true
      validation.fold(_.toSet, _ => Set.empty[String]) should contain("url not found in configuration")
    }

    "fail if no http-method is in argument list" in {
      val validation = variableManager.validateAndBuild(List("http-atom.test.url"), Map.empty, Map.empty, "")
      validation.isFailure shouldBe true
      validation.fold(_.toSet, _ => Set.empty[String]) should contain("http-method not found in configuration")
    }

    "fail if http-method is not valid" in {
      val arguments = List("http-atom.test.url",
        "http-atom.test.http-method"
      )
      val configuration = Map("http-atom.test.url" -> "www.google.it",
        "http-atom.test.http-method" -> "GOT")

      val validation = variableManager.validateAndBuild(arguments, configuration, Map.empty, "")
      validation.isFailure shouldBe true
      validation.fold(_.toSet, _ => Set.empty[String]) should
        contain("Error while extracting key <http-method>: GOT is an invalid method")
    }

    "fail if input template is present but has no substitutions" in {
      val arguments = List("http-atom.test.url",
        "http-atom.test.http-method",
        "http-atom.test.username",
        "http-atom.test.password",
        "http-atom.test.input-query-template"
      )
      val configuration = Map("http-atom.test.url" -> "www.google.it",
        "http-atom.test.http-method" -> "GET",
        "http-atom.test.username" -> "user",
        "http-atom.test.password" -> "pwd",
        "http-atom.test.input-query-template" -> "<http-atom.test.query-param>=<http-atom.test.query-value>",
        "http-atom.test.query-param" -> "a")

      val validation = variableManager.validateAndBuild(arguments, configuration, Map.empty, "")
      validation.isFailure shouldBe true
      validation.fold(_.toSet, _ => Set.empty[String]) should contain
      "Unable to found substitution in template: <http-atom.test.query-param>=<http-atom.test.query-value>"
    }

    "fail if basic auth type present but not username and password" in {
      val arguments = List("http-atom.test.url",
        "http-atom.test.http-method",
        "http-atom.test.authorization-type"
      )
      val configuration = Map("http-atom.test.url" -> "www.google.it",
        "http-atom.test.http-method" -> "GET",
        "http-atom.test.authorization-type" -> "basic")

      val validation = variableManager.validateAndBuild(arguments, configuration, Map.empty, "")
      validation shouldBe a[Failure[_]]
      validation.fold(_.toSet, _ => Set.empty[String]) should contain allOf
        ("username not found in configuration",
          "password not found in configuration")
    }

    "fail if bearer auth type present but not token" in {
      val arguments = List("http-atom.test.url",
        "http-atom.test.http-method",
        "http-atom.test.authorization-type"
      )
      val configuration = Map("http-atom.test.url" -> "www.google.it",
        "http-atom.test.http-method" -> "GET",
        "http-atom.test.authorization-type" -> "bearer")

      val validation = variableManager.validateAndBuild(arguments, configuration, Map.empty, "")
      validation shouldBe a[Failure[_]]
      validation.fold(_.toSet, _ => Set.empty[String]) should contain("token not found in configuration")
    }

    "fail if api key auth type present but not token" in {
      val arguments = List("http-atom.test.url",
        "http-atom.test.http-method",
        "http-atom.test.authorization-type"
      )
      val configuration = Map("http-atom.test.url" -> "www.google.it",
        "http-atom.test.http-method" -> "GET",
        "http-atom.test.authorization-type" -> "apiKey")

      val validation = variableManager.validateAndBuild(arguments, configuration, Map.empty, "")
      validation shouldBe a[Failure[_]]
      validation.fold(_.toSet, _ => Set.empty[String]) should contain("token not found in configuration")
    }

    "fail if api key auth type present but invalid store to option" in {
      val arguments = List("http-atom.test.url",
        "http-atom.test.http-method",
        "http-atom.test.authorization-type",
        "http-atom.test.token",
        "http-atom.test.store-to",
        "http-atom.test.key",
      )
      val configuration = Map("http-atom.test.url" -> "www.google.it",
        "http-atom.test.http-method" -> "GET",
        "http-atom.test.authorization-type" -> "apiKey",
        "http-atom.test.token" -> "asdads",
        "http-atom.test.store-to" -> "aaa",
        "http-atom.test.key" -> "aaa",
      )

      val validation = variableManager.validateAndBuild(arguments, configuration, Map.empty, "")
      validation shouldBe a[Failure[_]]
      validation.fold(_.toSet, _ => Set.empty[String]) should contain("Error while extracting key <store-to>: No value found for 'aaa'")
    }

    "fail if authorization type not supported" in {
      val arguments = List("http-atom.test.url",
        "http-atom.test.http-method",
        "http-atom.test.authorization-type"
      )
      val configuration = Map("http-atom.test.url" -> "www.google.it",
        "http-atom.test.http-method" -> "GET",
        "http-atom.test.authorization-type" -> "asd")

      val validation = variableManager.validateAndBuild(arguments, configuration, Map.empty, "")
      validation shouldBe a[Failure[_]]
      validation.fold(_.toSet, _ => Set.empty[String]) should
        contain("Error while extracting key <authorization-type>: No value found for 'asd'")
    }

    "fail if there is no configuration for variable" in {
      val arguments = List("http-atom.test.url",
        "http-atom.test.http-method",
        "http-atom.test.input-query-template"
      )
      val validation = variableManager.validateAndBuild(arguments, Map.empty, Map.empty, "")
      validation shouldBe a[Failure[_]]
    }

    val arguments = List("http-atom.test.url",
      "http-atom.test.http-method",
      "http-atom.test.input-content-type",
      "http-atom.test.username",
      "http-atom.test.password",
      "http-atom.test.input-query-template",
      "http-atom.test.output-content-type",
      "http-atom.test.output-status",
      "http-atom.test.output-data",
      "http-atom.test.output-score"
    )
    val configuration = Map("http-atom.test.url" -> "www.google.it",
      "http-atom.test.http-method" -> "GET",
      "http-atom.test.username" -> "user",
      "http-atom.test.password" -> "pwd",
      "http-atom.test.input-query-template" -> "<http-atom.test.param>=<http-atom.test.value>",
      "http-atom.test.param" -> "aaa",
      "http-atom.test.value" -> "bbb",
      "http-atom.test.output-content-type" -> "test.content-type",
      "http-atom.test.output-status" -> "test.status",
      "http-atom.test.output-data" -> "test.data",
      "http-atom.test.output-score" -> "test.score"
    )

    "validate atom if configuration is correct" in {
      val validation = variableManager.validateAndBuild(arguments, configuration, Map.empty, "")
      validation shouldBe a[Success[_]]
    }

    "fail if http-method post and no content-type" in {
      val brokenConf = configuration + ("http-atom.test.http-method" -> "POST")

      val validation = variableManager.validateAndBuild(arguments, brokenConf, Map.empty, "")
      validation shouldBe a[Failure[_]]
      validation.fold(_.toSet, _ => Set.empty[String]) should contain("input-content-type not found in configuration")
    }

    "fail if content-type is not a valid content type" in {
      val brokenConf = configuration + ("http-atom.test.http-method" -> "POST",
        "http-atom.test.input-content-type" -> "aaa")

      val validation = variableManager.validateAndBuild(arguments, brokenConf, Map.empty, "")
      validation shouldBe a[Failure[_]]
      validation.fold(_.toSet, _ => Set.empty[String]) should
        contain("Illegal HTTP header 'Content-Type': Invalid input 'EOI', expected tchar, OWS or '/' (line 1, column 4)")
    }

    "format template url" in {
      val newConf = configuration + ("http-atom.test.url" -> "www.google.it/<http-atom.test.url-parameter>",
        "http-atom.test.url-parameter" -> "aaaa")

      val conf: HttpRequestAtomicConfiguration = variableManager
        .validateAndBuild(arguments, newConf, Map.empty, "") match {
        case Success(conf) => conf
        case Failure(e) => throw new IllegalArgumentException(e.toList.mkString("; "))
      }

      conf.urlConf.url shouldEqual "www.google.it/aaaa"
    }

    "find conf in both system and analyzer data " in {
      val arguments = List("http-atom.test.url",
        "http-atom.test.http-method",
        "http-atom.test.username",
        "http-atom.test.password",
        "http-atom.test.input-query-template",
        "http-atom.test.output-content-type",
        "http-atom.test.output-status",
        "http-atom.test.output-data",
        "http-atom.test.output-score"
      )

      val systemConf = Map("http-atom.test.url" -> "www.google.it",
        "http-atom.test.http-method" -> "GET",
        "http-atom.test.username" -> "user",
        "http-atom.test.password" -> "pwd",
        "http-atom.test.input-query-template" -> "<http-atom.test.param>=<http-atom.test.value>",
        "http-atom.test.param" -> "aaa",
        "http-atom.test.value" -> "bbb",
      )
      val analyzerData = Map("http-atom.test.output-content-type" -> "test.content-type",
        "http-atom.test.output-status" -> "test.status",
        "http-atom.test.output-score" -> "test.score",
        "http-atom.test.output-data" -> "test.data")

      val validation = variableManager.validateAndBuild(arguments, systemConf, analyzerData, "")
      validation shouldBe a[Success[_]]
    }

    "find conf with temp values" in {
      val arguments = List("http-atom.test.url",
        "A__temp__.http-atom.test.http-method",
        "http-atom.test.username",
        "A__temp__.http-atom.test.password",
        "http-atom.test.input-query-template",
        "http-atom.test.output-content-type",
        "http-atom.test.output-status",
        "http-atom.test.output-data",
        "http-atom.test.output-score"
      )

      val analyzerData = Map("http-atom.test.output-content-type" -> "test.content-type",
        "http-atom.test.output-status" -> "test.status",
        "http-atom.test.output-data" -> "test.data",
        "http-atom.test.output-score" -> "test.score",
        "http-atom.test.url" -> "www.google.it",
        "A__temp__.http-atom.test.http-method" -> "GET",
        "http-atom.test.username" -> "user",
        "A__temp__.http-atom.test.password" -> "pwd",
        "http-atom.test.input-query-template" -> "<http-atom.test.param>=<A__temp__.http-atom.test.value>",
        "http-atom.test.param" -> "aaa",
        "A__temp__.http-atom.test.value" -> "bbb")

      val validation = variableManager.validateAndBuild(arguments, Map.empty, analyzerData, "")
      validation shouldBe a[Success[_]]
    }

    "fail if contains both json and querystring" in {
      val withJsonArguments = arguments :+ "http-atom.test.input-json"
      val withJsonConf = configuration + ("http-atom.test.input-json" -> """{"aa":"bb"}""")
      val validation = variableManager.validateAndBuild(withJsonArguments, withJsonConf, Map.empty, "")
      validation shouldBe a[Failure[_]]
      validation.fold(_.toSet, _ => Set.empty[String]) should
        contain("Both json and query string configuration enabled")
    }

    "fail if json has no substitutions " in {
      val withJsonArguments = arguments :+ "http-atom.test.input-json"
      val withJsonConf = (configuration
        + ("http-atom.test.input-json" -> """{"aa":"<unavailable.param>"}""")) - "http-atom.test.input-query-template"
      val validation = variableManager.validateAndBuild(withJsonArguments, withJsonConf, Map.empty, "")
      validation shouldBe a[Failure[_]]
      validation.fold(_.toSet, _ => Set.empty[String]) should
        contain("""Unable to found substitution in template: {"aa":"<unavailable.param>"}""")
    }

    "create conf and substitute json parameters" in {
      val withJsonArguments = arguments :+ "http-atom.test.input-json"
      val jsonConfiguration = Map("http-atom.test.url" -> "www.google.it",
        "http-atom.test.http-method" -> "POST",
        "http-atom.test.input-content-type" -> "application/json",
        "http-atom.test.input-json" -> """{"aa":"<http-atom.test.param>"}""",
        "http-atom.test.param" -> "aaa",
        "http-atom.test.output-content-type" -> "test.content-type",
        "http-atom.test.output-status" -> "test.status",
        "http-atom.test.output-data" -> "test.data",
        "http-atom.test.output-score" -> "test.score"
      )
      val validation = variableManager.validateAndBuild(withJsonArguments, jsonConfiguration, Map.empty, "")
      validation shouldBe a[Success[_]]
    }

    "create conf and substitute json parameters using argument configuration" in {
      val withJsonArguments = arguments :+ "http-atom.test.input-json" :+ "http-atom.test.param=aaa"
      val jsonConfiguration = Map("http-atom.test.url" -> "www.google.it",
        "http-atom.test.http-method" -> "POST",
        "http-atom.test.input-content-type" -> "application/json",
        "http-atom.test.input-json" -> """{"aa":"<http-atom.test.param>"}""",
        "http-atom.test.param" -> "aaa",
        "http-atom.test.output-content-type" -> "test.content-type",
        "http-atom.test.output-status" -> "test.status",
        "http-atom.test.output-data" -> "test.data",
        "http-atom.test.output-score" -> "test.score"
      )
      val validation = variableManager.validateAndBuild(withJsonArguments, jsonConfiguration, Map.empty, "")
      validation shouldBe a[Success[_]]
    }

    "create conf and substitute json parameters using argument query configuration" in {
      val withJsonArguments = arguments :+ "http-atom.test.input-json"
      val jsonConfiguration = Map("http-atom.test.url" -> "www.google.it",
        "http-atom.test.http-method" -> "POST",
        "http-atom.test.input-content-type" -> "application/json",
        "http-atom.test.input-json" -> """{"aa":"<query>"}""",
        "http-atom.test.output-content-type" -> "test.content-type",
        "http-atom.test.output-status" -> "test.status",
        "http-atom.test.output-data" -> "test.data",
        "http-atom.test.output-score" -> "test.score"
      )
      val validation = variableManager.validateAndBuild(withJsonArguments, jsonConfiguration, Map.empty, "aaa")
      validation shouldBe a[Success[_]]
    }

    "create a valid http atom configuration with spaces in input-query-template" in {
      val systemConf = SystemConfiguration
        .createMapFromPath("starchat.atom-values")
      val arguments = List("http-atom.test.url=www.google.it",
        "http-atom.test.http-method=GET",
        "http-atom.test.input-query-template",
        "http-atom.test.output-content-type=test.content-type",
        "http-atom.test.output-status=test.status",
        "http-atom.test.output-data=test.data",
        "http-atom.test.output-score=test.score"
      )
      val analyzerData = Map("http-atom.test.input-query-template" -> "aaa=b cd e")
      val configuration = variableManager.validateAndBuild(arguments,systemConf, analyzerData, "")

      configuration shouldBe a[Success[_]]
      configuration.foreach { conf =>
        val queryString = conf.inputConf.collect { case QueryStringConf(queryString) => queryString }.getOrElse("")
        queryString shouldEqual "aaa=b+cd+e"
      }
    }

    /*"test weather api call and do not execute call if done before and actual call is 0" in {
      val description = "desc"
      val humidity = "1"
      val temperature = "10"
      val cloudPerc = "20"
      val analyzerData = Map(
        "location" -> "Torino,IT",
        "weather.score" -> "1",
        "weather.status" -> "200 OK",
        "weather.description" -> description,
        "weather.humidity" -> humidity,
        "weather.temperature" -> temperature,
        "weather.cloud-perc" -> cloudPerc
      )
      val systemConf = SystemConfiguration.createMapFromPath("starchat.atom-values")
      val atom = new HttpRequestAtomic(List.empty, systemConf) with WeatherVariableManager

      val result = atom.evaluate("", AnalyzersDataInternal(extractedVariables = analyzerData))

      result.data.extractedVariables.foreach(println)
      result.data.extractedVariables.getOrElse("weather.score", "") shouldBe "1"
      result.data.extractedVariables.getOrElse("weather.status", "") shouldBe "200 OK"
      result.data.extractedVariables.getOrElse("weather.description", "") shouldEqual description
      result.data.extractedVariables.getOrElse("weather.humidity", "") shouldEqual humidity
      result.data.extractedVariables.getOrElse("weather.temperature", "") shouldEqual temperature
      result.data.extractedVariables.getOrElse("weather.cloud-perc", "") shouldEqual cloudPerc

    }*/

    "create a valid weather atom configuration" in {
      val variableManager = new WeatherVariableManager {}
      val systemConf = SystemConfiguration
        .createMapFromPath("starchat.atom-values")
      val configuration = variableManager.validateAndBuild(List("location=Torino,IT"), systemConf, Map.empty, "")
      configuration shouldBe a[Success[_]]
      configuration.map(println)
    }

    //TODO: these test can be enabled when the parameters are moved to a secret travis variable
    /*
    "test weather api call" in {
      val analyzerData = Map(
        "location" -> "Torino,IT"
      )
      val systemConf = SystemConfiguration.createMapFromPath("starchat.atom-values")
      val atom = new HttpRequestAtomic(List.empty, systemConf) with WeatherVariableManager
      val result = atom.evaluate("", AnalyzersDataInternal(extractedVariables = analyzerData))
      result.data.extractedVariables.foreach(println)
      result.data.extractedVariables.getOrElse("weather.score", "") shouldBe "1"
      result.data.extractedVariables.getOrElse("weather.status", "") shouldBe "200 OK"
    }

    "test weather api call with multi-token location" in {
      val analyzerData = Map(
        "location" -> "New York"
      )
      val systemConf = SystemConfiguration.createMapFromPath("starchat.atom-values")
      val atom = new HttpRequestAtomic(List.empty, systemConf) with WeatherVariableManager
      val result = atom.evaluate("", AnalyzersDataInternal(extractedVariables = analyzerData))
      result.data.extractedVariables.foreach(println)
      result.data.extractedVariables.getOrElse("weather.score", "") shouldBe "1"
      result.data.extractedVariables.getOrElse("weather.status", "") shouldBe "200 OK"
    }

    "create a valid date parser atom configuration " in {
      val variableManager = new ParseDateVariableManager {}
      val systemConf = SystemConfiguration
        .createMapFromPath("starchat.atom-values")

      val configuration = variableManager.validateAndBuild(List("language=en","timezone=GMT+1"), systemConf, Map.empty, "July 22nd, 1947")
      configuration shouldBe a [Success[_]]
      configuration.map(println)
    }

    "create a valid read s3 atom configuration" in {
      val variableManager = new ReadS3DataVariableManager {}
      val systemConf = SystemConfiguration
        .createMapFromPath("starchat.atom-values")

      val configuration = variableManager.validateAndBuild(List("s3-folder-id=demo","item-id=pippo"), systemConf, Map.empty, "")
      configuration shouldBe a [Success[_]]
      configuration.map(println)
    }
    */

  /*
    "create a valid husbot atom configuration" in {
      val variableManager = new SubmitHubspotVariableManager {}
      val systemConf = SystemConfiguration
        .createMapFromPath("starchat.atom-values")
      val analyzerData = Map("http-atom.submithubspot.input-email" -> "emanuele@getjenny.com")

      val configuration = variableManager.validateAndBuild(List.empty, systemConf, analyzerData, "")
      configuration shouldBe a[Success[_]]
      configuration.map(println)
    }
   */
    /*  "test call to hubspot" in {

        val analyzerData = Map("http-atom.submithubspot.input-email" -> "emanuele@getjenny.com")

        val atom = new HttpRequestAtomic(List.empty, Map.empty) with SubmitHubspotVariableManager

        val result = atom.evaluate("", AnalyzersDataInternal(data = analyzerData))
        result.data.extractedVariables.foreach(println)
      }*/

    /*"test dateParser" in {

      val analyzerData = Map(
        "language" -> "it",
        "timezone" -> "GMT+3"
      )

      val systemConf = SystemConfiguration
        .createMapFromPath("starchat.atom-values")

      val atom = new HttpRequestAtomic(List(), systemConf) with ParseDateVariableManager

      val result = atom.evaluate("24 Marzo 1985, 4 Aprile 2014, 2 Giugno 1989", AnalyzersDataInternal(extractedVariables = analyzerData))

      result.data.extractedVariables.foreach(println)
      result.data.extractedVariables.getOrElse("extracted_date.score", "") shouldBe "1"
      result.data.extractedVariables.getOrElse("extracted_date.status", "") shouldBe "200 OK"

      // 24th of March 1985
      result.data.extractedVariables.getOrElse("extracted_date.date_iso", "") shouldBe "1985-03-24T00:00:00"
      result.data.extractedVariables.getOrElse("extracted_date.year", "") shouldBe "1985"
      result.data.extractedVariables.getOrElse("extracted_date.month", "") shouldBe "03"
      result.data.extractedVariables.getOrElse("extracted_date.day_of_month", "") shouldBe "24"
      result.data.extractedVariables.getOrElse("extracted_date.day_of_week", "") shouldBe "7"
      result.data.extractedVariables.getOrElse("extracted_date.hour", "") shouldBe "00"
      result.data.extractedVariables.getOrElse("extracted_date.minute", "") shouldBe "00"
      result.data.extractedVariables.getOrElse("extracted_date.second", "") shouldBe "00"
      // 4th of April 2014
      result.data.extractedVariables.getOrElse("extracted_date.date_iso.1", "") shouldBe "2014-04-04T00:00:00"
      result.data.extractedVariables.getOrElse("extracted_date.year.1", "") shouldBe "2014"
      result.data.extractedVariables.getOrElse("extracted_date.month.1", "") shouldBe "04"
      result.data.extractedVariables.getOrElse("extracted_date.day_of_month.1", "") shouldBe "04"
      result.data.extractedVariables.getOrElse("extracted_date.day_of_week.1", "") shouldBe "5"
      result.data.extractedVariables.getOrElse("extracted_date.hour.1", "") shouldBe "00"
      result.data.extractedVariables.getOrElse("extracted_date.minute.1", "") shouldBe "00"
      result.data.extractedVariables.getOrElse("extracted_date.second.1", "") shouldBe "00"
      // 2nd of June 1989
      result.data.extractedVariables.getOrElse("extracted_date.date_iso.2", "") shouldBe "1989-06-02T00:00:00"
      result.data.extractedVariables.getOrElse("extracted_date.year.2", "") shouldBe "1989"
      result.data.extractedVariables.getOrElse("extracted_date.month.2", "") shouldBe "06"
      result.data.extractedVariables.getOrElse("extracted_date.day_of_month.2", "") shouldBe "02"
      result.data.extractedVariables.getOrElse("extracted_date.day_of_week.2", "") shouldBe "5"
      result.data.extractedVariables.getOrElse("extracted_date.hour.2", "") shouldBe "00"
      result.data.extractedVariables.getOrElse("extracted_date.minute.2", "") shouldBe "00"
      result.data.extractedVariables.getOrElse("extracted_date.second.2", "") shouldBe "00"
    }

    "test dateParser without date" in {

      val analyzerData = Map(
        "language" -> "en",
        "timezone" -> "GMT+3"
      )

      val systemConf = SystemConfiguration
        .createMapFromPath("starchat.atom-values")

      val atom = new HttpRequestAtomic(List(), systemConf) with ParseDateVariableManager

      val result = atom.evaluate("asdasd", AnalyzersDataInternal(extractedVariables = analyzerData))

      result.data.extractedVariables.foreach(println)
      result.data.extractedVariables.getOrElse("extracted_date.score", "") shouldBe "0"
      result.data.extractedVariables.getOrElse("extracted_date.status", "") shouldBe "200 OK"
    }*/

    /*
    "test nameParser" in {

      val systemConf = SystemConfiguration
        .createMapFromPath("starchat.atom-values")

      val atom = new HttpRequestAtomic(List(), systemConf) with ParseNameVariableManager

      val result = atom.evaluate("my name is titus, caius")
      println(result)

    }

     */

    /*
    "test entityExtractor" in {

      val systemConf = SystemConfiguration
        .createMapFromPath("starchat.entity-extractor")

      val atom = new HttpRequestAtomic(List("language=it"), systemConf) with EntityExtractorVariableManager

      val result = atom.evaluate("Milano e Roma son grandi città", AnalyzersDataInternal())
      println(result)

    }
    */
    /*
    "test s3 atom" in {

      val systemConf = SystemConfiguration
        .createMapFromPath("starchat.atom-values")

      val atom = new HttpRequestAtomic(List("s3-folder-id=demo","item-id=EI24ID30QB"), systemConf) with ReadS3DataVariableManager

      val result = atom.evaluate("", AnalyzersDataInternal())
      println(result)
      result.data.extractedVariables.foreach(println)
    }
     */
    /*
    "test ZendeskSearchTickets atom" in {

      val systemConf = SystemConfiguration
        .createMapFromPath("starchat.atom-values")

      val atom = new HttpRequestAtomic(List("user-email=customer@example.com"), systemConf) with ZendeskSearchTicketsVariableManager

      val result = atom.evaluate("", AnalyzersDataInternal())
      println(result)
      result.data.extractedVariables.foreach(println)
    }
     */
    /*
  "test ZendeskTicketComments atom" in {

    val systemConf = SystemConfiguration
      .createMapFromPath("starchat.atom-values")

    val atom = new HttpRequestAtomic(List("ticket-id=14"), systemConf) with ZendeskTicketCommentsVariableManager

    val result = atom.evaluate("", AnalyzersDataInternal())
    println(result)
    result.data.extractedVariables.foreach(println)
  }
     */
  }
}
