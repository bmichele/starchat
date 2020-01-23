package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{ContentTypes, HttpMethods}
import com.getjenny.starchat.analyzer.atoms.http._
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader._
import com.getjenny.starchat.analyzer.atoms.http.HttpRequestAtomicConstants.ParameterName._
import scalaz.Scalaz._
import spray.json._


trait ParseDateVariableManager extends GenericVariableManager {

  override def additionalArguments: List[String] = {
    List("http-atom.parsedate.token")
  }

  override def urlConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomUrlConf] = {
    val url = "https://dateparser-0.getjenny.com/dateparser/search_dates"
    HttpAtomUrlConf(url, HttpMethods.POST, ContentTypes.`application/json`).successNel
  }

  override def authenticationConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[Option[HttpAtomAuthConf]] = {
    as[String](token)
      .run(configMap)
        .map{token =>
          Some(ApiKeyAuth(key = "key", token = token, storeTo = StoreOption.HEADER))
        }
  }

  override def inputConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomInputConf] = {
    val queryStringTemplate = """{"text": "<query>"}"""
    substituteTemplate(queryStringTemplate, findProperty)
      .map(queryString => JsonConf(queryString))
  }

  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf] = {
    ParseDateOutput().successNel
  }
}

case class ParseDateOutput(
                             override val score: String = "extracted_date.score",
                             status: String = "extracted_date.status",
                             date: String = "extracted_date.date"
                           ) extends HttpAtomOutputConf {

  override def bodyParser(body: String, contentType: String, status: String): Map[String, String] = {
    val json = body.parseJson.asJsObject
    val dateList = json.fields.get("dates").map {
      case JsArray(dates) => dates.map(_.toString()).toArray
      case _ => Array.empty[String]
    }.getOrElse(Array.empty[String])

    Map(
      date -> dateList.headOption.getOrElse(""),
      score -> "1",
      status -> status
    )
  }

}
