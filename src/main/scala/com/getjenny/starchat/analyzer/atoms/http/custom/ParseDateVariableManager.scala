package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{ContentTypes, HttpMethods, StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http._
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader._
import com.getjenny.starchat.analyzer.atoms.http.HttpRequestAtomicConstants.ParameterName._
import scalaz.Scalaz._
import spray.json._

/**
  * parseDate("query=July 22nd, 1947")
  *
  * If no date is found, score is 0
  */

trait ParseDateVariableManager extends GenericVariableManager {

  override def confParamsList: List[String] = {
    List("http-atom.parsedate.token",
      "http-atom.parsedate.url",
      "http-atom.parsedate.http-method",
      "http-atom.parsedate.authorization-type",
      "http-atom.parsedate.input-content-type",
      "http-atom.parsedate.store-to",
      "http-atom.parsedate.key",
      "http-atom.parsedate.header",
      "http-atom.parsedate.input-json"
    )
  }

  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf] = {
    ParseDateOutput().successNel
  }
}

case class ParseDateOutput(
                             override val score: String = "extracted_date.score",
                             responseStatus: String = "extracted_date.status",
                             date: String = "extracted_date.date"
                           ) extends HttpAtomOutputConf {

  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {
    if(StatusCodes.OK.equals(status)){
      val json = body.parseJson.asJsObject
      val dateList = json.fields.get("dates").map {
        case JsArray(dates) => dates.map(_.toString()).toArray
        case _ => Array.empty[String]
      }.getOrElse(Array.empty[String])

      val s = if(dateList.isEmpty) "0" else "1"

      Map(
        date -> dateList.headOption.getOrElse(""),
        score -> s,
        responseStatus -> status.toString
      )
    } else {
      Map(
        score -> "0",
        responseStatus -> status.toString
      )
    }

  }

}
