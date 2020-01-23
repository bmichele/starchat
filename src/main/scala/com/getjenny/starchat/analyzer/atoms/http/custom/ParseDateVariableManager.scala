package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{ContentTypes, HttpMethods, StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http._
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader._
import com.getjenny.starchat.analyzer.atoms.http.HttpRequestAtomicConstants.ParameterName._
import scalaz.Scalaz._
import spray.json._

/**
  * parseDate()
  *
  * If no date is found, score is 0.
  *
  * TODO ATM we are using the conf template which does not allow change of input-json. It should accept also other parameter (eg timezone, prefix)
  */

trait ParseDateVariableManager extends GenericVariableManager {

  override def configurationPrefix: Option[String] = Some("http-atom.parsedate")

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
