package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http._
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader._
import scalaz.Scalaz._
import spray.json._

/**
  * Connects to the service nameparser (https://github.com/GetJenny/nameparser).
  *
  * Query: "My name is titius or caius"
  *
  * Sets: %extracted_name.name% = Titius
  *
  * TODO (not necessarily...) Extract more than one name and set variables name.0, name.1 etc
  *
  */

trait ParseNlpVariableManager extends GenericVariableManager {

  override def configurationPrefix: Option[String] = Some("http-atom.parsenlp")

  // override def createArgumentConfiguration(arguments: List[String]): Map[String, String] = Map.empty

  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf] = {
    ParseNlpOutput().successNel
  }
}

case class ParseNlpOutput(
                            override val score: String = "extracted_name.score",
                            responseStatus: String = "extracted_name.status",
                            name: String = "extracted_name.name",
                          ) extends HttpAtomOutputConf {

  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {
    if(StatusCodes.OK.equals(status)){
      val json = body.parseJson.asJsObject
      val nameList = json.fields.get("names").map {
        case JsArray(names) => names.toArray.map(_.toString)
        case _ => Array.empty[String]
      }.getOrElse(Array.empty[String])

      val s = if(nameList.isEmpty) "0" else "1"

      val extractedName = nameList.headOption.getOrElse("")

      Map(
        name -> extractedName.stripPrefix("\"").stripSuffix("\""),
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
