package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader._
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._
import spray.json._

/**
  * entityExtractor("language=it", "entity_type=LOC")
  *
  * Connects to the service entity-extractor (https://github.com/GetJenny/entity-extractor)
  * and extract defined entity
  *
  * Query: "Milano e Roma non sono piccole"
  *
  * %extracted_entities.LOC.0% = "Milano"
  * %extracted_entities.LOC.1% = "Roma"
  *
  */

trait EntityExtractorVariableManager extends GenericVariableManager {

  override def configurationPrefix: Option[String] = Some("http-atom.entity-extractor")

  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf] = {
    EntityExtractorOutput().successNel
  }
}

case class EntityExtractorOutput(
                            override val score: String = "extracted_entities.score",
                            responseStatus: String = "extracted_entities.status",
                            location: String = "extracted_entities.LOC",
                          ) extends HttpAtomOutputConf {

  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {
    if(StatusCodes.OK.equals(status)){
      val json = body.parseJson.asJsObject
      val locationList = json.fields.get("LOC").map {
        case JsArray(locations) => locations.toArray.map(_.toString.stripPrefix("\"").stripSuffix("\""))
        case _ => Array.empty[String]
      }.getOrElse(Array.empty[String])

      val s = if(locationList.isEmpty) "0" else "1"

      (locationList.indices.map(x => location + "." + x) zip locationList).toMap ++
      Map(
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
