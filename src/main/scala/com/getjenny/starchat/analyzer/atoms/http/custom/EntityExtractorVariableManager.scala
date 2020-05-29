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
  * Supported entities are ATM: LOC, NAMES, CITY_FI
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
                            firstName: String = "extracted_entities.NAMES",
                            cityFi: String = "extracted_entities.CITY_FI",
                          ) extends HttpAtomOutputConf {

  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {
    if(StatusCodes.OK.equals(status)){
      val json = body.parseJson.asJsObject
      // map over json, match entity_tye with case (non-null), create a list of strings
      val extractedEntitiesMap = json.fields.filter(x => x._2.toString() != "null")

      val s = if(extractedEntitiesMap.isEmpty) "0" else "1"

      val entityTypeAndList = extractedEntitiesMap.map {
        case (entityType, JsArray(extractedEntities)) => (entityType, extractedEntities.toList.map(_.toString.stripPrefix("\"").stripSuffix("\"")))
        case _ => List(List())
      }.headOption.getOrElse(List())

      def outputMap(extractedEntities: List[Any], typeEntity: String) =
        extractedEntities.zipWithIndex.map(x => (typeEntity + "." + x._2, x._1.toString)).toMap

      val entityType = entityTypeAndList match {
        case ("LOC", head :: tail) => outputMap(head :: tail, location)
        case ("NAMES", head :: tail) => outputMap(head :: tail, firstName)
        case ("CITY_FI", head :: tail) => outputMap(head :: tail, cityFi)
        case _ => outputMap(List(), location)
      }

      entityType ++
      Map(
        score -> s,
        responseStatus -> status.toString,
      )
    } else {
      Map(
        score -> "0",
        responseStatus -> status.toString
      )
    }
  }
}
