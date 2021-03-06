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
  * To add entities, add the proper label (matching with entity-extractor) in:
  *  - `EntityExtractorOutput` arguments,
  *  - `EntityExtractorOutput.bodyParser.entities` list,
  *  - additional case in `EntityExtractorOutput.bodyParser.entityType` match statement.
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

    val scoreExtracted = "1"
    val scoreNotExtracted = "0"

    val labelLoc = "LOC"
    val labelNames = "NAMES"
    val labelCityFi = "CITY_FI"

    val entities = List(labelLoc, labelNames, labelCityFi)

    if(StatusCodes.OK.equals(status)){
      val jsonFields = body.parseJson.asJsObject.fields
      val extractedEntitiesMap = jsonFields.filter{
        case (entity, value) => (value.toString =/= "null") & entities.contains(entity)
      }

      val s = if(extractedEntitiesMap.isEmpty) scoreNotExtracted else scoreExtracted

      val quoteMark = "\""
      val entityTypeAndList = extractedEntitiesMap.map {
        case (entityType, JsArray(extractedEntities)) => (entityType, extractedEntities.toList.map(_.toString.stripPrefix(quoteMark).stripSuffix(quoteMark)))
        case _ => List(List())
      }.headOption.getOrElse(List())

      def outputMap(extractedEntities: List[Any], typeEntity: String): Map[String, String] = {
        val output = extractedEntities.zipWithIndex.map{ case (item, i) => Tuple2(typeEntity + "." + i, item.toString)}
        output.toMap
      }

      val entityType = entityTypeAndList match {
        case (`labelLoc`, head :: tail) => outputMap(head :: tail, location)
        case (`labelNames`, head :: tail) => outputMap(head :: tail, firstName)
        case (`labelCityFi`, head :: tail) => outputMap(head :: tail, cityFi)
        case _ => outputMap(List(), location)
      }

      entityType ++
      Map(
        score -> s,
        responseStatus -> status.toString,
      )
    } else {
      Map(
        score -> scoreNotExtracted,
        responseStatus -> status.toString
      )
    }
  }
}
