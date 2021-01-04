package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader._
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
 * Author Henri Vuorinen
 *
 * Implement an atom which gets product info from an external service (designed as in GJ-DD-201130-0)
 * Arguments (can be stored in the botVariables)
 * productFeature.what
 * productFeature.where
 * productFeature.material
 * productFeature.age
 * productFeature.previousTreatment
 * productFeature.desiredTreatment
 *
 * The atom sends a request to external service with a payload
 * {"what":"<productFeature.what>", "where": "<productFeature.where>"...}
 *
 * Example usage:
 * ReadSheetsProductsByFeatures(ARGUMENTSHERE)
 *
 * Variables set by atom:
 * %readSheetsProductsByFeature.count%: number of products found by external service
 * %readSheetsProductsByFeature.results.i%: String json containing details for a product retrieved from the external service
 */

trait ReadSheetsProductsByFeaturesVariableManager extends GenericVariableManager {

  override def configurationPrefix: Option[String] = Some("http-atom.readSheetsProductsByFeature")

  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf] = {
    ReadSheetsProductsByFeaturesOutput().successNel
  }
}

case class ReadSheetsProductsByFeaturesOutput(override val score: String = "readSheetsProductsByFeature.score",
                                              readSheetsProductsByFeatureStatus: String = "readSheetsProductsByFeature.status",
                                              numberOfProducts: String = "readSheetsProductsByFeature.count",
                                              results: String = "readSheetsProductsByFeature.results"
                                             ) extends HttpAtomOutputConf {

  override def bodyParser (body: String, ContentType: String, status: StatusCode): Map[String, String] = {

    if(StatusCodes.OK.equals(status)) {

      val jsonList = body.parseJson.convertTo[List[JsValue]]

      jsonList.zipWithIndex.map{case (item, i) => Tuple2(results + "." + i, item.toString)}.toMap ++
        Map(
          numberOfProducts -> jsonList.size.toString,
          score -> "1",
          readSheetsProductsByFeatureStatus -> status.toString
        )
    } else {
      Map(
        score -> "0",
        readSheetsProductsByFeatureStatus -> status.toString
      )
    }
  }
}
