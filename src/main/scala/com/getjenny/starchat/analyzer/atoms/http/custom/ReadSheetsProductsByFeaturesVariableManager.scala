package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader._
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._
import scalaz.Success
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
 * Author Henri Vuorinen
 *
 * ReadSheetsProductsByFeatures()
 */

trait ReadSheetsProductsByFeaturesVariableManager extends GenericVariableManager {

  override def configurationPrefix: Option[String] = Some("http-atom.readSheetsProductsByFeatures")

  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf] = {
    ReadSheetsProductsByFeaturesOutput().successNel
  }
}

case class ReadSheetsProductsByFeaturesOutput(override val score: String = "product_feature.score",
                                              where: String = "product_feature.target",
                                              what: String = "product_feature.what",
                                              material: String = "product_feature.material",
                                              age: String = "product_feature.age",
                                              previous_treatment: String = "product_feature.previousTreatment",
                                              desired_treatment: String = "product_feature.previousTreatment"
                                             ) extends HttpAtomOutputConf {

  override def bodyParser (body: String, ContentType: String, status: StatusCode): Map[String, String] = {
    if(StatusCodes.OK.equals(status)) {
      val json = body.parseJson.asJsObject
//      val productFeatures: String = json.getFields("where", "what", "material", "age", "previous_treatment", "desired_treatment").headOption.flatMap {
//        case JsArray(elements) => elements.headOption.flatMap(e => e.asJsObject.fields.get(productFeatures))
//        case _ => None
//      }.map(_.convertTo[String]).getOrElse("")
      val whereProduct = extractField(json, "main", "where")
      val whatProduct = extractField(json, "main", "what")
      val materialProduct = extractField(json, "main", "material")
      val ageProduct = extractField(json, "main", "age")
      val previousTreatmentProduct = extractField(json, "main", "previous_treatment")
      val desiredTreatmentProduct = extractField(json, "main", "desired_treatment")
      Map(
        where -> whereProduct.toString,
        what -> whatProduct.toString,
        material -> materialProduct.toString,
        age -> ageProduct.toString,
        previous_treatment -> previousTreatmentProduct.toString,
        desired_treatment -> desiredTreatmentProduct.toString,
        score -> "1"
      )
    } else {
      Map(
      score -> "0",
      null
      )
    }
  }

  private[this] def extractField(json: JsObject, obj: String, field: String): Double = {
    json.getFields(obj).headOption.map {
      case obj: JsObject => obj.fields(field).convertTo[Double]
      case _ => 0
    }.getOrElse(0)
  }
}
