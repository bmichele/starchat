package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.VariableConfiguration
import com.getjenny.starchat.analyzer.atoms.http.{GenericVariableManager, HttpAtomOutputConf}
import scalaz.Scalaz._
import spray.json._

/**
 * Connect to the customer info api to get all customer info fields
 * ltCustomerInfo("lt-api-url=<base url>"")
 *
 * Takes <query> as parameter, query has to be a valid customer id
 */
trait LTCustomerInfoVariableManager extends GenericVariableManager{

  override def configurationPrefix: Option[String] = Some("http-atom.lt-customer-info")

  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf] = {
    LTCustomerInfoOutput().successNel
  }

}

case class LTCustomerInfoOutput(override val score: String = s"${LTCustomerInfoOutput.prefix}.score") extends HttpAtomOutputConf {
  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {
    if(StatusCodes.OK.equals(status)) {
      val json = body.parseJson
      json match {
        case JsArray(elements) =>
          elements
            .flatMap(_.asJsObject.fields.map {case (k,v) => s"${LTCustomerInfoOutput.prefix}.$k" -> v.toString})
            .toMap
        case _ => throw new IllegalArgumentException("Bad json format")
      }
    } else {
      Map(
        score -> "0",
        s"${LTCustomerInfoOutput.prefix}.status" -> status.toString
      )
    }
  }


}

object LTCustomerInfoOutput{
  val prefix = "ltCustomerInfo"
}