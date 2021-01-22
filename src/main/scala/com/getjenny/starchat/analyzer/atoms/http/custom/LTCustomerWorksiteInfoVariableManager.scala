package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.VariableConfiguration
import com.getjenny.starchat.analyzer.atoms.http.{GenericVariableManager, HttpAtomInputConf, HttpAtomOutputConf}
import scalaz.Scalaz._
import spray.json._

/**
 * Author Henri Vuorinen
 * requires botVariable ltCustomerInfo.worksiteNo to make the GET request. Calling this analyzer happens using "ltCustomerWorksiteNo()", no query is needed.
 * This variable is set in the request url and it will give in response lot of different values.
 * For this atom to works LTCustomerInfoVariableManager needs to be run first to collect the needed ltCustomerInfo.worksiteNo value.
 *
 */

trait LTCustomerWorksiteInfoVariableManager extends GenericVariableManager {

  override def configurationPrefix: Option[String] = Some("http-atom.ltCustomerWorksiteNo")

  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf] = {
    LTCustomerWorksiteInfoOutput().successNel
  }
}

case class LTCustomerWorksiteInfoOutput(override val score: String = s"${LTCustomerWorksiteInfoOutput.prefix}.score",
                                        customerBackOfficeId: String = s"${LTCustomerWorksiteInfoOutput.prefix}.customerBackOfficeId",
                                        routePointBackOfficeId: String = s"${LTCustomerWorksiteInfoOutput.prefix}.routePointBackOfficeId",
                                        documentId: String = s"${LTCustomerWorksiteInfoOutput.prefix}.documentId",
                                        contentTypeItemName: String = s"${LTCustomerWorksiteInfoOutput.prefix}.contentTypeItemName",
                                        address: String = s"${LTCustomerWorksiteInfoOutput.prefix}.address",
                                        routeDate: String = s"${LTCustomerWorksiteInfoOutput.prefix}.routeDate",
                                        taskListDate: String = s"${LTCustomerWorksiteInfoOutput.prefix}.taskListDate",
                                        contentTypeName: String = s"${LTCustomerWorksiteInfoOutput.prefix}.contentTypeName",
                                        containerTypeName: String = s"${LTCustomerWorksiteInfoOutput.prefix}.containerTypeName",
                                        quantity: String = s"${LTCustomerWorksiteInfoOutput.prefix}.quantity",
                                        orderStatus: String = s"${LTCustomerWorksiteInfoOutput.prefix}.orderStatus",
                                        responseStatus: String = s"${LTCustomerWorksiteInfoOutput.prefix}.status"
                                       ) extends HttpAtomOutputConf {
  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {
    if(StatusCodes.OK.equals(status)) {
      val json = body.parseJson
      val data = json match{
        case JsArray(elements) =>
          elements
          .flatMap(_.asJsObject.fields.map {case (k,v) => k -> v.toString})
          .toMap
        case _ => throw new IllegalArgumentException("bad Json or Bad request")
      }
      Map(
        score -> "1",
        customerBackOfficeId -> data.getOrElse("CustomerBackOfficeId", ""),
        routePointBackOfficeId -> data.getOrElse("RoutePointBackOfficeId", ""),
        documentId -> data.getOrElse("DocumentId", ""),
        contentTypeItemName -> data.getOrElse("ContentTypeItemName", ""),
        address -> data.getOrElse("Address", ""),
        routeDate -> data.getOrElse("RouteDate", ""),
        taskListDate -> data.getOrElse("TaskListDate", ""),
        contentTypeName -> data.getOrElse("ContentTypeName", ""),
        containerTypeName -> data.getOrElse("ContainerTypeName", ""),
        quantity -> data.getOrElse("Quantity", ""),
        orderStatus -> data.getOrElse("Status", ""),
        responseStatus -> status.toString
      )
    } else {
      Map(
        score -> "0",
        responseStatus -> status.toString,
      )
    }
  }

}

object LTCustomerWorksiteInfoOutput{
  val prefix = "ltCustomerWorksiteInfo"
}
