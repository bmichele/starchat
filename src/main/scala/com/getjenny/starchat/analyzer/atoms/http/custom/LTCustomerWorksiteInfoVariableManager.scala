package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.VariableConfiguration
import com.getjenny.starchat.analyzer.atoms.http.{GenericVariableManager, HttpAtomInputConf, HttpAtomOutputConf}
import scalaz.Scalaz._
import spray.json._

/**
 * Author Henri Vuorinen
 * requires botVariable customerWorksiteNo.result to make the GET request.
 * This variable is set in the request url and it will give in response lot of different values.
 * For this atom to works LTCustomerInfoVariableManager needs to be run first
 *
 */

trait LTCustomerWorksiteInfoVariableManager extends GenericVariableManager {

  override def configurationPrefix: Option[String] = Some("http-atom.ltCustomerWorksiteNo")

  //override def inputConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[Option[HttpAtomInputConf]] = {
//
  //
  //}

  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf] = {
    LTCustomerWorksiteInfoOutput().successNel
  }
}

case class LTCustomerWorksiteInfoOutput(override val score: String = s"${LTCustomerWorksiteInfoOutput.prefix}.score",
                                        customerBackOfficeId: String = "ltCustomerWorksiteNo.customerBackOfficeId",
                                        routePointBackOfficeId: String = "ltCustomerWorksiteNo.routePointBackOfficeId",
                                        documentId: String = "ltCustomerWorksiteNo.documentId",
                                        contentTypeItemName: String = "ltCustomerWorksiteNo.contentTypeItemName",
                                        address: String = "ltCustomerWorksiteNo.address",
                                        routeDate: String = "ltCustomerWorksiteNo.routeDate",
                                        taskListDate: String = "ltCustomerWorksiteNo.taskListDate",
                                        contentTypeName: String = "ltCustomerWorksiteNo.contentTypeName",
                                        containerTypeName: String = "ltCustomerWorksiteNo.containerTypeName",
                                        quantity: String = "ltCustomerWorksiteNo.quantity",
                                        orderStatus: String = "ltCustomerWorksiteNo.orderStatus"
                                       ) extends HttpAtomOutputConf {
  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {
    if(StatusCodes.OK.equals(status)) {
      val json = body.parseJson
      val data = json match{
        case JsArray(elements) =>
          elements
          .flatMap(_.asJsObject.fields.map {case (a,b) => s"${LTCustomerWorksiteInfoOutput.prefix}.$a" -> b.toString})
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
        routeDate -> data.getOrElse("RouteData", ""),
        taskListDate -> data.getOrElse("TaskListDate", ""),
        contentTypeName -> data.getOrElse("ContentTypeName", ""),
        containerTypeName -> data.getOrElse("ContainerTypeName", ""),
        quantity -> data.getOrElse("Quantity", ""),
        orderStatus -> data.getOrElse("Status", ""),
        s"${LTCustomerInfoOutput.prefix}.status" -> status.toString
      )
    } else {
      Map(
        score -> "0",
        s"${LTCustomerWorksiteInfoOutput.prefix}.status" -> status.toString
      )
    }
  }

}

object LTCustomerWorksiteInfoOutput{
  val prefix = "ltCustomerWorksiteInfo"
}
