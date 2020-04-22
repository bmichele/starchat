package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{ContentTypes, HttpMethods, HttpResponse, StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.VariableConfiguration
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._
import spray.json._

trait ZendeskSearchTicketsVariableManager extends GenericVariableManager {
  /** Implement an atom which provides all tickets relative to an user.
   * It accept as input:
   *
   * user-email:   customer@example.com
   *
   * zendeskSearchTickets("user-email=customer@example.com")
   *
   * Variables set:
   * %zendeskSearchTickets.count%: number of tickets found, e.g. 2
   * %zendeskSearchTickets.tickets%: String with ID and subject, e.g. "My printer is on fire!" (id: 14, open), "ui ticket" (id: 15, new)
   *
   * The idea is that the user can then ask info about a specific ticket with zendeskTicketInfo
   *
   */

  override def configurationPrefix: Option[String] = Some("http-atom.zendeskSearchTickets")
  val factoryName = s"zendeskSearchTickets"
  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]):
               AtomValidation[HttpAtomOutputConf] = {
    val itemId = findProperty("item-id")
      .toSuccessNel("item-id not found, unable to create output conf")
    itemId.map(x => ZendeskOutput(prefix = factoryName, score = factoryName + ".score"))
  }
}

case class ZendeskOutput(prefix: String,
                    override val score: String,
                   ) extends HttpAtomOutputConf {

  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {
    if(StatusCodes.OK.equals(status)){
      val json = body.parseJson.asJsObject

      val subject_id = body.parseJson.asJsObject.getFields("results").flatMap{ obj1: JsValue =>
        obj1.asInstanceOf[JsArray].elements.map { obj2 =>
          val fields = obj2.asJsObject.fields
          (fields.getOrElse("raw_subject", "unknown").toString.replaceAll("\"", ""),
            fields.getOrElse("id", "unknown").toString.replaceAll("\"", ""))
        }
      }.filter(x => x._1 =/= "unknown" || x._2 =/= "unknown").map { values =>
        s"""Ticket id ${values._2} with subject "${values._1}""""
      }.mkString("; ")

      val count = json.fields.get("count").toString  // there should be always at least 1 comment (the creation)

      Map(
        score -> "1",
        s"$prefix.status" -> status.toString,
        s"$prefix.count" -> count,
        s"$prefix.tickets" -> subject_id
      )
    } else {
      Map(
        score -> "0",
        s"$prefix.status" -> status.toString
      )
    }
  }

}
