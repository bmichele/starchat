package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.VariableConfiguration
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._
import spray.json._

trait ZendeskTicketCommentsVariableManager extends ZendeskVariableManager {
 /** Implement an atom which provides all tickets relative to an user.
  *
  * It accept as input:
  *
  * ticket-id:   ticket number researched
  * zendesk-user: admin email to log in into zendesk
  * zendesk-password: zendesk password
  * zendesk-domain: the zendesk url used in the search is https://<zendesk-domain>.zendesk.com/...
  *
  * zendeskTicketComments(
  *                         "ticket-id=14",
  *                          "zendesk-user=customer@example.com",
  *                          "zendesk-password=1234",
  *                          "zendesk-domain=zendesk-customer-domain"
  *                       )
  *
  * Variables set:
  *
  * %zendeskTicketComments.count%: number of comments found, e.g. 2
  * %zendeskTicketComments.comments%: String with ID and subject, e.g. "My printer is on fire!" (id: 14, open), "ui ticket" (id: 15, new)
  *
  * The idea is that the user can then ask info about a specific ticket with zendeskTicketInfo
  *
  */

  override def configurationPrefix: Option[String] = Some("http-atom.zendeskTicketComments")
  val factoryName = s"zendeskTicketComments"
  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]):
               AtomValidation[HttpAtomOutputConf] = {
    val ticketId = findProperty("ticket-id").toSuccessNel("ticket-id not found")
    ticketId.map(x => TicketCommentsOutput(prefix = factoryName,
      score = factoryName + ".score",
      commentsCount = factoryName + ".count",
      commentsInfo = factoryName + ".comments")
    )
  }
}

case class TicketCommentsOutput(prefix: String,
                                override val score: String,
                                commentsCount: String,
                                commentsInfo: String
                               ) extends HttpAtomOutputConf {

  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {
    if(StatusCodes.OK.equals(status)){
      val json = body.parseJson.asJsObject

      val commentsDate = body.parseJson.asJsObject.getFields("comments").flatMap{ obj1: JsValue =>
        obj1.asInstanceOf[JsArray].elements.map { obj2 =>
          val fields = obj2.asJsObject.fields
          (fields.getOrElse("plain_body", "unknown").toString.replaceAll("\"", ""),
            fields.getOrElse("created_at", "unknown").toString.replaceAll("\"", "").slice(1,10))
        }
      }.filter(x => x._1 =/= "unknown" || x._2 =/= "unknown").map { values =>
        s"${values._1} (${values._2})"
      }.mkString("; ")

      val count = json.fields.getOrElse("count", 0).toString  // there should be always at least 1 comment (the creation)

      Map(
        score -> "1",
        s"$prefix.status" -> status.toString,
        s"$prefix.count" -> count,
        s"$prefix.comments" -> commentsDate
      )
    } else {
      Map(
        score -> "0",
        s"$prefix.status" -> status.toString
      )
    }
  }

}
