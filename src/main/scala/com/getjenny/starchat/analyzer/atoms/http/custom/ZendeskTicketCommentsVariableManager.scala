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
  * zendesk-user: admin email to log in into zendesk (should end with /token)
  * zendesk-password: zendesk password
  * zendesk-domain: the zendesk url used in the search is https://<zendesk-domain>.zendesk.com/...
  *
  * zendeskTicketComments(
  *                         "ticket-id=14",
  *                          "zendesk-user=customer@example.com/token",
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
               AtomValidation[HttpAtomOutputConf] =
    TicketCommentsOutput().successNel
}

case class TicketCommentsOutput(
                                 override val score: String = "zendeskTicketComments.score",
                                 zendeskTicketCommentsStatus: String = "zendeskTicketComments.status",
                                 commentsCount: String = "zendeskTicketComments.count",
                                 commentsInfo: String = "zendeskTicketComments.comments"
                               ) extends HttpAtomOutputConf {

  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {

    if(StatusCodes.OK.equals(status)){
      val json = body.parseJson.asJsObject

      val comments = json.getFields("comments") match {
        case List(JsArray(elements)) => elements
      }

      val quoteMark = "\""

      val commentDetails = comments.map( x => x.asJsObject.getFields("plain_body", "created_at"))
        .toList.zipWithIndex
        .map {
        case (List(plainBody, createdAt), commentCount) => {
          "Comment " +
            (commentCount + 1).toString +
            ":\n" +
            plainBody.toString.replaceAll(quoteMark, "") +
            " (" +
            createdAt.toString.replaceAll(quoteMark, "").slice(0,10) +
            ")"
        }
      }.mkString("\n\n")

      val count = json.fields.getOrElse("count", 0).toString  // there should be always at least 1 comment (the creation)

      Map(
        score -> "1",
        zendeskTicketCommentsStatus -> status.toString,
        commentsCount -> count,
        commentsInfo -> commentDetails
      )
    } else {
      Map(
        score -> "0",
        zendeskTicketCommentsStatus -> status.toString
      )
    }
  }

}
