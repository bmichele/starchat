package com.getjenny.starchat.analyzer.atoms.http.custom


import akka.http.scaladsl.model.{ContentTypes, HttpMethods, StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader._
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._
import spray.json._

import scala.math.Ordering.Int.equiv

trait ZendeskSearchTicketsVariableManager extends GenericVariableManager {
  /** Implement an atom which provides all tickets relative to an user.
   * It accept as input:
   *
   * user-email: email for which you want to retrieve tickets
   * zendesk-user: admin email to log in into zendesk
   * zendesk-password: zendesk password
   * zendesk-domain: the zendesk url used in the search is https://<zendesk-domain>.zendesk.com/api/v2/search.json
   *
   * zendeskSearchTickets(
   *                        "user-email=user@example.com",
   *                        "zendesk-user=customer@example.com",
   *                        "zendesk-password=1234",
   *                        "zendesk-domain=zendesk-customer-domain"
   *                      )
   *
   * Variables set:
   * %zendeskSearchTickets.count%: number of tickets found, e.g. 2
   * %zendeskSearchTickets.tickets%: String with ID and subject, e.g. "My printer is on fire!" (id: 14, open), "ui ticket" (id: 15, new)
   *
   * The idea is that the user can then ask info about a specific ticket with zendeskTicketInfo
   *
   */

  override def urlConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomUrlConf] = {
    as[String]("zendesk-domain")
      .run(configMap)
      .map{ domain =>
        val url = s"https://$domain.zendesk.com/api/v2/search.json"
        HttpAtomUrlConf(url, HttpMethods.GET, ContentTypes.`application/x-www-form-urlencoded`)
      }
  }

  override def authenticationConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[Option[HttpAtomAuthConf]] = {
    (as[String]("zendesk-user") |@|
      as[String]("zendesk-password")) ((u, p) => (u.map(_ + "/token") |@| p ) (BasicAuth))
      .run(configMap)
      .map(_.some)
  }

  override def configurationPrefix: Option[String] = Some("http-atom.zendeskSearchTickets")
  val factoryName = s"zendeskSearchTickets"
  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]):
               AtomValidation[HttpAtomOutputConf] = {
    ZendeskOutput().successNel
  }
}

case class ZendeskOutput(
                          override val score: String = "zendeskSearchTickets.score",
                          zendeskSearchTicketsStatus: String = "zendeskSearchTickets.status",
                          numberOfTickets: String = "zendeskSearchTickets.count",
                          ticketsIdAndSubject: String = "zendeskSearchTickets.tickets",
                        ) extends HttpAtomOutputConf {

  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {

    val scoreExtracted = "1"
    val scoreNotExtracted = "0"

    if(StatusCodes.OK.value === status.value){
      val json = body.parseJson.asJsObject

      val results = body.parseJson.asJsObject.getFields("results") match {
        case List(JsArray(elements)) => elements
      }

      val ticketDetails = results.map( x => x.asJsObject.getFields("raw_subject", "id")).toList.map {
        case List(subject, identifier) =>
          s"""Ticket id $identifier with subject $subject"""
      }.mkString("; ")

      val count = json.fields.getOrElse("count", 0).toString  // there should be always at least 1 comment (the creation)

      if (equiv(count.toInt, 0)) {
        Map(
          score -> scoreNotExtracted,
          zendeskSearchTicketsStatus -> status.toString
        )
      } else {
        Map(
          score -> scoreExtracted,
          zendeskSearchTicketsStatus -> status.toString,
          numberOfTickets -> count,
          ticketsIdAndSubject -> ticketDetails
        )
      }

    } else {
      Map(
        score -> scoreNotExtracted,
        zendeskSearchTicketsStatus -> status.toString
      )
    }
  }

}
