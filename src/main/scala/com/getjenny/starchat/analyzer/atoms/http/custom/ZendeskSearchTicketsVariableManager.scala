package com.getjenny.starchat.analyzer.atoms.http.custom


import akka.http.scaladsl.model.{ContentTypes, HttpMethods, StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader._
import com.getjenny.starchat.analyzer.atoms.http.HttpRequestAtomicConstants.ParameterName._
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._
import spray.json._
import scalaz.Success

import scala.math.Ordering.Int.equiv

trait ZendeskSearchTicketsVariableManager extends GenericVariableManager {
  /** Implement an atom which provides all tickets relative to an user.
   * It accept as input:
   *
   * user-email:   customer@example.com
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
    val domain = findProperty("zendesk-domain")
      .toSuccessNel("zendesk domain not found, unable to perform request") match {
      case Success(x) => x
    }
    val url = s"https://d3v-$domain.zendesk.com/api/v2/search.json"
    HttpAtomUrlConf(url, HttpMethods.GET, ContentTypes.`application/x-www-form-urlencoded`).successNel
  }

  override def authenticationConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[Option[HttpAtomAuthConf]] = {

    val zendeskUser = findProperty("zendesk-user")
      .toSuccessNel("zendesk-user not found, unable to perform request") match {
      case Success(user) => user  + "/token"
    }
    val zendeskPassword = findProperty("zendesk-password")
      .toSuccessNel("zendesk password not found, unable to perform request") match {
      case Success(pwd) => pwd
    }

    val authMap = Map(username -> zendeskUser, password -> zendeskPassword)

    (as[String](username) |@| as[String](password)) ((u, p) => (u |@| p) (BasicAuth))
      .run(authMap)
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
