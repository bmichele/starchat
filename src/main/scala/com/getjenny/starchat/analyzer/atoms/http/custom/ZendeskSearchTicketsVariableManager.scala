package com.getjenny.starchat.analyzer.atoms.http.custom

import java.awt.Container

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.VariableConfiguration
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._
import spray.json._

import scala.math.Ordering.Int.equiv

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
    val userEmail = findProperty("user-email")
      .toSuccessNel("user-email not found, unable to create output conf")
    userEmail.map(_ => ZendeskOutput(prefix = factoryName, score = factoryName + ".score"))
  }
}

case class ZendeskOutput(prefix: String,
                    override val score: String,
                   ) extends HttpAtomOutputConf {

  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {

    val scoreExtracted = "1"
    val scoreNotExtracted = "0"
    val labelOutputStatus = s"$prefix.status"

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
          labelOutputStatus -> status.toString
        )
      } else {
        Map(
          score -> scoreExtracted,
          labelOutputStatus -> status.toString,
          s"$prefix.count" -> count,
          s"$prefix.tickets" -> ticketDetails
        )
      }

    } else {
      Map(
        score -> scoreNotExtracted,
        labelOutputStatus -> status.toString
      )
    }
  }

}
