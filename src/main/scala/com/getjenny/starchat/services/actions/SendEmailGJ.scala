package com.getjenny.starchat.services.actions

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 26/04/19.
 */

import com.getjenny.starchat.entities.io.DtActionResult
import com.typesafe.config.{Config, ConfigFactory}
import courier.Defaults._
import courier._
import javax.mail.internet.InternetAddress
import spray.json.DefaultJsonProtocol._
import spray.json.JsObject

import scala.concurrent.Await
import scala.concurrent.duration._

object SendEmailGJ extends DtAction {
  val config: Config = ConfigFactory.load()
  val enabled: Boolean = config.getBoolean("starchat.actions.SendEmailGJ.enabled")
  val username: String = config.getString("starchat.actions.SendEmailGJ.username")
  val password: String = config.getString("starchat.actions.SendEmailGJ.password")
  val host: String = config.getString("starchat.actions.SendEmailGJ.host")
  val port: Int = config.getInt("starchat.actions.SendEmailGJ.port")
  val from: InternetAddress = new InternetAddress(config.getString("starchat.actions.SendEmailGJ.from"))

  override def apply(indexName: String, stateName: String, actionInput: Seq[JsObject]): DtActionResult = {

    if (!enabled) {
      throw DtActionException("Action SendEmailGJ is disabled")
    }

    val parameters = actionInput.headOption match {
      case Some(p) => p.convertTo[Map[String, String]]
      case _ => throw DtActionException("arguments map not provided")
    }

    val mailer = Mailer(host, port)
      .auth(true)
      .as(user = username, pass = password)
      .startTls(true)()

    var envelope: Envelope = Envelope.from(from)

    parameters.get("to")
      .filter(_.nonEmpty)
      .foreach { to =>
        to.replace(" ", "").split(";")
          .map(addr => new InternetAddress(addr))
          .foreach(addr => {
            envelope = envelope.to(addr)
          })
      }

    parameters.get("replyTo")
      .filter(_.nonEmpty)
      .foreach(replyTo =>
        envelope = envelope.replyTo(new InternetAddress(replyTo))
      )

    parameters.get("cc")
      .filter(_.nonEmpty)
      .foreach { cc =>
        cc.replace(" ", "").split(";")
          .map(addr => new InternetAddress(addr))
          .foreach(addr => envelope = envelope.cc(addr))
      }

    parameters.get("bcc")
      .filter(_.nonEmpty)
      .foreach { bcc =>
        bcc.replace(" ", "").split(";")
          .map(addr => new InternetAddress(addr))
          .foreach(addr => envelope = envelope.bcc(addr))
      }

    val html: String = parameters.getOrElse("html", "false")
    val body: String = parameters.getOrElse("body", "")

    parameters.get("subject")
      .filter(_.nonEmpty) match {
      case Some(v) => envelope = envelope.subject(v)
      case _ => throw DtActionException("Field missing on SendEmailGJ Action: subject")
    }

    val content: Content = html match {
      case "true" => Multipart().html(body)
      case _ => Text(body)
    }

    envelope = envelope.content(content)

    val sendRes = mailer(envelope)

    val actionRes = sendRes map { _ =>
      DtActionResult(success = true)
    } recover {
      case e: Exception =>
        log.error("Error sending email through SendEmailGJ index(" +
          indexName + ") stateName(" + stateName + ") Envelope{" + envelope.toString + "}", e.getMessage)
        DtActionResult(code = 1)
    }
    Await.result(actionRes, 5.second)
  }
}