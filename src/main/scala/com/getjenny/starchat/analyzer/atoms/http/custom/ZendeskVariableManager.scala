package com.getjenny.starchat.analyzer.atoms.http.custom


import akka.http.scaladsl.model.{ContentTypes, HttpMethods}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader._
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._

trait ZendeskVariableManager extends GenericVariableManager {
  /**
   * authenticationConf used in zendesk integration atoms.
   * With this authenticationConf, atoms require the input arguments:
   *
   * zendesk-user: admin email to log in into zendesk
   * zendesk-password: zendesk password
   *
   */

  override def authenticationConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[Option[HttpAtomAuthConf]] = {
    (as[String]("zendesk-user") |@|
      as[String]("zendesk-password")) ((u, p) => (u.map(_ + "/token") |@| p) (BasicAuth))
      .run(configMap)
      .map(_.some)
  }

}
