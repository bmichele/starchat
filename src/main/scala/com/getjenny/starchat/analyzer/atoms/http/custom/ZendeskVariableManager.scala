package com.getjenny.starchat.analyzer.atoms.http.custom


import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader._
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._
import scalaz.{Failure, NonEmptyList, Reader, Success, Validation, ValidationNel}

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

    val keyZendeskUser = "zendesk-user"
    val keyZendeskPassword = "zendesk-password"

    keyFromMap(configMap, keyZendeskUser) match {
      case Success(value) => if (value.endsWith("/token")) {
        (as[String](keyZendeskUser) |@|
          as[String](keyZendeskPassword)) ((u, p) => (u |@| p) (BasicAuth))
          .run(configMap)
          .map(_.some)
      } else {
        Failure(NonEmptyList(s"""$keyZendeskUser should end with "/token""""))
      }
    }

  }
}
