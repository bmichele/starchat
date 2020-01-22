package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{ContentTypes, HttpMethods}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.VariableConfiguration
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._
import scalaz.Success

trait SubmitHubspotVariableManager extends GenericVariableManager {

  override def urlConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomUrlConf] = {
    val portalId = "4040542"
    val formGuid = "c1b44d8c-629f-49fe-9036-d32af2bfdc21"
    val url = s"https://forms.hubspot.com/uploads/form/v2/$portalId/$formGuid"
    HttpAtomUrlConf(url, HttpMethods.POST, ContentTypes.`application/x-www-form-urlencoded`).successNel
  }

  override def inputConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomInputConf] = {
    val queryStringTemplate = "email=<http-atom.submithubspot.input-email>"
    substituteTemplate(queryStringTemplate, findProperty)
      .map(queryString => QueryStringConf(queryString))
  }

  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf] = {
    Success(GenericHttpOutputConf("submithubspot.content-type", "submithubspot.status", "submithubspot.data", "submithubspot.score"))
  }

}
