package com.getjenny.starchat.analyzer.atoms.http
import akka.http.scaladsl.model.{ContentTypes, HttpMethods}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.VariableConfiguration
import scalaz.Success
trait SubmitHubspotVariableManager extends GenericVariableManager {

  override def urlConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[UrlConf] = {
    val urlTemplate = "https://forms.hubspot.com/uploads/form/v2/<http-atom.submithubspot.portal-id>/<http-atom.submithubspot.form-guid>"
    substituteTemplate(urlTemplate, findProperty)
      .map(url => UrlConf(url, HttpMethods.POST, ContentTypes.`application/x-www-form-urlencoded`))
  }

  override def inputConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[InputConf] = {
    val queryStringTemplate = "email=<http-atom.submithubspot.input-query-email>"
    substituteTemplate(queryStringTemplate, findProperty)
        .map(queryString => QueryStringConf(queryString))
  }

  override def outputConf(configMap: VariableConfiguration): AtomValidation[HttpAtomOutputConf] = {
    Success(GenericHttpOutputConf("submithubspot.content-type", "submithubspot.status", "submithubspot.data"))
  }

}
