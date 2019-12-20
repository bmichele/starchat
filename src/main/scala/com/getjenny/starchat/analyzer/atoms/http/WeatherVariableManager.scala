package com.getjenny.starchat.analyzer.atoms.http

import akka.http.scaladsl.model.{ContentTypes, HttpMethods}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.VariableConfiguration
import scalaz.Scalaz._

trait WeatherVariableManager extends GenericVariableManager {

  override def urlConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[UrlConf] = {
    val url = "https://api.openweathermap.org/data/2.5/weather"
    UrlConf(url, HttpMethods.GET, ContentTypes.NoContentType).successNel
  }

  override def authenticationConf(configMap: VariableConfiguration): AtomValidation[Option[HttpAtomAuthentication]] = {
    Some(ApiKeyAuth("APPID", "750dc7a5473ac93110d1dc9fb35adaa8", StoreOption.QUERY)).successNel
  }

  override def inputConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[InputConf] = {
    val queryStringTemplate = "q=<http-atom.weather.location>"
    substituteTemplate(queryStringTemplate, findProperty)
        .map(queryString => QueryStringConf(queryString))
  }

  override def outputConf(configMap: VariableConfiguration): AtomValidation[HttpAtomOutputConf] = {
    GenericHttpOutputConf("weather.content-type", "weather.status", "weather.data", "weather.score").successNel
  }
}