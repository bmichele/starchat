package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{ContentTypes, HttpMethods}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.{VariableConfiguration, as}
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._
import spray.json.DefaultJsonProtocol._
import spray.json._
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader._
import com.getjenny.starchat.analyzer.atoms.http.HttpRequestAtomicConstants.ParameterName._

trait WeatherVariableManager extends GenericVariableManager {


  override def additionalArguments: List[String] = {
    List("http-atom.weather.token")
  }

  override def urlConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomUrlConf] = {
    val url = "https://api.openweathermap.org/data/2.5/weather"
    HttpAtomUrlConf(url, HttpMethods.GET, ContentTypes.NoContentType).successNel
  }

  override def authenticationConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[Option[HttpAtomAuthConf]] = {
    as[String](token)
      .run(configMap)
        .map{token =>
          Some(ApiKeyAuth(key = "APPID", token = token, storeTo = StoreOption.QUERY))
        }
  }

  override def inputConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomInputConf] = {
    val queryStringTemplate = "q=<location>&units=metric"
    substituteTemplate(queryStringTemplate, findProperty)
      .map(queryString => QueryStringConf(queryString))
  }

  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf] = {
    WeatherOutput().successNel
  }
}

case class WeatherOutput(override val score: String = "weather.score",
                         weatherStatus: String = "weather.status",
                         description: String = "weather.description",
                         temperature: String = "weather.temperature",
                         umidity: String = "weather.humidity",
                         cloudPerc: String = "weather.cloud-perc"
                        ) extends HttpAtomOutputConf {

  override def bodyParser(body: String, contentType: String, status: String): Map[String, String] = {
    val json = body.parseJson.asJsObject
    val weatherDescription: String = json.getFields("weather").headOption.flatMap {
      case JsArray(elements) => elements.headOption.flatMap(e => e.asJsObject.fields.get("description"))
      case _ => None
    }.map(_.convertTo[String]).getOrElse("")
    val weatherTemperature = extractField(json, "main", "temp")
    val weatherHumidity = extractField(json, "main", "humidity")
    val weatherCloudPerc = extractField(json, "clouds", "all")
    Map(
      description -> weatherDescription,
      temperature -> weatherTemperature.toString,
      umidity -> weatherHumidity.toString,
      cloudPerc -> weatherCloudPerc.toString,
      score -> "1",
      weatherStatus -> status
    )
  }

  private[this] def extractField(json: JsObject, obj: String, field: String): Double = {
    json.getFields(obj).headOption.map {
      case obj: JsObject => obj.fields(field).convertTo[Double]
      case _ => 0
    }.getOrElse(0)
  }

}
