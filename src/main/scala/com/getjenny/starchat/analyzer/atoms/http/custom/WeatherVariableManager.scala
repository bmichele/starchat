package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{ContentTypes, HttpMethods, StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.{VariableConfiguration, as}
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._
import spray.json.DefaultJsonProtocol._
import spray.json._
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader._
import com.getjenny.starchat.analyzer.atoms.http.HttpRequestAtomicConstants.ParameterName._

/**
  * weather("location=London")
  */

trait WeatherVariableManager extends GenericVariableManager {

  override def configurationPrefix: Option[String] = Some("http-atom.weather")

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

  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {
    if(StatusCodes.OK.equals(status)) {
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
        weatherStatus -> status.toString
      )
    } else {
      Map(
        score -> "0",
        weatherStatus -> status.toString
      )
    }
  }

  private[this] def extractField(json: JsObject, obj: String, field: String): Double = {
    json.getFields(obj).headOption.map {
      case obj: JsObject => obj.fields(field).convertTo[Double]
      case _ => 0
    }.getOrElse(0)
  }

}
