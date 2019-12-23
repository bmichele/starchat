package com.getjenny.starchat.analyzer.atoms.http

import akka.http.scaladsl.model.{ContentTypes, HttpMethods, HttpResponse}
import akka.http.scaladsl.unmarshalling.Unmarshaller
import akka.stream.Materializer
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.VariableConfiguration
import scalaz.Scalaz._
import spray.json._
import spray.json.DefaultJsonProtocol._
import scala.concurrent.{ExecutionContext, Future}

trait WeatherVariableManager extends GenericVariableManager {

  override def urlConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[UrlConf] = {
    val url = "https://api.openweathermap.org/data/2.5/weather"
    UrlConf(url, HttpMethods.GET, ContentTypes.NoContentType).successNel
  }

  override def authenticationConf(configMap: VariableConfiguration): AtomValidation[Option[HttpAtomAuthentication]] = {
    Some(ApiKeyAuth("APPID", "750dc7a5473ac93110d1dc9fb35adaa8", StoreOption.QUERY)).successNel
  }

  override def inputConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[InputConf] = {
    val queryStringTemplate = "q=<http-atom.weather.location>&units=metric"
    substituteTemplate(queryStringTemplate, findProperty)
      .map(queryString => QueryStringConf(queryString))
  }

  override def outputConf(configMap: VariableConfiguration): AtomValidation[HttpAtomOutputConf] = {
    WeatherOutput().successNel
  }
}

case class WeatherOutput(override val score: String = "weather.score",
                         status: String = "weather.status",
                         description: String = "weather.description",
                         temperature: String = "weather.temperature",
                         umidity: String = "weather.humidity",
                         cloudPerc: String = "weather.cloud-perc"
                        ) extends HttpAtomOutputConf {

  override def toMap(response: HttpResponse)(implicit ec: ExecutionContext, materializer: Materializer): Future[Map[String, String]] = {
    Unmarshaller.stringUnmarshaller(response.entity)
      .map { body =>
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
          status -> response.status.toString
        )
      }
  }

  private[this] def extractField(json: JsObject, obj: String, field: String) = {
    json.getFields(obj).headOption.map {
      case obj: JsObject => obj.fields(field).convertTo[Double]
      case _ => 0
    }.getOrElse(0)
  }
}
