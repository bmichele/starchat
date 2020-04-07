package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http._
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader._
import scalaz.Scalaz._
import spray.json._
import java.text.SimpleDateFormat
import java.util.Calendar
import DefaultJsonProtocol._

/**
  * parseDate("language=it", "timezone=US/Eastern")
  * output:
  * "extracted_date.score" 0 if no date is found
  * "extracted_date.status"
  * "extracted_date.date_iso" full datetime formatted as ISO_LOCAL_DATE_TIME (yyyy-MM-ddTHH:mm:ss)
  * "extracted_date.year" year - yyyy
  * "extracted_date.month" month - MM
  * "extracted_date.day_of_month" day of month - dd
  * "extracted_date.day_of_week" day of week ([1, ..., 7] = [Monday, ..., Sunday]) - d
  * "extracted_date.hour" hour - HH
  * "extracted_date.minute" minutes - mm
  * "extracted_date.second" seconds - ss
  * Note that if no date is found output date is 1970-01-01T00:00:00
  *
  * TODO ATM we are using the conf template which does not allow change of input-json. It should accept also other parameter (eg timezone, prefix)
  */

trait ParseDateVariableManager extends GenericVariableManager {

  override def configurationPrefix: Option[String] = Some("http-atom.parsedate")

  // override def createArgumentConfiguration(arguments: List[String]): Map[String, String] = Map.empty

  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf] = {
    ParseDateOutput().successNel
  }
}

case class ParseDateOutput(
                            override val score: String = "extracted_date.score",
                            responseStatus: String = "extracted_date.status",
                            date: String = "extracted_date.date_iso",
                            year: String = "extracted_date.year",
                            month: String = "extracted_date.month",
                            dayOfMonth: String = "extracted_date.day_of_month",
                            dayOfWeek: String = "extracted_date.day_of_week",
                            hour: String = "extracted_date.hour",
                            minute: String = "extracted_date.minute",
                            second: String = "extracted_date.second"
                          ) extends HttpAtomOutputConf {

  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {
    if(StatusCodes.OK.equals(status)){
      val json = body.parseJson.asJsObject
      val dateList = json.fields.get("dates").map {
        case JsArray(dates) => dates.map(_.convertTo[String]).toArray
        case _ => Array.empty[String]
      }.getOrElse(Array.empty[String])

      if(dateList.isEmpty) {
        Map(
          score -> "0",
          responseStatus -> status.toString
        )
      } else {

        val format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")

        def dateToMap(dateIso: String) = {
          val dateObj = format.parse(dateIso)
          val cal = Calendar.getInstance()
          cal.setTime(dateObj)
          Map(
            date -> dateIso,
            year -> "%04d".format(cal.get(Calendar.YEAR)),
            month -> "%02d".format(cal.get(Calendar.MONTH) + 1),
            dayOfMonth -> "%02d".format(cal.get(Calendar.DAY_OF_MONTH)),
            dayOfWeek -> "%01d".format(cal.get(Calendar.DAY_OF_WEEK) - 1 match {case 0 => 7; case n => n}),
            hour -> "%02d".format(cal.get(Calendar.HOUR_OF_DAY)),
            minute -> "%02d".format(cal.get(Calendar.MINUTE)),
            second -> "%02d".format(cal.get(Calendar.SECOND))
          )
        }

        def appendMapIndex(dateMap: Map[String, String], i: Int) = {
          i match {
            case 0 => dateMap
            case _ => dateMap.map { case(key, value) => key + "." + i -> value}
          }
        }

        val dateMapList = dateList.map(x => dateToMap(x))
        val dateOutList = (dateMapList.indices zip dateMapList).map { case (i, d) => appendMapIndex(d, i) }

        (dateOutList reduce (_ ++ _)) ++
          Map(
            score -> "1",
            responseStatus -> status.toString
          )
      }
    } else {
      Map(
        score -> "0",
        responseStatus -> status.toString
      )
    }
  }
}
