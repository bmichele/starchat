package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader._
import com.getjenny.starchat.analyzer.atoms.http.HttpRequestAtomicConstants.ParameterName.inputJson
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
 * readSheetsProductsByName()
 */

trait ReadSheetsProductsByNameVariableManager extends GenericVariableManager {

  override def configurationPrefix: Option[String] = Some("http-atom.readSheetsProductsByName")

  override def inputConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[Option[HttpAtomInputConf]] = {

    extractInput[JsonConf](inputJson,
      configMap,
      findProperty) {
      JsonConf
      }.map(_.some)

  }

  private def extractInput[T](varName: String,
                              configMap: VariableConfiguration,
                              findProperty: String => Option[String])(buildInput: String => T): AtomValidation[T] = {
    as[String](varName)
      .run(configMap)
      .flatMap(t => substituteTemplate(t, findProperty))
      .map(buildInput)
  }

  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf] = {
    ReadSheetsProductsByNameOutput().successNel
  }
}

case class ReadSheetsProductsByNameOutput(override val score: String = "readSheetsProductsByName.score",
                                          readSheetsProductsByNameStatus: String = "readSheetsProductsByName.status",
                                          numberOfProducts: String = "readSheetsProductsByName.count",
                                          results: String = "readSheetsProductsByName.results"
                                         ) extends HttpAtomOutputConf {



  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {
    if(StatusCodes.OK.equals(status)) {

      val jsonList = body.parseJson.convertTo[List[JsValue]]

      jsonList.zipWithIndex.map{case (item, i) => Tuple2(results + "." + i, item.toString)}.toMap ++
      Map(
        numberOfProducts -> jsonList.size.toString,
        score -> "1",
        readSheetsProductsByNameStatus -> status.toString
      )
    } else {
      Map(
        score -> "0",
        readSheetsProductsByNameStatus -> status.toString
      )
    }
  }


}
