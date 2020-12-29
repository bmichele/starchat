package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader._
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._
import scalaz.Success
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
 * readSheetsProductsByName()
 */

trait ReadSheetsProductsByNameVariableManager extends GenericVariableManager {

  override def configurationPrefix: Option[String] = Some("http-atom.readSheetsProductsByName")

  // override inputConf so that an arbitrary number of product names can be stored in the input json
  override def inputConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[Option[HttpAtomInputConf]] = {

    // Get product names from variables (named "productName.0", "productName.1", ..., "productName.N")
    def dummy(number: Int, findProperty: String => Option[String]): AtomValidation[String] = {
      val prodName = "<productName." + number.toString + ">"
      substituteTemplate(prodName, findProperty)
    }
    val inputProductNames = Stream.iterate(0)(_ + 1)
      .takeWhile(dummy(_, findProperty).isSuccess)
      .toList
      .map("<productName." + _.toString + ">")
      .map(substituteTemplate(_, findProperty))
      .map { case Success(value) => value }

    Success(Some(JsonConf("[\"" + inputProductNames.mkString("\", \"") + "\"]")))

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
