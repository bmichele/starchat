package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader._
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._
import scalaz.{Failure, NonEmptyList, Success}
import spray.json.DefaultJsonProtocol._
import spray.json._

/** Implement an atom which gets product info from an external service (designed as in GJ-DD-201130-0)
 *
 * Arguments (can be stored in bot variables) are:
 * readSheetsProductsByName.productName.0
 * readSheetsProductsByName.productName.1
 * ...
 * readSheetsProductsByName.productName.n
 *
 * The atom send a request to the external service with the payload template:
 * [<readSheetsProductsByName.productName.0>, <readSheetsProductsByName.productName.1>, ...]
 *
 * Usage example:
 * readSheetsProductsByName()
 *
 * Variables set by the atom:
 * %readSheetsProductsByName.count%: number of products found by external service
 * %readSheetsProductsByName.results.i%: String json containing details for a product retrieved from the external service
 * Note that, in general, product details stored in readSheetsProductsByName.results.i does not refer to product
 * readSheetsProductsByName.productName.i (there is a mismatch in indices if, e.g., one of the product names is not found by the external service)
 *
*/

trait ReadSheetsProductsByNameVariableManager extends GenericVariableManager {

  override def configurationPrefix: Option[String] = Some("http-atom.readSheetsProductsByName")

  // override inputConf so that an arbitrary number of product names can be stored in the input json
  override def inputConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[Option[HttpAtomInputConf]] = {

    // Get product names from variables (named "readSheetsProductsByName.productName.0", "readSheetsProductsByName.productName.1", ...)
    def lookupProductNameVariable(variableIndex: Int, findProperty: String => Option[String]): AtomValidation[String] = {
      val prodName = "<readSheetsProductsByName.productName." + variableIndex.toString + ">"
      substituteTemplate(prodName, findProperty)
    }

    lookupProductNameVariable(0, findProperty) match {
      case Success(_) =>
        val inputProductNames = Stream.iterate(0)(_ + 1)
          .takeWhile(lookupProductNameVariable(_, findProperty).isSuccess)
          .toList
          .map("<readSheetsProductsByName.productName." + _.toString + ">")
          .map(substituteTemplate(_, findProperty))
          .map { case Success(value) => value }
        Success(Some(JsonConf("[\"" + inputProductNames.mkString("\", \"") + "\"]")))
      case Failure(_) => Failure(NonEmptyList(s"Unable to find readSheetsProductsByName.productName.0"))
    }
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
