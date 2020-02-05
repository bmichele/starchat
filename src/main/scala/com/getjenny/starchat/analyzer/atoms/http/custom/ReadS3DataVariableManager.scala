package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{ContentTypes, HttpMethods, HttpResponse, StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.VariableConfiguration
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._
import spray.json._

trait ReadS3DataVariableManager extends GenericVariableManager {
  /** Implement an atom which accept as input:
    *
    * s3-folder-id:   demoDishwasherProductIds
    * item-id:       XYZ1
    *
    * Will provide the information {"price": "350.00", "model": "Foo"}
    *
    * Because the demoDishwasherProductIds folder contains the following 2 files:
    *
    * XYZ1    content: {"price": "350.00", "model": "Foo"}
    * XYZ2    content: {"price": "1100.00", "model": "Bar"}
    *
    * atom will be called with:
    *
    * readS3Data("s3-folder-id=demoDishwasherProductIds","item-id=XYZ1")
    *
    * Variables set:
    * %XYZ1.price%
    * %XYZ1.model%
    * %XYZ1.properties% : will be price and model if they are there
    *
    */

  override def configurationPrefix: Option[String] = Some("http-atom.readS3Data")

  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]):
               AtomValidation[HttpAtomOutputConf] = {
    val itemId = findProperty("item-id")
      .toSuccessNel("item-id not found, unable to create output conf")
    itemId.map(x => S3Output(itemIdPrefix = x))
  }
}

case class S3Output(override val score: String = "readS3Data.score",
                    responseStatus: String = "readS3Data.status",
                    itemIdPrefix: String,
                    properties: String = "properties"
                   ) extends HttpAtomOutputConf {

  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {
    if(StatusCodes.OK.equals(status)){
      val json = body.parseJson.asJsObject
      val jsonMap = json.fields
      val extractedJsonProperties = jsonMap.keys.mkString(", ")
      val outputJsonMap = jsonMap.map {case (k,v) => s"$itemIdPrefix.$k" -> v.toString()}
      Map(
        score -> "1",
        responseStatus -> status.toString,
        properties -> extractedJsonProperties
      ) ++ outputJsonMap
    } else {
      Map(
        score -> "0",
        responseStatus -> status.toString
      )
    }
  }

}