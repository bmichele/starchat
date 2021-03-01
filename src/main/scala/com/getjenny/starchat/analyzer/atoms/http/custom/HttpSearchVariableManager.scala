package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.VariableConfiguration
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
 * embeddingSearch()
 */

trait HttpSearchVariableManager extends GenericVariableManager {

  override def configurationPrefix: Option[String] = Some("http-atom.http-search")

  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf] = {
    HttpSearchOutput().successNel
  }
}

case class HttpSearchOutput(override val score: String = "http-search.score",
                            httpSearchStatus: String = "http-search.status",
                            vector: String = "http-search.vector",
                           ) extends HttpAtomOutputConf {

  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {
    if(StatusCodes.OK.equals(status)) {
      val json = body.parseJson.asJsObject
      val queryVector = json.getFields("vector")

      Map(
        vector -> queryVector.toString,
        score -> "1",
        httpSearchStatus -> status.toString
      )
    } else {
      Map(
        score -> "0",
        httpSearchStatus -> status.toString
      )
    }
  }

}
