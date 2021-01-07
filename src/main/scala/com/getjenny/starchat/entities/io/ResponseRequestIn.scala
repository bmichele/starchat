package com.getjenny.starchat.entities.io

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 27/06/16.
 */

import com.getjenny.analyzer.entities.DtHistoryItem
import scalaz.Scalaz._

import scala.collection.immutable.Map

case class ResponseRequestInUserInput(
                                       text: Option[String] = None,
                                       img: Option[String] = None,
                                       audio: Option[String] = None
                                     )

case class ResponseRequestIn(conversationId: String,
                             traversedStates: Option[Vector[DtHistoryItem]] = Vector.empty[DtHistoryItem].some,
                             userInput: Option[ResponseRequestInUserInput] = None,
                             state: Option[List[String]] = None,
                             data: Option[Map[String, String]] = None,
                             threshold: Option[Double] = None,
                             evaluationClass: Option[String] = None,
                             maxResults: Option[Int] = None,
                             searchAlgorithm: Option[SearchAlgorithm.Value] = None
                            )
