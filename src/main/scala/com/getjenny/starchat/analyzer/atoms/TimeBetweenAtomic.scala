/**
  * Created by Mario Alemi <mario@getjenny.com> on 29/1/2020.
  */

package com.getjenny.starchat.analyzer.atoms

import java.time._

import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

/**
  * Atomic for checking if current time is between two times (typically for checking
  * if it's business hour or not)
  *
  * It accepts three arguments:
  * Arg1 = opening time as "HH:mm"
  * Arg2 = closing time as "HH:mm"
  * Arg3 = Time Zone
  * Arg4 = current time in Time Zone if "" or missing, otherwise input time to be compared
  *
  * Ex: timeBetween("09:15", "19:30", "CET")
  *
  */
class TimeBetweenAtomic(val arguments: List[String],
                        restrictedArgs: Map[String, String]) extends AbstractAtomic {

  val atomName: String = "timeBetween"

  val regex: Regex = "([0-9]{1,2}):([0-9]{1,2})".r

  val parseArguments: () => Try[(LocalTime, LocalTime, LocalTime)] = {
    for {
      regex(openingTimeH, openingTimeM) <- arguments.headOption
      regex(closingTimeH, closingTimeM) <- arguments.lift(1)
      timeZone <- arguments.lift(2)
    }
      yield {
        () => Try {
          val open = LocalTime.parse("%02d:%02d".format(openingTimeH.toInt, openingTimeM.toInt))
          val close = LocalTime.parse("%02d:%02d".format(closingTimeH.toInt, closingTimeM.toInt))

          val compare = arguments.lift(3) match {
            case Some(currTimeArg) => currTimeArg match {
              case regex(compareH, compareM) => LocalTime.parse("%02d:%02d".format(compareH.toInt, compareM.toInt))
              case _ => throw ExceptionAtomic(atomName + ": unparsable format (must be HH:MM) for date")
            }
            case _ =>
              val zone = ZoneId.of(timeZone)
              LocalTime.now(zone)
          }
          (open, close, compare)
        }
      }
  }.getOrElse(throw ExceptionAtomic(atomName + ": missing arguments"))

  override def toString: String = "timeBetween(\"" + arguments.mkString(", ") + "\")"

  val isEvaluateNormalized: Boolean = true

  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {

    parseArguments() match {
      case Success((open, close, compare)) =>
        // Using compareTo and not isBefore / After bc want =
        if (open.compareTo(compare) <= 0 &&  close.compareTo(compare) >= 0)
          Result(score = 1.0)
        else
          Result(score = 0.0)
      case Failure(exception) => throw ExceptionAtomic("Error while parsing arguments: ", exception)
    }
  }
}
