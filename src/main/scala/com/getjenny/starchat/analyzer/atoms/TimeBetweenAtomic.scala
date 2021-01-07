/**
 * Created by Mario Alemi <mario@getjenny.com> on 29/1/2020.
 */

package com.getjenny.starchat.analyzer.atoms

import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import com.getjenny.analyzer.entities.{AnalyzersDataInternal, Result}

import java.time._
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

  private[this] def getHhMm(hhMM: Option[String], default: Option[LocalTime] = None): LocalTime = {
    hhMM match {
      case Some(v) => v match {
        case regex(timeH, timeM) =>
          LocalTime.parse("%02d:%02d".format(timeH.toInt, timeM.toInt))
        case _ => default match {
          case Some(defValue) => defValue
          case _ => throw ExceptionAtomic(atomName + s": unparsable format for date (must be HH:MM): $hhMM")
        }
      }
      case _ => throw ExceptionAtomic(atomName + s": time was no specified")
    }
  }

  private[this] val openTime = getHhMm(arguments.headOption)
  private[this] val closeTime = getHhMm(arguments.lift(1))

  private[this] val zone = Try(ZoneId.of(arguments.lift(2).getOrElse(""))) match {
    case Success(value) => value
    case Failure(e) => throw ExceptionAtomic("Error parsing timezone", e)
  }

  private[this] val compareTimeString: String = arguments.lift(3) match {
    case Some(v) => v
    case _ => ""
  }
  //LocalTime.now(zone)
  override def toString: String = "timeBetween(\"" + arguments.mkString(", ") + "\")"

  val isEvaluateNormalized: Boolean = true

  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    // Using compareTo and not isBefore / After bc want =

    val timeToBeCompared: LocalTime = compareTimeString.isEmpty match {
      case true => LocalTime.now(zone)
      case _ => getHhMm(Some(compareTimeString))
    }

    if (openTime.compareTo(timeToBeCompared) <= 0 &&  closeTime.compareTo(timeToBeCompared) >= 0)
      Result(score = 1.0)
    else
      Result(score = 0.0)
  }
}
