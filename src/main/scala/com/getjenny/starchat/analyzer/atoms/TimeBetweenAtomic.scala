/**
  * Created by Mario Alemi <mario@getjenny.com> on 29/1/2020.
  */

package com.getjenny.starchat.analyzer.atoms

import java.time.format.DateTimeFormatter
import java.time._

import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}


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

  val openingTime: LocalTime = arguments.headOption match {
    case Some(t) =>
      try {
        LocalTime.parse(t)
      }
      catch {
        case _: Throwable => throw ExceptionAtomic(atomName + ": First argument is opening time and should be formatted as HH:mm")
      }
    case _ => throw ExceptionAtomic(atomName + ": must have 3 arguments (0)")
  }

  val closingTime: LocalTime = arguments.lift(1) match {
    case Some(t) => try {
      LocalTime.parse(t)    }
    catch {
      case _: Throwable => throw ExceptionAtomic(atomName + ": Second argument is closing time and should be a date formatted as HH:mm")
    }
    case _ => throw ExceptionAtomic(atomName + ": must have 3 arguments (1)")
  }

  val timeZone: ZoneId = arguments.lift(2) match {
    case Some(t) =>
      try {
        ZoneId.of(t)
      }
      catch {
        case _: Throwable => throw ExceptionAtomic(atomName + ": Third argument should be a valid timezone (eg CET, Europe/Moscow)")
      }
    case _ => throw ExceptionAtomic(atomName + ": must have 3 arguments (2)")
  }

  val compareTime: LocalTime = arguments.lift(3) match {
    case Some(t) =>
      try {
        t match {
          case "" => LocalTime.now(timeZone)
          case _ => LocalTime.parse(t)
        }
      }
      catch {
        case _: Throwable => throw ExceptionAtomic(atomName + ": Third argument is compareTime and should be formatted as HH:mm")
      }
    case _ => LocalTime.now(timeZone)
  }


  override def toString: String = "timeBetween(\"" + arguments.mkString(", ") + "\")"

  val isEvaluateNormalized: Boolean = true

  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    // Using compareTo and not isBefore / After bc want =
    if (openingTime.compareTo(compareTime) <= 0 &&  closingTime.compareTo(compareTime) >= 0)
      Result(score = 1.0)
    else
      Result(score = 0.0)
  }
}
