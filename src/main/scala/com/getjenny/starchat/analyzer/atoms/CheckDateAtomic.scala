/**
  * Created by Andrea Collamati <andrea@getjenny.com> on 07/12/2019.
  */

package com.getjenny.starchat.analyzer.atoms

import java.time.format.DateTimeFormatter
import java.time.{Duration, ZonedDateTime}

import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import com.getjenny.analyzer.util.{ComparisonOperators, Time}


/**
  * Atomic to compare dates
  *
  * It accepts three arguments:
  * Arg1 = inputDate  in ISO_OFFSET_DATE_TIME format
  * (https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#ISO_OFFSET_DATE_TIME) format
  * Arg2 = operator ("LessOrEqual","Less","Greater","GreaterOrEqual","Equal")
  * Arg3 = compareDate (as inputDate).
  * Arg4 = shift represent a delta time interval and is represented using the ISO-8601 duration format PnDTnHnMn.nS
  * with days considered to be exactly 24 hours (https://en.wikipedia.org/wiki/ISO_8601#Durations)
  *
  * The atomic return boolean comparison result between inputDate and (compareToDate + shift)
  *
  * If compareDate == "" then compareDate should be substituted with current date and time + shift.
  *
  *
  *
  * Ex: compareDate = CheckDate("2019-12-07T11:50:55+01:00","Greater","","P7D") compare 1st december 2019 12:00:00 CET > Now() + 7 days
  *
  */
class CheckDateAtomic(val arguments: List[String],
                      restrictedArgs: Map[String, String]) extends AbstractAtomic {

  val atomName: String = "checkDate"

  val argInputDate: ZonedDateTime = arguments.headOption match {
    case Some(t) => {
      try {
        ZonedDateTime.parse(t, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      }
      catch {
        case _: Throwable => throw ExceptionAtomic(atomName + ": First argument should be a date formatted as ISO_OFFSET_DATE_TIME")
      }

    }
    case _ => throw ExceptionAtomic(atomName + ": must have four arguments")
  }

  val argOperator: String = arguments.lift(1) match {
    case Some(t) => try {
      // call compare method to validate t string is a valid operator
      ComparisonOperators.compare(0, 0, t)
      t
    }
    catch {
      case _: Throwable => throw ExceptionAtomic(atomName + ": Second argument should be a valid operator")
    }
    case _ => throw ExceptionAtomic(atomName + ": must have four arguments")
  }

  val argShift: Duration = arguments.lift(3) match {
    case Some(t) => {
      try {
        Duration.parse(t)
      }
      catch {
        case _: Throwable => throw ExceptionAtomic(atomName + ": First argument should be a duration formatted using ISO-8601 duration format ")
      }

    }
    case _ => throw ExceptionAtomic(atomName + ": must have four arguments")
  }

  val argCompareDate: ZonedDateTime = arguments.lift(2) match {
    case Some(t) => {
      try {
        t match {
          case "" => ZonedDateTime.now().plusNanos(argShift.toNanos)
          case _ => ZonedDateTime.parse(t, DateTimeFormatter.ISO_OFFSET_DATE_TIME).plusNanos(argShift.toNanos)
        }
      }
      catch {
        case _: Throwable => throw ExceptionAtomic(atomName + ": First argument should be a date formatted as ISO_OFFSET_DATE_TIME")
      }

    }
    case _ => throw ExceptionAtomic(atomName + ": must have four arguments")
  }


  override def toString: String = "CheckDate(\"" + arguments + "\")"

  val isEvaluateNormalized: Boolean = true

  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    if (ComparisonOperators.compare(argInputDate.toEpochSecond, argCompareDate.toEpochSecond, argOperator))
      Result(score = 1.0)
    else
      Result(score = 0.0)
  }
}
