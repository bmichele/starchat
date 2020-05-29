/**
 * Created by Andrea Collamati <andrea@getjenny.com> on 07/12/2019.
 */

package com.getjenny.starchat.analyzer.atoms

import java.time.format.DateTimeFormatter
import java.time.{Duration, LocalDateTime, ZoneId}

import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import com.getjenny.analyzer.util.ComparisonOperators

import scala.util.{Failure, Success, Try}


/**
 * Atomic to compare dates
 *
 * It accepts these arguments:
 * Arg0 = inputDate in ISO_LOCAL_DATE_TIME format
 * (https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#ISO_LOCAL_DATE_TIME) format
 * Arg1 = operator ("LessOrEqual","Less","Greater","GreaterOrEqual","Equal")
 * Arg2 = shift represent a delta time interval and is represented using the ISO-8601 duration format PnDTnHnMn.nS
 * with days considered to be exactly 24 hours (https://en.wikipedia.org/wiki/ISO_8601#Durations)
 * Arg3 = Time Zone (Possibly as Europe/Helsinki). Used only in case compareDate is current time
 * Arg4 = compareDate in ISO_LOCAL_DATE_TIME format. If "" is current date and time + shift (Arg2)
 * The atomic return boolean comparison result between inputDate and (compareToDate + shift)
 *
 * Ex: compareDate = CheckDate("2019-12-07T11:50:55","Greater","","P7D", "Europe/Helsinki") compare 1st december 2019 12:00:00 CET > Now() + 7 days
 *
 */
class CheckDateAtomic(val arguments: List[String],
                      restrictedArgs: Map[String, String]) extends AbstractAtomic {

  val atomName: String = "checkDate"

  val inputDate: LocalDateTime = arguments.headOption match {
    case Some(t) => LocalDateTime.parse(t, DateTimeFormatter.ISO_LOCAL_DATE_TIME)
    case _ => throw ExceptionAtomic("CheckDateAtomic: arg 0/4 must be a time")
  }

  val operator: String = arguments.lift(1) match {
    case Some(t) => t
    case _ => throw ExceptionAtomic("CheckDateAtomic: arg 1/4 must be an operator string")
  }

  val shift: Duration = arguments.lift(2) match {
    case Some(t) => Duration.parse(t)
    case _ => throw ExceptionAtomic("CheckDateAtomic: arg 2/4 must be a duration")
  }

  val timeZone: ZoneId = arguments.lift(3) match {
    case Some(t) => ZoneId.of(t)
    case _ => throw ExceptionAtomic("CheckDateAtomic: arg 3/4 must be a timezone")
  }

  val timeToBeComparedString: String = arguments.lift(4) match {
    case Some(t) => t
    case _ => throw ExceptionAtomic("CheckDateAtomic: arg 4/4 must be a ISO_LOCAL_DATE_TIME string or an empty string")
  }

  override def toString: String = "CheckDate(\"" + arguments + "\")"

  val isEvaluateNormalized: Boolean = true

  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {

    val timeToBeCompared: LocalDateTime = timeToBeComparedString.isEmpty match {
      case true => LocalDateTime.now(timeZone).plusNanos(this.shift.toNanos)
      case _ => LocalDateTime.parse(timeToBeComparedString, DateTimeFormatter.ISO_LOCAL_DATE_TIME).plusNanos(shift.toNanos)
    }
    if (ComparisonOperators.compare(inputDate.compareTo(timeToBeCompared), 0, operator))
      Result(score = 1.0)
    else
      Result(score = 0.0)
  }
}
