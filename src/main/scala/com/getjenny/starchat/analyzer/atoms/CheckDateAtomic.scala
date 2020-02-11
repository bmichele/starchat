/**
  * Created by Andrea Collamati <andrea@getjenny.com> on 07/12/2019.
  */

package com.getjenny.starchat.analyzer.atoms

import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.{Duration, LocalDateTime, ZoneId}

import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import com.getjenny.analyzer.util.{ComparisonOperators, Time}

import scala.util.{Failure, Success, Try}


/**
  * Atomic to compare dates
  *
  * It accepts three arguments:
  * Arg1 = inputDate in ISO_LOCAL_DATE_TIME format
  * (https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#ISO_LOCAL_DATE_TIME) format
  * Arg2 = operator ("LessOrEqual","Less","Greater","GreaterOrEqual","Equal")
  * Arg3 = shift represent a delta time interval and is represented using the ISO-8601 duration format PnDTnHnMn.nS
  * with days considered to be exactly 24 hours (https://en.wikipedia.org/wiki/ISO_8601#Durations)
  * Arg4 = Time Zone (Possibly as Europe/Helsinki). Used only in case compareDate is current time
  * Arg5 = compareDate in ISO_LOCAL_DATE_TIME format. If "" is current date and time + shift
  * The atomic return boolean comparison result between inputDate and (compareToDate + shift)
  *
  * Ex: compareDate = CheckDate("2019-12-07T11:50:55","Greater","","P7D", "Europe/Helsinki") compare 1st december 2019 12:00:00 CET > Now() + 7 days
  *
  */
class CheckDateAtomic(val arguments: List[String],
                      restrictedArgs: Map[String, String]) extends AbstractAtomic {

  val atomName: String = "checkDate"

  val parseArguments: () => Try[(LocalDateTime, String, LocalDateTime)] = {
    for {
      argInputDate <- arguments.headOption
      argOperator <- arguments.lift(1)
      argShift <- arguments.lift(2)
      timeZone <- arguments.lift(3)
    } yield {
      () => Try {
        val shift = Duration.parse(argShift)
        val zone = ZoneId.of(timeZone)
        val compare = arguments.lift(4)
          .flatMap(x => if (x.isEmpty) None else Some(x))
          .map {
          x => LocalDateTime.parse(x, DateTimeFormatter.ISO_LOCAL_DATE_TIME).plusNanos(shift.toNanos)
        }.getOrElse(LocalDateTime.now(zone).plusNanos(shift.toNanos))

        ComparisonOperators.compare(0, 0, argOperator)

        (LocalDateTime.parse(argInputDate, DateTimeFormatter.ISO_LOCAL_DATE_TIME),
          argOperator,
          compare
        )
      }
    }
    }.getOrElse(throw ExceptionAtomic(atomName + ": must have arguments"))

  override def toString: String = "CheckDate(\"" + arguments + "\")"

  val isEvaluateNormalized: Boolean = true

  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {

    parseArguments() match {
      case Success((inputDate, operator, compare)) =>
        if (ComparisonOperators.compare(inputDate.compareTo(compare), 0, operator))
          Result(score = 1.0)
        else
          Result(score = 0.0)

      case Failure(exception) => throw ExceptionAtomic("Error while parsing date: ", exception)
    }
  }
}
