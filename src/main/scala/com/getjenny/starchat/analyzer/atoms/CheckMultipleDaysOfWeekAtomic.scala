/**
  * Created by Andrea Collamati <andrea@getjenny.com> on 16/01/2020.
  *
  * Atomic to evaluate if the current day of week is included in a given list of days of week.
  *
  * Two arguments are required
  * - The list of days of week, represented as list of integer where 1=Monday, 2=Tuesday, ... 7=Sunday
  * - Timezone, represented with its string code (Ex:CET)
  *
  * Ex: CheckMultipleDaysOfWeekAtomic("[1,2,5,6,7]","CET"}
  *
  * Atomic Result Score is:
  * 1.0  if the current day of week is included in the argument list, 0.0 otherwise.
  */

package com.getjenny.starchat.analyzer.atoms

import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import com.getjenny.analyzer.util.Time
import java.time.ZoneId


class CheckMultipleDaysOfWeekAtomic(val arguments: List[String],
                                    restrictedArgs: Map[String, String]) extends AbstractAtomic {
  val atomName: String = "checkMultipleDaysOfWeek"
  override val isEvaluateNormalized: Boolean = true


  // daylist argument deserialization and  validation
  val dayList: List[Int] = arguments.headOption match {
    case Some(dayListString) => try {
      // arg example "[1,4,5]"
      val dayListSplit = dayListString.replace("[", "").replace("]", "").split(",").toList
      val tmp = dayListSplit.map(_.toInt)
      // validate days are in [1,7] range
      if (tmp.forall(i => i >= 1 && i <= 7))
        tmp
      else
        throw ExceptionAtomic(atomName + ": First argument should be a list of int (1-7) enclosed in square brackets")
    }
    case _ => throw ExceptionAtomic(atomName + ": First argument should be a list of int (1-7) enclosed in square brackets")
  }


  val timeZone: String = arguments.lift(1) match {
    case Some(t) => try {
      // check timezone validity. If not valid an exception is raised
      ZoneId.of(t)
      t
    }
    catch {
      case _: Throwable => throw ExceptionAtomic(atomName + ": Second argument should be a valid timezone")
    }
    case _ => throw ExceptionAtomic(atomName + ": must have two arguments")
  }

  override def evaluate(query: String, data: AnalyzersDataInternal): Result = {
    val currentDayOfWeek: Int = Time.dayOfWeekInt(timeZone)
    if (dayList.contains(currentDayOfWeek))
      Result(1.0)
    else
      Result(0.0)
  }
}
