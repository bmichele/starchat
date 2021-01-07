package com.getjenny.starchat.services

import com.getjenny.analyzer.entities.{DtHistoryItem, DtHistoryType}
import scalaz.Scalaz._

object LoopDetection {

  /** loop detection algorithm
   *
   * @param traversedStates: the list of history items
   * @param maxCycleLength: the max cycle length M to detect
   * @param examinedLength: the number N of groups of M states to examine.
   * @param timeThreshold: the threshold T in milliseconds to check the sum of the timestamps in
   *  the suspected loop. If the sum of the intervals between the states is smaller than the
   *  threshold then the sequence is more likely a loop
   * @return true if a loop was detected, otherwise false
   */
  def check(traversedStates: Vector[DtHistoryItem],
            maxCycleLength: Int = 10,
            examinedLength: Int = 10,
            timeThreshold: Long = 500
           ): Boolean = {
    val reversedV = traversedStates.reverse
    val loopPattern = reversedV.take(maxCycleLength)
    val groups = reversedV.sliding(maxCycleLength, maxCycleLength)
      .filter(_.length === maxCycleLength)
      .take(examinedLength).toList
    if(groups.length >= examinedLength) {
      val indicators = groups.map(group => {
        val isRepeated = group.zip(loopPattern).forall { case (e1, e2) => e1.state === e2.state }
        val areStatesInternal = group.forall(e => e.`type` == DtHistoryType.INTERNAL)
        val sumOfTsInGroup = group.map(_.timestamp).sliding(2, 1).map(x => x.head - x.last).sum
        (isRepeated, sumOfTsInGroup < timeThreshold, areStatesInternal)
      }).foldLeft((true, true, true))((acc, e) => (
        acc._1 && e._1, acc._2 && e._2, acc._3 && e._3
      ))
      indicators._1 && indicators._2 || indicators._3
    } else {
      false
    }
  }

}
