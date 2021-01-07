package com.getjenny.starchat.analyzer.atoms

import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import com.getjenny.analyzer.entities.{AnalyzersDataInternal, Result}
import scalaz.Scalaz._

class HasTravStateInPositionAtomic(arguments: List[String], restrictedArgs: Map[String, String]) extends AbstractAtomic{
  val state: String = arguments.headOption match {
    case Some(t) => t
    case _ =>
      throw ExceptionAtomic("hasTravStateInPosition: missing first argument")
  }
  val position: Int = arguments.lift(1) match {
    case Some(t) => t.toInt
    case _ =>
      throw ExceptionAtomic("hasTravStateInPosition: missing second argument")
  }

  val isEvaluateNormalized: Boolean = true

  override def toString: String = "hasTravStateInPosition(\"" + state + "\", \"" + position + "\")"

  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    data.stateVariables.traversedStates.lift(position-1) match {
      case Some(state) => if (state.state === this.state) Result(1) else Result(0)
      case _ => Result(0)
    }
  }
}
