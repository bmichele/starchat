package com.getjenny.analyzer.atoms

import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import scalaz.Scalaz._

/**
 * Created by angelo on 10/02/20.
 */

/** test if a variable exists on dictionary of variables and match the value, eg:
  *
  * checkVariableValue(variable_name, variable_value)
  *
  * @param arguments: <variable>, <value>
  */
class CheckVariableValue(val arguments: List[String], restrictedArgs: Map[String, String]) extends AbstractAtomic {
  val varName: String = arguments.headOption match {
    case Some(t) => t
    case _ => throw ExceptionAtomic("CheckVariableValue: must have a variable name")
  }
  val varValue: String = arguments.lift(1) match {
    case Some(t) => t
    case _ => throw ExceptionAtomic("CheckVariableValue: must have a value")
  }
  override def toString: String = "checkVariableValue"
  val isEvaluateNormalized: Boolean = true

  /** Check if a variable named <varname> exists on the data variables dictionary and match the <value>
   *
   * @param query the user query
   * @param data the dictionary of variables
   * @return Result with 1.0 if the variable exists and match the specified value score = 0.0 otherwise
   */
  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    if(data.extractedVariables.getOrElse(varName, varValue + ".") === varValue) {
      Result(score = 1.0)
    } else {
      Result(score = 0.0)
    }
  }
}