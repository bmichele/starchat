package com.getjenny.analyzer.atoms

import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import scalaz.Scalaz._
import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import scala.util.matching.Regex

/**
 * Created by Henri Vuorinen on 14/10/2020.
 */

/** test if a variable exists on dictionary of variables and contains the value, eg:
 *
 * checkVariableContainsValue(variableName, variableValue)
 *
 * @param arguments: <variable>, <value>
 */

class CheckVariableContainsValue(val arguments: List[String], restrictedArgs: Map[String, String]) extends AbstractAtomic {
  val varName: String = arguments.headOption match {
    case Some(t) => t
    case _ => throw ExceptionAtomic("CheckVariableContainsValue: variable must have a name")
  }
  val varValue: String = arguments.lift(1) match{
    case Some(t) => t
    case _ => throw ExceptionAtomic("CheckVariableContainsValue: variable must have a value")
  }
  override def toString: String = "CheckVariableContainsValue"

  override val isEvaluateNormalized: Boolean = true

  /** Check if a variable named <varname> exists on the data variables dictionary and contains the <value>
   *
   * @param query the user query
   * @param data  the dictionary of variables
   * @return Result with 1.0 if the variable contains the specified value score = 0.0 otherwise
   */

  override def evaluate(query: String, data: AnalyzersDataInternal): Result = {
    if(data.extractedVariables.getOrElse(varName, varValue + ".").contains(varValue)) {
      Result(score = 1.0)
    } else {
      Result(score = 0.0)
    }
  }

}
