package com.getjenny.starchat.analyzer.atoms

/**
 * Created by Michele Boggia <michele.boggia@getjenny.com> on 14/10/2020.
 */

import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.getjenny.analyzer.entities.{AnalyzersDataInternal, StateVariables}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AtomsTest extends AnyWordSpec with Matchers with ScalatestRouteTest {

  val restrictedArgs = Map.empty[String, String]

  val analyzerData = Map(
    "variable1" -> "value1",
    "variable2" -> "value2",
  )

  val query = "whatever"

  "checkVariableContainsValue" should {
    "return score 1 when variable contains substring" in {
      val k = new CheckVariableContainsValue(List("variable1", "val"), restrictedArgs)
      k.evaluate(query, AnalyzersDataInternal(
        stateVariables = StateVariables(
          extractedVariables = analyzerData))
      ).score should be(1.0)
    }

    "return score 0 when variable does not contain substring" in {
      val k = new CheckVariableContainsValue(List("variable1", "xyz"), restrictedArgs)
      k.evaluate(query, AnalyzersDataInternal(
        stateVariables = StateVariables(
          extractedVariables = analyzerData))
      ).score should be(0.0)
    }

    "return score 0 when variablaName is not set" in {
      val k = new CheckVariableContainsValue(List("", "abc"), restrictedArgs)
      k.evaluate(query, AnalyzersDataInternal(
        stateVariables = StateVariables(
          extractedVariables = analyzerData))
      ).score should be(0.0)
    }

    "return score 0 when variableName corresponds to undefined variable" in {
      val k = new CheckVariableContainsValue(List("variable3", "value2"), restrictedArgs)
      k.evaluate(query, AnalyzersDataInternal(
        stateVariables = StateVariables(
          extractedVariables = analyzerData))
      ).score should be(0.0)
    }
  }

  "CheckVariableValue" should {
    "return score 1 when variableValue matches given value and variableName is given" in {
      val k = new CheckVariableValue(List("variable1", "value1"), restrictedArgs)
      k.evaluate(query, AnalyzersDataInternal(
        stateVariables = StateVariables(
          extractedVariables = analyzerData))
      ).score should be(1.0)
    }

    "return score 0 when variableValue does not match given value and variableName is set" in {
      val k = new CheckVariableValue(List("variable1", "val"), restrictedArgs)
      k.evaluate(query, AnalyzersDataInternal(
        stateVariables = StateVariables(
          extractedVariables = analyzerData))
      ).score should be(0.0)
    }

    "return score 0 when variableName is not set" in {
      val k = new CheckVariableValue(List("", "value1"), restrictedArgs)
      k.evaluate(query, AnalyzersDataInternal(
        stateVariables = StateVariables(
          extractedVariables = analyzerData))
      ).score should be(0.0)
    }
  }

}
