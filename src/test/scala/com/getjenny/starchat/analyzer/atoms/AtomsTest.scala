package com.getjenny.starchat.analyzer.atoms

/**
 * Created by Michele Boggia <michele.boggia@getjenny.com> on 14/10/2020.
 */

import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.getjenny.analyzer.atoms.CheckVariableContainsValue
import com.getjenny.analyzer.expressions.AnalyzersDataInternal
import org.scalatest._
import org.scalatest.wordspec.AnyWordSpec

class AtomsTest extends AnyWordSpec with Matchers with ScalatestRouteTest {

  val restrictedArgs = Map.empty[String, String]

  val analyzerData = Map(
    "variable1" -> "value1",
    "variable2" -> "value2",
  )

  "checkVariableContainsValue" should {
    "return score 1 when variable contains substring" in {
      val k = new CheckVariableContainsValue(List("variable1", "val"), restrictedArgs)
      k.evaluate("whatever", AnalyzersDataInternal(extractedVariables = analyzerData)).score should be(1.0)
    }

    "return score 0 when variable does not contain substring" in {
      val k = new CheckVariableContainsValue(List("variable1", "xyz"), restrictedArgs)
      k.evaluate("whatever", AnalyzersDataInternal(extractedVariables = analyzerData)).score should be(0.0)
    }
  }

}
