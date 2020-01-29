package com.getjenny.starchat.analyzer.atoms

/**
  * Created by Mario Alemi <mario@getjenny.com> on 29/01/2020
  */

import org.scalatest._

class AtomsTest extends FlatSpec with Matchers {

  val restrictedArgs = Map.empty[String, String]

  "An timeBetween atomic" should "always give true if opening and closing time are 00:00 and 23:59" in {
    val k = new TimeBetweenAtomic(List("00:00", "23:59", "CET"), restrictedArgs)
    k.evaluate("").score should be (1.0)
    k.matches("").score should be (1.0)
  }

}