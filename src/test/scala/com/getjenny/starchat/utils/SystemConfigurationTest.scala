package com.getjenny.starchat.utils

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{Matchers, WordSpec}

class SystemConfigurationTest extends WordSpec with Matchers with ScalatestRouteTest{

  "System configuration" should {
    "create hashmap containing configuration in a path" in {
      val conf = SystemConfiguration.mapFromSystemConfiguration("starchat.atom-values")
      conf.foreach{case (k, v) => println(s"$k -> $v")}
      conf.contains("http-atom.default-timeout") shouldBe true
    }
  }

}
