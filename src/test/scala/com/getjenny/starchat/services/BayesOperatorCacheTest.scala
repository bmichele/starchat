package com.getjenny.starchat.services

import com.getjenny.starchat.TestBase
import com.getjenny.starchat.entities.io.RefreshPolicy

class BayesOperatorCacheTest extends TestBase {

  private[this] val bayesOperatorCache = BayesOperatorCacheService

  private[this] val indexName = "index"

  "StarChat" should {

    "insert value in cache" in {
      bayesOperatorCache.put(indexName, "test", 5d, RefreshPolicy.`wait_for`)

      val value = bayesOperatorCache.get(indexName, "test")

      assert(value === Some(5d))
    }

    "update value in cache" in {
      bayesOperatorCache.put(indexName, "test", 8d, RefreshPolicy.`wait_for`)

      val value = bayesOperatorCache.get(indexName, "test")

      assert(value === Some(8d))
    }

    "get value, calculate and put it if not exists" in {
      val value = bayesOperatorCache.getOrElseUpdate(indexName,"test2", RefreshPolicy.`wait_for`){
        () => 55d
      }

      assert(value === 55d)

      val cacheResult = bayesOperatorCache.get(indexName,"test2")
      assert(cacheResult.isDefined)
      assert(cacheResult.getOrElse(0d) === 55d)
    }

    "bulk put values in cache" in {
      val values = List((indexName,"a", 1d), (indexName, "b", 2d))
      bayesOperatorCache.bulkPut(values, RefreshPolicy.`wait_for`)

      val res = bayesOperatorCache.get(indexName,"a")
      val res2 = bayesOperatorCache.get(indexName,"b")

      assert(res.isDefined)
      assert(res.getOrElse(0d) === 1d)
      assert(res2.isDefined)
      assert(res2.getOrElse(0d) === 2d)
    }

    "remove from cache" in {
      bayesOperatorCache.put(indexName,"test2", 5d, RefreshPolicy.`wait_for`)
      bayesOperatorCache.put(indexName,"test3", 5d, RefreshPolicy.`wait_for`)
      bayesOperatorCache.clear

      val value = bayesOperatorCache.get(indexName,"test")
      val value2 = bayesOperatorCache.get(indexName,"test2")
      val value3 = bayesOperatorCache.get(indexName,"test3")

      assert(value.isEmpty)
      assert(value2.isEmpty)
      assert(value3.isEmpty)
    }
  }


}
