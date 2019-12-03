package com.getjenny.starchat.resources

import com.getjenny.starchat.services.{InstanceRegistryDocument, InstanceRegistryService, InstanceRegistryStatus}

class InstanceRegistryTest extends TestBase {

  val instanceRegistry: InstanceRegistryService.type = InstanceRegistryService
  val indexName = "index_getjenny_english_test_0"
  val indexName2 = "index_getjenny_english_test_1"
  val invalidIndexName = "starchat_system.index_test"
  val timestamp = System.currentTimeMillis()

  "Instance Registry" should {

    "fail if index name is not valid" in {
       intercept[Exception]{
         instanceRegistry.addInstance(invalidIndexName)
      }
    }

    "map fields returned by elastic" in {
      val sourceMap = Map(
        "timestamp" -> 0L,
        "enabled" -> true,
        "delete" -> false,
        "deleted" -> false
      )
      val document = InstanceRegistryDocument(sourceMap)
      document.enabled.isDefined shouldEqual true
      document.delete.isDefined shouldEqual true
      document.deleted.isDefined shouldEqual true
    }

    "add an instance" in {
      instanceRegistry.addInstance(indexName)
      instanceRegistry.addInstance(indexName2)

      val instance = instanceRegistry.getInstance(indexName)
      val instance2 = instanceRegistry.getInstance(indexName2)

      instance.enabled.isDefined shouldEqual true
      instance.enabled shouldEqual Some(true)
      instance.status() shouldEqual InstanceRegistryStatus.Enabled
      instance2.enabled.isDefined  shouldEqual true
      instance2.enabled shouldEqual Some(true)
      instance2.status() shouldEqual InstanceRegistryStatus.Enabled
    }

    "fail if trying to search an invalid index name" in {
      intercept[Exception] {
        instanceRegistry.getInstance(invalidIndexName)
      }
    }

    "fail if trying to update instance with invalid index name" in {
      intercept[Exception] {
        instanceRegistry.disableInstance(invalidIndexName)
      }
    }

    "enable instance" in {
      instanceRegistry.enableInstance(indexName)

      val instance = instanceRegistry.getInstance(indexName)
      instance.enabled.isDefined shouldEqual true
      instance.enabled shouldEqual Some(true)
      instance.status() shouldEqual InstanceRegistryStatus.Enabled
    }

    "disable instance" in {
      instanceRegistry.disableInstance(indexName)

      val instance = instanceRegistry.getInstance(indexName)
      instance.enabled.isDefined shouldEqual true
      instance.enabled shouldEqual Some(false)
      instance.status() shouldEqual InstanceRegistryStatus.Disabled
    }

    "mark delete instance" in {
      instanceRegistry.markDeleteInstance(indexName)

      val instance = instanceRegistry.getInstance(indexName)
      instance.delete.isDefined shouldEqual true
      instance.delete shouldEqual Some(true)
      instance.status() shouldEqual InstanceRegistryStatus.MarkedForDeletion
    }

    "get all instances" in {

      val allInstances = instanceRegistry.getAll

      allInstances.nonEmpty shouldEqual true
      allInstances.size shouldEqual 2
    }

    "get all enabled instances timestamp" in {

      instanceRegistry.enableInstance(indexName)

      val allInstances = instanceRegistry.allEnabledInstanceTimestamp()

      allInstances.nonEmpty shouldEqual true


      allInstances.size shouldEqual 2

      val instancesMap = allInstances.map(x => x.indexName -> x.timestamp).toMap

      instancesMap.get(indexName2) shouldEqual Some(0)
    }
  }

}
