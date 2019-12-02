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
        "timestamp" -> 0l,
        "enabled" -> false,
        "delete" -> false,
        "deleted" -> false
      )
      val document = InstanceRegistryDocument(sourceMap)
      document.timestamp.isDefined shouldEqual true
      document.enabled.isDefined shouldEqual true
      document.delete.isDefined shouldEqual true
      document.deleted.isDefined shouldEqual true
    }

    "add an instance" in {
      instanceRegistry.addInstance(indexName)
      instanceRegistry.addInstance(indexName2)

      val instance = instanceRegistry.getInstance(indexName)
      val instance2 = instanceRegistry.getInstance(indexName2)

      instance.enabled.isDefined  shouldEqual true
      instance.enabled shouldEqual Some(false)
      instance.status() shouldEqual InstanceRegistryStatus.Disabled
      instance2.enabled.isDefined  shouldEqual true
      instance2.enabled shouldEqual Some(false)
      instance2.status() shouldEqual InstanceRegistryStatus.Disabled
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

    "delete entry" in {
      instanceRegistry.deleteEntry(List(indexName))

      val instance = instanceRegistry.getInstance(indexName)

      instance.isEmpty shouldEqual true
      instance.status() shouldEqual InstanceRegistryStatus.Missing
    }

    "recreate entry" in {
      instanceRegistry.addInstance(indexName)

      val instance = instanceRegistry.getInstance(indexName)
      instance.isEmpty shouldEqual false
      instance.status() shouldEqual InstanceRegistryStatus.Disabled
    }

    "update timestamp" in {
      instanceRegistry.updateTimestamp(indexName, timestamp, 1)

      val instance = instanceRegistry.instanceTimestamp(indexName)

      instance.timestamp shouldEqual timestamp
    }

    "get all enabled instances timestamp" in {

      instanceRegistry.enableInstance(indexName)

      val allInstances = instanceRegistry.allEnabledInstanceTimestamp()

      allInstances.nonEmpty shouldEqual true
      allInstances.size shouldEqual 1

      val instancesMap = allInstances.map(x => x.indexName -> x.timestamp).toMap

      instancesMap.get(indexName) shouldEqual Some(timestamp)
      instancesMap.get(indexName2) shouldEqual None
    }
  }

}
