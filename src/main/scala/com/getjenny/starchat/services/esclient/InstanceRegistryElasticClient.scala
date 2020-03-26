package com.getjenny.starchat.services.esclient

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 14/11/17.
 */

object InstanceRegistryElasticClient extends SystemElasticClient {
  override val indexSuffix: String = config.getString("es.system_instance_registry_suffix")

  // decision table changes awareness per index mechanism
  val instanceRegistryDeleteFrequency : Int = config.getInt("es.instance_registry_delete_frequency")

  override val mappingPath = "/index_management/json_index_spec/system/instance_registry.json"
  override val updateMappingPath = "/index_management/json_index_spec/system/update/instance_registry.json"
}
