package com.getjenny.starchat.services.esclient

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 14/11/17.
 */

trait SystemElasticClient extends ElasticClient {
  override val indexName: String = config.getString("es.system_index_name")

  val authMethod: String = config.getString("starchat.auth_method")
  val autoInitializeSystemIndex: Boolean = config.getBoolean("es.auto_initialize_system_index")

  val numberOfShards: Int = config.getInt("es.system_idx_number_of_shards")
  val numberOfReplicas: Int = config.getInt("es.system_idx_number_of_replicas")

  val mappingPath: String
  val updateMappingPath: String

  val enableDelete: Boolean = config.getBoolean("es.enable_delete_system_index")

  val analyzerJsonPath: String = "/index_management/json_index_spec/system/analyzer.json"
}
