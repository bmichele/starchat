package com.getjenny.starchat.services.esclient

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 14/11/17.
 */

object NodeDtLoadingStatusElasticClient extends SystemElasticClient {
  override val indexSuffix: String = "decision_table_node_status"

  // decision table changes awareness per index mechanism
  val dtReloadTimestampFieldName = "timestamp"
  val dtReloadCheckFrequency : Int = config.getInt("es.dt_reload_check_frequency")
  val clusterCleanDtLoadingRecordsInterval: Int = config.getInt("es.cluster_clean_dt_loading_records_interval")

  override val mappingPath = "/index_management/json_index_spec/system/decision_table_node_status.json"
  override val updateMappingPath = "/index_management/json_index_spec/system/update/decision_table_node_status.json"
}
