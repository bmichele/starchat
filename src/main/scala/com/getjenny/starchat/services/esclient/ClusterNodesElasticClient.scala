package com.getjenny.starchat.services.esclient

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 14/11/17.
 */

object ClusterNodesElasticClient extends SystemElasticClient {
  override val indexSuffix: String = "cluster_nodes"

  // alive nodes detection mechanism
  val clusterNodeAliveMaxInterval : Int = config.getInt("es.cluster_node_alive_max_interval")
  val clusterNodeAliveSignalInterval: Int = config.getInt("es.cluster_node_alive_sig_interval")
  val clusterNodeCleanDeadInterval: Int = config.getInt("es.cluster_node_clean_dead_interval")

  override val mappingPath = "/index_management/json_index_spec/system/cluster_nodes.json"
  override val updateMappingPath = "/index_management/json_index_spec/system/update/cluster_nodes.json"
}
