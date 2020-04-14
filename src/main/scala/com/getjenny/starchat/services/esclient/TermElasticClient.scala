package com.getjenny.starchat.services.esclient

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 12/03/17.
 */

object TermElasticClient extends ElasticClient {
  override val indexName: String = ""
  override val indexSuffix: String = config.getString("es.term_index_suffix")

  override val mappingPath: String = "/index_management/json_index_spec/general/term.json"
  override val updateMappingPath: String = "/index_management/json_index_spec/general/update/term.json"
  override val numberOfShards: Int = config.getInt("es.term_idx_number_of_shards")
  override val numberOfReplicas: Int = config.getInt("es.term_idx_number_of_shards")
}
