package com.getjenny.starchat.services.esclient

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 01/07/16.
 */

object DecisionTableElasticClient extends ElasticClient {
  override val indexName: String = ""
  override val indexSuffix: String = config.getString("es.dt_index_suffix")

  val queryMinThreshold : Float = config.getDouble("es.dt_query_min_threshold").toFloat
  val boostExactMatchFactor : Float = config.getDouble("es.dt_boost_exact_match_factor").toFloat
  val queriesScoreMode: String = config.getString("es.dt_queries_score_mode").toLowerCase
  val getNextResponseThreshold: Double =
    config.getDouble("starchat.get_next_response_threshold")

  val loopDetectionMaxCycleLength: Int = config.getInt("starchat.loopDetectionMaxCycleLength")
  val loopDetectionExaminedLength: Int = config.getInt("starchat.loopDetectionExaminedLength")
  val loopDetectionTimeThreshold: Long = config.getLong("starchat.loopDetectionTimeThreshold")

  val cleanDeletedDTStatesProcessInterval: Long =
    config.getLong("starchat.clean_deleted_dt_states_process_interval")
  val cleanDeletedDTStatesAge: Long = config.getLong("starchat.clean_deleted_dt_states_age")

  override val mappingPath: String = "/index_management/json_index_spec/general/state.json"
  override val updateMappingPath: String = "/index_management/json_index_spec/general/update/state.json"
  override val numberOfShards: Int = config.getInt("es.state_idx_number_of_shards")
  override val numberOfReplicas: Int = config.getInt("es.state_idx_number_of_replicas")
}
