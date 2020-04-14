package com.getjenny.starchat.services.esclient

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 22/05/18.
 */

object ConversationLogsElasticClient extends QuestionAnswerElasticClient {
  override val indexName: String = ""
  override val indexSuffix: String = config.getString("es.logs_data_index_suffix")
  override val dictSizeCacheMaxSize: Int = config.getInt("es.dictSizeCacheMaxSize")
  override val totalTermsCacheMaxSize: Int = config.getInt("es.totalTermsCacheMaxSize")
  override val countTermCacheMaxSize: Int = config.getInt("es.countTermCacheMaxSize")
  override val cacheStealTimeMillis: Int = config.getInt("es.cacheStealTimeMillis")

  override val mappingPath: String = "/index_management/json_index_spec/general/question_answer.json"
  override val updateMappingPath: String = "/index_management/json_index_spec/general/update/question_answer.json"
  override val numberOfShards: Int = config.getInt("es.logs_data_idx_number_of_shards")
  override val numberOfReplicas: Int = config.getInt("es.logs_data_idx_number_of_replicas")
}
