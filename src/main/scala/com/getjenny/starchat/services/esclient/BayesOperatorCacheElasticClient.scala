package com.getjenny.starchat.services.esclient

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 01/07/16.
  */

object BayesOperatorCacheElasticClient extends SystemElasticClient {
  override val indexSuffix: String = "bayes_operator_cache"

  override val mappingPath = "/index_management/json_index_spec/system/bayes_operator_cache.json"
  override val updateMappingPath = "/index_management/json_index_spec/system/update/bayes_operator_cache.json"
}
