package com.getjenny.starchat.services.esclient

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 14/11/17.
 */

object UserElasticClient extends SystemElasticClient {
  override val indexSuffix: String = config.getString("es.user_index_suffix")

  override val mappingPath = "/index_management/json_index_spec/system/user.json"
  override val updateMappingPath = "/index_management/json_index_spec/system/update/user.json"
}
