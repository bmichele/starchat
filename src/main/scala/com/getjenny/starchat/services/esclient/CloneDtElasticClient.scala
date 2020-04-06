package com.getjenny.starchat.services.esclient

object CloneDtElasticClient extends SystemElasticClient {
  override val indexSuffix: String = "clone_dt_staging_table"

  override val mappingPath = "/index_management/json_index_spec/general/state.json"
  override val updateMappingPath = "/index_management/json_index_spec/general/update/state.json"

  override val analyzerJsonPath: String = "/index_management/json_index_spec/english/analyzer.json"
}

