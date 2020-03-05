#!/usr/bin/env bash

PORT=${1:-8888}
INDEX_NAME=${2:-index_english}
UPDATE_TYPE=${3:-settings}
INDEX_SUFFIX=${4:-""}
if [[ ! -z ${INDEX_SUFFIX} ]]; then
  SUFFIX="&indexSuffix=${INDEX_SUFFIX}"
fi
curl -v -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
 -H "Content-Type: application/json" -X PUT "http://localhost:${PORT}/language_index_management/${UPDATE_TYPE}?index_name=${INDEX_NAME}${SUFFIX}"

