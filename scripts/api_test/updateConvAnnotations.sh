#!/usr/bin/env bash

CONVID=${1:-"conv:1000"}
PORT=${2:-8888}
INDEX_NAME=${3:-index_getjenny_english_0}
#ROUTE=${4:-prior_data}
ROUTE=${4:-conversation_logs}
#ROUTE=${4:-knowledgebase}

curl -v -H "Authorization: Basic $(echo -n 'test_user:p4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/conv/annotations/${ROUTE}/convIdxCounter" -d "${CONVID}"

