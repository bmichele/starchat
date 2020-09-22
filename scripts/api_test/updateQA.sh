#!/usr/bin/env bash

INDEX_NAME=${1:-index_getjenny_english_0}
ROUTE=${2:-conversation_logs}
PORT=${3:-8888}

curl -v -H "Authorization: Basic $(echo -n 'test_user:p4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X PUT "http://localhost:${PORT}/${INDEX_NAME}/${ROUTE}?refresh=wait_for" -d '{
  "id": ["eb9f400a-3de2-11ea-8cbf-0242ac110018_7"],
  "annotations": {
    "feedbackAnswerScore": 0.5,
  }
}'


