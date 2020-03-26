#!/usr/bin/env bash

PORT=${1:-8888}
INDEX_NAME=${2:-index_getjenny_english_0}
#ROUTE=${3:-prior_data}
ROUTE=${3:-conversation_logs}
#ROUTE=${3:-knowledgebase}

curl -v -H "Authorization: Basic $(echo -n 'test_user:p4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X PUT "http://localhost:${PORT}/${INDEX_NAME}/${ROUTE}?refresh=wait_for" -d '{
  "id": ["443b47ea-3de3-11ea-9abb-0242ac11001e_7", "5be2ba6a-3de6-11ea-9235-0242ac11001e_6"],
  "coreData": {
    "verified": true,
    "done": true,
    "question": "blabla"
  },
  "annotations": {
    "doctype": "CANNED"
  }
}'


