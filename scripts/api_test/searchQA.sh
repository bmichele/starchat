#!/usr/bin/env bash

QUERY="${1}"
TSLTE=${2:-1603379617000}
PORT=${3:-8443}
INDEX_NAME=${4:-index_getjenny_english_0}
ROUTE=${5:-conversation_logs}

curl -k -H "Authorization: Basic $(echo -n 'test_user:p4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "https://localhost:${PORT}/${INDEX_NAME}/${ROUTE}/search" -d "{
  \"coreData\": {
    \"question\": \"${QUERY}\"
  },
  \"size\": 1000,
  \"minScore\": 0.0
}"

