#!/usr/bin/env bash

QUERY="${1}"
TSGTE=${2:-1603379517000}
TSLTE=${3:-1603379617000}
PORT=${4:-8443}
INDEX_NAME=${5:-index_getjenny_english_0}
ROUTE=${6:-conversation_logs}

curl -k -H "Authorization: Basic $(echo -n 'test_user:p4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "https://localhost:${PORT}/${INDEX_NAME}/${ROUTE}/update/question" -d "{
  \"coreData\": {
    \"question\": \"\"
  },
  \"timestampGte\": ${TSGTE},
  \"timestampLte\": ${TSLTE}
}"


