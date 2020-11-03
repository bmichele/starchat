#!/usr/bin/env bash

VALUE=${1:-0}
PORT=${2:-8443}
INDEX_NAME=${3:-index_getjenny_english_0}
ROUTE=${4:-conversation_logs}

curl -k -H "Authorization: Basic $(echo -n 'test_user:p4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "https://localhost:${PORT}/${INDEX_NAME}/${ROUTE}/anonymization/olderthan?value=${VALUE}" 


