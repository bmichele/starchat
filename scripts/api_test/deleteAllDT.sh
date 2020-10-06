#!/usr/bin/env bash

INDEX_NAME=${1:-index_getjenny_english_0}
PERM=${2:-false}
PORT=${3:-8888}
curl -v -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
 -H "Content-Type: application/json" -X DELETE "http://localhost:${PORT}/${INDEX_NAME}/decisiontable/all?permanent=${PERM}"

