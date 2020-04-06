#!/usr/bin/env bash

PORT=${1:-8888}
SUFFIX=${2:-""}
curl -v -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/system_index_management/create${SUFFIX}"

