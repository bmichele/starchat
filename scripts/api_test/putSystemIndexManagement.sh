#!/usr/bin/env bash

PORT=${1:-8443}
curl -k -v -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X PUT "https://localhost:${PORT}/system_index_management"

