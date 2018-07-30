#!/usr/bin/env bash

PORT=${1:-8888}
USER_ID=${2:-test_user}
curl -v -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X DELETE http://localhost:${PORT}/user/${USER_ID}
