#!/usr/bin/env bash

INDEX_NAME=${1:-index_getjenny_english_0}
FILENAME=${2:-"../../doc/decision_table_starchat_doc.csv"}
HOST=${3:-http://localhost}
PORT=${4:-8888}
FORMAT=${5:-csv}

curl -k -v -H "Authorization: Basic $(echo -n 'test_user:p4ssw0rd' | base64)" \
 -X POST --form "${FORMAT}=@${FILENAME}" "${HOST}:${PORT}/${INDEX_NAME}/decisiontable/upload/${FORMAT}"

