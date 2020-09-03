#!/usr/bin/env bash

HOST=${1:-http://localhost}
PORT=${2:-8888}
INDEX_NAME=${3:-index_getjenny_english_0}
FILENAME=${4:-"../../doc/decision_table_starchat_doc.csv"}
FORMAT=${5:-csv}

curl -k -v -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
 -X POST --form "${FORMAT}=@${FILENAME}" "${HOST}:${PORT}/${INDEX_NAME}/decisiontable/upload/${FORMAT}"

