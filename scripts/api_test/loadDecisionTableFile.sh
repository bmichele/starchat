#!/usr/bin/env bash

INDEX_NAME=${1:-index_getjenny_english_0}
FILENAME=${2:-"../../doc/decision_table_starchat_doc.csv"}
FORMAT=${3:-csv}
HOST=${4:-http://localhost}
PORT=${5:-8888}

curl -k -v -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
 -X POST --form "${FORMAT}=@${FILENAME}" "${HOST}:${PORT}/${INDEX_NAME}/decisiontable/upload/${FORMAT}"

