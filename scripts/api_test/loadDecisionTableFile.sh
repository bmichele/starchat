#!/usr/bin/env bash

HOST=${1:-http://localhost}
PORT=${2:-8888}
INDEX_NAME=${3:-index_getjenny_english_0}
STATE_NAME=${4:-further_details_access_question}
FILENAME=${5:-"../../doc/decision_table_starchat_doc.csv"}
FORMAT=${6:-csv}

curl -k -v -H "Authorization: Basic $(echo -n 'test_user:p4ssw0rd' | base64)" \
 -X POST --form "${FORMAT}=@${FILENAME}" "${HOST}:${PORT}/${INDEX_NAME}/decisiontable/upload/${FORMAT}"

