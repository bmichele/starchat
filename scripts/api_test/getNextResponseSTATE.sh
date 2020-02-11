#!/usr/bin/env bash

HOST=${1:-http://localhost}
PORT=${2:-8888}
INDEX_NAME=${3:-index_getjenny_english_0}
STATE_NAME=${4:-further_details_access_question}

curl -v -H "Authorization: Basic $(echo -n 'test_user:p4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST ${HOST}:${PORT}/${INDEX_NAME}/get_next_response -d "{
	\"conversationId\": \"1234\",
	\"userInput\": { \"text\": \"\" },
	\"state\": [\"${STATE_NAME}\"],
	\"evaluationClass\": \"start\"
}"

