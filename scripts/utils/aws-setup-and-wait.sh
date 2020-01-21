#!/usr/bin/env bash
#   Use this script to setup the build environment with Elasticsearch's certificates.
#   This scripts expects:
#      * ES_CERTIFICATE env variable: it contains a base64-encoded PKCS12 certificate
#      * the /starchat folder to be present and writable

if [ -n "$ES_CERTIFICATE" ]; then
  echo -n "$ES_CERTIFICATE" | base64 -d > /starchat/es.p12
fi

"$(dirname $0)"/wait-for-it.sh "$@"
