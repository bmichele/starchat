#!/usr/bin/env bash
#   Use this script to setup the build environment with Elasticsearch's certificates.
#   This scripts expects:
#      * ES_CERTIFICATE env variable: it contains a base64-encoded X.509 root certificate
#      * the /starchat folder to be present and writable

if [ -n "$ES_CERTIFICATE" ]; then
  echo -n "$ES_CERTIFICATE" | base64 -D > /starchat/es.cert
  keytool -import -trustcacerts -file /starchat/es.crt -keystore $JAVA_HOME/lib/security/cacerts -storepass changeit -alias "elastic"
fi

"$(dirname $0)"/wait-for-it.sh "$@"