#!/usr/bin/env bash
echo "Logging on Docker..."
echo "$DOCKER_PASSWORD" | docker login -u "$DOCKER_USERNAME" --password-stdin
sbt docker:publish
