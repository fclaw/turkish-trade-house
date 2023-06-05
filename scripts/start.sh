#!/bin/sh

rm .env

sha=$(git rev-parse HEAD@{$1})
branch=$(git rev-parse --abbrev-ref HEAD)

tag="${branch}_${sha}"

cat <<EOT >> .env
  TAG=$tag
EOT

if [ $# -ge 1 ] && [ -n "$2" ]
then 
  composer=$2

  if [ $composer == "yes" ]
  then
     exec docker-compose up
  else
     echo "arg must be yes" 
  fi
fi