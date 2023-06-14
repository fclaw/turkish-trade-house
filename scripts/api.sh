#!/bin/sh

url=$1
file=$2
api=$3

generate() { 
  node api-downloader.mjs $url $file
  openapi-generator-cli \
  generate -i $file -g javascript -o src/core/TTHouse/Api/Foreign/$api \
  --additional-properties=usePromises=true,emitModelMethods=true
   find src/ -type f -name "*.js" -exec js-beautify -r {} \;
}

generate