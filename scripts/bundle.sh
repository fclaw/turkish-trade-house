#!/bin/sh

dir="$(dirname "$0")"

abort()
{
    echo >&2 '
***************
*** ABORTED ***
***************
'
    echo "An error occurred. Exiting..." >&2
    exit 1
}

trap 'abort' 0

set -e

echo >&2 '
************
*** START *** 
************
'

"$dir/bowser.sh"

go() {
   
   yes | rm node_modules/purescript/purs.bin
   cp $(which purs) node_modules/purescript 
   mv node_modules/purescript/purs node_modules/purescript/purs.bin
   spago build
   cp -r src/lib/TTHouse/Web/Bowser ./output/TTHouse.Web.Platform
   cp -r src/core/TTHouse/Api/Foreign/Scaffold ./output/TTHouse.Api.Foreign.Scaffold
   node esbuild.mjs
}

go

trap : 0

echo >&2 '
************
*** DONE *** 
************
'