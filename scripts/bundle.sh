#!/bin/sh

dir="$(dirname "$0")"

"$dir/bowser.sh"

go() {
   
   yes | rm node_modules/purescript/purs.bin
   cp $(which purs) node_modules/purescript 
   mv node_modules/purescript/purs node_modules/purescript/purs.bin
   spago build
   cp -r src/lib/TTHouse/Web/Bowser ./output/TTHouse.Web.Platform
   node esbuild.mjs
}

go
