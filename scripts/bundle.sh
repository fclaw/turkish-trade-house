#!/bin/sh

dir="$(dirname "$0")"

"$dir/bowser.sh"

go() {
   
   yes | rm node_modules/purescript/purs.bin
   cp $(which purs) node_modules/purescript 
   mv node_modules/purescript/purs node_modules/purescript/purs.bin
   cp -r src/lib/Web/Bowser output/Web
   spago build
   node esbuild.mjs
}

go
