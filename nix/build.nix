let
   pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-23.05.tar.gz") {};
   nodejs = pkgs.nodejs-18_x;
   python3 = pkgs.python3;
   python = pkgs.python;
   git = pkgs.git;
   ps = pkgs.purescript;
in
pkgs.mkShell { 
  buildInputs = [ git nodejs ps ];
  shellHook = ''
    npm install spago
    npm install purs-tidy
    npm install --save-exact esbuild
    npm install @openapitools/openapi-generator-cli -D
    npm install querystring
    npm install superagent
    export PATH="./node_modules/.bin:$PATH"
   '';
  }