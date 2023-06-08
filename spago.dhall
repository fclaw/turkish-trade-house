{ name = "halogen-template"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  -- , "arrays"
  -- , "bifunctors"
  -- , "codec-argonaut"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  -- , "formatters"
  , "halogen"
  , "prelude"
  -- , "profunctor"
  -- , "strings"
  , "undefined"
  -- , "validation"
  , "transformers"
  , "halogen-store"
  , "routing-duplex"
  , "routing"
  , "aff"
  , "maybe"
  , "safe-coerce"
  , "affjax-web"
  , "affjax"
  , "form-urlencoded"
  , "functions"
  , "now" 
  , "tuples"
  , "formatters"
  , "datetime"
  , "exceptions"
  , "web-html"
  , "fork"
  , "dom-indexed"
  , "arrays"
  , "enums"
  , "halogen-subscriptions"
  , "tailrec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
