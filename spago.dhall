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
  , "newtype"
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
  , "foreign-object"
  , "form-urlencoded"
  , "functions"
  , "now" 
  , "tuples"
  , "formatters"
  , "datetime"
  , "exceptions"
  , "web-html"
  , "fork"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
