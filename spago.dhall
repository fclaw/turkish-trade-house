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
  -- , "datetime"
  , "affjax-web"
  , "affjax"
  , "foreign-object"
  , "form-urlencoded"
  , "functions"
  , "now" 
  , "tuples"
  , "formatters"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
