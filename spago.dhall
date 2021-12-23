{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "beacon"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "node-streams"
  , "optparse"
  , "prelude"
  , "psci-support"
  , "refs"
  , "spec"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
