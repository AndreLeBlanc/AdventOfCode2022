{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "node-process"
  , "numbers"
  , "ordered-collections"
  , "parsing"
  , "partial"
  , "prelude"
  , "profunctor"
  , "strings"
  , "stringutils"
  , "tuples"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
