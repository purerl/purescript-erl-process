{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erl-process"
, dependencies =
  [ "datetime"
  , "effect"
  , "either"
  , "foreign"
  , "integers"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
