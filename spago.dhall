{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erl-process"
, dependencies = [ "effect", "either", "foreign", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
