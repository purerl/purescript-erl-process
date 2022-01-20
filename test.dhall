let conf = ./spago.dhall

in    conf
    ⫽ { sources = conf.sources # [ "test/**/*.purs" ]
      , dependencies = conf.dependencies # [ "console", "assert", "erl-test-eunit", "exceptions", "free", "unsafe-coerce"]
      }
