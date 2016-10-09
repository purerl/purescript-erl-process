module Test.Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Raw as Raw
import Test.Process as Process

main = do
  log "Raw tests:"
  Raw.test
  log "Wrapped tests:"
  Process.test
