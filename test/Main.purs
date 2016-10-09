module Test.Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Raw as Raw
import Test.Process as Process

import Test.Counter as Counter

main = do
  log "Raw tests:"
  Raw.test
  log "Wrapped tests:"
  Process.test
  log "Counter:"
  Counter.test
