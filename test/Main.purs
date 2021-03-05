module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Counter as Counter
import Test.Process as Process
import Test.Raw as Raw

main :: Effect Unit
main = do
  log "Raw tests:"
  Raw.test
  log "Wrapped tests:"
  Process.test
  log "Counter:"
  Counter.test
