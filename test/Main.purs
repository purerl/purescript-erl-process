module Test.Main where

import Prelude (Unit, bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Erl.Process.Raw (PROCESS)
import Test.Raw as Raw
import Test.Process as Process

import Test.Counter as Counter

main :: forall eff. Eff (console :: CONSOLE, process :: PROCESS | eff) Unit
main = do
  log "Raw tests:"
  Raw.test
  log "Wrapped tests:"
  Process.test
  log "Counter:"
  Counter.test
