module Test.Raw where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Erl.Process.Raw (PROCESS, receive, send, spawn)

proc :: forall eff. Eff (console :: CONSOLE, process :: PROCESS | eff) Unit
proc = do
  n :: Int <- receive
  log $ "RAW Received: " <> show n
  proc

test :: forall eff. Eff (console :: CONSOLE, process :: PROCESS | eff) Unit
test = do
  log "RAW Spawning proc"
  pid <- spawn proc
  send pid 42


  send pid 12
  log "RAW Spawned proc"
