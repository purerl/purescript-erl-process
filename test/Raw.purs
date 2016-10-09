module Test.Raw where

import Prelude
import Control.Monad.Eff
import Erl.Process.Raw
import Control.Monad.Eff.Console (CONSOLE, log)

proc :: forall eff. Eff (console :: CONSOLE, process :: PROCESS | eff)   Unit
proc = do
  log "RAW Started proc"
  rec unit
  where
  rec :: forall eff'. Unit -> Eff (console :: CONSOLE,  process :: PROCESS | eff') Unit
  rec _ = do
    n :: Int <- receive
    log $ "RAW Received: " <> show n
    rec unit

test = do
  log "RAW Spawning proc"
  pid <- spawn proc
  send pid 42


  send pid 12
  log "RAW Spawned proc"
