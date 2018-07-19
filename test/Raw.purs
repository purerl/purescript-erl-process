module Test.Raw where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Erl.Process.Raw (receive, send, spawn)

proc :: Effect Unit
proc = do
  n :: Int <- receive
  log $ "RAW Received: " <> show n
  proc

test :: Effect Unit
test = do
  log "RAW Spawning proc"
  pid <- spawn proc
  send pid 42
  send pid 12
  log "RAW Spawned proc"
