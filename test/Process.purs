module Test.Process where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Erl.Process (PROCESS, REC, receive, spawn, spawn', (!))

noReceive :: forall eff. Eff eff Unit
noReceive = pure unit

spawnNoReceive :: forall eff. Eff (process :: PROCESS | eff) Unit
spawnNoReceive = void $ spawn noReceive

ignore2 :: forall eff z. Eff (rec :: REC z String, process :: PROCESS, console :: CONSOLE | eff) Unit
ignore2 = do
  a <- receive
  log $ "PHANTOM IGNORE recieved: " <> a
  pure unit

ignore :: forall eff z. Eff (rec :: REC z String, process :: PROCESS, console :: CONSOLE | eff) Unit
ignore = do
  a <- receive
  p <- spawn' ignore2
  p ! "goodbye world"
  log $ "PHANTOM IGNORE recieved: " <> a
  pure unit

spawnIgnore  :: forall eff. Eff (process :: PROCESS, console :: CONSOLE | eff) Unit
spawnIgnore = do
  spawn ignore
  pure unit

logger :: forall eff z. Eff (rec :: REC z Int, process :: PROCESS, console :: CONSOLE | eff) Unit
logger = do
  a <- receive
  p <- spawn' ignore
  log $ "PHANTOM Received: " <> show a
  p ! "hello world"
  p ! "foo"
  pure unit

test :: forall eff. Eff (process :: PROCESS, console :: CONSOLE | eff) Unit
test = do
  log "PHANTOM Spawning proc"
  p <- spawn logger
  p ! 123
  p0 <- spawn noReceive
  _ <- spawn spawnIgnore
  pure unit
