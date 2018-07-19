module Test.Process where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Erl.Process (spawn, (!))

noReceive :: forall a. a -> Effect Unit
noReceive _ = pure unit

spawnNoReceive :: Effect Unit
spawnNoReceive = void $ spawn noReceive

ignore2 :: forall a. Effect a -> Effect Unit
ignore2 receive = do
  a <- receive
  log $ "PHANTOM IGNORE received: " <> show "asfd"
  pure unit

ignore :: Effect String -> Effect Unit
ignore receive = do
  a <- receive
  p <- spawn ignore2
  p ! 42
  log $ "PHANTOM IGNORE received: " <> a
  pure unit

spawnIgnore  :: Effect Unit
spawnIgnore = do
  _ <-  spawn ignore
  pure unit

logger :: Effect Int -> Effect Unit
logger receive = do
  a :: Int <- receive
  p <- spawn ignore
  log $ "PHANTOM Received: " <> show a
  p ! "hello world"
  p ! "foo"
  pure unit

test :: Effect Unit
test = do
  log "PHANTOM Spawning proc"
  p <- spawn logger
  p ! 123
  p0 <- spawn noReceive
  _ <- spawnIgnore
  pure unit
