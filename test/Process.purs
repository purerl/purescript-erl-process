module Test.Process where

import Prelude

import Effect (Effect)
import Erl.Process (SpawnedProcessState, spawn, (!))


noReceive :: forall a. a -> Effect Unit
noReceive _ = pure unit

spawnNoReceive :: Effect Unit
spawnNoReceive = void $ spawn noReceive

ignore2 :: forall a. SpawnedProcessState a -> Effect Unit
ignore2 {receive} = do
  a <- receive
  pure unit

ignore :: SpawnedProcessState String -> Effect Unit
ignore {receive} = do
  a <- receive
  p <- spawn ignore2
  p ! 42
  pure unit

spawnIgnore  :: Effect Unit
spawnIgnore = do
  _ <-  spawn ignore
  pure unit

logger :: forall a. Show a => SpawnedProcessState a -> Effect Unit
logger {receive} = do
  a <- receive
  p <- spawn ignore
  p ! "hello world"
  p ! "foo"
  pure unit

test :: Effect Unit
test = do
  p <- spawn logger
  p ! 123
  p2 <- spawn logger
  p2 ! "one two three"
  p0 <- spawn noReceive
  _ <- spawnIgnore
  pure unit
