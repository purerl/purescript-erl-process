module Test.Process where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Process (ProcessM, receive, spawn, (!))

noReceive :: forall a. ProcessM a Unit
noReceive = pure unit

spawnNoReceive :: Effect Unit
spawnNoReceive = void $ spawn noReceive

ignore2 :: forall a. ProcessM a Unit
ignore2 = do
  a <- receive
  pure unit

ignore :: ProcessM String Unit
ignore = do
  a <- receive
  p <- liftEffect $ spawn ignore2
  liftEffect $ p ! 42
  pure unit

spawnIgnore  :: Effect Unit
spawnIgnore = do
  _ <-  spawn ignore
  pure unit

logger :: forall a. Show a => ProcessM a Unit
logger = do
  a <- receive
  liftEffect do
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
