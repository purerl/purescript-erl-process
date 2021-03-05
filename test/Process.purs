module Test.Process where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Erl.Process (ExitMsg(..), ExitReason(..), ProcessM, receive, receiveWithTrap, spawn, trapExit, (!))

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

trapTest :: forall a. Show a => ProcessM a Unit
trapTest = do
  a <- receive
  trapExit do
    -- spawn thing that exits here
    receiveWithTrap >>= case _ of
      Left (ExitReason _ Killed) -> liftEffect $ log "killed"
      _ -> pure unit


test :: Effect Unit
test = do
  p <- spawn logger
  p ! 123
  p2 <- spawn logger
  p2 ! "one two three"
  p0 <- spawn noReceive
  _ <- spawnIgnore
  pure unit
