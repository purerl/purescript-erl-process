module Erl.Process.Raw (Pid, spawn, spawnLink, send, receive, receiveWithTimeout) where

import Prelude

import Effect (Effect)

foreign import data Pid :: Type

instance eqPid :: Eq Pid where
  eq = eqNative

foreign import eqNative :: forall a. a -> a -> Boolean

foreign import spawn :: (Effect Unit) -> Effect Pid
foreign import spawnLink :: (Effect Unit) -> Effect Pid

foreign import send :: forall a. Pid -> a -> Effect Unit

foreign import receive :: forall a. Effect a

foreign import receiveWithTimeout :: forall a. Int -> a -> Effect a
