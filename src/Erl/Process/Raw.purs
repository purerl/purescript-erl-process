module Erl.Process.Raw (Pid, spawn, send, receive, setFlag) where

import Prelude

import Effect (Effect)
import Erl.Process.Flags (Flag)

foreign import data Pid :: Type

instance eqPid :: Eq Pid where
  eq = eqNative

foreign import eqNative :: forall a. a -> a -> Boolean

foreign import spawn :: (Effect Unit) -> Effect Pid

foreign import send :: forall a. Pid -> a -> Effect Unit

foreign import receive :: forall a. Effect a

foreign import setFlag forall a. Flag a -> a -> Effect a