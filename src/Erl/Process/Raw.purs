module Erl.Process.Raw
  ( Pid
  , spawn
  , spawnLink
  , send
  , receive
  , receiveWithTimeout
  , receiveWithTrap
  , receiveWithTrapAndTimeout
  , setProcessFlagTrapExit
  , ExitReason(..)
  , ExitMsg(..)
  , self
  , class HasPid
  , getPid
  , exit
  ) where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Foreign (Foreign)

foreign import data Pid :: Type

instance eqPid :: Eq Pid where
  eq = eqNative

foreign import eqNative :: forall a. a -> a -> Boolean

foreign import spawn :: (Effect Unit) -> Effect Pid
foreign import spawnLink :: (Effect Unit) -> Effect Pid

foreign import send :: forall a. Pid -> a -> Effect Unit

foreign import receive :: forall a. Effect a

foreign import receiveWithTimeout :: forall a. Int -> a -> Effect a

foreign import self :: Effect Pid

class HasPid a where
  getPid :: a -> Pid

instance pidHasPid :: HasPid Pid where
  getPid = identity

data ExitReason = ExitReason Pid ExitMsg
data ExitMsg
  = Normal
  | Killed
  | Other Foreign

foreign import receiveWithTrap :: forall a. Effect (Either ExitReason a)

foreign import receiveWithTrapAndTimeout :: forall a. Int -> a -> Effect (Either ExitReason a)

foreign import setProcessFlagTrapExit :: Boolean -> Effect Boolean

foreign import exit :: Foreign -> Effect Unit