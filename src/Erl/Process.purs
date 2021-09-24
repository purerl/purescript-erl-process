module Erl.Process
  ( Process
  , ProcessM
  , ProcessTrapM
  , toPid
  , send
  , self
  , getProcess
  , (!)
  , receive
  , receiveWithTimeout
  , spawn
  , spawnLink
  , class HasProcess
  , class ReceivesMessage
  , class HasSelf
  , trapExit
  , receiveWithTrap
  , receiveWithTrapAndTimeout
  , unsafeRunProcessM
  , module RawExport
  ) where

import Prelude
import Data.Either (Either)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Process.Raw (ExitReason(..), ExitMsg(..)) as RawExport
import Erl.Process.Raw (ExitReason)
import Erl.Process.Raw as Raw

newtype Process (a :: Type)
  = Process Raw.Pid

toPid :: forall a. Process a -> Raw.Pid
toPid (Process pid) = pid

instance eqProcess :: Eq (Process a) where
  eq a b = eq (toPid a) (toPid b)

newtype ProcessM (a :: Type) b
  = ProcessM (Effect b)
derive newtype instance functorProcessM :: Functor (ProcessM a)
derive newtype instance applyProcessM :: Apply (ProcessM a)
derive newtype instance applicativeProcessM :: Applicative (ProcessM a)
derive newtype instance bindProcessM :: Bind (ProcessM a)
derive newtype instance monadProcessM :: Monad (ProcessM a)

unsafeRunProcessM :: forall a b. ProcessM a b -> Effect b
unsafeRunProcessM (ProcessM b) = b

instance monadEffectProcessM :: MonadEffect (ProcessM a) where
  liftEffect = ProcessM

receive :: forall a. ProcessM a a
receive = ProcessM Raw.receive

receiveWithTimeout :: forall a. Int -> a -> ProcessM a a
receiveWithTimeout n a = ProcessM $ Raw.receiveWithTimeout n a

newtype ProcessTrapM (a :: Type) b
  = ProcessTrapM (Effect b)
derive newtype instance functorProcessTrapM :: Functor (ProcessTrapM a)
derive newtype instance applyProcessTrapM :: Apply (ProcessTrapM a)
derive newtype instance applicativeProcessTrapM :: Applicative (ProcessTrapM a)
derive newtype instance bindProcessTrapM :: Bind (ProcessTrapM a)
derive newtype instance monadProcessTrapM :: Monad (ProcessTrapM a)

instance monadEffectProcessTrapM :: MonadEffect (ProcessTrapM a) where
  liftEffect = ProcessTrapM

receiveWithTrap :: forall a. ProcessTrapM a (Either ExitReason a)
receiveWithTrap = ProcessTrapM Raw.receiveWithTrap

receiveWithTrapAndTimeout :: forall a. Int -> a -> ProcessTrapM a (Either ExitReason a)
receiveWithTrapAndTimeout timeout default = ProcessTrapM $ Raw.receiveWithTrapAndTimeout timeout default

trapExit :: forall a b. ProcessTrapM a b -> ProcessM a b
trapExit (ProcessTrapM e) =
  ProcessM
    $ liftEffect do
        void $ Raw.setProcessFlagTrapExit true
        res <- e
        void $ Raw.setProcessFlagTrapExit false
        pure res

send :: forall a. Process a -> a -> Effect Unit
send p x = Raw.send (toPid p) x

infixr 6 send as !

spawn :: forall a. ProcessM a Unit -> Effect (Process a)
spawn (ProcessM e) = Process <$> Raw.spawn e

spawnLink :: forall a. ProcessM a Unit -> Effect (Process a)
spawnLink (ProcessM e) = Process <$> Raw.spawnLink e

class HasProcess b a where
  getProcess :: a -> Process b

class HasSelf (x :: Type -> Type) a | x -> a where
  self :: x (Process a)

instance processHasProcess :: HasProcess b (Process b) where
  getProcess = identity

instance processHasPid :: Raw.HasPid (Process b) where
  getPid (Process pid) = pid

class ReceivesMessage :: forall k. k -> Type -> Constraint
class ReceivesMessage a msg | a -> msg

instance messageTypeProcessM :: ReceivesMessage (ProcessM msg) msg

instance messageTypeProcessTrapM :: ReceivesMessage (ProcessTrapM msg) msg
