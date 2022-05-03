module Erl.Process
  ( (!)
  , Process
  , ProcessM
  , ProcessTrapM
  , StartResponse
  , StartResult
  , class HasProcess
  , class HasReceive
  , class HasSelf
  , getProcess
  , module RawExport
  , receive
  , receiveWithTimeout
  , receiveWithTrap
  , receiveWithTrapAndTimeout
  , self
  , send
  , sendExitSignal
  , spawn
  , spawnLink
  , start
  , startLink
  , toPid
  , trapExit
  , unsafeRunProcessM
  ) where

import Prelude
import Data.Either (Either)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Process.Raw (ExitReason(..), ExitMsg(..)) as RawExport
import Erl.Process.Raw (ExitReason)
import Erl.Process.Raw as Raw
import Foreign (Foreign)

newtype Process (a :: Type)
  = Process Raw.Pid

toPid :: forall a. Process a -> Raw.Pid
toPid (Process pid) = pid

instance eqProcess :: Eq (Process a) where
  eq a b = eq (toPid a) (toPid b)

instance ordProcess :: Ord (Process a) where
  compare a b = compare (toPid a) (toPid b)

instance Show (Process pid) where
  show (Process pid) = "(Process " <> show pid <> ")"

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

receiveWithTrapAndTimeout :: forall a. Milliseconds -> a -> ProcessTrapM a (Either ExitReason a)
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

type StartResult msg r
  = { pid :: Process msg
    , result :: r
    }

type StartResponse msg r
  = { cont :: ProcessM msg Unit
    , result :: r
    }

foreign import start :: forall msg r. ProcessM msg (StartResponse msg r) -> Effect (Either Foreign (StartResult msg r))
foreign import startLink :: forall msg r. ProcessM msg (StartResponse msg r) -> Effect (Either Foreign (StartResult msg r))

-- This is just to suppress an unused export warning in the FFI module...
foreign import launcher :: Void -> Void -> Void

sendExitSignal :: forall a. Foreign -> Process a -> Effect Unit
sendExitSignal reason (Process pid) = do
  Raw.sendExitSignal reason pid

class HasProcess b a where
  getProcess :: a -> Process b

class HasSelf (x :: Type -> Type) a | x -> a where
  self :: x (Process a)

instance processHasProcess :: HasProcess b (Process b) where
  getProcess = identity

instance processHasPid :: Raw.HasPid (Process b) where
  getPid (Process pid) = pid

instance selfProcessM :: HasSelf (ProcessM a) a where
  self :: forall a. ProcessM a (Process a)
  self = ProcessM $ Process <$> Raw.self

class HasReceive :: (Type -> Type) -> Type -> Type -> Constraint
class HasReceive a msg r | a -> msg r where
  receive :: a r

  receiveWithTimeout :: Milliseconds -> msg -> a r

instance HasReceive (ProcessM msg) msg msg where
  receive = ProcessM Raw.receive
  receiveWithTimeout t d = ProcessM $ Raw.receiveWithTimeout t d

instance HasReceive (ProcessTrapM msg) msg (Either ExitReason msg) where
  receive = ProcessTrapM Raw.receiveWithTrap
  receiveWithTimeout t d = ProcessTrapM $ Raw.receiveWithTrapAndTimeout t d
