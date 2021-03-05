module Erl.Process
  ( Process
  , ProcessM
  , toPid
  , send
  , (!)
  , spawn
  , spawnLink
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Erl.Process.Class (class HasSelf)
import Erl.Process.Raw as Raw

newtype Process (a :: Type) = Process Raw.Pid

toPid :: forall a. Process a -> Raw.Pid
toPid (Process pid) = pid

instance eqProcess :: Eq (Process a) where
  eq a b = eq (toPid a) (toPid b)


newtype ProcessM (a :: Type) b = ProcessM (Effect b)
derive newtype instance functorProcessM :: Functor (ProcessM a)
derive newtype instance applyProcessM :: Apply (ProcessM a)
derive newtype instance applictativeProcessM :: Applicative (ProcessM a)
derive newtype instance bindProcessM :: Bind (ProcessM a)
derive newtype instance monadProcessM :: Monad (ProcessM a)

instance monadEffectProcessM :: MonadEffect (ProcessM a) where
  liftEffect = ProcessM

receive :: forall a. ProcessM a a
receive = ProcessM Raw.receive

receiveWithTimeout :: forall a. Int -> a -> ProcessM a a
receiveWithTimeout n a = ProcessM $ Raw.receiveWithTimeout n a

instance selfProcessM :: HasSelf (ProcessM a) (Process a) where
  self :: forall a. ProcessM a (Process a)
  self = ProcessM $ Process <$> Raw.self

send :: forall a. Process a -> a -> Effect Unit
send p x = Raw.send (toPid p) x

infixr 6 send as !

spawn :: forall a. ProcessM a Unit -> Effect (Process a)
spawn (ProcessM e) = Process <$> Raw.spawn e

spawnLink :: forall a. ProcessM a Unit -> Effect (Process a)
spawnLink (ProcessM e) = Process <$> Raw.spawnLink e
