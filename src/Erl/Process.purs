module Erl.Process
  ( Process(..)
  , SpawnedProcessState(..)
  , runProcess
  , send
  , (!)
  , spawn
  , spawnLink
  ) where

import Prelude

import Effect (Effect)
import Erl.Process.Raw as Raw

newtype Process a = Process Raw.Pid

type SpawnedProcessState a = { receive :: Effect a
                             , receiveWithTimeout :: Int -> a -> Effect a
                             }

runProcess :: forall a. Process a -> Raw.Pid
runProcess (Process pid) = pid

instance eqProcess :: Eq (Process a) where
  eq a b = eq (runProcess a) (runProcess b)

send :: forall a. Process a -> a -> Effect Unit
send p x = Raw.send (runProcess p) x

infixr 6 send as !

spawn :: forall a. (SpawnedProcessState a -> Effect Unit) -> Effect (Process a)
spawn e = Process <$> Raw.spawn (e  { receive: Raw.receive
                                    , receiveWithTimeout: Raw.receiveWithTimeout
                                    }
                                    )

spawnLink :: forall a. (SpawnedProcessState a -> Effect Unit) -> Effect (Process a)
spawnLink e = Process <$> Raw.spawnLink (e  { receive: Raw.receive
                                    , receiveWithTimeout: Raw.receiveWithTimeout
                                    }
                                    )
