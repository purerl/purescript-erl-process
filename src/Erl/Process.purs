module Erl.Process
  ( Process(..)
  , runProcess
  , send
  , (!)
  , spawn
  ) where

import Prelude

import Effect (Effect)
import Erl.Process.Raw as Raw

newtype Process a = Process Raw.Pid

runProcess :: forall a. Process a -> Raw.Pid
runProcess (Process pid) = pid

instance eqProcess :: Eq (Process a) where
  eq a b = eq (runProcess a) (runProcess b)

send :: forall a. Process a -> a -> Effect Unit
send p x = Raw.send (runProcess p) x

infixr 6 send as !

spawn :: forall a. (Effect a -> Effect Unit) -> Effect (Process a)
spawn e = Process <$> Raw.spawn (e Raw.receive)

