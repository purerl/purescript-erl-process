module Erl.Process
  ( REC
  , Process(..)
  , runProcess
  , receive
  , send
  , (!)
  , spawn
  , module RawExport
  ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Erl.Process.Raw (PROCESS) as RawExport
import Erl.Process.Raw as Raw

foreign import data REC :: Type -> Type -> Effect

newtype Process a = Process Raw.Pid

runProcess :: forall a. Process a -> Raw.Pid
runProcess (Process pid) = pid

instance eqProcess :: Eq (Process a) where
  eq a b = eq (runProcess a) (runProcess b)

receive :: forall z a eff. Eff (rec :: REC z a, process :: Raw.PROCESS | eff) a
receive = Raw.receive

send :: forall eff a. Process a -> a -> Eff (process :: Raw.PROCESS | eff) Unit
send p x = Raw.send (runProcess p) x

infixr 6 send as !

spawn :: forall eff a. (forall z. Eff (rec :: REC z a, process :: Raw.PROCESS | eff) Unit) -> Eff (process :: Raw.PROCESS | eff) (Process a)
spawn e =  Process <$> Raw.spawn (coerce e)
  where
  coerce :: forall z r. Eff (rec :: REC z a, process :: Raw.PROCESS | eff) r -> Eff (process :: Raw.PROCESS | eff) r
  coerce = unsafeCoerceEff
