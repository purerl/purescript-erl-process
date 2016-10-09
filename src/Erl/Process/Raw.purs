module Erl.Process.Raw (Pid, spawn, PROCESS, send, receive) where

import Prelude
import Control.Monad.Eff (Eff)

foreign import data Pid :: *

foreign import data PROCESS :: !

instance eqPid :: Eq Pid where
  eq = eqNative

foreign import eqNative :: forall a. a -> a -> Boolean

foreign import spawn :: forall eff. (Eff (process :: PROCESS | eff) Unit) -> Eff (process :: PROCESS | eff) Pid

foreign import send :: forall eff a. Pid -> a -> Eff (process :: PROCESS | eff) Unit

foreign import receive :: forall eff a. Eff (process :: PROCESS | eff) a
