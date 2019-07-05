module Erl.Process.Flags
  ( Flag
  , trapExit
  ) where

import Erl.Atom (atom)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Flag :: Type -> Type

trapExit :: Flag Boolean
trapExit = mkFlag "trap_exit"

mkFlag :: forall a. String -> Flag a
mkFlag = unsafeCoerce <<< atom