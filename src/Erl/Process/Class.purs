module Erl.Process.Class where

class HasSelf (x :: Type -> Type) a | x -> a where
  self :: x a