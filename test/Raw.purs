module Test.Raw where

import Prelude

import Control.Monad.Free (Free)
import Erl.Process.Raw (receive, self, send, spawn, spawnLink)
import Erl.Test.EUnit (TestF, suite, test)
import Test.Assert (assert, assertFalse, assertTrue, assertTrue')

tests :: Free TestF Unit
tests = 
  suite "raw tests" do
    test "send stuff to spawned process" do
      parent <- self
      pid <- spawn do
        anInt :: Int <- receive
        aString :: String <- receive
        parent `send` (anInt == 42 && aString == "foo")
      pid `send` 42
      pid `send` "foo"
      receive >>= assertTrue

    test "send stuff to spawnLinked process" do
      parent <- self
      pid <- spawnLink do
        anInt :: Int <- receive
        aString :: String <- receive
        parent `send` (anInt == 42 && aString == "foo")
      pid `send` 42
      pid `send` "foo"
      receive >>= assertTrue

    test "spawned process just sends" do
      parent <- self
      _pid <- spawn do
        parent `send` 111
        parent `send` true
      receive >>= \n -> assert $ n == 111
      receive >>= assertTrue

    test "spawnLinked process just sends" do
      parent <- self
      _pid <- spawnLink do
        parent `send` 111
        parent `send` true
      receive >>= \n -> assert $ n == 111
      receive >>= assertTrue
    
    test "eq" do
      parent <- self
      pid <- spawnLink do
        pid' <- self
        parent `send` pid'
      pid' <- receive
      assertTrue' "pids are equal" $ pid == pid'
      assertFalse $ parent == pid
