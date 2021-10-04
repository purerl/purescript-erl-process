module Test.Process where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..), isLeft)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Erl.Process (ExitReason, ProcessM, receive, receiveWithTrap, self, spawn, spawnLink, trapExit, (!))
import Erl.Process.Raw as Raw
import Erl.Test.EUnit (TestF, suite, test)
import Foreign as Foreign
import Test.Assert (assertTrue)
import Unsafe.Coerce (unsafeCoerce)

data Foo = Foo Int | Blah String | Whatever Boolean Number
derive instance eqFoo :: Eq Foo

tests :: Free TestF Unit
tests = 
  suite "process tests" do
    -- Use raw process communication to talk to the test process as it is not a typed Process
    test "send stuff to spawned process" do
      parent <- Raw.self
      -- We can also do this inline or infer the types
      let proc :: ProcessM Int Unit
          proc = do
            a :: Int <- receive
            b :: Int <- receive
            liftEffect $ parent `Raw.send` (a == 1 && b == 2)
      p <- spawn proc
      p ! 1
      p ! 2
      Raw.receive >>= assertTrue

    test "send stuff to spawned process, another type" do
      parent <- Raw.self
      p <- spawn do
        a <- receive
        b <- receive
        liftEffect $ parent `Raw.send` (a == Foo 42 && b == Whatever true 1.0)
      p ! Foo 42
      p ! Whatever true 1.0
      Raw.receive >>= assertTrue

    test "sending tospawnLinked" do
      parent <- Raw.self
      p <- spawnLink do
        a <- receive
        b <- receive
        liftEffect $ parent `Raw.send` (a == Foo 42 && b == Whatever true 1.0)
      p ! Foo 42
      p ! Whatever true 1.0
      Raw.receive >>= assertTrue

    test "self eq" do
      parent <- Raw.self
      p <- spawnLink do
        child <- self
        liftEffect $ parent `Raw.send` child
      p' <- Raw.receive
      assertTrue $ p == p'

    test "trapExit" do
      testPid <- Raw.self
      void $ spawnLink do
        parent <- self
        
        trapExit do
          _ <- liftEffect $ spawnLink do
            liftEffect $ parent ! 1
            liftEffect $ Raw.exit (Foreign.unsafeToForeign true)
            pure unit

          first <- receiveWithTrap
          liftEffect $ case (unsafeCoerce first) :: Either ExitReason Int of 
            Right 1 -> pure unit
            _other -> do
              throw "failed recv"
          second <- receiveWithTrap
          liftEffect $ testPid `Raw.send` (isLeft second)
      Raw.receive >>= assertTrue
