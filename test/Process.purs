module Test.Process where

import Prelude
import Control.Monad.Free (Free)
import Data.Either (Either(..), fromLeft', fromRight', isLeft)
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Erl.Process (ExitReason, ProcessM, receive, receiveWithTimeout, self, spawn, spawnLink, start, startLink, trapExit, (!))
import Erl.Process.Raw as Raw
import Erl.Test.EUnit (TestF, suite, test)
import Foreign as Foreign
import Partial.Unsafe (unsafeCrashWith)
import Test.Assert (assertEqual, assertTrue)
import Unsafe.Coerce (unsafeCoerce)

data Foo
  = Foo Int
  | Blah String
  | Whatever Boolean Number
derive instance eqFoo :: Eq Foo

tests :: Free TestF Unit
tests = do
  spawnTests
  startTests

startTests :: Free TestF Unit
startTests =
  suite "process start tests" do
    test "Get initial response and messages from start" $ mkSimpleStartTest start
    test "Get initial response and messages from startLink" $ mkSimpleStartTest startLink
    test "Crashes in the starter are returned as Left (start)" $ crashStartTest
    test "Crashes in the starter are returned as Left (startLink)" $ crashStartLinkTest
  where

  mkSimpleStartTest startMethod = do
    parent <- Raw.self
    let
      starter = do
        -- Confirm we can receive messages in our starter
        me <- self
        liftEffect $ me ! 1
        x <- receive
        pure
          { result: "initialResponse" <> show x
          , cont
          }
      cont = do
        -- Confirm we can receive messages in our runner
        a <- receive
        b <- receive
        liftEffect $ parent `Raw.send` (a == 1 && b == 2)
    { result: initialResult, pid: p } <- unsafeFromRight "we should get an initial result" <$> startMethod starter
    assertEqual
      { actual: initialResult
      , expected: "initialResponse1"
      }
    p ! 1
    p ! 2
    Raw.receive >>= assertTrue

  crashStartTest = do
    let starter = unsafeCoerce 1
    _foreignError <- unsafeFromLeft "we should get an error from init" <$> start starter
    pure unit

  crashStartLinkTest = do
    -- Run the process inside a linked process that itself catches Exit
    testPid <- Raw.self
    void
      $ spawnLink do
          trapExit do
            resp <- liftEffect do startLink $ (unsafeCoerce 1 :: ProcessM Int _)
            let _foreignError = unsafeFromLeft "we should get an error from init" resp
            liftEffect $ testPid `Raw.send` true
            pure unit
    Raw.receive >>= assertTrue
    --void Raw.receive
    pure unit

spawnTests :: Free TestF Unit
spawnTests =
  suite "process spawn tests" do
    -- Use raw process communication to talk to the test process as it is not a typed Process
    test "send stuff to spawned process" do
      parent <- Raw.self
      -- We can also do this inline or infer the types
      let
        proc :: ProcessM Int Unit
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
      p <-
        spawn do
          a <- receive
          b <- receive
          liftEffect $ parent `Raw.send` (a == Foo 42 && b == Whatever true 1.0)
      p ! Foo 42
      p ! Whatever true 1.0
      Raw.receive >>= assertTrue
    test "sending tospawnLinked" do
      parent <- Raw.self
      p <-
        spawnLink do
          a <- receive
          b <- receive
          liftEffect $ parent `Raw.send` (a == Foo 42 && b == Whatever true 1.0)
      p ! Foo 42
      p ! Whatever true 1.0
      Raw.receive >>= assertTrue
    test "receive timeout" do
      parent <- Raw.self
      _p <-
        spawnLink do
          a <- receiveWithTimeout (Milliseconds 10.0) "default"
          liftEffect $ parent `Raw.send` (a == "default")
      Raw.receive >>= assertTrue
    test "self eq" do
      parent <- Raw.self
      p <-
        spawnLink do
          child <- self
          liftEffect $ parent `Raw.send` child
      p' <- Raw.receive
      assertTrue $ p == p'
    test "trapExit" do
      testPid <- Raw.self
      void
        $ spawnLink do
            parent <- self
            trapExit do
              _ <-
                liftEffect
                  $ spawnLink do
                      liftEffect $ parent ! 1
                      liftEffect $ Raw.exit (Foreign.unsafeToForeign true)
                      pure unit
              first <- receive
              liftEffect
                $ case (unsafeCoerce first) :: Either ExitReason Int of
                    Right 1 -> pure unit
                    _other -> do
                      throw "failed recv"
              second <- receive
              liftEffect $ testPid `Raw.send` (isLeft second)
      Raw.receive >>= assertTrue
    test "receive with trap timeout" do
      testPid <- Raw.self
      void
        $ spawnLink do
            trapExit do
              _ <-
                liftEffect
                  $ spawnLink do
                      _ <- receive
                      pure unit
              a <- receiveWithTimeout (Milliseconds 100.0) "default"
              liftEffect
                $ case a of
                    Right "default" -> testPid `Raw.send` true
                    _ -> testPid `Raw.send` false
      Raw.receive >>= assertTrue

unsafeFromRight :: forall a b. String -> Either a b -> b
unsafeFromRight s = fromRight' (\_ -> unsafeCrashWith s)
unsafeFromLeft :: forall a b. String -> Either a b -> a
unsafeFromLeft s = fromLeft' (\_ -> unsafeCrashWith s)
