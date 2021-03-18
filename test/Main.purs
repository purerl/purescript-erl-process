module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Effect (Effect)
import Erl.Test.EUnit (TestF, runTests)
import Test.Counter as Counter
import Test.Process as Process
import Test.Raw as Raw

main :: Effect Unit
main = do
  Counter.test
  void $ runTests tests

tests :: Free TestF Unit
tests = do
  Raw.tests
  Process.tests