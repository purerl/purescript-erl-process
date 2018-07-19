module Test.Counter where

import Prelude

import Effect (Effect, forE)
import Effect.Console (log)
import Erl.Process (Process, spawn, (!))

data Message = Add Int | Subtract Int | GetTotal (Process Int)

counter :: Effect Message -> Effect Unit
counter receive = counter' 0
  where
    counter' :: Int -> Effect Unit
    counter' n =
      receive >>= case _ of
        Add m -> counter' (n+m)
        Subtract m -> counter' (n-m)
        GetTotal proc -> proc ! n

logger :: forall a. (Show a) => Effect a -> Effect Unit
logger receive = do
  a <- receive
  log $ show a

test :: Effect Unit
test = do
  pLog <- spawn logger
  pCounter <- spawn counter
  forE 0 100000 \_ -> pCounter ! Add 1
  pCounter ! Subtract 5
  pCounter ! GetTotal pLog
