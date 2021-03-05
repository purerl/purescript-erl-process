module Test.Counter where

import Prelude

import Effect (Effect, forE)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Erl.Process (Process, ProcessM, receive, spawn, (!))

data Message = Add Int | Subtract Int | GetTotal (Process Int)

counter :: ProcessM Message Unit
counter = counter' 0
  where
    counter' :: Int -> ProcessM Message Unit
    counter' n =
      receive >>= case _ of
        Add m -> counter' (n+m)
        Subtract m -> counter' (n-m)
        GetTotal proc -> liftEffect $ proc ! n

logger :: forall a. Show a => ProcessM a Unit
logger = do
  a <- receive
  liftEffect $ log $ show a

test :: Effect Unit
test = do
  pLog <- spawn logger
  pCounter <- spawn counter
  forE 0 100000 \_ -> pCounter ! Add 1
  pCounter ! Subtract 5
  pCounter ! GetTotal pLog
