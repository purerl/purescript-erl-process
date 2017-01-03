module Test.Counter where

import Prelude (Unit, bind, show, ($), (+), (-), (>>=))
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Erl.Process (PROCESS, Process, REC, receive, spawn, (!))

data Message = Add Int | Subtract Int | GetTotal (Process Int)

counter :: forall eff z. Eff (rec :: REC z Message, process :: PROCESS, console :: CONSOLE | eff) Unit
counter = counter' 0
  where
    counter' :: Int -> Eff (rec :: REC z Message, process :: PROCESS, console :: CONSOLE | eff) Unit
    counter' n =
      receive >>= case _ of
        Add m -> counter' (n+m)
        Subtract m -> counter' (n-m)
        GetTotal proc -> proc ! n

logger :: forall eff z. Eff (rec :: REC z Int, process :: PROCESS, console :: CONSOLE | eff) Unit
logger = do
  a <- receive
  log $ show a

test :: forall eff. Eff (process :: PROCESS, console :: CONSOLE | eff) Unit
test = do
  pLog <- spawn logger
  pCounter <- spawn counter
  forE 0 100000 \_ -> pCounter ! Add 1
  pCounter ! Subtract 5
  pCounter ! GetTotal pLog
