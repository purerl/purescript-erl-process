# purescript-erl-process

Bindings to Erlang processes (spawn, send and receive).

## Type-safe bindings

Bindings with an attempt to enforce a layer of type-safety are given in `Erl.Process` (this assumes both sender and receiver are constructed via this mechanism).

A process is given some type `Process a`, and only values of type `a` can be sent to it. All process spawning, message sending and receiving takes place in the `Effect` monad.

Firstly we can define a receiving function:

```purescript
logger :: forall a. (Show a) => Effect a -> Effect Unit
logger receive = do
  a <- receive
  log $ "Received: " <> show a <> "\n"
```

And then we can launch the process with `spawn` and send messages to it with `(!)`:

```purescript
main :: Effect Unit
main = do
  p <- spawn logger
  p ! 42
```

In practice the `a` in `Process a` is likely to be some ADT representing the various possible messages that may be sent to the process.

More examples can be found in [the tests](https://github.com/purerl/purescript-erl-process/tree/master/test).

## Raw (unsafe) bindings

Low level (unsafe) FFI bindings are provided in `Erl.Process.Raw`.

```purescript
proc :: forall eff. Efffect Unit
proc = do
  n :: Int <- receive
  log $ "Received: " <> show n
  proc
 
main = do
  p <- spawn proc
  p ! 42
```

Here the `receive` could be given any type annotation, and if a different type of value is sent, the process will crash.

This corresponds to the Erlang code

```erlang
proc() ->
  receive
    X -> io:format("~p~n", [X])
  end.
  
main() ->
  Pid <- spawn(fun proc/1),
  Pid ! 42.
```
