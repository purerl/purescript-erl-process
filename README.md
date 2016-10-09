# purescript-erl-process

Bindings to Erlang processes (spawn, send and receive).

## Raw bindings

Low level (unsafe) FFI bindings are provided in `Erl.Process.Raw`.

```purescript
proc :: forall eff. Eff (console :: CONSOLE, process :: PROCESS | eff) Unit
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

## Type-safe bindings

Bindings with an attempt to enforce a layer of type-safety over the raw bindings are given in `Erl.Process`. A process
is given some type `Process a`, and only values of type `a` can be sent to it.

Two effects are defined, `PROCESS` which is as per above, an indication that an expression may spawn a process or
receive a message, and `REC z a`, which indicates an expression which receieves a message of type `a`. The parameter
`z` is used as a quantified phantom type as in https://github.com/purescript/purescript-st.

```purescript
logger :: forall eff z. Eff (rec :: REC z Int, process :: PROCESS, console :: CONSOLE | eff) Unit
logger = do
  a <- receive
  log $ "Received: " <> show a
  
main = do
  p <- spawn logger
  p ! 42
```

A wrinkle is that inside of an expression which contains a `receive`, it is impossible to use `spawn` as it requires
there to be no `REC` effect in the outer expression. Actually all that is really required is that the outer `REC` effect
does not appear in the inner expression, so an alternative `spawn'` is provided which removes it (see tests for a use of this).
