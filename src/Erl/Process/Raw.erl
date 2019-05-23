-module(erl_process_raw@foreign).
-export([
         eqNative/2,
         spawn/1,
         spawnLink/1,
         send/1,
         'receive'/0,
         receiveWithTimeout/2
        ]).

eqNative(X, Y) -> X == Y.

spawn(F) -> fun () -> erlang:spawn(fun () -> F() end) end.
spawnLink(F) -> fun () -> erlang:spawn_link(fun () -> F() end) end.

send(Pid) -> fun (X) ->
  fun () ->
    Pid ! X
  end
end.

'receive'() ->
  fun () ->
    receive X -> X end
  end.

receiveWithTimeout(Timeout, Msg) ->
  fun () ->
    receive
      X -> X
    after
      Timeout -> Msg
    end
  end.
