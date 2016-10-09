% module Erl.Process.Raw
-module(erl_process_raw@foreign).
-export([eqNative/1, spawn/1, send/1, 'receive'/0]).

eqNative(X) -> fun (Y) -> X == Y end.

spawn(F) -> fun () -> erlang:spawn(fun () -> F() end) end.

send(Pid) ->
  fun (X) ->
    fun () ->
      Pid ! X
    end
  end.

'receive'() ->
  fun () ->
    receive X -> X end
  end.
