-module(erl_process_raw@foreign).
-export([
         eqNative/2,
         spawn/1,
         spawnLink/1,
         send/1,
         'receive'/0,
         receiveWithTimeout/2,
         receiveWithTrap/0,
         receiveWithTrapAndTimeout/2,
         self/0,
         setProcessFlagTrapExit/1,
         exit/1
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

receiveWithTrap() ->
  fun () ->
    receive
      {'EXIT', Pid, killed} ->
        {left, {exitMsg, Pid, {kill}}};
      {'EXIT', Pid, normal} ->
        {left, {exitMsg, Pid, {normal}}};
      {'EXIT', Pid, Other } ->
        {left, {exitMsg, Pid, {other, Other}}};
      X                     -> 
        {right, X}
    end
  end.

receiveWithTrapAndTimeout(Timeout, Msg) ->
  fun () ->
    receive
      {'EXIT', Pid, killed} -> {left, {exitMsg, Pid, {killed}}};
      {'EXIT', Pid, normal} -> {left, {exitMsg, Pid, {normal}}};
      {'EXIT', Pid, Other } -> {left, {exitMsg, Pid, {other, Other}}};
      X                     -> {right, X}
    after
      Timeout -> Msg
    end
  end.

self() -> fun () -> erlang:self() end.

setProcessFlagTrapExit(TrapExit) -> fun() ->
  erlang:process_flag(trap_exit, TrapExit)
end.

exit(Term) -> fun () -> erlang:exit(Term) end.