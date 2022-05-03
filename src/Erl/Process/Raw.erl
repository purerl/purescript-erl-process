-module(erl_process_raw@foreign).
-export([
         eqNative/2,
         compareNative/2,
         spawn/1,
         spawnLink/1,
         send/1,
         'receive'/0,
         receiveWithTimeout_/2,
         receiveWithTrap/0,
         receiveWithTrapAndTimeout_/2,
         self/0,
         setProcessFlagTrapExit/1,
         exit/1,
         sendExitSignal/2,
         unlink/1,
         show_/1
        ]).

eqNative(X, Y) -> X == Y.

compareNative(X, Y) when X < Y -> {lT};
compareNative(X, Y) when X == Y -> {eQ};
compareNative(_X, _Y) -> {gT}.

spawn(F) -> fun () -> erlang:spawn(F) end.
spawnLink(F) -> fun () -> erlang:spawn_link(F) end.

send(Pid) -> fun (X) ->
  fun () ->
    Pid ! X
  end
end.

'receive'() ->
  fun () ->
    receive X -> X end
  end.

receiveWithTimeout_(Timeout, Msg) ->
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

receiveWithTrapAndTimeout_(Timeout, Msg) ->
  fun () ->
    receive
      {'EXIT', Pid, killed} -> {left, {exitMsg, Pid, {killed}}};
      {'EXIT', Pid, normal} -> {left, {exitMsg, Pid, {normal}}};
      {'EXIT', Pid, Other } -> {left, {exitMsg, Pid, {other, Other}}};
      X                     -> {right, X}
    after
      Timeout -> {right, Msg}
    end
  end.

self() -> fun () -> erlang:self() end.

setProcessFlagTrapExit(TrapExit) -> fun() ->
  erlang:process_flag(trap_exit, TrapExit)
end.

exit(Term) -> fun () -> erlang:exit(Term) end.

sendExitSignal(Term, Pid) -> fun () -> erlang:exit(Pid, Term) end.

unlink(Pid) -> fun() -> erlang:unlink(Pid) end.

show_(Pid) ->
    list_to_binary(erlang:pid_to_list(Pid)).
