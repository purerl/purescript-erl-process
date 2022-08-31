-module(erl_process@foreign).
-export([ start/1
        , startLink/1
        , launcher/2
        ]).

-define(left(A), {left, A}).
-define(right(A), {right, A}).

start(F) -> do_start(fun proc_lib:start/3, F).
startLink(F) -> do_start(fun proc_lib:start_link/3, F).

do_start(ChosenStarter, F) ->
    fun() ->
            case ChosenStarter(?MODULE, launcher, [self(), F]) of
                {error, Err} -> ?left(Err);
                Resp -> ?right(Resp)
            end
    end.


launcher(ParentPid, F) ->
    #{result := Result, cont := Cont} = F(),
    ok = proc_lib:init_ack(ParentPid,  #{result => Result, pid => self()}),
    Cont().
