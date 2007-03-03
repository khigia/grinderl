%%% @author Ludovic Coquelle <lcoquelle@gmail.com>
%%% @copyright 2007 Ludovic Coquelle
%%% @doc Shared declarations of application.
%%%
%%% For license information see LICENSE.txt
%%% @end


% ~~ Declaration: OTP relative
%empty


% ~~ Declaration: API

-define(DEBUG(Msg, Params),   grd_util:log(?MODULE, ?LINE, dbg, Msg, Params)).
-define(WARNING(Msg, Params), grd_util:log(?MODULE, ?LINE, wrn, Msg, Params)).
-define(INFO(Msg, Params),    grd_util:log(?MODULE, ?LINE, inf, Msg, Params)).
-define(ERROR(Msg, Params),   grd_util:log(?MODULE, ?LINE, err, Msg, Params)).

    %((...) -> {ok, Pid::pid(), Values::list()} | {error, Pid::pid(), Reason})
-record(task, {
    callable    = fun() -> {ok, self(), []} end,
    args_spec   = [],
    result_spec = []
}).
-record(test, {
    nick     = "unknowTest",
    task     = #task{},
    mode     = {sequence, 0}, % concurrent|{sequence, Delay(milliseconds)}
    repeat_n = 1
}).


% ~~ Declaration: Internal
%empty


% ~~ Implementation: API
%empty


% ~~ Implementation: Behaviour callbacks
%empty


% ~~ Implementation: Internal
%empty
