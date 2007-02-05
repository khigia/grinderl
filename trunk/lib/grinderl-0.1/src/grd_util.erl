%%% @author Ludovic Coquelle <lcoquelle@gmail.com>
%%% @copyright 2007 Ludovic Coquelle
%%% @doc Utilities (log, config).
%%%
%%% API to some shared utility.
%%%
%%% For license information see LICENSE.txt
%%% @end
-module(grd_util).

% Declaration: OTP relative
%empty

% Declaration: API
-include("grinderl.hrl").
-export([
    log/5,
    timeit/1,
    choice/1,
    randomize/1,
    shuffle/1
]).

% Declaration: Internal
%empty


% ~~ Implementation: API

%%% @doc  Add one information in the log streams.
%%% If Level is <em>dbg</em>, print message on <em>stdout</em>; else use the standard application <em>error_logger</em> (levels stands for info, warning and error).<br/>
%%% This function is used through applications macros (<em>grinderl.hrl</em>) which automaticaly capture <em>Level</em>, <em>Module</em> and <em>Line</em>.
%%% @spec (Module::atom(), Line::integer(), Level, Msg::string(), Params) -> integer()
%%%   Level = dbg | inf | wrn | err
%%%   Params = [term()]
%%% @end
log(Module, Line, dbg, Msg, Params) ->
    io:format(
        "Debug:~p:~p: " ++ Msg ++ "~n",
        [Module, Line] ++ Params
    );
log(Module, Line, inf, Msg, Params) ->
    error_logger:info_msg(
        "~p:~p: " ++ Msg,
        [Module, Line] ++ Params
    );
log(Module, Line, wrn, Msg, Params) ->
    error_logger:warning_msg(
        "~p:~p: " ++ Msg,
        [Module, Line] ++ Params
    );
log(Module, Line, err, Msg, Params) ->
    error_logger:error_msg(
        "~p:~p: " ++ Msg,
        [Module, Line] ++ Params
    ).

%%% @doc  Time application of a function.
%%% Apply the function and return a pair formed of evaluation's time in microseconds and function result (same behavior as {@link timer:tc/3} but does no require a module reference that is, does not assume a fun reference to be a 2-tuple).
%%% @spec ( () -> Result ) -> {Time::integer(), Result}
%%% @end
timeit(Fun) ->
    T0 = now(),
    Res = Fun(),
    TFun = timer:now_diff(now(), T0),
    {TFun, Res}.

%%% @doc  Choose randomly one element from a list (equi-probability).
%%% @spec ([term()]) -> term()
%%% @end
choice(List) when is_list(List) ->
    lists:nth(random:uniform(length(List)), List).

%%% @doc  Simple shuffle of a list.
%%% @spec (List::list()) -> NewList::list()
%%% @end
randomize(List) ->
    D = lists:map(
        fun(A) -> {random:uniform(), A} end,
        List
    ),
    {_, D1} = lists:unzip(lists:keysort(1, D)),
    D1.

%%% @doc  Fair shuffle of a list.
%%% Apply randomize multiple times depending on list length.
%%% source code from: http://erlang.org/ml-archive/erlang-questions/200608/msg00621.html
%%% @spec (List::list()) -> NewList::list()
%%% @end
shuffle(List) ->
    %% Determine the log n portion then randomize the list.
    randomize_repeat(round(math:log(length(List)) + 0.5), List).

% ~~ Implementation: Behaviour callbacks
%empty


% ~~ Implementation: Internal

%%% @doc  Apply randomize N times on a list L.
%%% @spec (N::integer(), L::list()) -> ShuffledList::list()
%%% @end
randomize_repeat(1, List) ->
    randomize(List);
randomize_repeat(T, List) ->
    lists:foldl(
        fun(_E, Acc) -> randomize(Acc) end,
        randomize(List),
        lists:seq(1, (T - 1))
    ).

