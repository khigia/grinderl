%%% @author Ludovic Coquelle <lcoquelle@gmail.com>
%%% @copyright 2007 Ludovic Coquelle
%%% @doc Test module.
%%%
%%% @end
-module(eu_grd_random_srv).

-import(grd_random_srv, [
    start_link/0,
    handle_call/3
]).

-include_lib("eunit/include/eunit.hrl").

start_link_test() ->
    {ok, Pid} = start_link(),
    ?assert(global:whereis_name(grd_random_srv) == Pid).
    
handle_call_test_() -> [
    % successful calls
    fun() ->
        {reply, {random_seed, A1, A2, A3}, nostate} = handle_call(
            get_seed, self(), nostate
        ),
        ?assert(is_integer(A1) and is_integer(A2) and is_integer(A3))
    end,
    % failing calls
    ?_assert(
        handle_call(bad_request, self(), nostate) == {
            reply, noresult, nostate
        }
    )
].
