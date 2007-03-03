%%% @doc Test module.
%%%
%%% @end
-module(eu_grd_extcmd).

-import(grd_extcmd, [
    command_2_taskfun/2
]).

-include_lib("eunit/include/eunit.hrl").

command_2_taskfun_test_() -> [
    fun() ->
        F = command_2_taskfun("echo", 1),
        ?assert(is_function(F, 1))
    end
].
