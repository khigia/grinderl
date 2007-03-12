%%% @doc Helpers to create tasks from command line.
%%%
%%% For license information see LICENSE.txt
%%% @end
-module(grd_extcmd).


% Declaration: OTP relative
%empty


% Declaration: API
-export([
    command_2_taskfun/2,
    command_2_taskfun/3
]).

% Declaration: Internal
-include("include/grinderl.hrl").


% ~~ Implementation: API

%%% @doc  Create a task's callable using raw output of an external command.
%%% @spec (Cmd::string(), Arity::integer()) -> fun()
%%% @end
command_2_taskfun(Cmd, Arity) ->
    ResultParser = fun interpret_command_result/1,
    command_2_taskfun(Cmd, Arity, ResultParser).

%%% @doc  Create a task's callable interpreting output of an external command.
%%% @spec (Cmd::string(), Arity::integer(), OutputParser::fun()) -> fun()
%%% @end
command_2_taskfun(Cmd, _Arity=0, ResultParser) ->
    fun() ->
        exec_command(Cmd, ResultParser)
    end;
command_2_taskfun(Cmd, _Arity=1, ResultParser) ->
    fun(X) ->
        ExtCmd = lists:flatten(io_lib:format("~s ~s", [Cmd, X])),
        exec_command(ExtCmd, ResultParser)
    end;
command_2_taskfun(Cmd, _Arity=2, ResultParser) ->
    fun(A, B) ->
        ExtCmd = lists:flatten(io_lib:format("~s ~s ~s", [Cmd, A, B])),
        exec_command(ExtCmd, ResultParser)
    end;
command_2_taskfun(Cmd, _Arity=3, ResultParser) ->
    fun(A, B, C) ->
        ExtCmd = lists:flatten(io_lib:format("~s ~s ~s ~s", [Cmd, A, B, C])),
        exec_command(ExtCmd, ResultParser)
    end.

% ~~ Implementation: Behaviour callbacks
%empty


% ~~ Implementation: Internal

%@doc Execute the external command and "pipe its output" in ResultParser
exec_command(ExtCmd, ResultParser) ->
    Opts = [stream, exit_status, use_stdio, stderr_to_stdout, in, eof],
    P = open_port({spawn, ExtCmd}, Opts),
    {Status, Data} = receive_command_output(P, []),
    ResultParser({Status, Data}).

%@doc Listen to port and receive all the data until eof
receive_command_output(Port, Acc) ->
    receive
        {Port, {data, D}} ->
            receive_command_output(Port, [Acc|D]);
        {Port, eof} ->
            port_close(Port),
            receive
                {Port, {exit_status, N}} ->
                    %{N, lists:reverse(Acc)}
                    {N, Acc}
            end
    end.

%@doc Default interpretation of command output (status 0 is ok, else error)
interpret_command_result({0, Data}) ->
    {ok, self(), [Data]};
interpret_command_result({Status, Data}) ->
    {error, self(), [Status, Data]}.
