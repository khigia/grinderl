-module(test).
-include("include/grinderl.hrl").
-export([
    init/0,
    test_seq1/0,
    test_con1/0,
    test_seq2/0,
    test_con2/0,
    test_seq3/0,
    test_con3/0
]).

init() ->
    % add some nodes to grinderl
    grinderl:add_nodes([node(), node()]).

test_seq1() ->
    % create task1 and run it on each node 3 times, sleeping 1s each time
    Test = create_test1({sequence, 1000}, 3),
    grinderl:run_test(Test).

test_con1() ->
    % create task1 and run it on each node 3 times concurrently
    Test = create_test1(concurrent, 3),
    grinderl:run_test(Test).

create_test1(Mode, Repeat) ->
    % Function to evaluate for the task:
    % Write a silly message on DEBUG stream about its arguments.
    % got 2 arguments:
    %  * a 'writer' name
    %  * a 'writen' value
    % return 2 values which will be manage by grinderl:
    %  * the time of execution,
    %  * a 2-tuple of its 2 arguments
    TaskFun = fun(Writer, WritenValue) ->
        FWrite = fun() -> ?DEBUG(
            "task1's function got two arguments: ~s, ~w",
            [Writer, WritenValue]
        ) end,
        {WriteTime, _Res} = grd_util:timeit(FWrite),
        {ok, self(), [WriteTime, {Writer, WritenValue}]}
    end,
    % Test description
    #test{
        nick        = 'Test 1',
        task = #task{
            callable    = TaskFun,
            args_spec   = [
                % 1st argument of the task will be taken in sequence
                {seq, ["bea", "pouf", "paf"]},
                % 2nd argument of the task will be choosen at random
                {choice, [0, 1, 12, 42, 77]}
            ],
            result_spec = [ % list of task results
                % 1st result of the task will be memorized as a list of values
                {mean, writetime},
                % 2nd result of the task will be a count per return value
                {count, writer_val}
            ]
        },
        mode        = Mode,
        repeat_n    = Repeat
    }.
    
test_seq2() ->
    % create task1 and run it on each node 3 times, sleeping 1s each time
    Test = create_test2({sequence, 1000}, 3),
    grinderl:run_test(Test).

test_con2() ->
    % create task1 and run it on each node 3 times concurrently
    Test = create_test2(concurrent, 3),
    grinderl:run_test(Test).

create_test2(Mode, Repeat) ->
    % use 'ping' command line to retrieve its raw output (text)
    #test{
        nick = 'Textual ping by command line',
        task = #task{
            callable    = grd_extcmd:command_2_taskfun(
                % use "ping" command line as a function
                "ping -n -c 1",
                % how many argument on command line? =>
                1
            ),
            args_spec   = [
                % choose at random a host to ping
                {choice, ["127.0.0.1", "localhost"]}
            ],
            result_spec = [
                {acc, ping_raw_output}
            ]
        },
        mode        = Mode,
        repeat_n    = Repeat
    }.
    
test_seq3() ->
    % create task1 and run it on each node 3 times, sleeping 1s each time
    Test = create_test3({sequence, 1000}, 3),
    grinderl:run_test(Test).

test_con3() ->
    % create task1 and run it on each node 3 times concurrently
    Test = create_test3(concurrent, 3),
    grinderl:run_test(Test).

create_test3(Mode, Repeat) ->
    % use 'ping' command line, retrieve and parse its output
    #test{
        nick = 'Textual ping by command line',
        task = #task{
            callable    = grd_extcmd:command_2_taskfun(
                % use "ping" command line as a function
                "ping -n -c 1",
                % how many argument on command line? =>
                1,
                % how to interpret output of this command
                fun ping_output_parser/1
            ),
            args_spec   = [
                % choose at random a host to ping
                {choice, ["127.0.0.1", "localhost"]}
            ],
            result_spec = [
                {acc, nicer_output}
            ]
        },
        mode        = Mode,
        repeat_n    = Repeat
    }.

ping_output_parser({0, Data}) ->
    % Data is the rw output of the command
    % ... let's extract some information
    % WARNING: no test at all! may failed if the output of ping is slightly
    % different of what I had during my tests...
    Str = lists:flatten(Data),
    {match, IPStart, IPLength} = regexp:match(
        Str,
        "\([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+\)"
    ),
    IPStr = string:substr(Str, IPStart + 1, IPLength - 2),
    {match, TimesStart, TimesLength} = regexp:match(
        Str,
        "([0-9]+\.[0-9]+)/([0-9]+\.[0-9]+)/([0-9]+\.[0-9]+)/([0-9]+\.[0-9]+)"
    ),
    TimesStr = string:substr(Str, TimesStart, TimesLength),
    {ok, TimeStrings} = regexp:split(TimesStr, "/"),
    Times = lists:map(
        fun(FStr) -> {F,_R} = string:to_float(FStr), F end,
        TimeStrings
    ),
    {ok, self(), [{IPStr, Times}]};
ping_output_parser({Status, Data}) ->
    {error, self(), [Status, Data]}.
    
