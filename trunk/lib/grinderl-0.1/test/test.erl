-module(test).
-include("include/grinderl.hrl").
-export([
    init/0,
    test_seq1/0,
    test_con1/0
]).

init() ->
    % add some nodes to grinderl
    grinderl:add_nodes([node(), node()]).

test_seq1() ->
    % create task1 and run it on each node 3 times, sleeping 1s each time
    Task = create_task1({sequence, 1000}, 3),
    grinderl:run_task(Task).

test_con1() ->
    % create task1 and run it on each node 3 times concurrently
    Task = create_task1(concurrent, 3),
    grinderl:run_task(Task).

create_task1(Mode, Repeat) ->
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
    % Task description
    #task{
        nick        = 'Testing task 1',
        task_fun    = TaskFun,
        arglst      = [
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
        ],
        mode        = Mode,
        repeat_n    = Repeat
    }.
    
