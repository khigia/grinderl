%%% @author Ludovic Coquelle <lcoquelle@gmail.com>
%%% @copyright 2007 Ludovic Coquelle
%%% @doc Worker server: execute a test.
%%%
%%% Called by the stress server, this process will execute a test and report
%%% the result (test definition in include/grinderl.hrl).
%%%
%%% For license information see LICENSE.txt
%%% @end
-module(grd_worker_srv).


% ~~ Declaration: OTP relative
-behaviour(gen_server).
-export([
    init/1,         % (InitArgs) -> Result
    handle_call/3,  % (Request, From, State) -> Result
    handle_cast/2,  % (Request, State) -> Result
    handle_info/2,  % (Info, State) -> Result
    terminate/2,    % (Reason, State) -> term() % result is not used
    code_change/3   % (OldVsn, State, Extra) -> {ok, NewState}
]).


% ~~ Declaration: API
-export([
    start/0,
    run_test/3
]).


% ~~ Declaration: Internal
-include("include/grinderl.hrl").
-export([
    statvar_loop/7 % to be spawn
]).


% ~~ Implementation: API

%%% @doc  Start a local server (un-registered).
%%% @spec () -> Result
%%% @end
start() ->
    gen_server:start(
        ?MODULE,
        [],
        []
    ).

%%% @doc  Start a test.
%%% @spec (Worker::pid(), Test::test(), ReportTo::pid()) -> ok | {error, Reason}
%%% @end
run_test(Worker, Test, ReportTo) ->
    gen_server:cast(Worker, {run_test, Test, ReportTo}).

% ~~ Implementation: Behaviour callbacks

%%% @doc  Initialization include those of random generator.
%%% OTP meaning: initialize the server state.
%%% @see  gen_server:init/1
%%% @spec (InitArgs) -> Result
%%% @end
init(_InitArgs) ->
    {random_seed, A1, A2, A3} = grd_random_srv:get_seed(),
    random:seed(A1, A2, A3),
    {ok, nostate}.

%%% @doc  Do nothing.
%%% OTP meaning: handle any request sent through {@link gen_server:call/2} or {@link gen_server:multi_call/2}.
%%% @see  gen_server:handle_call/3
%%% @spec (Request, From, State) -> {reply|noreply|stop, Params}
%%% @end
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%% @doc  Handle request {run_test, Tast, ReportTo}; else nothing.
%%% OTP meaning: handle any request sent through {@link gen_server:cast/2} or {@link gen_server:multi_cast/2}.
%%% @see  gen_server:handle_cast/3
%%% @spec (Request, State) -> {noreply|stop, Params}
%%% @end
handle_cast({run_test, Test, ReportTo}, State) ->
    ?DEBUG("worker ~w is working on ~p ...", [self(), Test#test.nick]),
    run_test(Test, ReportTo),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%%% @doc  Do nothing.
%%% OTP meaning: called when receiving something else than request (e.g. timeout).
%%% @see  gen_server:handle_info/2
%%% @spec (Info, State) -> {noreply|stop, Params}
%%% @end
handle_info(_Info, State) ->
    {noreply, State}.

%%% @doc  Do nothing.
%%% OTP meaning: called when is about to terminate.
%%% @see  gen_server:terminate/2
%%% @spec (Reason, State) -> term()
%%% @end
terminate(_Reason, _State) ->
    ?DEBUG("work is finished for ~w!", [self()]),
    ok.


%%% @doc  Do nothing.
%%% OTP meaning: manage release handling.
%%% @see  gen_server:code_change/3
%%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%%% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% ~~ Implementation: Internal

run_test(Test, ReportTo) ->
    TaskFun        = (Test#test.task)#task.callable,
    TaskArgsSpec   = (Test#test.task)#task.args_spec,
    TaskResultSpec = (Test#test.task)#task.result_spec,
    Mode       = Test#test.mode,
    Repeat     = Test#test.repeat_n,
    Supervisor = create_test_supervisor(
        ReportTo,
        Repeat,
        TaskResultSpec
    ),
    MeasuredTaskFun = wrap_task(Supervisor, TaskFun),
    run_task(Mode, Repeat, MeasuredTaskFun, TaskArgsSpec).

wrap_task(TestSupervisor, TaskFun) ->
    fun(ArgLst) ->
        %TODO measure time of task execution
        Res = erlang:apply(TaskFun, ArgLst),
        ?DEBUG(
            "process ~w ran task and will report [~w] to supervisor ~w",
            [self(), Res, TestSupervisor]
        ),
        case Res of
            {ok, Pid, ValLst} ->
                TestSupervisor ! {stat, Pid, ValLst};
            {error, Pid, Reason} ->
                TestSupervisor ! {error, Pid, Reason}
        end
    end.

run_task({sequence, _Delay}, 0, _TaskFun, _TaskArgsSpec) ->
    ok;
run_task(Mode = {sequence, Delay}, N, TaskFun, TaskArgsSpec) ->
    {NewTaskArgsSpec, Args} = choose_task_args(TaskArgsSpec),
    TaskFun(Args),
    timer:sleep(Delay), %TODO use of timer:apply_after?
    run_task(Mode, N-1, TaskFun, NewTaskArgsSpec);
run_task(concurrent, 0, _TaskFun, _TaskArgsSpec) ->
    ok;
run_task(concurrent, N, TaskFun, TaskArgsSpec) ->
    {NewTaskArgsSpec, Args} = choose_task_args(TaskArgsSpec),
    spawn(fun() -> TaskFun(Args) end),
    run_task(concurrent, N-1, TaskFun, NewTaskArgsSpec).

choose_task_args(ArgsSpec) ->
    lists:mapfoldl(
        fun(Args, ArgAcc) -> choose_arg_from_spec(Args, ArgAcc) end,
        [],
        ArgsSpec
    ).

choose_arg_from_spec({fixed, Uniq}, ArgAcc) ->
    {{fixed, Uniq}, ArgAcc ++ [Uniq]};
choose_arg_from_spec({seq, [Arg|ArgQ]}, ArgAcc) ->
    {{seq, ArgQ++[Arg]}, ArgAcc ++ [Arg]};
choose_arg_from_spec({choice, ArgLst}, ArgAcc) ->
    {{choice, ArgLst}, ArgAcc ++ [grd_util:choice(ArgLst)]};
choose_arg_from_spec({rr, [], UsedArgs}, ArgAcc) ->
    NewArgs = grd_util:shuffle(UsedArgs),
    choose_arg_from_spec({rr, NewArgs, []}, ArgAcc);
choose_arg_from_spec({rr, [Arg|ArgQ], UsedArgs}, ArgAcc) ->
    {{rr, ArgQ, [Arg|UsedArgs]}, ArgAcc ++ [Arg]}.

create_test_supervisor(ReportTo, Repeat, TaskResultSpec) ->
    StatVars = lists:map(fun statvar_create/1, TaskResultSpec),
    StatVarsUpdater = fun(LocalStatVars, LocalValLst) -> lists:zipwith(
        fun statvar_update/2,
        LocalStatVars,
        LocalValLst
    ) end,
    Supervisor = spawn(
        ?MODULE, statvar_loop, [
            ReportTo,
            now(),
            Repeat,
            1,
            0,
            StatVarsUpdater,
            StatVars
        ]
    ),
    ?DEBUG(
        "worker ~w create Supervisor ~w on node ~w",
        [self(), Supervisor, node()]
    ),
    Supervisor.

statvar_loop(ReportTo, T0, NTest, NTest, NError, StatVarsUpdater, StatVars) ->
    receive
        {stat, Pid, ValList} ->
            ?DEBUG(
                "Supervisor ~w end on values from ~w:~w",
                [self(), Pid, ValList]
            ),
            NewStatVars = StatVarsUpdater(StatVars, ValList),
            ReportTo ! {
                finish,
                self(),
                timer:now_diff(now(), T0),
                NTest,
                NError,
                NewStatVars
            },
            ok;
        {error, Pid, Reason} ->
            ?DEBUG(
                "Supervisor ~w end on error from ~w:~w",
                [self(), Pid, Reason]
            ),
            ReportTo ! {
                finish,
                self(),
                timer:now_diff(now(), T0),
                NTest,
                NError + 1,
                StatVars
            },
            ok
    end;
statvar_loop(ReportTo, T0, NTest, CurrentTest, NError, StatVarsUpdater, StatVars) when CurrentTest < NTest ->
    receive
        {stat, Pid, ValList} ->
            ?DEBUG(
                "Supervisor ~w got values from ~w: ~w",
                [self(), Pid, ValList]
            ),
            NewStatVars = StatVarsUpdater(StatVars, ValList),
            statvar_loop(ReportTo, T0, NTest, CurrentTest+1, NError, StatVarsUpdater, NewStatVars);
        {error, Pid, Reason} ->
            ?DEBUG(
                "Supervisor ~w got error from ~w: ~w",
                [self(), Pid, Reason]
            ),
            statvar_loop(ReportTo, T0, NTest, CurrentTest+1, NError+1, StatVarsUpdater, StatVars)
    end.

statvar_create({mean, Name}) ->
    {mean, Name, []};
statvar_create({count, Name}) ->
    {count, Name, dict:new()}.

statvar_update({mean, Name, Cur}, Val) ->
    {mean, Name, [Val|Cur]};
statvar_update({count, Name, Cur}, Val) ->
    {count, Name, dict:update_counter(Val, 1, Cur)}.

