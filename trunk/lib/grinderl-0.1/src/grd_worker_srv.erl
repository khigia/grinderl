%%% @author Ludovic Coquelle <lcoquelle@gmail.com>
%%% @copyright 2007 Ludovic Coquelle
%%% @doc Worker server: execute a task.
%%%
%%% Called by the stress server, this process will execute a task and report
%%% the result (task definition in include/grinderl.hrl).
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
    run_task/3
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

%%% @doc  Start a task.
%%% @spec (Worker::pid(), Task::task(), ReportTo::pid()) -> ok | {error, Reason}
%%% @end
run_task(Worker, Task, ReportTo) ->
    gen_server:cast(Worker, {run_task, Task, ReportTo}).

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

%%% @doc  Handle request {run_task, Task, ReportTo}; else nothing.
%%% OTP meaning: handle any request sent through {@link gen_server:cast/2} or {@link gen_server:multi_cast/2}.
%%% @see  gen_server:handle_cast/3
%%% @spec (Request, State) -> {noreply|stop, Params}
%%% @end
handle_cast({run_task, Task, ReportTo}, State) ->
    ?DEBUG("worker ~w is working on ~p ...", [self(), Task#task.nick]),
    run_task(Task, ReportTo),
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

run_task(Task, ReportTo) ->
    TaskFun        = Task#task.task_fun,
    TaskArgLst     = Task#task.arglst,
    TaskResultSpec = Task#task.result_spec,
    TaskMode       = Task#task.mode,
    TaskRepeat     = Task#task.repeat_n,
    TaskSupervisor = create_task_supervisor(
        ReportTo,
        TaskRepeat,
        TaskResultSpec
    ),
    MeasuredTaskFun = wrap_task(TaskSupervisor, TaskFun),
    run_task_fun(TaskMode, TaskRepeat, MeasuredTaskFun, TaskArgLst).

wrap_task(TaskSupervisor, TaskFun) ->
    fun(ArgLst) ->
        %TODO measure time of task execution
        Res = erlang:apply(TaskFun, ArgLst),
        ?DEBUG(
            "process ~w ran task and will report [~w] to supervisor ~w",
            [self(), Res, TaskSupervisor]
        ),
        case Res of
            {ok, Pid, ValLst} ->
                TaskSupervisor ! {stat, Pid, ValLst};
            {error, Pid, Reason} ->
                TaskSupervisor ! {error, Pid, Reason}
        end
    end.

run_task_fun({sequence, _Delay}, 0, _TaskFun, _TaskArgLst) ->
    ok;
run_task_fun(Mode = {sequence, Delay}, N, TaskFun, TaskArgLst) ->
    {NewTaskArgLst, Args} = choose_task_args(TaskArgLst),
    TaskFun(Args),
    timer:sleep(Delay), %TODO use of timer:apply_after?
    run_task_fun(Mode, N-1, TaskFun, NewTaskArgLst);
run_task_fun(concurrent, 0, _TaskFun, _TaskArgLst) ->
    ok;
run_task_fun(concurrent, N, TaskFun, TaskArgLst) ->
    {NewTaskArgLst, Args} = choose_task_args(TaskArgLst),
    spawn(fun() -> TaskFun(Args) end),
    run_task_fun(concurrent, N-1, TaskFun, NewTaskArgLst).

choose_task_args(ArgDescLst) ->
    lists:mapfoldl(
        fun(ArgDesc, ArgAcc) -> choose_arg_from_desc(ArgDesc, ArgAcc) end,
        [],
        ArgDescLst
    ).

choose_arg_from_desc({fixed, Uniq}, ArgAcc) ->
    {{fixed, Uniq}, ArgAcc ++ [Uniq]};
choose_arg_from_desc({seq, [Arg|ArgQ]}, ArgAcc) ->
    {{seq, ArgQ++[Arg]}, ArgAcc ++ [Arg]};
choose_arg_from_desc({choice, ArgLst}, ArgAcc) ->
    {{choice, ArgLst}, ArgAcc ++ [grd_util:choice(ArgLst)]};
choose_arg_from_desc({rr, [], UsedArgs}, ArgAcc) ->
    NewArgs = grd_util:shuffle(UsedArgs),
    choose_arg_from_desc({rr, NewArgs, []}, ArgAcc);
choose_arg_from_desc({rr, [Arg|ArgQ], UsedArgs}, ArgAcc) ->
    {{rr, ArgQ, [Arg|UsedArgs]}, ArgAcc ++ [Arg]}.

create_task_supervisor(ReportTo, TaskRepeat, TaskResultSpec) ->
    StatVars = lists:map(fun statvar_create/1, TaskResultSpec),
    StatVarsUpdater = fun(LocalStatVars, LocalValLst) -> lists:zipwith(
        fun statvar_update/2,
        LocalStatVars,
        LocalValLst
    ) end,
    TaskSupervisor = spawn(
        ?MODULE, statvar_loop, [
            ReportTo,
            now(),
            TaskRepeat,
            1,
            0,
            StatVarsUpdater,
            StatVars
        ]
    ),
    ?DEBUG(
        "worker ~w create TaskSupervisor ~w on node ~w",
        [self(), TaskSupervisor, node()]
    ),
    TaskSupervisor.

statvar_loop(ReportTo, T0, NTest, NTest, NError, StatVarsUpdater, StatVars) ->
    receive
        {stat, Pid, ValList} ->
            ?DEBUG(
                "TaskManager ~w end on values from ~w:~w",
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
                "TaskManager ~w end on error from ~w:~w",
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
                "TaskManager ~w got values from ~w: ~w",
                [self(), Pid, ValList]
            ),
            NewStatVars = StatVarsUpdater(StatVars, ValList),
            statvar_loop(ReportTo, T0, NTest, CurrentTest+1, NError, StatVarsUpdater, NewStatVars);
        {error, Pid, Reason} ->
            ?DEBUG(
                "TaskManager ~w got error from ~w: ~w",
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
    {count, Name, dict:update(Val, fun(Count) -> Count + 1 end, 1, Cur)}.

