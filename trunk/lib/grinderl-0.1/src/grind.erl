-module(grind).

%% -compile(export_all).
%% 
%% % main module API
%% -export([
%%     init/1,            % ([Node]) -> [{Node, atom}]
%%     distribute_task/2  % ([Node] -> TaskDesc) -> [RunnerPid]
%% ]).
%% 
%% % spawned functions
%% -export([
%%     init_random/2,         % (ReportTo, Seed) -> atom
%%     statistic_gathering/2, % (T0, NTask) -> atom
%%     statistician_loop/6,   % (ReportTo, T0, NTest, CurrentTestN, NError, StatVarList) -> atom
%%     run_task/2,            % (StatListener, Task) -> atom
%%     run_test/4             % (sequence|concurrent, N, Test, TestArgLst) -> atom
%% ]).
%% 
%% % tools
%% 
%% % testing
%% -export([
%%     test_print/3  % (concurrent|sequence, Num, [Node]) -> atom
%% ]).
%% 
%% 
%% % == testing code ==
%% 
%% %DEBUG -compile(export_all).
%% 
%% % ex: test_print(concurrent, 5, [node(), node()]).
%% %% @spec test_print(
%% %%     concurrent|sequential,
%% %%     N::integer(),
%% %%     NodeLst::list()
%% %% ) -> test_print_launched
%% %% @doc Do a beautiful test!
%% test_print(Mode, N, NodeLst) ->
%%     distribute_task(
%%         NodeLst,
%%         {
%%             {Mode, N},
%%             [{mean, writetime}, {count, writer_val}],
%%             fun(Writer, WritenValue) ->
%%                 FWrite = fun() -> io:format("~s got ~w~n", [Writer, WritenValue]) end,
%%                 {WriteTime, _Res} = timeit(FWrite),
%%                 {ok, self(), [WriteTime, {Writer, WritenValue}]}
%%             end,
%%             [{rr, ["bea", "pouf"]}, {choice, [0, 1, 12, 42, 77]}]
%%         }
%%     ),
%%     test_print_launched.
%% 
%% 
%% 
%% % == module implementation ==
%% 
%% 
%% % NodeLst::list()
%% % Task::tuple(
%% %     {Mode::(sequence|concurrent), N::int()},
%% %     StatDesc::list({(mean|count), id::atom()})
%% %     TestFun::fun(ArgLst) -> {ok, pid(), Vals::list()}
%% %                           | {error, Pid}
%% %         length(ArgLst)=length(TestArgLst)
%% %         length(Vals)=length(StatDesc) 
%% %     TestArgLst::list( {fixed, Val} | {rr|choice,list()} )
%% % )
%% distribute_task(NodeLst, Task) ->
%%     NodeLstLen = length(NodeLst),
%%     %%% TO REPRODUCE
%%     StatListener = spawn(grind, statistic_gathering, [now(), NodeLstLen]),
%%     io:format(
%%         "~w create statistic gathering process ~w on node ~w~n",
%%         [self(), StatListener, node()]
%%     ),
%%     TaskRunnerCreator = fun(Node) -> spawn(
%%         Node,
%%         grind,
%%         run_task,
%%         [StatListener, Task]
%%     ) end,
%%     Runners = lists:map(TaskRunnerCreator, NodeLst),
%%     io:format(
%%         "~w create a list of task runners: ~w~n",
%%         [self(), Runners]
%%     ),
%%     Runners.
%% 
%% 
%% % run a testing task
%% 
%% run_task(StatListener, {{Mode, N}, StatDesc, TestFun, TestArgLst} = _Task) ->
%%     Statistician = statistician_create(StatListener, N, StatDesc),
%%     TestWithStat = wrap_test(Statistician, TestFun),
%%     run_test(Mode, N, TestWithStat, TestArgLst).
%% 
%% wrap_test(Statistician, TestFun) ->
%%     fun(ArgLst) ->
%%         Res = erlang:apply(TestFun, ArgLst),
%%         io:format(
%%             "~w (reporting to statistician ~w) ran ~w(~w) and got: ~w~n",
%%             [self(), Statistician, TestFun, ArgLst, Res]
%%         ),
%%         case Res of
%%             {ok, Pid, ValLst} ->
%%                 Statistician ! {stat, Pid, ValLst};
%%             {error, Pid, Reason} ->
%%                 Statistician ! {error, Pid, Reason}
%%         end
%%     end.
%% 
%% run_test(sequence, 0, _Test, _TestArgLst) ->
%%     end_run_test_sequence;
%% run_test(sequence, N, Test, TestArgLst) ->
%%     {TestArgLstNew, Args} = choose_test_args(TestArgLst),
%%     Test(Args),
%%     run_test(sequence, N-1, Test, TestArgLstNew);
%% run_test(concurrent, 0, _Test, _TestArgLst) ->
%%     end_run_test_concurrent;
%% run_test(concurrent, N, Test, TestArgLst) ->
%%     {TestArgLstNew, Args} = choose_test_args(TestArgLst),
%%     spawn(fun() -> Test(Args) end),
%%     run_test(concurrent, N-1, Test, TestArgLstNew).
%% 
%% choose_test_args(ArgDescLst) ->
%%     lists:mapfoldl(
%%         fun(ArgDesc, ArgAcc) -> choose_arg_from_desc(ArgDesc, ArgAcc) end,
%%         [],
%%         ArgDescLst
%%     ).
%% 
%% choose_arg_from_desc({fixed, Uniq}, ArgAcc) ->
%%     {{fixed, Uniq}, ArgAcc++[Uniq]};
%% choose_arg_from_desc({choice, ArgLst}, ArgAcc) ->
%%     {{choice, ArgLst}, ArgAcc ++ [choice(ArgLst)]};
%% choose_arg_from_desc({rr, [Arg|ArgQ]}, ArgAcc) ->
%%     {{rr, ArgQ++[Arg]}, ArgAcc ++ [Arg]}.
%% 
%% 
%% % statistician
%% % TODO gen_event?
%% 
%% statistician_create(ReportTo, NTest, VarList) ->
%%     StatVarList = lists:map(fun statistician_var_create/1, VarList),
%%     Statistician = spawn(grind, statistician_loop, [ReportTo, now(), NTest, 1, 0, StatVarList]),
%%     io:format(
%%         "~w create statistician ~w on node ~w~n",
%%         [self(), Statistician, node()]
%%     ),
%%     Statistician.
%% 
%% statistician_loop(ReportTo, T0, NTest, CurrentTest, NError, StatVarList) ->
%%     Updater = fun(SVarLst, ValLst) -> lists:zipwith(
%%         fun statistician_var_update/2,
%%         SVarLst,
%%         ValLst
%%     ) end,
%%     receive
%%         {stat, Pid, ValList} when CurrentTest < NTest ->
%%             io:format("Statistician ~w got stat from ~w:~w~n", [self(), Pid, ValList]),
%%             StatVarListNew = Updater(StatVarList, ValList),
%%             statistician_loop(ReportTo, T0, NTest, CurrentTest+1, NError, StatVarListNew);
%%         {stat, Pid, ValList} when CurrentTest == NTest ->
%%             io:format("Statistician ~w got stat from ~w:~w~n", [self(), Pid, ValList]),
%%             StatVarListNew = Updater(StatVarList, ValList),
%%             ReportTo ! {finish, self(), timer:now_diff(now(), T0), NTest, NError, StatVarListNew},
%%             done_statistician_loop;
%%         {error, Pid, Reason} when CurrentTest < NTest ->
%%             io:format("Statistician ~w got error from ~w:~w~n", [self(), Pid, Reason]),
%%             statistician_loop(ReportTo, T0, NTest, CurrentTest+1, NError+1, StatVarList);
%%         {error, Pid, Reason} when CurrentTest == NTest ->
%%             io:format("Statistician ~w got error from ~w:~w~n", [self(), Pid, Reason]),
%%             ReportTo ! {finish, self(), timer:now_diff(now(), T0), NTest, NError+1, StatVarList},
%%             done_statistician_loop
%%     end.
%% 
%% statistician_var_create({mean, VarName}) ->
%%     statvar_mean_create(VarName);
%% statistician_var_create({count, VarName}) ->
%%     statvar_count_create(VarName).
%% 
%% statistician_var_update({VarName, Cur, Updater, Displayer}, Val) ->
%%     {VarName, Updater(Cur, Val), Updater, Displayer}.
%% 
%% 
%% statvar_mean_create(Name) ->
%%     {Name, [], fun statvar_mean_update/2, fun statvar_mean_display/1}.
%%     
%% statvar_mean_update(Current, Val) ->
%%     Current ++ [float(Val)].
%%     
%% statvar_mean_display({VarName, ValLst, _Updater, _Displayer})->
%%     % TODO crash if ValLst is empty
%%     ValLstLen = length(ValLst),
%%     Sum = lists:foldl(fun(X, Acc) -> X + Acc end, 0.0, ValLst),
%%     Avg = Sum / ValLstLen,
%%     Dev = math:sqrt(lists:foldl(fun(X, Acc) -> ((X-Avg)*(X-Avg)) + Acc end, 0, ValLst) / ValLstLen),
%%     Sorted = lists:sort(ValLst),
%%     Med = lists:nth(round(length(Sorted)/2), Sorted),
%%     Min = lists:nth(1, Sorted),
%%     Max = lists:last(Sorted),
%%     io:format(
%%         "~10s sum=~10.2f avg=~10.2f dev=~10.2f min=~10.2f max=~10.2f med=~10.2f~n",
%%         [atom_to_list(VarName)] ++ lists:map(fun float/1, [Sum, Avg, Dev, Min, Max, Med])
%%     ).
%% 
%% statvar_count_create(Name) ->
%%     {Name, dict:new(), fun statvar_count_update/2, fun statvar_count_display/1}.
%%     
%% statvar_count_update(Current, Val) ->
%%     dict:update(Val, fun(Count) -> Count + 1 end, 1, Current).
%%     
%% statvar_count_display({VarName, ValLst, _Updater, _Displayer})->
%%     io:format(
%%         "~10s lst=~w~n",
%%         [VarName, dict:to_list(ValLst)]
%%     ).
%% 
%% 
%% % gathering of statistics of all statisticians
%% 
