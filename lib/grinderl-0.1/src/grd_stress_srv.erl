%%% @author Ludovic Coquelle <lcoquelle@gmail.com>
%%% @copyright 2007 Ludovic Coquelle
%%% @doc Main stress server.
%%%
%%% API to the main function of stress server: manage nodes and requests
%%% for scenario to run on those nodes.
%%%
%%% For license information see LICENSE.txt
%%% @end
-module(grd_stress_srv).


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
    start_link/0,
    add_node/1,
    distribute_test/1
]).


% ~~ Declaration: Internal
-include("include/grinderl.hrl").
-define(GRD_REG_STRESSSRV, ?MODULE).
-record(state, {
    node_count = 0,
    node_list = []
}).
-export([
    worker_manager_loop/2 % internal spawn
]).


% ~~ Implementation: API

%%% @doc  Start a supervised server locally registered.
%%% @spec () -> Result
%%% @end
start_link() ->
    gen_server:start_link(
        {local, ?GRD_REG_STRESSSRV},
        ?MODULE,
        [],
        []
    ).

%%% @doc  Add one node to run some scenario.
%%% @spec (Node::atom()) -> ok | {error, Reason}
%%% @end
add_node(Node) when is_atom(Node) ->
    gen_server:call(?GRD_REG_STRESSSRV, {add_node, Node}).

%%% @doc  Run a test on multiple nodes if needed.
%%% @spec (Test) -> ok
%%% @end
distribute_test(Test) ->
    gen_server:call(?GRD_REG_STRESSSRV, {run_test, Test}).


% ~~ Implementation: Behaviour callbacks

%%% @doc  Do nothing.
%%% OTP meaning: initialize the server state.
%%% @see  gen_server:init/1
%%% @spec (InitArgs) -> Result
%%% @end
init(_InitArgs) ->
    State = #state{},
    {ok, State}.

%%% @doc  Handle request {add_node, Node} and {run_test, Test}; else nothing.
%%% OTP meaning: handle any request sent through {@link gen_server:call/2} or {@link gen_server:multi_call/2}.
%%% @see  gen_server:handle_call/3
%%% @spec (Request, From, State) -> {reply|noreply|stop, Params}
%%% @end
handle_call({add_node, Node}, _From, State) ->
    handle_add_node(State, Node);
handle_call({run_test, Test}, _From, State) ->
    NodeList = State#state.node_list,
    NodeCount = State#state.node_count,
    ?DEBUG("begin test distribution on nodes:~w", [NodeList]),
    % create a process to handle all results
    % ... TODO Gatherer should maybe supervise all the workers
    % ... TODO all this gathering stuff could be done through Mnesia?
    Gatherer = create_manager(NodeCount),
    % create one worker per node
    Workers = lists:map(fun create_worker/1, NodeList),
    ?DEBUG("workers=~w", [Workers]),
    % distribute the same test to all workers
    lists:foreach(
        fun(Worker) -> grd_worker_srv:run_test(Worker, Test, Gatherer) end,
        Workers
    ),
    ?DEBUG("end test distribution", []),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%% @doc  Do nothing.
%%% OTP meaning: handle any request sent through {@link gen_server:cast/2} or {@link gen_server:multi_cast/2}.
%%% @see  gen_server:handle_cast/3
%%% @spec (Request, State) -> {noreply|stop, Params}
%%% @end
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
    ok.


%%% @doc  Do nothing.
%%% OTP meaning: manage release handling.
%%% @see  gen_server:code_change/3
%%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%%% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% ~~ Implementation: Internal

%%% @doc  Handle request to add an Erlang node to the current state.
%%% @spec (State::state(), Node::node()) -> NewState::state()
%%% @end
handle_add_node(State, Node) ->
    case net_adm:ping(Node) of
        pong ->
            {Reply, NewState} = add_node(State, Node),
            {reply, Reply, NewState};
        pang ->
            {reply, {error, "Not able to ping this node"}, State}
    end.

%%% @doc  Add an Erlang node to the current state if not already know.
%%% @spec (State::state(), Node::node()) -> {ok|already, NewState::state()}
%%% @end
add_node(State, Node) ->
    CurNodeCount = State#state.node_count,
    CurNodeList  = State#state.node_list,
    %case lists:member(Node, CurNodeList) of
    %    true ->
    %        {already, State};
    %    _ ->
            {ok, State#state{
                node_count = CurNodeCount + 1,
                node_list  = [Node] ++ CurNodeList
            }}
    %end.
    .

%%% @doc  Make a RPC call on node to run a worker's server.
%%% @spec (Node::node()) -> Pid::pid()
%%% @end
create_worker(Node) ->
    {ok, PID} = rpc:call( % TODO check error ?
        Node,
        grd_worker_srv, start, []
    ),
    PID.

create_manager(WorkerNumber) ->
    spawn_link(
        ?MODULE,
        worker_manager_loop,
        [now(), WorkerNumber]
    ).

worker_manager_loop(T0, 0) ->
    TStat = timer:now_diff(now(), T0),
    ?DEBUG(
        "Gatherer ~w: finish listening all workers after ~w ms",
        [self(), TStat]
    ),
    ok;
worker_manager_loop(T0, N) ->
    receive
        {finish, Pid, Duration, NTest, NError, VarList} ->
            ?DEBUG(
                "Gatherer ~w: finish listening worker ~w (took ~w):"
                ++ " NTest=~w, NError=~w =>",
                [self(), Pid, Duration, NTest, NError]
            ),
            lists:map(fun manager_display_result/1, VarList),
            % TODO good time to save raw data in file
            worker_manager_loop(T0, N - 1);
        stop ->
            worker_manager_loop(T0, 0)
    end.

manager_display_result({acc, Name, ValLst})->
    ?DEBUG("~s ~w", [atom_to_list(Name), ValLst]);
manager_display_result({mean, Name, ValLst})->
    % TODO this will crash if ValLst is empty or non numeric
    ValLstLen = length(ValLst),
    Sum = lists:foldl(fun(X, Acc) -> X + Acc end, 0.0, ValLst),
    Avg = Sum / ValLstLen,
    Dev = math:sqrt(lists:foldl(fun(X, Acc) -> ((X-Avg)*(X-Avg)) + Acc end, 0, ValLst) / ValLstLen),
    Sorted = lists:sort(ValLst),
    Med = lists:nth(round(length(Sorted)/2), Sorted),
    Min = lists:nth(1, Sorted),
    Max = lists:last(Sorted),
    ?DEBUG(
        "~10s sum=~10.2f avg=~10.2f dev=~10.2f min=~10.2f max=~10.2f med=~10.2f",
        [atom_to_list(Name)] ++ lists:map(
            fun float/1,
            [Sum, Avg, Dev, Min, Max, Med]
        )
    );
manager_display_result({count, Name, ValLst})->
    ?DEBUG(
        "~10s lst=~w",
        [Name, dict:to_list(ValLst)]
    ).
