%%% @author Ludovic Coquelle <lcoquelle@gmail.com>
%%% @copyright 2007 Ludovic Coquelle
%%% @doc Application API.
%%%
%%% For license information see LICENSE.txt
%%% @end
-module(grinderl).


% Declaration: OTP relative
%empty


% Declaration: API
-export([
    start/0,
    stop/0,
    stop/1,
    add_node/1,
    add_nodes/1,
    run_test/1
]).

% Declaration: Internal
-include("include/grinderl.hrl").


% ~~ Implementation: API

%%% @doc  Start localy the OTP application (run main application).
%%% @see  application:start/2
%%% @spec () -> Result
%%% @end
start() ->
    application:start(grinderl).

%%% @doc  Stop the local OTP application.
%%% @spec () -> ok
%%% @end
stop() ->
    application:stop(grinderl).

%%% @doc  Stop the OTP application AND shutdown remote node AND local node!
%%% @spec ([Node]) -> ok
%%% @end
stop([Node]) ->
    case locate_node(Node) of
        {ok, FullNode} ->
            % stop the application,
            ?DEBUG("Stopping remote application", []),
            rpc:call(FullNode, application, stop, [grinderl]),
            ?DEBUG("Shutdown remote node", []),
            rpc:call(FullNode, init, stop, []),
            ?INFO("Stopped.", []);
        {error, Reason} ->
            ?ERROR("Can't locate application/node: ~s.", [Reason])
    end,
    init:stop().

%%% @doc  Add a node in grinderl.
%%% @see  grd_stress_srv:add_node/1
%%% @spec (Node::node()) -> Result
%%% @end
add_node(Node) ->
    grd_stress_srv:add_node(Node).

%%% @doc  Add a list of nodes to grinderl.
%%% @spec ([node()]) -> [Result]
%%% @end
add_nodes(Nodes) when is_list(Nodes) ->
    lists:map(fun grd_stress_srv:add_node/1, Nodes).

%%% @doc  Start a task distribution on grinderl's nodes..
%%% @see  grd_stress_srv:distribute_task/1
%%% @spec (Task::task()) -> Result
%%% @end
run_test(Test) ->
    grd_stress_srv:distribute_test(Test).

% ~~ Implementation: Behaviour callbacks
%empty


% ~~ Implementation: Internal

locate_node(MaybeLocalNode) ->
    case net_adm:ping(MaybeLocalNode) of
        pong ->
            {ok, MaybeLocalNode};
        _ ->
            LNodeStr = atom_to_list(MaybeLocalNode) ++ "@localhost",
            LNodeAtom = list_to_atom(LNodeStr),
            case net_adm:ping(LNodeAtom) of
                pong ->
                    {ok, LNodeAtom};
                _ ->
                    {
                        error,
                        "Can't find node " ++ LNodeStr ++
                            " on host " ++ net_adm:localhost()
                    }
            end
    end.

