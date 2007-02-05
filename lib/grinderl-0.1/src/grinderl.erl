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
    add_node/1,
    add_nodes/1,
    run_task/1
]).

% Declaration: Internal
%empty


% ~~ Implementation: API

%%% @doc  Start the application.
%%% @see  application:start/2
%%% @spec () -> Result
%%% @end
start() ->
    application:start(grinderl).

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
run_task(Task) ->
    grd_stress_srv:distribute_task(Task).

% ~~ Implementation: Behaviour callbacks
%empty


% ~~ Implementation: Internal
%empty

