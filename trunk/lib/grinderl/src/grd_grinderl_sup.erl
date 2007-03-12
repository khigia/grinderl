%%% @author Ludovic Coquelle <lcoquelle@gmail.com>
%%% @copyright 2007 Ludovic Coquelle
%%% @doc Main grinderl supervisor.
%%%
%%% Maintains general state of the application.
%%%
%%% For license information see LICENSE.txt
%%% @end
-module(grd_grinderl_sup).


% ~~ Declaration: OTP relative
-behaviour(supervisor).
-export([
    init/1
]).


% ~~ Declaration: API
-export([
    start_link/0
]).


% ~~ Declaration: Internal
-define(GRD_REG_GRINDERL, ?MODULE).


% ~~ Implementation: API

%%% @doc  Start the supervisor in linked mode.
%%% @see  supervisor:start_link/3
%%% @spec () -> Result
%%% @end
start_link() ->
    supervisor:start_link({local, ?GRD_REG_GRINDERL}, ?MODULE, []).


% ~~ Implementation: Behaviour callbacks

%%% @doc  Initialize this supervisor.
%%% Three children are supervised: one to distribute random seed, one to gather results, and a last one to launch stress test requests.
%%% @see  supervisor:init/1
%%% @spec ([term()]) -> {ok, Policy}
init(_Args) ->
    % one child as random seed server
    RandomSrv = {
        grd_random_srv,
        {grd_random_srv, start_link, []},
        permanent,
        5000,
        worker,
        [grd_random_srv]
    },
    % one child as stress server
    StressSrv = {
        grd_stress_srv,
        {grd_stress_srv, start_link, []},
        permanent,
        5000,
        worker,
        [grd_stress_srv]
    },
    % supervisor policy
    {ok, {
        {one_for_all, 2, 5},
        [RandomSrv, StressSrv]
    }}.


% ~~ Implementation: Internal
%empty
