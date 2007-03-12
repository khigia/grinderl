%%% @author Ludovic Coquelle <lcoquelle@gmail.com>
%%% @copyright 2007 Ludovic Coquelle
%%% @doc Main grinderl application.
%%%
%%% Implement entry point of this OTP application.
%%%
%%% For license information see LICENSE.txt
%%% @end
-module(grd_grinderl_app).


% ~~ Declaration: OTP relative
-behaviour(application).
-export([
    start/2,
    stop/1
]).


% ~~ Declaration: API
%empty


% ~~ Declaration: Internal
%empty


% ~~ Implementation: API
%empty


% ~~ Implementation: Behaviour callbacks

%%% @doc  Start the main supervisor of the application.
%%% @see  application:start/2
%%% @see  grd_grinderl_sup:start_link/0
%%% @spec (StartType, StartArgs) -> Result
%%% @end
start(_StartType, _StartArgs) ->
    grd_grinderl_sup:start_link().

%%% @doc  Stop the main supervisor of the application.
%%% @see  application:stop/2
%%% @spec (State::term()) -> NewState::term()
%%% @end
stop(State) ->
    % close the main supervisor
    State.


% ~~ Implementation: Internal
%empty
