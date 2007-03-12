%%% @author Ludovic Coquelle <lcoquelle@gmail.com>
%%% @copyright 2007 Ludovic Coquelle
%%% @doc Random Server: auto-seed random generation and serve new seeds.
%%%
%%% This process initilized its own random generator based on datetime.
%%% Then it act as a server to distribute seed for other processes who
%%% request it.
%%%
%%% For license information see LICENSE.txt
%%% @end
-module(grd_random_srv).


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
    get_seed/0
]).


% ~~ Declaration: Internal
-define(GRD_REG_RND, ?MODULE).
-define(GRD_RND_RANGESIZE, 100000).


% ~~ Implementation: API
%%% @doc  Start the server globally registered.
%%% @see  supervisor:start_link/3
%%% @spec () -> Result
%%% @end
start_link() ->
    gen_server:start_link({global, ?GRD_REG_RND}, ?MODULE, [], []).


%%% @doc  Get a seed to init a random number generator.
%%% @spec () -> {random_seed, A1::integer(), A2::integer(), A3::integer()}
%%% @end
get_seed() ->
    gen_server:call({global, ?GRD_REG_RND}, get_seed).

% ~~ Implementation: Behaviour callbacks

%%% @doc  Initialize the server state.
%%% @see  gen_server:init/1
%%% @spec (InitArgs) -> Result
%%% @end
init(_InitArgs) ->
    % init random generator
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    % no explicit state
    {ok, nostate}.

%%% @doc  Return a random seed.
%%% If request is  'get_seed' return a 4-uplet (random_seed, A1, A2, A3}.
%%% OTP meaning: handle any request sent through {@link gen_server:call/2} or {@link gen_server:multi_call/2}.
%%% @see  gen_server:handle_call/3
%%% @spec (Request, From, State) -> {reply|noreply|stop, Params}
%%% @end
handle_call(get_seed, _From, State) ->
    Result = {
        random_seed,
        random:uniform(?GRD_RND_RANGESIZE),
        random:uniform(?GRD_RND_RANGESIZE),
        random:uniform(?GRD_RND_RANGESIZE)
    },
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, noresult, State}.

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
%%% OTP meaning: @doc  Called when is about to terminate.
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
%empty
