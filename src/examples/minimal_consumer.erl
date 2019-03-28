-module(minimal_consumer).
-behaviour(kiks_consumer_protocol).
-behaviour(gen_server).

%% API
-export([start_link/0,
	 process/2]).

%% Behaviour callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

process(Pid, Payload) ->
    gen_server:call(Pid, {process, Payload}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

%% @hidden
init(_) ->
    {ok, #{}}.

%% @hidden
handle_call({process, Payload}, _From, State) ->
    Res = priv_process(Payload, rand:uniform()),
    {reply, Res, State};
handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

%% @hidden
handle_cast(_What, State) ->
    {noreply, State}.

%% @hidden
handle_info(_What, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

priv_process(Payload, F) when F < 0.5 ->
    io:fwrite("Accepting [~p] ~p~n", [F, Payload]),
    ok;
priv_process(Payload, F) ->
    io:fwrite("Rejecting [~p] ~p~n", [F, Payload]),
    error.
