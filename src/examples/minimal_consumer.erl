-module(minimal_consumer).
-behaviour(kiks_consumer_protocol).
-behaviour(gen_server).

%% API
-export([start_link/2,
	 process/3]).

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

start_link(Name, Threshold) ->
    gen_server:start_link(?MODULE, [Name, Threshold], []).

process(Pid, Payload, RoutingKey) ->
    gen_server:call(Pid, {process, Payload, RoutingKey}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

%% @hidden
init([Name, Threshold]) ->
    {ok, #{name => Name, threshold => Threshold}}.

%% @hidden
handle_call({process, Payload, RoutingKey}, _From, State) ->
    #{name := Name, threshold := Threshold} = State,
    Res = priv_process(Name, Payload, RoutingKey, Threshold, rand:uniform()),
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

priv_process(Name, Payload, RoutingKey, Th, F) when F < Th ->
    io:fwrite("[~p] ~p accepting ~p ~p~n", [F, Name, Payload, RoutingKey]),
    ok;

priv_process(Name, Payload, RoutingKey, _Th, F) ->
    io:fwrite("[~p] ~p rejecting ~p ~p~n", [F, Name, Payload, RoutingKey]),
    error.
