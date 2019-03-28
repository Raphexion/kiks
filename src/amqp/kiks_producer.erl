%% @doc Defines a producer over amqp
%% @end

-module(kiks_producer).
-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

%% API
-export([start_link/1,
	 send/2]).

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

start_link(Info) ->
    gen_server:start_link(?MODULE, Info, []).

send(Pid, Payload) ->
    gen_server:cast(Pid, {send, Payload}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

%% @hidden
init(Info) ->
    {ok, Channel} = kiks_amqp_connections:get(),

    Exchange = maps:get(exchange, Info),
    amqp_common:ensure_exchange(Channel, Exchange),

    {ok, Info#{channel => Channel}}.

%% @hidden
handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

%% @hidden
handle_cast({send, Payload}, State) ->
    Channel = maps:get(channel, State),
    Exchange = maps:get(exchange, State),
    RoutingKey = maps:get(routing_key, State),

    Publish = #'basic.publish'{exchange = Exchange, routing_key = RoutingKey},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),

    {noreply, State};
handle_cast(_What, State) ->
    {noreply, State}.

%% @hidden
handle_info(What, State) ->
    io:fwrite("INFO ~p~n", [What]),
    {noreply, State}.

%% @hidden
terminate(Reason, _State) ->
    io:fwrite("Terminaate ~p~n", [Reason]),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
