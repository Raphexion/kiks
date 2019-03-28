-module(kiks_producer).

-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([start_link/1,
	 send/2]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

send(Pid, Payload) ->
    gen_server:cast(Pid, {send, Payload}).

init(Opts) ->
    amqp_common:ensure_exchange(Opts),
    {ok, Opts}.

handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

handle_cast({send, Payload}, State) ->
    Channel = maps:get(channel, State),
    Exchange = maps:get(exchange, State),
    RoutingKey = maps:get(routing_key, State),

    Publish = #'basic.publish'{exchange = Exchange, routing_key = RoutingKey},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),

    {noreply, State};

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(What, State) ->
    io:fwrite("INFO ~p~n", [What]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:fwrite("Terminaate ~p~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
