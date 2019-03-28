-module(kiks_consumer).

-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([start_link/1,
	 add_listener/2]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(consumer, {channel, listeners = []}).

start_link(Info) ->
    gen_server:start_link(?MODULE, Info, []).

add_listener(Pid, Listener) ->
    gen_server:cast(Pid, {add_listener, Listener}).

init(#{exchange := Exchange, queue := Queue, routing_key := RoutingKey}) ->
    {ok, Channel} = kiks_amqp_connections:get(),

    amqp_common:ensure_exchange(Channel, Exchange),
    amqp_common:ensure_queue(Channel, Queue),

    Binding = #'queue.bind'{queue       = Queue,
			    exchange    = Exchange,
			    routing_key = RoutingKey},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),

    Sub = #'basic.consume'{queue = Queue},
    #'basic.consume_ok'{consumer_tag = _Tag} =
	amqp_channel:subscribe(Channel, Sub, self()),

    {ok, #consumer{channel=Channel}}.

handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

handle_cast({add_listener, Listener}, S=#consumer{listeners=Listeners}) ->
    {noreply, S#consumer{listeners=[Listener|Listeners]}};

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Payload}}, State) ->
    notify(Payload, State),
    ack(Tag, State),
    {noreply, State};

handle_info(_What, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%
%%%

notify(Payload, #consumer{listeners=Listeners}) ->
    notify(Payload, Listeners);

notify(_Payload, []) ->
    ok;

notify(Payload, [Listener|Listeners]) ->
    Listener ! Payload,
    notify(Payload, Listeners).

ack(Tag, #consumer{channel=Channel}) ->
    ack(Tag, Channel);

ack(Tag, Channel) ->
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}).
