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

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

add_listener(Pid, Listener) ->
    gen_server:cast(Pid, {add_listener, Listener}).

init(Opts = #{channel := Channel,
	      queue := Queue,
	      exchange := Exchange,
	      routing_key := RoutingKey}) ->
    amqp_common:ensure_exchange(Opts),
    amqp_common:ensure_queue(Opts),

    Binding = #'queue.bind'{queue       = Queue,
			    exchange    = Exchange,
			    routing_key = RoutingKey},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),

    Sub = #'basic.consume'{queue = Queue},
    #'basic.consume_ok'{consumer_tag = _Tag} =
	amqp_channel:subscribe(Channel, Sub, self()),

    {ok, []}.

handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

handle_cast({add_listener, Listener}, Listeners) ->
    Listener ! {ok, added, Listener, self()},
    {noreply, [Listener|Listeners]};

handle_cast(What, State) ->
    io:fwrite("CAST: ~p~n", [What]),
    {noreply, State}.

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info({#'basic.deliver'{delivery_tag = _Tag}, #amqp_msg{payload = Payload}}, Listeners) ->
    notify(Payload, Listeners),
    {noreply, Listeners};

handle_info(What, State) ->
    io:fwrite("INFO ~p~n", [What]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%
%%%

notify(_Payload, []) ->
    ok;

notify(Payload, [Listener|Listeners]) ->
    Listener ! Payload,
    notify(Payload, Listeners).
