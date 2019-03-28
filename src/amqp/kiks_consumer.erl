%% @doc Defines a consumer over amqp
%% @end
-module(kiks_consumer).
-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

%% API
-export([start_link/1]).

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

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

%% @hidden
init(Info) ->
    #{exchange := Exchange,
      queue := Queue,
      routing_key := RoutingKey,
      mod := Mod,
      pid := Pid} = Info,

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

    {ok, Info#{channel => Channel}}.

%% @hidden
handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

%% @hidden
handle_cast(_What, State) ->
    {noreply, State}.

%% @hidden
handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};
handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Payload}}, State) ->
    #{mod := Mod, pid := Pid} = State,
    case Mod:process(Pid, Payload) of
	ok ->
	    ack(Tag, State);
	_ ->
	    nack(Tag, State)
    end,
    {noreply, State};

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

ack(Tag, #{channel := Channel}) ->
    ack(Tag, Channel);

ack(Tag, Channel) ->
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}).

nack(Tag, #{channel := Channel}) ->
    nack(Tag, Channel);

nack(Tag, Channel) ->
    amqp_channel:cast(Channel, #'basic.nack'{delivery_tag = Tag}).
