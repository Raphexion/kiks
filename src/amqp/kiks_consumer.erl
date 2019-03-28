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
    gen_server:cast(Pid, {add_listener, Pid}).

init(Opts = #{channel := Channel,
	      queue := Queue,
	      exchange := Exchange}) ->
    ensure_exchange(Opts),
    ensure_queue(Opts),

    Sub = #'basic.consume'{queue = Queue},
    #'basic.consume_ok'{consumer_tag = Tag} =
	amqp_channel:subscribe(Channel, Sub, self()),

    io:fwrite("TAG: ~p~n", [Tag]),
    {ok, []}.

handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

handle_cast({add_listener, Listener}, State) ->
    Listener ! {ok, added, self()},
    {noreply, State};

handle_cast(_What, State) ->
    {noreply, State}.


%% loop(Channel) ->
%%     receive
%%         %% This is the first message received
%%         #'basic.consume_ok'{} ->
%%             loop(Channel);
%%
%%         %% This is received when the subscription is cancelled
%%         #'basic.cancel_ok'{} ->
%%             ok;
%%
%%         %% A delivery
%%         {#'basic.deliver'{delivery_tag = Tag}, Content} ->
%%             %% Do something with the message payload
%%             %% (some work here)
%%
%%             %% Ack the message
%%             amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
%%
%%             %% Loop
%%             loop(Channel)
%%     end.
%%

handle_info(What, State) ->
    io:fwrite("INFO ~p~n", [What]),
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


ensure_exchange(#{channel := Channel, exchange := Exchange}) ->
    Declare = #'exchange.declare'{exchange = Exchange},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare).

ensure_queue(#{channel := Channel, queue := Queue}) ->
    Declare = #'queue.declare'{queue = Queue},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare).
