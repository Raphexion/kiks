-module(kiks_queue).
-behaviour(kiks_consumer_protocol).
-behaviour(gen_server).

%% API
-export([start_link/3,
	 process/3,
	 empty/1,
	 pop/1]).

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

start_link(Exchange, Queue, RoutingKey) ->
    gen_server:start_link(?MODULE, [Exchange, Queue, RoutingKey], []).

process(Queue, Payload, _RoutingKey) ->
    gen_server:call(Queue, {process, Payload}).

empty(Queue) ->
    gen_server:call(Queue, empty).

pop(Queue) ->
    gen_server:call(Queue, pop).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

%% @hidden
init([Exchange, Queue, RoutingKey]) ->
    {ok, _} = kiks_consumer_sup:add_child(Exchange, Queue, RoutingKey, ?MODULE, self()),
    {ok, queue:new()}.

%% @hidden
handle_call({process, Payload}, _From, Queue) ->
    {reply, ok, queue:in(Payload, Queue)};

handle_call(empty, _From, Queue) ->
    case queue:peek(Queue) of
	empty ->
	    {reply, true, Queue};
	{value, _Item} ->
	    {reply, false, Queue}
    end;

handle_call(pop, _From, Queue) ->
    case queue:peek(Queue) of
	empty ->
	    {reply, empty, Queue};
	{value, Item} ->
	    {reply, Item, queue:drop(Queue)}
    end;

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
