%% @doc Defines a producer over amqp
%% @end

-module(kiks_producer).
-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").
-import(kiks_support, [b/1]).

%% API
-export([start_link/1,
	 send/3]).

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

send(Pid, Payload, Topic) ->
    gen_server:cast(Pid, {send, b(Payload), b(Topic)}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

%% @hidden
init(Info=#{exchange := Exchange}) ->
    {ok, Channel} = kiks_connections:get(),
    kiks_common:ensure_exchange(Channel, Exchange),

    {ok, Info#{channel => Channel}}.

%% @hidden
handle_call(What, _From, State) ->
    lager:warning("~p doesn't implement ~p", [?MODULE, What]),
    {reply, {ok, What}, State}.

%% @hidden
handle_cast({send, Payload, Topic}, State) ->
    #{channel := Channel,
      exchange := Exchange} = State,

    Publish = #'basic.publish'{exchange = Exchange, routing_key = Topic},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),

    {noreply, State};
handle_cast(What, State) ->
    lager:warning("~p doesn't implement ~p", [?MODULE, What]),
    {noreply, State}.

%% @hidden
handle_info(What, State) ->
    lager:warning("~p doesn't implement ~p", [?MODULE, What]),
    {noreply, State}.

%% @hidden
terminate(Reason, _State) ->
    lager:debug("~p terminated with ~p", [?MODULE, Reason]),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
