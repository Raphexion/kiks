-module(kiks_amqp_connections).
-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

-import(kiks_support, [get_config_as_string/2,
		       get_config_as_binary/2]).

-define(SERVER, ?MODULE).
-define(RESTART_TIMEOUT, 5000).

%% API
-export([start_link/0,
	 get/0,
	 with_channel/1]).

%% Behaviour callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {connection, with_channel, channels=[]}).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get() ->
    gen_server:call(?SERVER, get).

with_channel(Fun) ->
    gen_server:call(?SERVER, {with_channel, Fun}).

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

%% @hidden
init(_) ->
    {ok, #state{}, 0}.

%% @hidden
handle_call(get, _From, S=#state{connection=Connection, channels=Channels}) ->
    {ok, Channel} = amqp_connection:open_channel(Connection),
    {reply, {ok, Channel}, S#state{channels=[Channel|Channels]}};

handle_call({with_channel, Fun}, _From, S=#state{with_channel=Channel}) ->
    Reply = try Fun(Channel) of
		Result ->
		    {ok, Result}
	    catch
		A:B ->
		    {error, A, B}
	    end,
    {reply, Reply, S}.

handle_cast(_What, State) ->
    {noreply, State}.

%% @hidden
handle_info(timeout, _State) ->
    lager:debug("attempt to connect to broker"),

    %% note - must be binary
    Username = get_config_as_binary(rabbitmq_username, "guest"),
    Password = get_config_as_binary(rabbitmq_password, "guest"),

    %% note - must be string
    Hostname = get_config_as_string(rabbitmq_hostname, "localhost"),
    Port = get_config_as_string(rabbitmq_port, "5672"),

    PrivDir = code:priv_dir(kiks),

    init_response(amqp_connection:start(#amqp_params_network{
					   username = Username,
					   password = Password,
					   host = Hostname,
					   port = list_to_integer(Port)
					  }));

handle_info(_What, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, #state{connection=Connection, channels=Channels}) ->
    close_channels(Channels),
    close_connection(Connection),
    ok.

%% @hidden
code_change(_, _, State) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%% Behaviour callbacks
%%------------------------------------------------------------------------------

init_response({ok, Connection}) ->
    lager:debug("connection established"),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    {noreply, #state{with_channel=Channel,
		     connection=Connection}};

init_response(Error) ->
    lager:warning("connection attempt failed"),
    {noreply, Error, ?RESTART_TIMEOUT}.

close_channels([]) ->
    ok;
close_channels([Channel|Channels]) ->
    amqp_channel:close(Channel),
    close_channels(Channels).

close_connection(Connection) ->
    amqp_connection:close(Connection).
