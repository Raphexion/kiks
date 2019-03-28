-module(kiks_amqp_connections).
-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0,
	 get/0]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {connection, channels=[]}).

%%% ---------------------------------------------------------------------
%%
%%% ---------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get() ->
    gen_server:call(?SERVER, get).

%%% ---------------------------------------------------------------------
%%
%%% ---------------------------------------------------------------------

get_config_as_string(Name) ->
    {ok, Value} = application:get_env(kiks, Name),
    %% logger:info(#{value => Value}),
    Value.

get_config_as_binary(Name) ->
    String = get_config_as_string(Name),
    %% logger:info(#{string => String}),
    list_to_binary(String).

init(_) ->
    application:ensure_all_started(amqp_client),

    %% note - must be binary
    Username = get_config_as_binary(rabbitmq_username),
    %% logger:info(#{username => Username}),
    Password = get_config_as_binary(rabbitmq_password),
    %% logger:info(#{password => Password}),

    %% note - must be string
    Hostname = get_config_as_string(rabbitmq_hostname),
    Port = get_config_as_string(rabbitmq_port),

    PrivDir = code:priv_dir(kiks),
    CaCert = filename:join(PrivDir, "cacert.pem"),

    init_response(amqp_connection:start(#amqp_params_network{
					   username = Username,
					   password = Password,
					   host = Hostname,
					   port = list_to_integer(Port),
					   ssl_options = [{verify, verify_peer},
							  {cacertfile, CaCert}]
					  })).

handle_call(get, _From, S=#state{connection=Connection, channels=Channels}) ->
    {ok, Channel} = amqp_connection:open_channel(Connection),
    {reply, {ok, Channel}, S#state{channels=[Channel|Channels]}}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(_What, State) ->
    {noreply, State}.

terminate(_Reason, #state{connection=Connection, channels=Channels}) ->
    close_channels(Channels),
    close_connection(Connection),
    ok.

code_change(_, _, State) ->
    {ok, State}.


%%% ---------------------------------------------------------------------
%%
%%% ---------------------------------------------------------------------

init_response({ok, Connection}) ->
    {ok, #state{connection=Connection}};

init_response(Error) ->
    {stop, Error}.

%%

close_channels([]) ->
    ok;

close_channels([Channel|Channels]) ->
    amqp_channel:close(Channel),
    close_channels(Channels).

%%

close_connection(Connection) ->
    amqp_connection:close(Connection).
