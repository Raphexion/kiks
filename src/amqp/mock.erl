-module(mock).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([setup_channel/0]).

setup_channel() ->
    application:ensure_all_started(amqp_client),

    Username = list_to_binary(os:getenv("RABBITMQ_USERNAME")),
    Password = list_to_binary(os:getenv("RABBITMQ_PASSWORD")),

    io:fwrite("Username ~p~n", [Username]),
    io:fwrite("Password ~p~n", [Password]),

    {ok, Connection} =
	amqp_connection:start(#amqp_params_network{
				 username = Username,
				 password = Password,
				 host = "mq.couch-red.com",
				 port = 5671,
				 ssl_options = [{verify, verify_peer},
						{cacertfile, "cacert.pem"}]
				}),

    {ok, Channel} = amqp_connection:open_channel(Connection),
    Channel.
