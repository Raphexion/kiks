-module(kiks_amqp_sup).
-behaviour(supervisor).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},

    Connections = #{id => kiks_connections,
                    start => {kiks_connections, start_link, []},
                    shutdown => brutal_kill,
		    type => worker},

    ConsumerSup = #{id => kiks_consumer_sup,
                    start => {kiks_consumer_sup, start_link, []},
                    shutdown => brutal_kill,
		    type => supervisor},

    ProducerSup = #{id => kiks_producer_sup,
		    start => {kiks_producer_sup, start_link, []},
		    shutdown => brutal_kill,
		    type => supervisor},

    ChildSpecs = [Connections,
		  ConsumerSup,
		  ProducerSup],

    {ok, {SupFlags, ChildSpecs}}.
