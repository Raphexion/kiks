-module(kiks_consumer_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

-export([start_link/0,
	 add_child/4]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add_child(Exchange, Queue, RoutingKey, Listener) ->
    Info = #{exchange => support:b(Exchange),
	     queue => support:b(Queue),
	     routing_key => support:b(RoutingKey),
	     listener => Listener},

    supervisor:start_child(?MODULE, [Info]).

init(_) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},

    ChildSpecs = [#{id => kiks_consumer,
                    start => {kiks_consumer, start_link, []},
                    shutdown => brutal_kill}],

    {ok, {SupFlags, ChildSpecs}}.
