-module(kiks_consumer_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).
-import(kiks_support, [b/1]).

-export([start_link/0,
	 add_child/5,
	 add_child/6]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add_child(Exchange, Queue, RoutingKey, Mod, Pid) ->
    add_child(no_tag, Exchange, Queue, RoutingKey, Mod, Pid).

add_child(Tag, Exchange, Queue, RoutingKey, Mod, Pid) ->
    Info = #{tag => Tag,
	     exchange => b(Exchange),
	     queue => b(Queue),
	     routing_key => b(RoutingKey),
	     mod => Mod,
	     pid => Pid},

    supervisor:start_child(?MODULE, [Info]).

init(_) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},

    ChildSpecs = [#{id => kiks_consumer,
                    start => {kiks_consumer, start_link, []},
                    shutdown => brutal_kill}],

    {ok, {SupFlags, ChildSpecs}}.
