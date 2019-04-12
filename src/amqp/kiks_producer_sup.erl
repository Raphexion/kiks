-module(kiks_producer_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([init/1,
	 add_child/1]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add_child(Exchange) ->
    Opts = #{exchange => support:b(Exchange)},
    supervisor:start_child(?MODULE, [Opts]).

init(_) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},

    ChildSpecs = [#{id => kiks_producer,
                    start => {kiks_producer, start_link, []},
                    shutdown => brutal_kill}],

    {ok, {SupFlags, ChildSpecs}}.
