%%%-------------------------------------------------------------------
%% @doc kiks top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(kiks_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
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

%%====================================================================
%% Internal functions
%%====================================================================
