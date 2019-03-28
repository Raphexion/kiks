-module(driver).

-export([main/0]).

main() ->
    Channel = mock:setup_channel(),
    Info = #{channel => Channel,
	     exchange => <<"x">>,
	     queue => <<"q">>,
	     routing_key => <<"black">>},

    {ok, C} = kiks_consumer:start_link(Info),
    kiks_consumer:add_listener(C, self()),

    {ok, P} = kiks_producer:start_link(Info),
    kiks_producer:send(P, <<"Hello World">>).
