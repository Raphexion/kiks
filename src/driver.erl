-module(driver).

-export([main/0]).

main() ->
    application:ensure_all_started(amqp_client),

    %%  Channel = mock:setup_channel(),
    %% channel => Channel,

    Info = #{exchange => <<"x">>,
	     queue => <<"q">>,
	     routing_key => <<"black">>},

    kiks_amqp_sup:start_link(),

    {ok, C} = kiks_consumer_sup:add_child(Info),
    kiks_consumer:add_listener(C, self()),

    {ok, P} = kiks_producer_sup:add_child(Info),
    kiks_producer:send(P, <<"Hello World 2">>),

    ok.
