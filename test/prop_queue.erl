-module(prop_queue).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL({Content, Exchange, Topic}, test_data(),
	    begin
		RandomQueue = "",
		AllTopics = "#",
		Retries = 100,
		Timeout = 20,

		application:ensure_all_started(kiks),
		{ok, Q} = kiks_queue:start_link(Exchange, RandomQueue, AllTopics),
		{ok, P} = kiks_producer_sup:add_child(Exchange),
		kiks_producer:send(P, Content, Topic),

		Content =:= kiks_queue:pop(Q, Retries, Timeout)
	    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
content() ->
    binary().

lower() ->
    integer(98, 122).

upper() ->
    integer(65, 90).

valid_exchange_char() ->
    oneof([lower(), upper()]).

valid_topic_char() ->
    oneof([lower(), upper()]).

exchange() ->
    non_empty(list(valid_exchange_char())).

topic() ->
    non_empty(list(valid_topic_char())).

test_data() ->
    {content(), exchange(), topic()}.
