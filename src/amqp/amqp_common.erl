-module(amqp_common).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([ensure_exchange/1,
	 ensure_queue/1]).

ensure_exchange(#{channel := Channel, exchange := Exchange}) ->
    Declare = #'exchange.declare'{exchange = Exchange},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare).

ensure_queue(#{channel := Channel, queue := Queue}) ->
    Declare = #'queue.declare'{queue = Queue},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare).
