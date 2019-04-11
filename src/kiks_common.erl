-module(kiks_common).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([ensure_exchange/2,
	 ensure_queue/2]).

ensure_exchange(Channel, Exchange) ->
    Declare = #'exchange.declare'{exchange = support:b(Exchange),
				  type = <<"topic">>,
				  durable = true},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare).

ensure_queue(Channel, Queue) ->
    Declare = #'queue.declare'{queue = support:b(Queue), durable = true},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare).
