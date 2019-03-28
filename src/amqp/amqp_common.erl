-module(amqp_common).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([ensure_exchange/2,
	 ensure_queue/2]).

ensure_exchange(Channel, Exchange) ->
    Declare = #'exchange.declare'{exchange = Exchange},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare).

ensure_queue(Channel, Queue) ->
    Declare = #'queue.declare'{queue = Queue},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare).
