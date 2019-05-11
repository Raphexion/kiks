-module(kiks_common).

-include_lib("amqp_client/include/amqp_client.hrl").

-import(support, [b/1]).

-export([ensure_exchange/2,
	 ensure_queue/2]).
-export([delete_exchange/2,
	 delete_queue/2,
	 unbind_queue/4]).

ensure_exchange(Channel, Exchange) ->
    Declare = #'exchange.declare'{exchange = b(Exchange),
				  type = <<"topic">>,
				  durable = true},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare).

ensure_queue(Channel, Queue) ->
    Declare = #'queue.declare'{queue = b(Queue), durable = true},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare).

delete_exchange(Channel, Exchange) ->
    Delete = #'exchange.delete'{exchange = b(Exchange)},
    #'exchange.delete_ok'{} = amqp_channel:call(Channel, Delete).

delete_queue(Channel, Queue) ->
    Delete = #'queue.delete'{queue = b(Queue)},
    #'queue.delete_ok'{} = amqp_channel:call(Channel, Delete).

unbind_queue(Channel, Queue, Exchange, RoutingKey) ->
    Binding = #'queue.bind'{queue       = b(Queue),
			    exchange    = b(Exchange),
			    routing_key = b(RoutingKey)},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding).
