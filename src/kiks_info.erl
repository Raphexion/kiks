-module(kiks_info).

-export([build_info/3]).

build_info(Exchange, Queue, RoutingKey) ->
    #{
       exchange => ensure_binary(Exchange),
       queue => ensure_binary(Queue),
       routing_key => ensure_binary(RoutingKey)
     }.

ensure_binary(Name) when is_binary(Name) ->
    Name;
ensure_binary(Name) when is_list(Name) ->
    list_to_binary(Name);
ensure_binary(_) ->
    error.
