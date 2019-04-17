# kiks

[![Build Status](https://travis-ci.org/Raphexion/kiks.svg?branch=master)](https://travis-ci.org/Raphexion/kiks)

Small help library in Erlang / RabbitMQ

## Prerequisites

```
docker run -d --rm -p 5672:5672 -p 15672:15672 rabbitmq:3.7-management
```

## Gettings started

```
application:ensure_all_started(kiks).
```

## Example usage

```
application:ensure_all_started(kiks).

{ok, Pid1} = minimal_consumer:start_link("pid1", 0.5).
{ok, Pid2} = minimal_consumer:start_link("pid2", 0.5).

{ok, C1} = kiks_consumer_sup:add_child("e", "q", <<"a.b.*">>, minimal_consumer, Pid1).
{ok, C2} = kiks_consumer_sup:add_child("e", "q", <<"a.b.*">>, minimal_consumer, Pid2).

{ok, Q1} = kiks_queue:start_link("e", "", "#").
{ok, Q2} = kiks_queue:start_link("e", "", "#").

{ok, P1} = kiks_producer_sup:add_child("e").

[kiks_producer:send(P1, <<X>>, <<"a.b.c">>) || X <- lists:seq(1, 100)].
```
