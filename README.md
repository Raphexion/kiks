# kiks
Small help library in Erlang / RabbitMQ

## Prerequisites

```
docker run -d --rm -p 5672:5672 -p 15672:15672 rabbitmq:3.7-management
```

## Gettings started

```
application:ensure_all_started(kiks).
```