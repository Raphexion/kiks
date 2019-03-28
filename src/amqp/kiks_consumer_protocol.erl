-module(kiks_consumer_protocol).

-callback process(pid(), binary()) -> 'ok' | 'error'.
