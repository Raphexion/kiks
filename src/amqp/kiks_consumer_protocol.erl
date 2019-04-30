-module(kiks_consumer_protocol).

-callback process(atom(), pid(), binary(), binary()) -> 'ok' | 'error'.
