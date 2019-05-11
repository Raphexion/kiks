-module(support).

-export([b/1,
	 get_config_as_string/2,
	 get_config_as_binary/2]).

b(Variable) when is_binary(Variable) ->                                                                                                              Variable;                                                                                                                                    b(Variable) when is_list(Variable) ->                                                                                                                list_to_binary(Variable).

get_config_as_string(Name, Default) ->
    Env = atom_to_list(Name),
    get_config_as_string(Name, Default, os:getenv(Env)).

get_config_as_string(Name, Default, false) ->
    case application:get_env(kiks, Name) of
	{ok, Value} ->
	    Value;
	_ ->
	    Default
    end;
get_config_as_string(_Name, _Default, Value) ->
    Value.

get_config_as_binary(Name, Default) ->
    String = get_config_as_string(Name, Default),
    list_to_binary(String).
