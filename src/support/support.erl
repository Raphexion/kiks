-module(support).

-export([b/1]).

b(Variable) when is_binary(Variable) ->                                                                                                              Variable;                                                                                                                                    b(Variable) when is_list(Variable) ->                                                                                                                list_to_binary(Variable).
