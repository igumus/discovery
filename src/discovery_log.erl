-module(discovery_log).

-export([debug/3, debug/2]).

%% ==== API functions.

%% Prints debug line.
debug(M,F,A) ->
    io:format("DEBUG: ~p function in ~p called with ~p~n", [F, M, A]).

debug(M,F) ->
    io:format("DEBUG: ~p function in ~p called.~n", [F, M]).

