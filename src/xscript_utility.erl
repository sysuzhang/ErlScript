%% @author rongjie
%% @doc @todo Add description to xscript_utility.


-module(xscript_utility).

%% ====================================================================
%% API functions
%% ====================================================================
-export([while/2]).
-export([test/0]).

while(ConditionFun, Fun) ->
    case ConditionFun() of
        true ->
            Fun(),
            while(ConditionFun, Fun);
        _ ->
            next
    end,
    ok.


test() ->
    put(v, 1),
    
    while(fun() ->
                  get(v) < 3
          end,
          fun() ->
                  T = get(v),
                  io:format("T = ~w~n", [T]), 
                  put(v, T + 1)
          end),
    
    ok.
%% ====================================================================
%% Internal functions
%% ====================================================================


