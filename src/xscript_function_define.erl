%% @author rongjie
%% @doc @todo Add description to xscript_function_define.


-module(xscript_function_define).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile(export_all).

create_monster() ->
    io:format("create_monster~n").

create_monster(Arg1, Arg2) ->
    io:format("create_monster(~p)~n", [{Arg1, Arg2}]).

level() ->
    io:format("Level()~n", []).

apply(Arg1, Arg2, Arg3) ->
    io:format("apply(~p)~n", [{Arg1, Arg2, Arg3}]).

find_target(Arg1, Arg2, Arg3) ->
    io:format("find_target(~p)~n", [{Arg1, Arg2, Arg3}]).
    

%% ====================================================================
%% Internal functions
%% ====================================================================


