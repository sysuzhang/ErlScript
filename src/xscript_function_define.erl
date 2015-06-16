%% @author rongjie
%% @doc @todo Add description to xscript_function_define.


-module(xscript_function_define).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile(export_all).

wait(Time, ScriptId, TailFun) ->
    ok.

create_monster() ->
    io:format("create_monster~n").

create_monster(Arg1, Arg2) ->
    io:format("create_monster(~p)~n", [{Arg1, Arg2}]).

create_monster(Arg1, Arg2, Arg3) ->
    io:format("create_monster(~p)~n", [{Arg1, Arg2, Arg3}]).

level() ->
    io:format("Level()~n", []).

apply(Arg1, Arg2, Arg3) ->
    io:format("apply(~p)~n", [{Arg1, Arg2, Arg3}]).

find_target() ->
    ok.
find_target(Arg) ->
    ok.
find_target(Arg1, Arg2) ->
    io:format("find_target(~p)~n", [{Arg1, Arg2}]).

find_target(Arg1, Arg2, Arg3) ->
    io:format("find_target(~p)~n", [{Arg1, Arg2, Arg3}]).

find_target(Num, Scope, Length, Width) ->
    ok.
    
random_find(Num, num) ->
    ok.

moverandom() ->
    ok.
moverandom(Time) ->
    ok.

move(X, Y, Z) ->
    ok.
enemy_scope(X, Y, Z, D, Length, Width) ->
    ok.

check_target(Num, num) ->
    ok.

attack_target(Target) ->
    ok.

attack_target(X, Y, Z, D) ->
    ok.

attack_target(Target, Hurt) ->
    ok.

enemy_scope(X, Y, Z, Dir) ->
    ok.

check_apply_skill(SkillId, ObjKey) ->
    ok.

notify_effect() ->
    ok.

throw(Msg) ->
    ok.

get_attr(AttrNameList) ->
    [Name || Name <- AttrNameList].

set_attr(AttrTupleList) ->
    ok.
%% ====================================================================
%% Internal functions
%% ====================================================================


