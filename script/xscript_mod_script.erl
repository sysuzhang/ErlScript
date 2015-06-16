%% autogen from "xscript_compile"
-module(xscript_mod_script).
-compile(export_all).
-include("xscript.hrl").

script_execute(ScriptId) ->
    script_execute(ScriptId, 0, []).
script_execute(ScriptId, Args) ->
    script_execute(ScriptId, 0, Args).

%%erlang
script_execute(7, 0, []) ->
    {ok,T} = xscript_function_define:find_target(200,{300,200}),
    [Hp,Mp] = xscript_function_define:get_attr([hp,mp]),
    PlayerLv  = xscript_function_define:playerlv(),
    AttackFight  = 0,
    Bless  = 0,
    T  = (0.98+0.9-Bless/(PlayerLv*100))+AttackFight;
script_execute(ScriptId, FunId, Args) ->
    ?LOG_DEBUG("Not Defined ScriptId: ~w, FunId:~w, Args:~w", [ScriptId, FunId, Args]).

script_loop(CondFun, LoopFun) ->
    case CondFun() of
        true ->
            LoopFun(),
            script_loop(CondFun, LoopFun);
        false ->
            ok
    end.
