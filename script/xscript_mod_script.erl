%%自动生成,请不要修改
%%@datetime:{{2015,6,11}{18,43,55}}
-module(xscript_mod_script).

-compile([export_all]).

-include("xscript.hrl").

script_execute(ScriptId) ->
    script_execute(ScriptId, 0, []).
script_execute(ScriptId, Args) ->
    script_execute(ScriptId, 0, Args).

%%prarm
script_execute(5, 0, [SKillID,ObjectKey,Pos]) ->
    case 
        xscript_function_define:check_apply_skill(SKillID,ObjectKey) =:= true  of 
        true ->
            xscript_function_define:notify_effect(),
            TailFun1 = 
                fun() ->
                    xscript_function_define:attack_target([ObjectKey])
                end, 
            xscript_function_define:wait(3000, 5, TailFun1);
        false ->
            xscript_function_define:throw(error)
    end;
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
