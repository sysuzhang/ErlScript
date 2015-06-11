%%自动生成,请不要修改
%%@datetime:{{2015,6,11}{20,31,15}}
-module(xscript_mod_script).

-compile([export_all]).

-include("xscript.hrl").

script_execute(ScriptId) ->
    script_execute(ScriptId, 0, []).
script_execute(ScriptId, Args) ->
    script_execute(ScriptId, 0, Args).

%%seq
script_execute(1, 0, []) ->
    T = xscript_function_define:find_target(200,300),
    TailFun1 = 
        fun() ->
            xscript_function_define:create_monster(T,100,200),
            TailFun2 = 
                fun() ->
                    xscript_function_define:create_monster(T,300,400),
                    TailFun3 = 
                        fun() ->
                            xscript_function_define:create_monster(T,400,500)
                        end, 
                    xscript_function_define:wait(300, 1, TailFun3)
                end, 
            xscript_function_define:wait(200, 1, TailFun2)
        end, 
    xscript_function_define:wait(100, 1, TailFun1);
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
