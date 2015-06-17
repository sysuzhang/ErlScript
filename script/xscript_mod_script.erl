%% autogen from "xscript_compile"
-module(xscript_mod_script).
-include("xscript.hrl").
-compile(export_all).
script_execute(ScriptId) ->
    script_execute(ScriptId, 0, []).
script_execute(ScriptId, Args) ->
    script_execute(ScriptId, 0, Args).

%%loop
script_execute(3, 0, []) ->
    T  = xscript_function_define:find_target(),
    CondFun1 = 
        fun() ->
            xscript_function_define:random_find( 1 , num ) > 0 
        end,
    LoopFun2 = 
        fun() ->
            xscript_function_define:attack_target( T , 100 ),
            TailFun3 = 
                fun() ->
                    xscript_function_define:create_monster( 1 , 500 )
                end, 
            xscript_function_define:wait( 100 , 3, TailFun3)
        end,
    script_loop(CondFun1, LoopFun2),
    xscript_function_define:move( 100 , 200 , 300 );
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
