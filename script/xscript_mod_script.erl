%%自动生成,请不要修改
%%@datetime:{{2015,6,5}{14,13,1}}
-module(xscript_mod_script).

-compile([export_all]).

-include("xscript.hrl").

script_execute(ScriptId) ->
    script_execute(ScriptId, 0, []).
script_execute(ScriptId, Args) ->
    script_execute(ScriptId, 0, Args).

%%测试
script_execute(4, 0, []) ->
    xscript_function_define:find_target(1,xscript_function_define:enemy_scope(1,0,0,0),170,40),
    TailFun1 = 
        fun() ->
                CondFun2 = 
                    fun() ->
                        xscript_function_define:random_find(1,num) > 0 
                    end,
                LoopFun3 = 
                    fun() ->
                        xscript_function_define:attack_target(100),
                        xscript_function_define:create_monster(1,500)
                    end,
                script_loop(CondFun2, LoopFun3)
        end, 
    case 
        xscript_function_define:check_target(1,num) > 0  andalso
        xscript_function_define:level() > 3  of 
        true ->
            xscript_function_define:apply(1,skill,1204),
            TailFun4 = 
                fun() ->
                    xscript_function_define:moverandom(1500),
                    TailFun1()
                end, 
            xscript_function_define:wait(2000, TailFun4);
        false ->
            xscript_function_define:find_target(xscript_function_define:enemy_scope(1,0,0,0,1000,1000)),
            case 
                xscript_function_define:check_target(2,num) > 0  of 
                true ->
                    xscript_function_define:move(3,10000,500),
                    TailFun5 = 
                        fun() ->
                            xscript_function_define:apply(12,skill,22),
                            TailFun1()
                        end, 
                    xscript_function_define:wait(500, TailFun5);
                false ->
                    xscript_function_define:moverandom(1000),
                    TailFun6 = 
                        fun() ->
                            xscript_function_define:apply(55,skill,66),
                            TailFun1()
                        end, 
                    xscript_function_define:wait(1000, TailFun6)
            end
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
