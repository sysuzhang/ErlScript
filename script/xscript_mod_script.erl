%%自动生成,请不要修改
%%@datetime:{{2015,6,5}{15,2,17}}
-module(xscript_mod_script).

-compile([export_all]).

-include("xscript.hrl").

script_execute(ScriptId) ->
    script_execute(ScriptId, 0, []).
script_execute(ScriptId, Args) ->
    script_execute(ScriptId, 0, Args).

%%loop
script_execute(3, 0, []) ->
    T = xscript_function_define:find_target(),
        CondFun1 = 
            fun() ->
                xscript_function_define:random_find(1,num) > 0 
            end,
        LoopFun2 = 
            fun() ->
                xscript_function_define:attack_target(T,100),
                TailFun3 = 
                    fun() ->
                        xscript_function_define:create_monster(1,500)
                    end, 
                xscript_function_define:wait(100, TailFun3)
            end,
        script_loop(CondFun1, LoopFun2),
    xscript_function_define:move(100,200,300);

%%if
script_execute(2, 0, []) ->
    xscript_function_define:moverandom(),
    TailFun1 = 
        fun() ->
            xscript_function_define:move(100,200,300)
        end, 
    case 
        xscript_function_define:level() =/= false  of 
        true ->
            xscript_function_define:apply(1,skill,1204),
            TailFun1();
        false ->
            xscript_function_define:apply(2,skill,1205),
            TailFun2 = 
                fun() ->
                    TailFun3 = 
                        fun() ->
                            xscript_function_define:find_target(xscript_function_define:enemy_scope(1,0,0,0,1000,1000)),
                            case 
                                xscript_function_define:level() > 0  of
                                true ->
                                    xscript_function_define:moverandom(),
                                    TailFun4 = 
                                        fun() ->
                                            xscript_function_define:find_target(120,150,300,200),
                                            xscript_function_define:attack_target(120,150,300,3),
                                            TailFun1()
                                        end, 
                                    xscript_function_define:wait(1000, TailFun4);
                                _ ->
                                    TailFun1()
                            end
                        end, 
                    case 
                        xscript_function_define:level() > 5  of
                        true ->
                            TailFun5 = 
                                fun() ->
                                    xscript_function_define:attack_target(200,300,400,500),
                                    TailFun3()
                                end, 
                            xscript_function_define:wait(300, TailFun5);
                        _ ->
                            TailFun3()
                    end
                end, 
            xscript_function_define:wait(200, TailFun2)
    end;

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
                    xscript_function_define:wait(300, TailFun3)
                end, 
            xscript_function_define:wait(200, TailFun2)
        end, 
    xscript_function_define:wait(100, TailFun1);
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
