%%自动生成,请不要修改
%%@datetime:{{2015,5,13}{10,36,44}}
-module(xscript_mod_script).

-compile([export_all]).

-include("xscript.hrl").

script_execute(ScriptId) ->
    script_execute(ScriptId, 0).

%%if
script_execute(2, 0) ->
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
script_execute(ScriptId, FunId) ->
    ?LOG_DEBUG("Not Defined ScriptId: ~w, FunId:~w", [ScriptId, FunId]).
