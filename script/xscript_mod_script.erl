%%自动生成,请不要修改
%%@datetime:{{2015,5,12}{15,21,55}}
-module(xscript_mod_script).

-compile([export_all]).

-include("xscript.hrl").

script_execute(ScriptId) ->
    script_execute(ScriptId, 0).

%%测试
script_execute(4, 0) ->
    xscript_function_define:find_target(1,xscript_function_define:enemy_scope(1,0,0,0),170,40),
    TailFun1 = 
        fun() ->
            script_execute(4, 2)
        end, 
    case 
        xscript_function_define:check_target(1,num) > 0  andalso
        xscript_function_define:level() > 3  of 
        true ->
            xscript_function_define:apply(1,skill,1204),
            TailFun3 = 
                fun() ->
                    xscript_function_define:moverandom(1500),
                    TailFun1()
                end, 
            xscript_function_define:wait(2000, TailFun3);
        false ->
            xscript_function_define:find_target(xscript_function_define:enemy_scope(1,0,0,0,1000,1000)),
            case 
                xscript_function_define:check_target(2,num) > 0  of 
                true ->
                    xscript_function_define:move(3,10000,500),
                    TailFun4 = 
                        fun() ->
                            xscript_function_define:apply(12,skill,22),
                            TailFun1()
                        end, 
                    xscript_function_define:wait(500, TailFun4);
                false ->
                    xscript_function_define:moverandom(1000),
                    TailFun5 = 
                        fun() ->
                            xscript_function_define:apply(55,skill,66),
                            TailFun1()
                        end, 
                    xscript_function_define:wait(1000, TailFun5)
            end
    end;
script_execute(4, 2) ->
    case 
        xscript_function_define:random_find(1,num) > 0  of 
        true ->
            xscript_function_define:attack_target(100),
            xscript_function_define:create_monster(1,500),
            script_execute(4, 2);
        _ ->
            ok
    end;
script_execute(ScriptId, FunId) ->
    ?LOG_DEBUG("Not Defined ScriptId: ~w, FunId:~w", [ScriptId, FunId]).
