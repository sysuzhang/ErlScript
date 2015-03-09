%%自动生成,请不要修改
%%@datetime:{{2015,3,9}{17,0,40}}
-module(xscript_mod_script).

-compile([export_all]).

-include("xscript.hrl").

script_execute(ScriptId) ->
    script_execute(ScriptId, 0).

%%seq
script_execute(1, 0) ->
    xscript_function_define:create_monster(),
    xscript_function_define:create_monster(),
    xscript_function_define:wait(1000, 1, {xscript_function_define, 1, []});
script_execute(1, 1) ->
    xscript_function_define:create_monster(100,200),
    xscript_function_define:create_monster(200,300),
    xscript_function_define:wait(2000, 1, {xscript_function_define, 2, []});
script_execute(1, 2) ->
    xscript_function_define:create_monster(300,400),
    xscript_function_define:create_monster(500,600);

%%if
script_execute(2, 0) ->
    xscript_function_define:moverandom(),
    case 
        xscript_function_define:level() > 3  of 
        true ->
            xscript_function_define:apply(1,skill,1204),
            xscript_function_define:wait(100, 2, {xscript_function_define, 2, []});
        false ->
            xscript_function_define:apply(2,skill,1205),
            xscript_function_define:wait(200, 2, {xscript_function_define, 3, []})
    end;
script_execute(2, 1) ->
    xscript_function_define:move(100,200,300);
script_execute(2, 3) ->
    case 
        xscript_function_define:level() > 5  of
        true ->
            xscript_function_define:wait(300, 2, {xscript_function_define, 5, []});
        _ ->
            script_execute(2, 4)
    end;
script_execute(2, 4) ->
    xscript_function_define:find_target(xscript_function_define:enemy_scope(1,0,0,0,1000,1000)),
    case 
        xscript_function_define:level() > 0  of
        true ->
            xscript_function_define:moverandom(),
            xscript_function_define:wait(1000, 2, {xscript_function_define, 6, []});
        _ ->
            script_execute(2, 1)
    end;
script_execute(2, 6) ->
    xscript_function_define:find_target(120,150,300,200),
    xscript_function_define:attack_target(120,150,300,3),
    script_execute(2, 1);
script_execute(2, 5) ->
    xscript_function_define:attack_target(200,300,400,500),
    script_execute(2, 4);
script_execute(2, 2) ->
    xscript_function_define:find_target(300,222,12),
    script_execute(2, 1);

%%loop
script_execute(3, 0) ->
    script_execute(3, 2);
script_execute(3, 2) ->
    case 
        xscript_function_define:random_find(1,num) > 0  of 
        true ->
            xscript_function_define:attack_target(100),
            xscript_function_define:wait(100, 3, {xscript_function_define, 3, []});
        _ ->
            script_execute(3, 1)
    end;
script_execute(3, 3) ->
    xscript_function_define:create_monster(1,500),
    script_execute(3, 2);
script_execute(3, 1) ->
    xscript_function_define:move(100,200,300);

%%测试
script_execute(4, 0) ->
    xscript_function_define:find_target(1,xscript_function_define:enemy_scope(1,0,0,0),170,40),
    case 
        xscript_function_define:check_target(1,num) > 0  andalso
        xscript_function_define:level() > 3  of 
        true ->
            xscript_function_define:apply(1,skill,1204),
            xscript_function_define:wait(2000, 4, {xscript_function_define, 2, []});
        false ->
            xscript_function_define:find_target(xscript_function_define:enemy_scope(1,0,0,0,1000,1000)),
            case 
                xscript_function_define:check_target(2,num) > 0  of 
                true ->
                    xscript_function_define:move(3,10000,500),
                    xscript_function_define:wait(500, 4, {xscript_function_define, 3, []});
                false ->
                    xscript_function_define:moverandom(1000),
                    xscript_function_define:wait(1000, 4, {xscript_function_define, 4, []})
            end
    end;
script_execute(4, 1) ->
    script_execute(4, 5);
script_execute(4, 5) ->
    case 
        xscript_function_define:random_find(1,num) > 0  of 
        true ->
            xscript_function_define:attack_target(100),
            xscript_function_define:create_monster(1,500),
            script_execute(4, 5);
        _ ->
            ok
    end;
script_execute(4, 4) ->
    xscript_function_define:apply(55,skill,66),
    script_execute(4, 1);
script_execute(4, 3) ->
    xscript_function_define:apply(12,skill,22),
    script_execute(4, 1);
script_execute(4, 2) ->
    xscript_function_define:moverandom(1500),
    script_execute(4, 1);
script_execute(ScriptId, FunId) ->
    ?LOG_DEBUG("Not Defined ScriptId: ~w, FunId:~w", [ScriptId, FunId]).
