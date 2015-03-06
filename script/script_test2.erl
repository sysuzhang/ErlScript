%%自动生成,请不要修改
%%@datetime:{{2015,3,6}{17,35,27}}
-module(script_test2).

-compile([expert_all]).

execute() ->
    xscript_function_define:moverandom(),
    case 
        xscript_function_define:level() > 3  of 
        true ->
            xscript_function_define:apply(1,skill,1204),
            xscript_function_define:wait(100, {xscript_function_define, 2, []});
        false ->
            xscript_function_define:apply(2,skill,1205),
            xscript_function_define:wait(200, {xscript_function_define, 3, []})
    end.
script_function(1) ->
    xscript_function_define:move(100,200,300).
script_function(3) ->
    case 
        xscript_function_define:level() > 5  of
        true ->
            xscript_function_define:wait(300, {xscript_function_define, 5, []});
        _ ->
            script_function(4)
    end.
script_function(4) ->
    xscript_function_define:find_target(xscript_function_define:enemy_scope(1,0,0,0,1000,1000)),
    case 
        xscript_function_define:level() > 0  of
        true ->
            xscript_function_define:moverandom(),
            script_function(1);
        _ ->
            script_function(1)
    end.
script_function(5) ->
    xscript_function_define:attack_target(200,300,400,500),
    script_function(4).
script_function(2) ->
    xscript_function_define:find_target(300,222,12),
    script_function(1).