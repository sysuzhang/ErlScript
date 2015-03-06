%%自动生成,请不要修改
%%@datetime:{{2015,3,6}{18,54,29}}
-module(script_test3).

-compile([expert_all]).

execute() ->
    script_function(2).
script_function(2) ->
    case 
        xscript_function_define:random_find(1,num) > 0  of 
        true ->
            xscript_function_define:attack_target(100),
            xscript_function_define:wait(100, {xscript_function_define, 3, []});
        _ ->
            script_function(1)
    end
script_function(3) ->
    xscript_function_define:create_monster(1,500),
    script_function(2).
script_function(1) ->
    xscript_function_define:move(100,200,300).