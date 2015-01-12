%%自动生成,请不要修改
%%@datetime:{{2015,1,12}{17,39,55}}
-module(script_test3).

-export([execute/0]).

execute() ->
    xscript_utility:while(
        fun() ->
            xscript_function_define:random_find(1,num) > 0 
        end,
        fun() ->
            xscript_function_define:attack_target(100),
            xscript_function_define:create_monster(1,500)
        end).