%%自动生成,请不要修改
%%@datetime:{{2015,1,12}{17,39,55}}
-module(script_test2).

-export([execute/0]).

execute() ->
    case 
        xscript_function_define:level() > 3  of 
        true ->
            xscript_function_define:apply(1,skill,1204),
            xscript_function_define:find_target(300,222,12);
        false ->
            xscript_function_define:apply(2,skill,1205),
            xscript_function_define:find_target(xscript_function_define:enemy_scope(1,0,0,0,1000,1000))
    end.