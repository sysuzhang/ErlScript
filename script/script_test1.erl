%%自动生成,请不要修改
%%@datetime:{{2015,1,12}{17,39,55}}
-module(script_test1).

-export([execute/0]).

execute() ->
    xscript_function_define:create_monster(),
    xscript_function_define:create_monster(1,500),
    xscript_function_define:create_monster(2,400).