%%自动生成,请不要修改
%%@datetime:{{2015,3,6}{21,1,54}}
-module(script_test1).

-compile([expert_all]).

execute() ->
    xscript_function_define:create_monster(),
    xscript_function_define:create_monster(5),
    xscript_function_define:wait(100, {xscript_function_define, 0, []}).