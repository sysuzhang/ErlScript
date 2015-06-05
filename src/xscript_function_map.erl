%% @author rongjie
%% @doc @todo Add description to xscript_function_map.


-module(xscript_function_map).
-export([get_function_map/1]).
%% ====================================================================
%% API functions
%% ====================================================================
-define(DEFINE_FUN_MAP(Function, Module),
           get_function_map(Function) -> {Module, Function}).

-export([]).

?DEFINE_FUN_MAP(create_monster, xscript_function_define);
?DEFINE_FUN_MAP(level, xscript_function_define);
?DEFINE_FUN_MAP(apply, xscript_function_define);
?DEFINE_FUN_MAP(find_target, xscript_function_define);
?DEFINE_FUN_MAP(enemy_scope, xscript_function_define);
?DEFINE_FUN_MAP(attack_target, xscript_function_define);
?DEFINE_FUN_MAP(check_target, xscript_function_define);
?DEFINE_FUN_MAP(random_find, xscript_function_define);
?DEFINE_FUN_MAP(wait, xscript_function_define);
?DEFINE_FUN_MAP(moverandom, xscript_function_define);
?DEFINE_FUN_MAP(move, xscript_function_define);
?DEFINE_FUN_MAP(check_apply_skill, xscript_function_define);
?DEFINE_FUN_MAP(notify_effect, xscript_function_define);
?DEFINE_FUN_MAP(throw, xscript_function_define); 
get_function_map(_Function) -> 
    unknow.

%% ====================================================================
%% Internal functions
%% ====================================================================


