-ifndef(XSCRIPT_COMPILE_HRL_FILE).
-define(XSCRIPT_COMPILE_HRL_FILE, xscript_compile_hrl_file).


-define(DEFAULT_OUTPUT_FILE, "xscript_mod_script").
-define(INCLUDE_FILE, "-include(\"xscript.hrl\").\n\n").

%%函数映射模块
-define(FUNCTION_MAP_MODULE, xscript_function_map).
-define(FUNCTION_MAP_FUNCTION, get_function_map).


-endif.