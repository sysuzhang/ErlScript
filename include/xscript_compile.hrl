-ifndef(XSCRIPT_COMPILE_HRL_FILE).
-define(XSCRIPT_COMPILE_HRL_FILE, xscript_compile_hrl_file).


-define(DEFAULT_OUTPUT_FILE, "xscript_mod_script").
-define(INCLUDE_FILE, "-include(\"xscript.hrl\").\n\n").

-define(FUNC_DEFINE_TYPE_MODULE, 0).  %%指定模块搜索函数定义
-define(FUNC_DEFINE_TYPE_DIR,    1).  %%指定目录搜索函数定义
-define(FUNC_DEFINE_TYPE, ?FUNC_DEFINE_TYPE_MODULE).    %%当前使用的函数定义配置方式

-define(FUNCTION_STD_MODULE, [erlang]).
-define(FUNCTION_DEFINE_MODULE, [xscript_function_define]).
-define(FUNCTION_DEFINE_DIR,    "./src/Game/Script/").


-endif.