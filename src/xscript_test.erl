%% @author Administrator
%% @doc @todo Add description to xscript_test.


-module(xscript_test).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).


test() -> 
    InfoList = xscript_function_define:module_info(),
    ExportList = lists:keyfind(exports, 1, InfoList),
    {ok, erlang:element(2, ExportList)}.

%% ====================================================================
%% Internal functions
%% ====================================================================


