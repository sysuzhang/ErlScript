%% @author Administrator
%% @doc @todo Add description to xscript_file.


-module(xscript_file).

%% ====================================================================
%% API functions
%% ====================================================================
-export([open/2,path_open/3,close/1,format/3,request/1,compile_forms/2,write_file/2]).

open(File, Options) ->
    file:open(File,Options).

path_open(Path, File, Modes) ->
    file:path_open(Path, File, Modes).

close(FileRef) ->
    file:close(FileRef).

format(FileRef, FormatString, WriteFields) ->
    Msg = io_lib:format(FormatString, WriteFields),
    Bytes = unicode:characters_to_binary(Msg),
    file:write(FileRef, Bytes).
    %io:format(FileRef, FormatString, WriteFields).

request(InFile) ->
    io:request(InFile,{get_until,prompt,xscript_scanner,token,[1]}).

compile_forms(Forms, Options) ->
    compile:forms(Forms, [return] ++ Options).

write_file(File, Bytes) ->
    file:write_file(File,Bytes).

%% ====================================================================
%% Internal functions
%% ====================================================================


