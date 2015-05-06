%% @author rongjie
%% @time 2015-01-01
%% @doc @todo Add description to xscript_compile.


-module(xscript_compile).

%% ====================================================================
%% API functions
%% ====================================================================
-export([generate_file/1, generate_file/2]).
-export([generate_dir/1, generate_dir/2]).
-export([comment/1]).

-define(INDENT_SPACE, 4).
-define(SINGLE_STATEMENT, -1).
-define(DEFAULT_FUNID , 0).
-define(DEFAULT_TAILFUNID, 0).

-define(DEFAULT_OUTPUT_FILE, "xscript_mod_script").
-define(INCLUDE_FILE, "-include(\"xscript.hrl\").\n\n").

%%函数映射模块
-define(FUNCTION_MAP_MODULE, xscript_function_map).
-define(FUNCTION_MAP_FUNCTION, get_function_map).

%%代码生成保存
-record(script_source, {scriptid = undefined,
                        functions = [],
                        main = [],
                        funid = 0
                        }).


-export([test/0, test1/0, test2/0, test3/0, test4/0, test_one/0, test_dir/0]).
test() ->
    test1(),
    test2(),
    test3(),
    test4(),
    
    ok.
    
test1() ->
    generate_file("./script/1.seq.script", [{out_dir, "./script/"}]).
test2() ->
    generate_file("./script/2.if.script", [{out_dir, "./script/"}]).
test3() ->
    generate_file("./script/3.loop.script", [{out_dir, "./script/"}]).
test4() ->
    generate_file("./script/4.测试.script", [{out_dir, "./script/"}]).

test_one() ->
    generate_files(["./script/1.seq.script", "./script/2.if.script", "./script/3.loop.script"], [{out_dir, "./script/"}]).

test_dir() ->
    generate_dir("./script/", [{out_dir, "./script/"}]).

get_funid() ->
    case get(script_source) of
        undefined ->
            erlang:put(script_source, #script_source{funid = 1}),
            1;
        ScriptSource ->
           #script_source{funid = FunId} = ScriptSource,
           NewScriptSource = ScriptSource#script_source{funid = FunId + 1},
           put(script_source, NewScriptSource),
           FunId + 1
    end.

generate_file(ProtoFile) ->
  generate_file(ProtoFile,[]).

get_output_filename(Options) ->
    proplists:get_value(out_file, Options, ?DEFAULT_OUTPUT_FILE).
%%Options:[{key, value}]
%%{one_output, true|false} : 是否输出一个文件
%%{out_dir, OutDir} : 输出目录
generate_file(ScriptFile, Options) ->
    generate_files([ScriptFile], Options).
generate_files(ScriptFiles,Options) when is_list(ScriptFiles) ->    
    {IsAllInOne, OutputOneFileName} =
        case proplists:get_value(one_output, Options) of 
            true ->
                {true, get_output_filename(Options)};
            false ->
                {false, ""};
            _ ->
                {true, get_output_filename(Options)}
        end,
    
    case IsAllInOne of
        true ->            
            %%所有脚本生成一个文件

            OutDir = 
                case proplists:get_value(out_dir, Options) of
                    undefined ->
                        ".";
                    Dir ->
                        filename:dirname(Dir)
                end,
            
            OutputFile = OutDir ++ "/" ++ OutputOneFileName ++ ".erl",
            
            {ok, FileRef} = xscript_file:open(OutputFile, [write]),    
            output_header(FileRef, OutputOneFileName),
            output_include(FileRef),
            
            output_default_funtion(FileRef),
            
            lists:foreach(fun(File) -> 
                                  ScriptId = rootname(File),
                                  {ok,String} = parse_file(File),   %%词法分析
                                  {ok,FirstParsed} = xscript_parser:parse(String),
                                  put(script_source, #script_source{scriptid =list_to_integer(ScriptId)}), 
                                  %%添加注释
                                  Comment = comment(File),
                                  
                                  CommentBin = unicode:characters_to_binary(Comment),
                                  CommentStr = io_lib:format("\n%%~ts", [CommentBin]),
                                  gen_output(FileRef, 0, CommentStr),
                                  output_source(FileRef, FirstParsed)
                          end, ScriptFiles),   
            
            output_other_function_match(FileRef),
            xscript_file:close(FileRef),
            ok;
        false ->
            %%每个脚本对应一个独立的文件
            lists:foreach(fun(File) -> 
                                  OutDir =   
                                      case proplists:get_value(out_dir, Options) of
                                          undefined ->
                                              filename:dirname(File);
                                          Dir ->
                                              filename:dirname(Dir)
                                      end,
                                  ScriptId = rootname(File),
                                  Basename = "script_" ++ ScriptId , 
                                  OutputFile = OutDir ++ "/" ++ Basename ++ ".erl",
                                  filelib:ensure_dir(OutputFile),
                                  
                                  {ok, FileRef} = xscript_file:open(OutputFile, [write]),    
                                  output_header(FileRef, Basename),
                                  
                                  {ok,String} = parse_file(File),   %%词法分析
                                  {ok,FirstParsed} = xscript_parser:parse(String),
                                  put(script_source, #script_source{scriptid =ScriptId}), 
                                  output_source(FileRef, FirstParsed),
                                  xscript_file:close(FileRef)
                          end, ScriptFiles), 
            ok
    end,
     
    ok.
 
%%按目录生成
%%Options: [{key, value}]
%%
generate_dir(Dirname, Options) ->
    case file:list_dir(Dirname) of
        {ok, FileList} ->
            Files = lists:foldr(fun(File, Acc) ->
                                        case filename:extension(File) of
                                            ".script" ->
                                                [Dirname ++ File | Acc];
                                            _ ->
                                                Acc
                                        end
                                end, [], FileList),
            generate_files(Files, Options);
        _ ->
            dd
    end, 
    ok.

generate_dir(Dirname) ->
    generate_dir(Dirname, []).

%% ====================================================================
%% Internal functions
%% ====================================================================


%% @hidden
%%第1个.号之前是rootname
rootname(Filename) ->  
    case filename:rootname(Filename) of
        Filename ->
            filename:basename(Filename);
        Rootname ->
            rootname(Rootname)
    end.

%%获得文件名包含的注释
comment(Filename) ->    
    try 
        Length = max(string:rchr(Filename, $.), 1) - 1,  
        SubFile = string:substr(Filename, 1, Length), 
        string:sub_string(filename:extension(SubFile), 2) 
    of
        Comment ->
            Comment
    catch
        _:_ ->
            ""
    end.
 
get_scriptid() ->
    case get(script_source) of 
        #script_source{scriptid = ScriptId}->
            ScriptId;
        _ ->
            0
    end.
        

output_source(FileRef, ScriptParse) ->
    ScriptId = get_scriptid(),
    Output = io_lib:format("\nscript_execute(~w, 0) ->\n", [ScriptId]),
    gen_output(FileRef, 0, Output),
    scripts(FileRef, ?INDENT_SPACE, ScriptParse),   
    output_functions(),     
    gen_output(FileRef, 0, "\n"), 
     
    ok.

output_header(FileRef, Module) ->
    Header = io_lib:format("%%自动生成,请不要修改\n%%@datetime:{~w~w}\n-module(~s).\n\n-compile([export_all]).\n\n", 
                           [erlang:date(), erlang:time(), Module]), 
    gen_output(FileRef, 0, Header),
    ok.

output_include(FileRef) ->
    IncludeStr = io_lib:format(?INCLUDE_FILE, []),
    gen_output(FileRef, 0, IncludeStr).
    

output_default_funtion(FileRef) ->
    FunStr = io_lib:format("script_execute(ScriptId) ->\n", []), 
    gen_output(FileRef, 0, FunStr),
    FunBodyStr = io_lib:format("script_execute(ScriptId, 0).\n", []),
    gen_output(FileRef, ?INDENT_SPACE, FunBodyStr),
    ok.

output_other_function_match(FileRef) ->
    FunStr = io_lib:format("script_execute(ScriptId, FunId) ->\n", []), 
    gen_output(FileRef, 0, FunStr),
    FunBodyStr = io_lib:format("?LOG_DEBUG(\"Not Defined ScriptId: ~cw, FunId:~cw\", [ScriptId, FunId]).\n", [$~, $~]),
    gen_output(FileRef, ?INDENT_SPACE, FunBodyStr),
    ok.


parse_file(FileName) ->
    {ok, InFile} = xscript_file:open(FileName, [read]),
    String = parse_file(InFile,[]),
    file:close(InFile),
    {ok,String}.

%% @hidden
parse_file(InFile,Acc) ->
    case xscript_file:request(InFile) of
        {ok,Token,_EndLine} ->
            parse_file(InFile,Acc ++ [Token]);
        {error,token} ->
            exit(scanning_error);
        {eof,_} ->
            Acc
    end.
 

%%语法规则解析, 对应语法分析器
scripts(_FileRef, _Indent, []) -> 
    ok;
scripts(FileRef, Indent, Statements) ->
    statements(FileRef, Indent, Statements, ?DEFAULT_FUNID, ?DEFAULT_TAILFUNID),
    gen_output(FileRef, 0, ";"),   %%脚本结尾
    ok.

output_functions() ->
    case get(script_source) of 
        #script_source{functions = [Fun|OtherFun]} = ScriptSource->
            put(script_source, ScriptSource#script_source{functions = OtherFun}),
            output_functions(Fun),
            output_functions();
        _ ->
            next
    end,
    ok.

output_functions(Fun) ->
    case Fun of
        {FunId, FileRef, {while, WhileStatements}} ->
            ScriptId = get_scriptid(),
            StrFun = io_lib:format("\nscript_execute(~w, ~w) ->\n", [ScriptId, FunId]),
            gen_output(FileRef, 0, StrFun),
            TailFunId = 
                case get({func, FunId}) of
                    undefined ->
                        ?DEFAULT_TAILFUNID;
                    TFID ->
                        TFID
                end,
            while_statement(FileRef, ?INDENT_SPACE, WhileStatements, FunId, TailFunId),
            gen_output(FileRef, 0, ";");   %% Function结束
        {FunId, FileRef, Statements} ->
            ScriptId = get_scriptid(),
            StrFun = io_lib:format("\nscript_execute(~w, ~w) ->\n", [ScriptId, FunId]),
            gen_output(FileRef, 0, StrFun),
            TailFunId = 
                case get({func, FunId}) of
                    undefined ->
                        ?DEFAULT_TAILFUNID;
                    TFID ->
                        TFID
                end,
            statements(FileRef, ?INDENT_SPACE, Statements, FunId, TailFunId),
            gen_output(FileRef, 0, ";");  %%statement结束
        _ ->
            next
    end,
    ok.
 

shift_statement(FileRef, Statements, FunId) ->
    add_function(FileRef, FunId, Statements),
    ok.

add_function(FileRef, Funid, Statements) ->
    case get(script_source) of
        undefined ->
            erlang:put(script_source, #script_source{functions = [{Funid, FileRef, Statements}]}),
            ok;
        ScriptSource ->
            #script_source{functions = Funs} = ScriptSource,
            NewScriptSource = ScriptSource#script_source{functions =[{Funid, FileRef, Statements} | Funs]},
            put(script_source, NewScriptSource),
            ok
    end.


%%Statements分析(段落分析)
statements(FileRef, Indent, Statements, SID, TailFunID) ->
    case Statements of
        [{function, Fun_statement}] ->
            function(FileRef, Indent, Fun_statement),
            if TailFunID =/= ?DEFAULT_TAILFUNID -> 
                   gen_output(FileRef, 0, ",\n"),
                   ScriptId = get_scriptid(),
                   Str = io_lib:format("script_execute(~w, ~w)", [ScriptId, TailFunID]),
                   gen_output(FileRef, Indent, Str);
               true ->
                   next
            end;
        [{function, Fun_statement}| OtherStatements] ->
            function(FileRef, Indent, Fun_statement),
            gen_output(FileRef, 0, ",\n"),
            statements(FileRef, Indent, OtherStatements, SID, TailFunID),
            ok;
        {wait_function, Wait_statement} -> 
            wait_statement(FileRef, Indent, Wait_statement, 0), 
            ok;
        [{wait_function, Wait_statement, OtherStatements}] ->
            NewFunID = get_funid(),  
            put({level, SID} , NewFunID),
            put({func, NewFunID}, TailFunID), %%当前尾函数的尾函数
            wait_statement(FileRef, Indent, Wait_statement, NewFunID), 
            shift_statement(FileRef, OtherStatements, NewFunID),
            ok;
        {if_statement, If_statment} ->
            NewSID = SID + 1,
            if_statement(FileRef, Indent, If_statment, NewSID, TailFunID),
            ok;
        [{if_statement, If_statement, OtherStatements}] ->
            NewFunID = get_funid(),   
            NewSID = SID + 1,
            put({level, SID}  , NewFunID),  %%当前层的尾函数
            put({func, NewFunID}, TailFunID), %%当前尾函数的尾函数
            if_statement(FileRef, Indent, If_statement, NewSID, NewFunID), 
            shift_statement(FileRef, OtherStatements, NewFunID),
            ok;
        {while_statement, WhileStatements} ->
            NewFunID = get_funid(),
            ScriptId = get_scriptid(),
            StrWhile = io_lib:format("script_execute(~w, ~w)", [ScriptId, NewFunID]),
            gen_output(FileRef, Indent, StrWhile),
            put({func, NewFunID}, TailFunID),         
            shift_statement(FileRef, {while, WhileStatements}, NewFunID),
            ok;
        [{while_statement, WhileStatements, OtherStatements}] ->
            
            NewFunID = get_funid(),
            put({func, NewFunID}, TailFunID),
            shift_statement(FileRef, OtherStatements, NewFunID),
            
            NewWhileFunID = get_funid(),
            ScriptId = get_scriptid(),
            StrWhile = io_lib:format("script_execute(~w, ~w)", [ScriptId, NewWhileFunID]),
            gen_output(FileRef, Indent, StrWhile),
            put({func, NewWhileFunID}, NewFunID),
            shift_statement(FileRef, {while, WhileStatements}, NewWhileFunID),
            ok;
        {var, Var, Fun_statement} ->%%支持变量
            StrVar = io_lib:format("~w = ", [Var]),
            gen_output(FileRef, Indent, StrVar),
            function(FileRef, Indent, Fun_statement),
            ok;
        [{var, Var, Fun_statement} | OtherStatements] ->
            StrVar = io_lib:format("~s = ", [Var]),
            gen_output(FileRef, Indent, StrVar),
            function(FileRef, 0, Fun_statement),
            gen_output(FileRef, 0, ",\n"),
            statements(FileRef, Indent, OtherStatements, SID, TailFunID),
            ok;
        _ ->
            ok
    end.
    
wait_statement(FileRef, Indent, WaitStatement, FunId) ->
    case WaitStatement of
        {'WAIT', [Time]} ->
            case ?FUNCTION_MAP_MODULE:?FUNCTION_MAP_FUNCTION (wait) of
                {Module, _} ->
                    ScriptId = get_scriptid(),
                    Output = io_lib:format("~w:~w(~w, ~w, {~w, ~w, ~w})", [Module, wait, Time, ScriptId, Module, FunId, []]),
                    gen_output(FileRef, Indent, Output);
                _ ->
                    throw({"Not Define Function: ~w", [wait]})
            end;
        _ ->
            ok
    end.
     

if_statement(FileRef, Indent, IfStatement, SID, TailFunID) -> 
    case IfStatement of
        {'IF', ConditionsClause, TrueStatements} ->    
            %%是否需要表达式反转
            {IsReverse, Conditions} = 
                case ConditionsClause of
                    {'not', Cond} ->
                        {true, Cond};
                    _ ->
                        {false, ConditionsClause}
                end,
            if IsReverse ->
                   gen_output(FileRef, Indent, "case not (\n"), 
                   conditions(FileRef, Indent + ?INDENT_SPACE, Conditions),
                   gen_output(FileRef, 0, " )");
               true ->
                   gen_output(FileRef, Indent, "case \n"), 
                   conditions(FileRef, Indent + ?INDENT_SPACE, Conditions)
            end,         
            gen_output(FileRef, 0, " of\n"),          
            gen_output(FileRef, Indent + ?INDENT_SPACE, "true ->\n"),    %% true         
            statements(FileRef, Indent + ?INDENT_SPACE * 2, TrueStatements, SID, TailFunID), 
            gen_output(FileRef, 0, ";\n"),
            
            gen_output(FileRef, Indent + ?INDENT_SPACE, "_ ->\n"),   
            
            if TailFunID =/= ?DEFAULT_TAILFUNID ->
                   ScriptId = get_scriptid(),
                   TailStr = io_lib:format("script_execute(~w, ~w)\n", [ScriptId, TailFunID]),
                   gen_output(FileRef, Indent + ?INDENT_SPACE * 2, TailStr);
               true ->
                   gen_output(FileRef, Indent + ?INDENT_SPACE * 2, "ok\n")
            end,
            gen_output(FileRef, Indent, "end"),
            ok;
        {'IF', Conditions, TrueStatements, FalseStatements} -> 
            gen_output(FileRef, Indent, "case \n"), 
            conditions(FileRef, Indent + ?INDENT_SPACE, Conditions),              
            gen_output(FileRef, 0, " of \n"),          
            gen_output(FileRef, Indent + ?INDENT_SPACE, "true ->\n"),    %% true         
            statements(FileRef, Indent + ?INDENT_SPACE * 2, TrueStatements, SID, TailFunID), 
            gen_output(FileRef, 0, ";\n"),
            gen_output(FileRef, Indent + ?INDENT_SPACE, "false ->\n"),    %% false         
            statements(FileRef, Indent + ?INDENT_SPACE * 2, FalseStatements, SID, TailFunID),  
            gen_output(FileRef, 0, "\n"),             
            gen_output(FileRef, Indent, "end"),
            ok;
        _ ->
            ok
    end,    
    ok.
 
while_statement(FileRef, Indent, WhileStatements, SID, TailFunID) ->
    case WhileStatements of
        {'WHILE', Conditions, Statements} ->
             gen_output(FileRef, Indent, "case \n"), 
             conditions(FileRef, Indent + ?INDENT_SPACE, Conditions),              
             gen_output(FileRef, 0, " of \n"),          
             gen_output(FileRef, Indent + ?INDENT_SPACE, "true ->\n"),    %% true         
             statements(FileRef, Indent + ?INDENT_SPACE * 2, Statements, SID, SID), 
             gen_output(FileRef, 0, ";\n"),
             
             gen_output(FileRef, Indent + ?INDENT_SPACE, "_ ->\n"),
             if TailFunID =/= ?DEFAULT_TAILFUNID ->
                    ScriptId = get_scriptid(),
                    TailStr = io_lib:format("script_execute(~w, ~w)\n", [ScriptId, TailFunID]),
                    gen_output(FileRef, Indent + ?INDENT_SPACE * 2, TailStr);
               true ->
                    gen_output(FileRef, Indent + ?INDENT_SPACE * 2, "ok\n")
             end,
             gen_output(FileRef, Indent, "end"),
             ok;
        _ ->
            ok
    end,
    ok.


conditions(FileRef, Indent, Conditions) ->
    case Conditions of
        {condition, Condition} ->
            condition(FileRef, Indent, Condition),
            ok;
        {Condition, Logic, OtherConditions} ->
            condition(FileRef, Indent, Condition),
            logic(FileRef, 0, Logic),
            gen_output(FileRef, 0, "\n"),
            conditions(FileRef, Indent, OtherConditions),
            ok;
        _ ->
            io:format("", [])
    end,
    ok.

condition(FileRef, Indent, Condition) ->
    case Condition of
        {express, Express} ->
            express(FileRef, Indent, Express),
            ok;
        {express, Express, Compare, OtherCondition} ->
            express(FileRef, Indent, Express),
            compare(FileRef, 0, Compare),
            condition(FileRef, Indent, OtherCondition),
            ok;
        {function, Function} ->
            function(FileRef, Indent, Function),
            ok;
        {function, Function, Compare, OtherCondition} ->
            function(FileRef, Indent, Function),
            compare(FileRef, 0, Compare),
            condition(FileRef, Indent, OtherCondition),
            ok;
        _ ->
            ok
    end,
    ok.

%%遇到终结符时才有输出
logic(FileRef, Indent, Logic) ->
    case Logic of
        '&&' ->
            gen_output(FileRef, Indent, " andalso"),
            ok;
        '||' ->
            gen_output(FileRef, Indent, " orelse"),
            ok;
        '!' ->            
            gen_output(FileRef, Indent, " not"),
            ok;
            
        _ ->
            dd
    end,
    ok.

compare(FileRef, Indent, Compare) ->    
    case Compare of
        '>' ->
            gen_output(FileRef, Indent, " >"),
            ok;
        '<' ->
            gen_output(FileRef, Indent, " <"),
            ok;
        '==' ->            
            gen_output(FileRef, Indent, " =:="),
            ok;            
        _ ->
            dd
    end, 
    ok.

arithmetic(FileRef, Indent, Arithmetic) ->
    StrAtith = atom_to_list(Arithmetic),
    gen_output(FileRef, Indent, StrAtith),
    ok.

vars(FileRef, Indent, Vars) ->
    StrVars = 
        case Vars of
            V when erlang:is_integer(V) orelse erlang:is_float(V) ->
                io_lib:format(" ~w ", [Vars]);
            _ ->
                io_lib:format(" ~s ", [Vars])
        end,
            
    gen_output(FileRef, Indent, StrVars),
    ok. 



express(FileRef, Indent, Express) ->
    case Express of
        {vars, Vars} ->
            vars(FileRef, 0, Vars),
            ok;
        {Vars, Arithmetic,  OtherExpress} ->
            vars(FileRef, 0, Vars),
            arithmetic(FileRef, 0, Arithmetic),
            express(FileRef, Indent, OtherExpress),
            ok;
        _ ->
            io:format("unknow statement function [~w]", [Express])
    end,
    ok.


function(FileRef, Indent, Statement) ->
    case Statement of
        {func, FuncName, Args} -> 
            case xscript_function_map:get_function_map(FuncName) of
                {Module, _} ->
                    Output = io_lib:format("~w:~w(", [Module, FuncName]),
                    gen_output(FileRef, Indent, Output),
                    args(FileRef, 0, Args),            
                    gen_output(FileRef, 0, ")");
                _ ->
                    
                    Output = io_lib:format("~w(", [FuncName]),
                    gen_output(FileRef, Indent, Output),
                    args(FileRef, 0, Args),            
                    gen_output(FileRef, 0, ")")
                    %throw({"Not Define Function", [FuncName]})
            end,
            ok;
        _ ->
            throw({"unknow statement function [~w]", [Statement]})
    end,
    ok.

args(_FileRef, _Indent, []) ->
    ok;
args(FileRef, _Indent, [Arg]) ->
    arg(FileRef, 0, Arg),
    ok;
args(FileRef, Indent, [Arg|OtherArgs]) ->
    arg(FileRef, 0, Arg),
    gen_output(FileRef, 0, ","),
    args(FileRef, Indent, OtherArgs),
    ok.

arg(FileRef, Indent, Arg) ->
    case Arg of
        {function, FuncName} ->
            function(FileRef, 0, FuncName);
        {var, StrVar} ->
            Output = io_lib:format("~s", [StrVar]),
            gen_output(FileRef, Indent, Output),
            ok;
        Arg ->                    
            Output = io_lib:format("~w", [Arg]),
            gen_output(FileRef, Indent, Output),
            ok
    end,
    ok.

gen_output(FileRef, IndentNum, Message) ->
    ok = indent(FileRef, IndentNum), 
    xscript_file:format(FileRef, "~ts", [Message]).

indent(FileRef, IndentNum) ->
    lists:foldl(fun(_, _Acc) ->
                        xscript_file:format(FileRef, "~s", [" "]),
                        _Acc
                 end, ok, lists:seq(1, IndentNum)).