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
-export([test1/0, test2/0, test3/0, test4/0, test5/0]).
-include("xscript_compile.hrl").

-define(INDENT_SPACE, 4).
-define(SINGLE_STATEMENT, -1).
-define(DEFAULT_FUNID , 0).
-define(DEFAULT_TAILFUNID, 0).


%%代码生成保存
-record(script_file, {sources = orddict:new(),
                      header = [],  %%Header代码
                      include = [], %%include代码
                      default_function = [],
                      default_match = [],
                      loop_function = []
                      }).

%%每个代码文件
-record(script_source, {scriptid = 0,
                        funid = 0,
                        param = [],
                        comment = [],
                        function = [],   %%参数
                        body = [],    %%函数体
                        functions = [],
                        tailfun = dict:new()
                        }).


-export([test/0, test1/0, test2/0, test3/0, test4/0, test5/0, test_one/0, test_dir/0]).
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
test5() ->
    generate_file("./script/5.prarm.script", [{out_dir, "./script/"}]).

test_one() ->
    generate_files(["./script/1.seq.script", "./script/2.if.script", "./script/3.loop.script"], [{out_dir, "./script/"}]).

test_dir() ->
    generate_dir("./script/", [{out_dir, "./script/"}]).

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
    erlang:erase(),
    {IsAllInOne, OutputOneFileName} =
        case proplists:get_value(one_output, Options) of 
            true ->
                {true, get_output_filename(Options)};
            false ->
                {false, ""};
            _ ->
                {true, get_output_filename(Options)}
        end,
    
    put(script_file, #script_file{}),
    
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
            
            GenScriptFun = 
                fun(File) ->               
                        erlang:erase(cur_scriptid),  
                        erlang:erase(script_source),
                        
                        ScriptId = rootname(File),
                        put(cur_scriptid, list_to_integer(ScriptId)),%%设置当前处理的Script
                        
                        {ok,String} = parse_file(File),   %%词法分析
                        {ok,FirstParsed} = xscript_parser:parse(String), %%语法分析,得到语法树
                                               
                        
                        generate_commnet(File),  %%生成注释

                        generate_source(FirstParsed),  %%分析语法树,生成目标代码
                        generate_script_execute(), %%生成函数头
                        ok
                end,
            lists:foreach(fun(File) -> 
                                  case catch GenScriptFun(File) of
                                      ok ->
                                          ok;
                                      Err -> 
                                          Str = io_lib:format("File : ~ts~nError : ~p~n", [File, Err]),
                                          CharData = unicode:characters_to_binary(Str),
                                          io:put_chars(CharData)
                                  end
                          end, ScriptFiles),       
             
            generate_default_funtion(),
            generate_include(),            
            generate_header(OutputOneFileName),
            
            output_other_script_match(),%%通用匹配 
            generate_loop_fun(),
            
            write_to_file(OutputFile),
            ok;
        false ->
            %%每个脚本对应一个独立的文件
            lists:foreach(fun(File) ->          
                                  erlang:erase(),
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
                                  generate_header(Basename),
                                  
                                  {ok,String} = parse_file(File),   %%词法分析
                                  {ok,FirstParsed} = xscript_parser:parse(String),
                                  put(script_source, #script_source{scriptid =ScriptId}), 
                                  generate_source(FirstParsed),
                                  xscript_file:close(FileRef)
                          end, ScriptFiles), 
            ok
    end,
     
    ok.

write_to_file(OutputFile) ->
    {ok, FileRef} = xscript_file:open(OutputFile, [write]),      
    %%输出
    case get(script_file) of
        #script_file{sources = OrddictSource, header = HeaderList, include = IncludeList,
                     default_function = DefaultFuntionList, default_match = DefaultMatch, loop_function = LoopFuntions} -> 
            lists:foldr(fun({Indent, OutStr}, Acc) ->
                                gen_output(FileRef, Indent, OutStr),
                                Acc
                        end, ok, HeaderList),
            lists:foldr(fun({Indent, OutStr}, Acc) ->
                                gen_output(FileRef, Indent, OutStr),
                                Acc
                        end, ok, IncludeList),
            lists:foldr(fun({Indent, OutStr}, Acc) ->
                                gen_output(FileRef, Indent, OutStr),
                                Acc
                        end, ok, DefaultFuntionList),
            
            %%输出脚本函数代码
            SourceList = orddict:to_list(OrddictSource),            
            lists:foldr(fun({ScriptID, #script_source{comment = Comment, function = Function, body = Body}}, Acc) ->
                                
                                lists:foldr(fun({Indent, OutStr}, Acc1) ->
                                                    gen_output(FileRef, Indent, OutStr),
                                                    Acc1
                                            end, ok, Comment),
                                
                                lists:foldr(fun({Indent, OutStr}, Acc1) ->
                                                    gen_output(FileRef, Indent, OutStr),
                                                    Acc1
                                            end, ok, Function),
                                
                                lists:foldr(fun({Indent, OutStr}, Acc1) ->
                                                    gen_output(FileRef, Indent, OutStr),
                                                    Acc1
                                            end, ok, Body),                                
                                
                                Acc
                        end, ok, SourceList),
            
            lists:foldr(fun({Indent, OutStr}, Acc) ->
                                gen_output(FileRef, Indent, OutStr),
                                Acc
                        end, ok, DefaultMatch),
        
            lists:foldr(fun({Indent, OutStr}, Acc) ->
                                gen_output(FileRef, Indent, OutStr),
                                Acc
                        end, ok, LoopFuntions);
        _ ->
            none
    end, 
    
    xscript_file:close(FileRef),
    ok.
 
%%按目录生成
%%Options: [{key, value}]
%%
generate_dir(Dirname, Options) ->  
    case file:list_dir_all(Dirname) of
        {ok, FileList} ->
            Files = lists:foldr(fun(File, Acc) -> 
                                        FileBin = unicode:characters_to_binary(File),
                                        case filename:extension(FileBin) of
                                            <<".script">> ->
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

set_script_id(ScriptId) ->
    case get(script_source) of
        #script_source{} = ScriptSource ->
            put(script_source, ScriptSource#script_source{scriptid = ScriptId});
        _ ->
            put(script_source, #script_source{scriptid = ScriptId})
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
        

generate_source(ScriptParse) -> 
    scripts(?INDENT_SPACE, ScriptParse),     
    ok.

generate_script_execute() ->
    case get(script_file) of
        #script_file{sources = OrddictSources} -> 
            ScriptID = get_cur_scriptid(),
            ParamList = 
                case orddict:find(ScriptID, OrddictSources) of
                    {ok, #script_source{param = [{_, Param}]}} ->
                        Param;
                    _ ->
                        []
                end, 
            ArgsStr = 
                lists:foldr(fun(Param, Acc) ->
                                    case Acc of
                                        [] ->
                                            [Param];
                                        _ ->
                                            [Param, ","|Acc]                                    
                                    end
                            end, [], ParamList),
            NewArgsStr = lists:concat(ArgsStr),
            Output = io_lib:format("\nscript_execute(~w, 0, [~s]) ->\n", [ScriptID, NewArgsStr]),
            add_script_source_element(0, Output, #script_source.function), 
            ok;
        _ ->
            none
    end,   
    ok.

generate_commnet(File) ->    
    %%添加注释
    Comment = comment(File),  
    CommentBin = unicode:characters_to_binary(Comment),
    CommentStr = io_lib:format("\n%%~ts", [CommentBin]),
    add_script_source_element(0, CommentStr, #script_source.comment), 
    ok.

output_functions() ->
    case get(script_source) of 
        #script_source{functions = FunList} = ScriptSource->
            %put(script_source, ScriptSource#script_source{functions = OtherFun}),
            [output_functions(Fun) || Fun <- FunList],
            ok;
        _ ->
            next
    end,
    ok.

output_functions(Fun) ->
    case Fun of
        {FunId, {while, WhileStatements}} ->
            ScriptId = get_scriptid(),
            StrFun = io_lib:format("\nscript_loop(~w, ~w) ->\n", [ScriptId, FunId]),
            add_body(0, StrFun), 
            while_statement(?INDENT_SPACE, WhileStatements),
            add_body(0, ";");   %% Function结束 
        _ ->
            next
    end,
    ok.

generate_header(Module) ->
    Header = io_lib:format("%%自动生成,请不要修改\n%%@datetime:{~w~w}\n-module(~s).\n\n-compile([export_all]).\n\n", 
                           [erlang:date(), erlang:time(), Module]), 
    add_script_file_element(0, Header, #script_file.header),
    ok.

generate_include() ->
    IncludeStr = io_lib:format(?INCLUDE_FILE, []),
    add_script_file_element(0, IncludeStr, #script_file.include).
    

generate_default_funtion() ->
    FunStr = io_lib:format("script_execute(ScriptId) ->\n", []), 
    add_script_file_element(0, FunStr, #script_file.default_function),
    FunBodyStr = io_lib:format("script_execute(ScriptId, 0, []).\n", []),
    add_script_file_element(?INDENT_SPACE, FunBodyStr, #script_file.default_function),
    
    FunStr2 = io_lib:format("script_execute(ScriptId, Args) ->\n", []), 
    add_script_file_element(0, FunStr2, #script_file.default_function),
    FunBodyStr2 = io_lib:format("script_execute(ScriptId, 0, Args).\n", []),
    add_script_file_element(?INDENT_SPACE, FunBodyStr2, #script_file.default_function),
    ok.

generate_loop_fun() ->
    FunStr =         "
script_loop(CondFun, LoopFun) ->
    case CondFun() of
        true ->
            LoopFun(),
            script_loop(CondFun, LoopFun);
        false ->
            ok
    end.
",
     add_script_file_element(0, FunStr, #script_file.loop_function),
     ok.

output_other_script_match() ->
    FunStr = io_lib:format("script_execute(ScriptId, FunId, Args) ->\n", []), 
    add_script_file_element(0, FunStr, #script_file.default_match),
    FunBodyStr = io_lib:format("?LOG_DEBUG(\"Not Defined ScriptId: ~cw, FunId:~cw, Args:~cw\", [ScriptId, FunId, Args]).\n", [$~, $~, $~]),
    add_script_file_element(?INDENT_SPACE, FunBodyStr, #script_file.default_match),
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
 
scripts(Indent, Statements) ->
    put(funid, ?DEFAULT_FUNID),
    statements(Indent, Statements, ?DEFAULT_FUNID),
    add_body(0, ";\n"),
    ok.
  

%%Statements分析(段落分析)
statements(Indent, [], FunID) -> 
%%     case get_tail_funid(FunID) of
%%         TailFunID when is_integer(TailFunID) -> 
%%             %add_body(0, ",\n"),
%%             TailStr = io_lib:format("TailFun~w()", [TailFunID]),
%%             add_body(Indent, TailStr);
%%         undefined ->
%%             %add_body(Indent , "ok"),
%%             ok
%%     end,
    ok;
statements(Indent, Statements, FunID) ->
    case Statements of
        [{metascript, MetaScript} | OtherStatements] ->
            %%参数生成
            metascript(MetaScript),
            statements(Indent, OtherStatements, FunID);
        [{statement, Statement} | OtherStatements] ->
            statement(Indent, Statement, FunID),
            case OtherStatements of
                [] ->
                    case get_tail_funid(FunID) of
                        TailFunID when is_integer(TailFunID) -> 
                            add_body(0, ",\n"),
                            TailStr = io_lib:format("TailFun~w()", [TailFunID]),
                            add_body(Indent, TailStr);
                        undefined ->
                            %add_body(Indent , "ok"),
                            ok
                    end,
                    none;
                _ ->
                    add_body(0, ",\n")
            end,                    
            statements(Indent, OtherStatements, FunID); 
        [{wait_function, Wait_statement, OtherStatements}] ->
            NewFunID = get_fun_id(),   
            set_tail_funid(NewFunID, get_tail_funid(FunID)),
            StrFun = io_lib:format("TailFun~w = \n", [NewFunID]),
            add_body(Indent, StrFun),
            
            StrFun2 = io_lib:format("fun() ->\n", []),
            add_body(Indent + ?INDENT_SPACE, StrFun2),
            statements(Indent + ?INDENT_SPACE * 2, OtherStatements, NewFunID),            
            add_body(0, "\n"),
            add_body(Indent + ?INDENT_SPACE, "end, \n"),                   
            wait_statement(Indent, Wait_statement, NewFunID),  
            ok; 
        [{if_statement, If_statement, []}] ->
            if_statement(Indent, If_statement, FunID),
            ok;
        [{if_statement, If_statement, OtherStatements}] ->
            NewFunID = get_fun_id(),   
            set_tail_funid(NewFunID, get_tail_funid(FunID)),
            StrFun = io_lib:format("TailFun~w = \n", [NewFunID]),
            add_body(Indent, StrFun),
            StrFun2 = io_lib:format("fun() ->\n", []),
            add_body(Indent + ?INDENT_SPACE, StrFun2),
            statements(Indent + ?INDENT_SPACE * 2, OtherStatements, NewFunID),            
            add_body(0, "\n"),
            add_body(Indent + ?INDENT_SPACE, "end, \n"),  
            
            
            %NewFunID2 = get_fun_id(),   
            set_tail_funid(FunID, NewFunID), %%更新当前FunID的尾函数
            if_statement(Indent, If_statement, FunID),  
            ok; 
        [{while_statement, WhileStatements, []}] -> 
            {ok, CondFunID, LoopFunID} = while_statement(Indent, WhileStatements),
            StrWhile = io_lib:format("script_loop(CondFun~w, LoopFun~w)", [CondFunID, LoopFunID]),
            add_body(Indent + ?INDENT_SPACE, StrWhile),    
            ok;
        [{while_statement, WhileStatements, OtherStatements}] -> 
            {ok, CondFunID, LoopFunID} = while_statement(Indent, WhileStatements),
            StrWhile = io_lib:format("script_loop(CondFun~w, LoopFun~w),\n", [CondFunID, LoopFunID]),
            add_body(Indent + ?INDENT_SPACE, StrWhile),  
            statements(Indent, OtherStatements, FunID),
            ok; 
        _ ->
            ok
    end.

metascript(MetaScript) ->
    case MetaScript of
        {param, ParamList} ->
            Params = [Param || {_, Param} <- ParamList],
            add_script_source_element(0, Params, #script_source.param);
        _ ->
            none
    end,
    ok.

statement(Indent, Statement, FunID) ->
    case Statement of
        {express, Express} ->
            express(Indent, Express);
        {assignment, Var, Express} ->
            AssignStr = io_lib:format("~s = ", [Var]),
            add_body(Indent, AssignStr),
            express(0, Express);
        {assert, Atom, Express} ->
            AssignStr = io_lib:format("~w = ", [Atom]),
            add_body(Indent, AssignStr),
            express(0, Express)
    end,
    ok.
    
wait_statement(Indent, WaitStatement, FunId) ->
    case WaitStatement of
        {'WAIT', [Time]} ->
            case ?FUNCTION_MAP_MODULE:?FUNCTION_MAP_FUNCTION (wait) of
                {Module, _} ->                     
                    CurScriptId = get_cur_scriptid(),
                    Output = 
                        if FunId =/= undefined ->
                               io_lib:format("~w:~w(~w, ~w, TailFun~w)", [Module, wait, Time, CurScriptId, FunId]);
                           true ->
                               io_lib:format("~w:~w(~w, ~w, undefined)", [Module, wait, Time, CurScriptId])
                        end,
                    add_body(Indent, Output);
                _ ->
                    throw({"Not Define Function: ~w", [wait]})
            end;
        _ ->
            ok
    end.
     

if_statement(Indent, IfStatement, FunID) -> 
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
                   add_body(Indent, "case not (\n"), 
                   conditions(Indent + ?INDENT_SPACE, Conditions),
                   add_body(0, " )");
               true ->
                   add_body(Indent, "case \n"), 
                   conditions(Indent + ?INDENT_SPACE, Conditions)
            end,         
            add_body(0, " of\n"),          
            add_body(Indent + ?INDENT_SPACE, "true ->\n"),    %% true         
            statements(Indent + ?INDENT_SPACE * 2, TrueStatements, FunID), 
            add_body(0, ";\n"),
            
            add_body(Indent + ?INDENT_SPACE, "_ ->\n"),   
            
            case get_tail_funid(FunID) of
                TailFunID when is_integer(TailFunID) -> 
                    TailStr = io_lib:format("TailFun~w()\n", [TailFunID]),
                    add_body(Indent + ?INDENT_SPACE * 2, TailStr);
                undefined ->
                    add_body(Indent + ?INDENT_SPACE * 2, "ok\n")
            end,
            add_body(Indent, "end"),
            ok;
        {'IF', Conditions, TrueStatements, FalseStatements} -> 
            add_body(Indent, "case \n"), 
            conditions(Indent + ?INDENT_SPACE, Conditions),              
            add_body(0, " of \n"),          
            add_body(Indent + ?INDENT_SPACE, "true ->\n"),    %% true         
            statements(Indent + ?INDENT_SPACE * 2, TrueStatements, FunID), 
            add_body(0, ";\n"),
            add_body(Indent + ?INDENT_SPACE, "false ->\n"),    %% false         
            statements(Indent + ?INDENT_SPACE * 2, FalseStatements, FunID),  
            add_body(0, "\n"),             
            add_body(Indent, "end"),
            ok;
        _ ->
            ok
    end,    
    ok.
 
while_statement(Indent, WhileStatements) ->
    case WhileStatements of
        {'WHILE', Conditions, Statements} ->
             CondFunID = get_fun_id(),
             CondFunStr = io_lib:format("CondFun~w = \n", [CondFunID]),
             add_body(Indent + ?INDENT_SPACE, CondFunStr),
             add_body(Indent + ?INDENT_SPACE * 2 , "fun() ->\n"),
             conditions(Indent + ?INDENT_SPACE * 3, Conditions), 
             add_body(0, "\n"),
             add_body(Indent + ?INDENT_SPACE * 2, "end,\n"),
             
             LoopFunID = get_fun_id(),
             LoopFunStr = io_lib:format("LoopFun~w = \n", [LoopFunID]),
             add_body(Indent + ?INDENT_SPACE, LoopFunStr),
             add_body(Indent + ?INDENT_SPACE * 2 , "fun() ->\n"),
             statements(Indent + ?INDENT_SPACE * 3, Statements, ?DEFAULT_TAILFUNID), 
             add_body(0, "\n"),
             add_body(Indent + ?INDENT_SPACE *2, "end,\n"),
              
             {ok, CondFunID, LoopFunID};
        _ ->
            ok
    end.

shift_statement(Statements, FunId) ->
    add_function(FunId, Statements),
    ok.

add_function(Funid, Statements) ->
    case get(script_source) of
        undefined ->
            erlang:put(script_source, #script_source{functions = [{Funid, Statements}]}),
            ok;
        ScriptSource ->
            #script_source{functions = Funs} = ScriptSource,
            NewScriptSource = ScriptSource#script_source{functions =[{Funid, Statements} | Funs]},
            put(script_source, NewScriptSource),
            ok
    end.



conditions(Indent, Conditions) ->
    case Conditions of
        {condition, Condition} ->
            condition(Indent, Condition),
            ok;
        {Condition, Logic, OtherConditions} ->
            condition(Indent, Condition),
            logic(0, Logic),
            add_body(0, "\n"),
            conditions(Indent, OtherConditions),
            ok;
        _ ->
            io:format("", [])
    end,
    ok.

condition(Indent, Condition) ->
    case Condition of
        {express, Express} ->
            express(Indent, Express),
            ok;
        {express, Express, Compare, OtherCondition} ->
            express(Indent, Express),
            compare(0, Compare),
            condition(0, OtherCondition),
            ok;
        {function, Function} ->
            function(Indent, Function),
            ok;
        {function, Function, Compare, OtherCondition} ->
            function(Indent, Function),
            compare(0, Compare),
            condition(0, OtherCondition),
            ok;
        _ ->
            ok
    end,
    ok.

%%遇到终结符时才有输出
logic(Indent, Logic) ->
    case Logic of
        '&&' ->
            add_body(Indent, " andalso"),
            ok;
        '||' ->
            add_body(Indent, " orelse"),
            ok;
        '!' ->            
            add_body(Indent, " not"),
            ok;
            
        _ ->
            dd
    end,
    ok.

compare(Indent, Compare) ->    
    case Compare of
        '>' ->
            add_body(Indent, " >"),
            ok;
        '<' ->
            add_body(Indent, " <"),
            ok;
        '==' ->            
            add_body( Indent, " =:="),
            ok;   
        '!=' ->            
            add_body( Indent, " =/="),
            ok;            
        _ ->
            dd
    end, 
    ok.

arithmetic(Indent, Arithmetic) ->
    StrAtith = atom_to_list(Arithmetic),
    add_body(Indent, StrAtith),
    ok.

vars(Indent, Vars) ->
    StrVars = 
        case Vars of
            V when erlang:is_integer(V) orelse erlang:is_float(V) ->
                io_lib:format(" ~w ", [Vars]);
            _ ->
                io_lib:format(" ~s ", [Vars])
        end,
            
    add_body(Indent, StrVars),
    ok. 



express(Indent, Express) ->
    case Express of
        {vars, Vars} ->
            vars(Indent, Vars),
            ok;
        {atom, Atom} ->
            StrAtom = io_lib:format(" ~w ", [Atom]),
            add_body(Indent, StrAtom),
            ok;
        {function, Function} ->
            function(Indent, Function),
            ok;
        {Vars, Arithmetic,  OtherExpress} ->
            vars(0, Vars),
            arithmetic(0, Arithmetic),
            express(Indent, OtherExpress),
            ok;
        _ ->
            io:format("unknow statement function [~w]", [Express])
    end,
    ok.


function(Indent, Statement) ->
    case Statement of
        {func, FuncName, Args, Flag} -> 
            case ?FUNCTION_MAP_MODULE:?FUNCTION_MAP_FUNCTION(FuncName) of
                {Module, _} ->
                    Output = 
                        case Flag of
                            args ->
                                io_lib:format("~w:~w(", [Module, FuncName]);
                            list ->
                                io_lib:format("~w:~w([", [Module, FuncName])
                        end,
                    add_body(Indent, Output),
                    args(0, Args), 
                    case Flag of
                        args ->
                            add_body(0, ")");
                        list ->
                            add_body(0, "])")
                    end;
                _ ->
                    
                    Output = io_lib:format("~w(", [FuncName]),
                    add_body(Indent, Output),
                    args(0, Args),            
                    add_body(0, ")")
                    %throw({"Not Define Function", [FuncName]})
            end,
            ok;
        _ ->
            throw({"unknow statement function [~w]", [Statement]})
    end,
    ok.

args(_Indent, []) ->
    ok;
args(_Indent, [Arg]) ->
    arg(0, Arg),
    ok;
args(Indent, [Arg|OtherArgs]) ->
    arg(0, Arg),
    add_body(0, ","),
    args(Indent, OtherArgs),
    ok.

arg(Indent, Arg) ->
    case Arg of
        {function, FuncName} ->
            function(0, FuncName);
        {var, StrVar} ->
            Output = io_lib:format("~s", [StrVar]),
            add_body(Indent, Output),
            ok;
        Arg ->                    
            Output = io_lib:format("~w", [Arg]),
            add_body(Indent, Output),
            ok
    end,
    ok.

get_cur_scriptid() ->
    case get(cur_scriptid) of
        undefined ->
            0;
        Value ->
            Value
    end.

add_body_end(IndentNum, Message) ->
    add_body(IndentNum, Message, back).

add_body(IndentNum, Message) ->
    add_body(IndentNum, Message, front).

add_body(IndentNum, Message, Dir) ->
    add_script_source_element(IndentNum, Message, #script_source.body, Dir).

add_script_source_element(IndentNum, Message, ELementIndex) ->
    add_script_source_element(IndentNum, Message, ELementIndex, front).

add_script_file_element(IndentNum, Message, ELementIndex) ->
    ScriptFile = get(script_file),
    ElementList = erlang:element(ELementIndex, ScriptFile),
    NewElement = [{IndentNum, Message} | ElementList ],
    NewScriptFile = erlang:setelement(ELementIndex, ScriptFile, NewElement),
    put(script_file, NewScriptFile),
    ok.

add_script_source_element(IndentNum, Message, ELementIndex, Dir) ->
    ScriptFile = get(script_file),
    CurScriptID = get_cur_scriptid(),
    #script_file{sources = OrddictSource} = ScriptFile,
    case orddict:find(CurScriptID, OrddictSource) of
        error ->
            ScriptSource = #script_source{},
            NewScriptSource = erlang:setelement(ELementIndex, ScriptSource, [{IndentNum, Message}]),
            
            NewOrddictSource = orddict:store(CurScriptID, NewScriptSource, OrddictSource),
            NewSourceFile = ScriptFile#script_file{sources = NewOrddictSource},
            put(script_file, NewSourceFile),
            ok;
        {ok, ScriptSource} ->
            Element = erlang:element(ELementIndex, ScriptSource),
            
            NewElement =
                case Dir of
                    back ->
                        Element ++ [{IndentNum, Message}];
                    _ ->
                        [{IndentNum, Message} | Element]
                end,
            NewScriptSource = setelement(ELementIndex, ScriptSource, NewElement),
            NewOrddictSource = orddict:store(CurScriptID, NewScriptSource, OrddictSource),
            NewSourceFile = ScriptFile#script_file{sources = NewOrddictSource},
            put(script_file, NewSourceFile),
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

get_fun_id() ->
    case get(funid) of
        undefined ->
            put(funid, 1),
            1;
        Id ->
            NewId = Id + 1,
            put(funid, NewId),
            NewId
    end.

get_tail_funid(FunId) -> 
    CurScriptID = get_cur_scriptid(),
    ScriptFile = get(script_file),
    #script_file{sources = OrddictSources} = ScriptFile,
    case orddict:find(CurScriptID, OrddictSources) of
        {ok, #script_source{tailfun = TailFunDict}} ->            
            case dict:find(FunId, TailFunDict) of
                {ok, TailFunId} ->
                    TailFunId;
                error ->
                    undefined
            end;
        _ ->
           undefined
    end.

set_tail_funid(FunId, TailFunId) ->
    CurScriptID = get_cur_scriptid(),
    ScriptFile = get(script_file),
    #script_file{sources = OrddictSources} = ScriptFile,
    case orddict:find(CurScriptID, OrddictSources) of
        {ok, #script_source{tailfun = TailFunDict} = ScriptSource} ->  
            NewTailFunDict = dict:store(FunId, TailFunId, TailFunDict),
            NewScriptSource = ScriptSource#script_source{tailfun = NewTailFunDict},
            NewOrddictSource = orddict:store(CurScriptID, NewScriptSource, OrddictSources),
            NewScriptFile = ScriptFile#script_file{sources = NewOrddictSource},
            put(script_file, NewScriptFile);
        _ ->
           undefined
    end.
 
