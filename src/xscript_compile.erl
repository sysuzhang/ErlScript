%% @author rongjie
%% @time 2015-01-01
%% @doc @todo Add description to xscript_compile.


-module(xscript_compile).

%% ====================================================================
%% API functions
%% ====================================================================
-export([generate_file/1, generate_file/2]).
-export([generate_dir/1, generate_dir/2]).

-define(INDENT_SPACE, 4).


-export([test/0, test1/0, test2/0, test3/0, test4/0]).
test() ->
    test1(),
    test2(),
    test3(),
    test4(),
    ok.
    
test1() ->
    generate_file("./script/test1.seq.script", []).
test2() ->
    generate_file("./script/test2.if.script", [{out_dir, "./script/"}]).
test3() ->
    generate_file("./script/test3.loop.script", [{out_dir, "./script/"}]).
test4() ->
    generate_file("./script/test4.测试.script", [{out_dir, "./script/"}]).

test_dir() ->
    generate_dir("./script/", []).

generate_file(ProtoFile) ->
  generate_file(ProtoFile,[]).


generate_file(ScriptFile,Options) when is_atom(ScriptFile) ->
    generate_file(atom_to_list(ScriptFile) ++ ".script", Options);
generate_file(ScriptFile,Options) when is_list(ScriptFile) ->
    Rootname = rootname(ScriptFile),
    DefaultOutDir = filename:dirname(ScriptFile),
    Basename = "script_" ++ Rootname , 
    {ok,String} = parse_file(ScriptFile),   %%词法分析
    {ok,FirstParsed} = xscript_parser:parse(String),
    output_source(Basename, FirstParsed, Options, DefaultOutDir),
    ok.


generate_dir(Dirname) ->
    ok.

generate_dir(Dirname, Options) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


%% @hidden

rootname(Filename) ->  
    case filename:rootname(Filename) of
        Filename ->
            filename:basename(Filename);
        Rootname ->
            rootname(Rootname)
    end.


output_source(Basename, Scripts, Options, DefaultOutDir) ->
    OutDir = 
        case proplists:get_value(out_dir, Options) of
            undefined ->
                DefaultOutDir;
            Dir ->
                filename:dirname(Dir)
        end,
    OutputFile = OutDir ++ "/" ++ Basename ++ ".erl",
    filelib:ensure_dir(OutputFile),
    {ok, FileRef} = xscript_file:open(OutputFile, [write]),    
    output_header(FileRef, Basename),
    Output = io_lib:format("execute() ->\n", []),
    gen_output(FileRef, 0, Output),
    scripts(FileRef, ?INDENT_SPACE, Scripts),   
    gen_output(FileRef, 0, "."),
    xscript_file:close(FileRef),
    ok.

output_header(FileRef, Module) ->
    Header = io_lib:format("%%自动生成,请不要修改\n%%@datetime:{~w~w}\n-module(~s).\n\n-export([execute/0]).\n\n", 
                           [erlang:date(), erlang:time(), Module]), 
    gen_output(FileRef, 0, Header),
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
    statements(FileRef, Indent, Statements).


statements(FileRef, Indent, [Statement]) ->
     statement(FileRef, Indent, Statement);
statements(FileRef, Indent,[Statement | Statements]) ->
    statement(FileRef, Indent, Statement),
    gen_output(FileRef, 0, ",\n"),
    statements(FileRef, Indent, Statements).
    
statement(FileRef, Indent, Statement) ->
    case Statement of 
        {function, Func} ->
            function(FileRef, Indent, Func), 
            ok;
        {'IF', Conditions, TrueStatements} ->
            gen_output(FileRef, Indent, "case \n"), 
            conditions(FileRef, Indent + ?INDENT_SPACE, Conditions),            
            gen_output(FileRef, 0, " of\n"),          
            gen_output(FileRef, Indent + ?INDENT_SPACE, "true ->\n"),    %% true         
            statements(FileRef, Indent + ?INDENT_SPACE * 2, TrueStatements),
            statements(FileRef, 0, ";\n"),
            
            gen_output(FileRef, Indent + ?INDENT_SPACE, "_ ->\n"),     
            gen_output(FileRef, Indent + ?INDENT_SPACE * 2, "ok\n"),     
            gen_output(FileRef, Indent, "end"),
            ok;
        {'IF', Conditions, TrueStatements, FalseStatements} ->  
            gen_output(FileRef, Indent, "case \n"), 
            conditions(FileRef, Indent + ?INDENT_SPACE, Conditions),            
            gen_output(FileRef, 0, " of \n"),          
            gen_output(FileRef, Indent + ?INDENT_SPACE, "true ->\n"),    %% true         
            statements(FileRef, Indent + ?INDENT_SPACE * 2, TrueStatements),
            gen_output(FileRef, 0, ";\n"),
            gen_output(FileRef, Indent + ?INDENT_SPACE, "false ->\n"),    %% false         
            statements(FileRef, Indent + ?INDENT_SPACE * 2, FalseStatements),    
            gen_output(FileRef, 0, "\n"),             
            gen_output(FileRef, Indent, "end"),
            ok;
        {'WHILE', Conditions, TrueStatements} ->
            %%while的等价式
            gen_output(FileRef, Indent, "xscript_utility:while(\n"),
            gen_output(FileRef, Indent + ?INDENT_SPACE, "fun() ->\n"), 
            conditions(FileRef, Indent + ?INDENT_SPACE * 2, Conditions),   
            gen_output(FileRef, 0, "\n"),      
            gen_output(FileRef, Indent + ?INDENT_SPACE, "end,\n"),      
            gen_output(FileRef, Indent + ?INDENT_SPACE, "fun() ->\n"),    %% true 
            statements(FileRef, Indent + ?INDENT_SPACE * 2, TrueStatements),
            gen_output(FileRef, 0, "\n"),             
            gen_output(FileRef, Indent + ?INDENT_SPACE, "end)"),             
            ok;
        {'WAIT', Conditions, Statements} ->
            ok;
        _ ->
            ok
    end.

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
    StrVars = io_lib:format(" ~w ", [Vars]),
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
                    throw({"Not Define Function", [FuncName]})
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