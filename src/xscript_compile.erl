%% @author rongjie
%% @time 2015-01-01
%% @doc 脚本生成工具 TODO


-module(xscript_compile).

%% ====================================================================
%% API functions
%% ====================================================================
-export([generate_file/1, generate_file/2]).
-export([gen_script/1]).
-export([comment/1]).
-export([get_all_define_functions/1]).
-include("xscript_compile.hrl").

-define(INDENT_SPACE, 4).
-define(SINGLE_STATEMENT, -1).
-define(DEFAULT_FUNID, 0).
-define(DEFAULT_TAILFUNID, 0).


%%代码生成保存
-record(script_file, {sources = orddict:new(),
    defined_function = [], %%已定义的函数
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


-export([test/0, test1/0, test2/0, test3/0, test4/0, test5/0, test6/0, test7/0, test_one/0, test_dir/0]).
test() ->
    test1(),
    test2(),
    test3(),
    test4(),

    ok.

test1() ->
    generate_file("./script/1.seq.script").
test2() ->
    generate_file("./script/2.if.script").
test3() ->
    generate_file("./script/3.loop.script").
test4() ->
    generate_file("./script/4.测试.script").
test5() ->
    generate_file("./script/5.prarm.script").
test6() ->
    generate_file("./script/6.localfun本地函数.script").
test7() ->
    generate_file("./script/7.erlang.script").

test_one() ->
    generate_files(["./script/1.seq.script", "./script/2.if.script", "./script/3.loop.script"], [{out_dir, "./script/"}]).

test_dir() ->
    todo.
%%     gen_script("./script/", [{out_dir, "./script/"}]).

generate_file(ProtoFile) ->
    {ok, Options} = file:consult("./include/xscript.config"),
    generate_file(ProtoFile, Options).

get_output_filename(Options) ->
    proplists:get_value(out_file, Options, ?DEFAULT_OUTPUT_FILE).
%%Options:[{key, value}]
%%{one_output, true|false} : 是否输出一个文件
%%{out_dir, OutDir} : 输出目录 
generate_file(ScriptFile, Options) ->
    generate_files([ScriptFile], Options).
generate_files(ScriptFiles, Options) when is_list(ScriptFiles) ->
    erlang:erase(),
    IsAllInOne = proplists:get_bool(one_output, Options),

    put(script_file, #script_file{defined_function = get_all_define_functions(Options)}),

    {template, Template} = proplists:lookup(template, Options),

    case IsAllInOne of
        true ->
            OutDir = proplists:get_value(dst_dir, Options, "."),
            {mod_name, Mod} = proplists:lookup(mod_name, Options),
            OutputFile = OutDir ++ "/" ++ atom_to_list(Mod) ++ ".erl",

            GenScriptFun =
                fun(File) ->
                    erlang:erase(cur_scriptid),
                    erlang:erase(script_source),

                    ScriptId = rootname(File),
                    put(cur_scriptid, list_to_integer(ScriptId)),%%设置当前处理的Script

                    {ok, String} = parse_file(File),   %%词法分析

                        %%语法分析,得到语法树
                    FirstParsed =
                        case xscript_parser:parse(String) of
                                {ok, Parsed}  ->
                                    Parsed;
                            Error ->
                                    ErrStr = io:format("Script:~ts parse error:~n~p", [ScriptId, Error]),
                                    throw({parse_script_error, ErrStr})
                            end,


                    generate_commnet(File),  %%生成注释

                    generate_source(FirstParsed),  %%分析语法树,生成目标代码
                    generate_script_execute(), %%生成函数头
                    ok
                end,
            lists:foreach(fun(File) ->
                case GenScriptFun(File) of
                    ok ->
                        ok;
                    Err ->
                        Str = io_lib:format("File : ~ts~nError : ~p~n", [File, Err]),
                        CharData = unicode:characters_to_binary(Str),
                        io:put_chars(CharData)
                end
            end, ScriptFiles),

            generate_default_funtion(),

            generate_header(Mod, Template),

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
                Basename = "script_" ++ ScriptId,
                OutputFile = OutDir ++ "/" ++ Basename ++ ".erl",
                filelib:ensure_dir(OutputFile),

                {ok, FileRef} = xscript_file:open(OutputFile, [write]),
                generate_header(list_to_atom(Basename), Template),

                {ok, String} = parse_file(File),   %%词法分析
                {ok, FirstParsed} = xscript_parser:parse(String),
                put(script_source, #script_source{scriptid = ScriptId}),
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
gen_script(Cfg) ->
    {src_dir, Dirname} = proplists:lookup(src_dir, Cfg),
    Files = [Dirname ++ X || X <- filelib:wildcard("*.script", Dirname)],
    generate_files(Files, Cfg),
    ok.

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
        #script_source{scriptid = ScriptId} ->
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
                            [Param, "," | Acc]
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
        #script_source{functions = FunList} = ScriptSource ->
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

%% TODO
generate_header(Module, Template) ->
    Lines = [
        io_lib:format("%% autogen from ~p", [?MODULE_STRING]),
        io_lib:format("-module(~p).", [Module]),
        proplists:get_value(head, Template)
    ],
    add_script_file_element(0, string:join(Lines, "\n"), #script_file.header),
    ok.

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
    FunStr = "
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
    String = parse_file(InFile, []),
    file:close(InFile),
    {ok, String}.

%% @hidden
parse_file(InFile, Acc) ->
    case xscript_file:request(InFile) of
        {ok, Token, _EndLine} ->
            parse_file(InFile, Acc ++ [Token]);
        {error, token} ->
            exit(scanning_error);
        {eof, _} ->
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
            add_body(Indent, StrWhile),
            ok;
        [{while_statement, WhileStatements, OtherStatements}] ->
            {ok, CondFunID, LoopFunID} = while_statement(Indent, WhileStatements),
            StrWhile = io_lib:format("script_loop(CondFun~w, LoopFun~w),\n", [CondFunID, LoopFunID]),
            add_body(Indent, StrWhile),
            statements(Indent, OtherStatements, FunID),
            ok;
        _ ->
            ok
    end.

metascript(MetaScript) ->
    case MetaScript of
        {param, ParamList} ->
            %% TODO 需要根据语法定义来做
            Params = [Param || {_, {_, {_, Param}}} <- ParamList],
            add_script_source_element(0, Params, #script_source.param);
        _ ->
            none
    end,
    ok.

statement(Indent, Statement, FunID) ->
    case Statement of
        {expresses, Express} ->
            expresses(Indent, Express);
        {match, MatchLeft, Expresses} ->
            match(Indent, MatchLeft),
            AssignStr = io_lib:format(" = ", []),
            add_body(0, AssignStr),
            expresses(0, Expresses);
        {return, Arg} ->
            add_body(Indent, ""),
            arg(0, Arg)
    end,
    ok.

wait_statement(Indent, WaitStatement, FunId) ->
    case WaitStatement of
        {'WAIT', Args} ->
            case get_function_define(wait, 3) of
                {Module, _} ->
                    CurScriptId = get_cur_scriptid(),
                    Str1 = io_lib:format("~w:~w(", [Module, wait]),
                    add_body(Indent, Str1),
                    args(0, Args),
                    Output =
                        if FunId =/= undefined ->
                               io_lib:format(", ~w, TailFun~w)", [CurScriptId, FunId]);
                            true ->
                                io_lib:format(", ~w, undefined)", [CurScriptId])
                        end,                    
                    add_body(0, Output),
                    ok;
                _ ->
                    throw({not_defined_function, wait, 1})
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
            add_body(Indent, CondFunStr),
            add_body(Indent + ?INDENT_SPACE * 1, "fun() ->\n"),
            conditions(Indent + ?INDENT_SPACE * 2, Conditions),
            add_body(0, "\n"),
            add_body(Indent + ?INDENT_SPACE * 1, "end,\n"),

            LoopFunID = get_fun_id(),
            LoopFunStr = io_lib:format("LoopFun~w = \n", [LoopFunID]),
            add_body(Indent, LoopFunStr),
            add_body(Indent + ?INDENT_SPACE * 1, "fun() ->\n"),
            statements(Indent + ?INDENT_SPACE * 2, Statements, ?DEFAULT_TAILFUNID),
            add_body(0, "\n"),
            add_body(Indent + ?INDENT_SPACE * 1, "end,\n"),

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
            NewScriptSource = ScriptSource#script_source{functions = [{Funid, Statements} | Funs]},
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
        {expresses, Expresses} ->
            expresses(Indent, Expresses),
            ok;
        {expresses, Expresses, Compare, OtherCondition} ->
            expresses(Indent, Expresses),
            compare(0, Compare),
            condition(0, OtherCondition),
            ok;
        _ ->
            io:format("unhandle condition:~p~n", [Condition]),
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
            add_body(Indent, " =:="),
            ok;
        '!=' ->
            add_body(Indent, " =/="),
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

expresses(Indent, Expresses) ->
    case Expresses of
        {express, Express} ->
            express(Indent, Express),
            ok;
        {operation, OtherExpress1, Arithmetic, OtherExpresses2} ->
            express(Indent, OtherExpress1),
            arithmetic(0, Arithmetic),
            expresses(0, OtherExpresses2),
            ok
    end,
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
        {priority, OtherExpresses} ->
            add_body(Indent, "("),
            expresses(0, OtherExpresses),
            add_body(0, ")"),
            ok;
        _ ->
            io:format("unknow statement function [~w]", [Express])
    end,
    ok.

%%匹配
match(Indent, MatchLeft) ->
    case MatchLeft of
        {tuple, Tuple} ->
            tuple(Indent, Tuple);
        {list, List} ->
            list(Indent, List);
        {assignment, Var} ->
            AssignStr = io_lib:format("~s ", [Var]),
            add_body(Indent, AssignStr);
        {assert, Atom} ->
            AssignStr = io_lib:format("~w  ", [Atom]),
            add_body(Indent, AssignStr)
    end,
    ok.

function(Indent, Statement) ->
    case Statement of
        {func, FuncName, Args} ->
            case get_function_define(FuncName, length(Args)) of
                false ->
                    throw({not_defined_function, FuncName, length(Args)});
                {Module, _} ->
                    Output = io_lib:format("~w:~w(", [Module, FuncName]),
                    add_body(Indent, Output),
                    args(0, Args),
                    add_body(0, ")");
                _ ->
                    %%ToDo : 本地函数
                    Output = io_lib:format("~w(", [FuncName]),
                    add_body(Indent, Output),
                    args(0, Args),
                    add_body(0, ")")
            %throw({"Not Define Function", [FuncName]})
            end,
            ok;
        {func, ModuleName, FuncName, Args} ->
            Output = io_lib:format("~w:~w(", [ModuleName, FuncName]),
            add_body(Indent, Output),
            args(0, Args),
            add_body(0, ")"),
            ok;
        _ ->
            throw({"unknow statement function [~w]", [Statement]})
    end,
    ok.

tuple(Indent, Tuple) ->
    add_body(Indent, "{"),
    case Tuple of
        {element, Args} ->
            args(Indent, Args)
    end,
    add_body(0, "}").


list(Indent, Tuple) ->
    add_body(Indent, "["),
    case Tuple of
        {element, Args} ->
            args(Indent, Args)
    end,
    add_body(0, "]").

args(_Indent, []) ->
    ok;
args(_Indent, [Arg]) ->
    arg(0, Arg),
    ok;
args(Indent, [Arg | OtherArgs]) ->
    arg(0, Arg),
    add_body(0, ","),
    args(0, OtherArgs),
    ok.

arg(Indent, Arg) ->
    case Arg of
        {expresses, Express} ->
            expresses(0, Express);
        {list, List} ->
            list(Indent, List),
            ok;
        {tuple, Tuple} ->
            tuple(Indent, Tuple)
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
    NewElement = [{IndentNum, Message} | ElementList],
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

%% 函数映射模块 TODO 配置化
get_script_modules() ->
    case ?FUNC_DEFINE_TYPE of
        ?FUNC_DEFINE_TYPE_MODULE ->
            ?FUNCTION_DEFINE_MODULE ++ ?FUNCTION_STD_MODULE;
        ?FUNC_DEFINE_TYPE_DIR ->
            Dir = ?FUNCTION_DEFINE_DIR,
            true = filelib:is_dir(Dir),
            FileNames = filelib:wildcard("*.erl", Dir),
            [list_to_atom(filename:basename(FileName, ".erl")) || FileName <- FileNames] ++ ?FUNCTION_STD_MODULE;
        _ ->
            throw(config_error)
    end.

-spec get_all_define_functions(proplists:proplist()) -> [{Mod :: atom(), {Fun :: atom(), Arity :: integer()}}].
%% @doc 拿到所有函数定义, 如果有函数重定义, 抛异常
get_all_define_functions(Cfg) ->
    {def_mods, DefindModuleList} = proplists:lookup(def_mods, Cfg),
    lists:foldl(fun(Module, Acc) ->
        ExportList = Module:module_info(exports),
        DefineList = [{Module, {FunName, Arity}} || {FunName, Arity} <- ExportList, FunName =/= module_info],
        case lists:any(fun({_Module, {FunName2, Argc2}}) ->
            case lists:keyfind({FunName2, Argc2}, 2, Acc) of
                false ->
                    false;
                Tuple ->
                    throw({ok, FunName2, Argc2, Tuple})
            end
        end, DefineList) of
            {ok, FunName, Argc, Tuple} ->
                io:format("Fun:~w:~w/~w, Already Default in ~p", [Module, FunName, Argc, Tuple]),
                throw({redefined, Module, FunName, Argc, Tuple});
            _ ->
                none
        end,
        DefineList ++ Acc
    end, [], DefindModuleList).

%%获得函数定义
get_function_define(FunName, Argc) ->
    case get(script_file) of
        #script_file{defined_function = DefinedFunction} ->
            lists:keyfind({FunName, Argc}, 2, DefinedFunction);
        _ ->
            false
    end.