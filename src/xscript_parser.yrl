%%非终结符
Nonterminals
scripts metascript statements statement if_statement while_statement function wait_function args arg expresses express conditions condition compare vars tuple list match arithmetic logic.

%终结符
Terminals '+' '-' '*' '/' '.' '=' ':' '&&' '||' '!' '>' '<' '==' '!=' ';' ',' '(' ')' '[' ']' '{' '}' 'if' 'else' 'while' 'wait' 'script' 'return' atom integer float var.

Rootsymbol scripts.

%%语法分析器
%scripts -> '$empty' : [].
scripts -> statements : '$1'.

%%段落分析 
statements -> '$empty' : []. 
statements -> metascript '.' statements : [{metascript,'$1'} |'$3'].         %%脚本参数定义
statements -> wait_function ';' statements : [{wait_function, '$1', '$3'}].  %%异步等待子句
statements -> if_statement statements : [{if_statement, '$1', '$2'}].        %%IF子句
statements -> while_statement statements : [{while_statement, '$1', '$2'}].  %%WHILE子句
statements -> statement ';' statements : [{statement, '$1'} | '$3'].         %%普通子句
    
%%参数
metascript -> '-' 'script' '(' '[' args ']' ')'  :{param, '$5'}.   %%支持参数
 
%%语句分析
%%IF子句 
if_statement -> 'if' '(' conditions ')' '{'  statements '}' : {'IF', '$3', '$6'}. 
if_statement -> 'if' '(' conditions ')' '{' statements '}' 'else' '{' statements '}' :  {'IF','$3','$6','$10'}.

while_statement -> 'while' '(' conditions ')' '{' statements '}' : {'WHILE', '$3', '$6'}.

%%wait子句
wait_function -> 'wait' '(' args  ')' : {'WAIT', '$3'}.

%%语句
statement -> expresses  : {expresses, '$1'}.
statement -> match '=' expresses : {match, '$1', '$3'}. 
statement -> 'return' arg : {return, '$2'}.  %%支持脚本返回值 

%%条件分析
conditions -> condition : {condition, '$1'}.
conditions -> condition logic conditions : {'$1','$2','$3'}.

condition -> expresses : {expresses, '$1'}.
condition -> expresses compare condition: {expresses, '$1', '$2', '$3'}.

%%表达式
%Expressions only contain identifiers, literals and operators, where operators include arithmetic and boolean operators, 
%the function call operator () the subscription operator [] and similar, and can be reduced to some kind of "value"
expresses -> express : {express, '$1'}. 
expresses -> express arithmetic expresses : {operation, '$1', '$2', '$3'}.
  
express -> vars : {vars, '$1'}.
express -> atom : {atom, unwrap('$1')}.
express -> function : {function, '$1'}.
express -> '(' expresses ')' : {priority, '$2'}.%%支持优先级 
   
%%函数
function -> atom '(' args ')' : {func, unwrap('$1'), '$3'}.  
function -> atom ':' atom '(' args ')' : {func, unwrap('$1'),  unwrap('$3'), '$5'}.   %%支持命名空间

%%匹配
match -> tuple : {tuple, '$1'}.
match -> list : {list, '$1'}.
match -> atom : {assert, unwrap('$1')}.
match -> vars : {assignment, '$1'}.

%%元组
tuple -> '{' args '}': {element, '$2'}.

%%列表
list -> '[' args ']': {element, '$2'}.

%%参数
args -> '$empty' : [].
args -> arg : ['$1']. 
args -> arg ',' args : ['$1'| '$3'].
 
arg -> expresses : {expresses, '$1'}.
arg -> tuple : {tuple, '$1'}.   %%参数支持元组
arg -> list : {list, '$1'}.     %%支持列表

%%变量
vars -> integer : unwrap('$1').
vars -> float : unwrap('$1').
vars -> var : unwrap('$1').

%%逻辑运算符
logic -> '&&' : unwrap('$1').
logic -> '||' : unwrap('$1').
logic -> '!' : unwrap('$1').


%%比较运算符
compare -> '>' : unwrap('$1').
compare -> '<' : unwrap('$1').
compare -> '==' : unwrap('$1').
compare -> '!=' : unwrap('$1').

%%算术运算符
arithmetic -> '+' : unwrap('$1').
arithmetic -> '-' : unwrap('$1').
arithmetic -> '*' : unwrap('$1').
arithmetic -> '/' : unwrap('$1').


Erlang code.
unwrap({_,_,V}) -> V;
unwrap({V,_}) -> V. 
