%%非终结符
Nonterminals
scripts statements if_statement while_statement function wait_function args arg express conditions condition compare vars arithmetic logic.

%终结符
Terminals '+' '-' '*' '/' '&&' '||' '!' '>' '<' '==' ';' ',' '(' ')' '{' '}' 'IF' 'ELSE' 'WHILE' 'WAIT' atom integer float var.

Rootsymbol scripts.

%%语法分析器
scripts -> '$empty' : [].
scripts -> statements : '$1'.

%%段落分析 
statements -> wait_function ';' : {wait_function, '$1'}.
statements -> wait_function ';' statements : [{wait_function, '$1', '$3'}].  %%异步等待语句
statements -> if_statement : {if_statement, '$1'}.
statements -> if_statement statements : [{if_statement, '$1', '$2'}].        %%IF语句
statements -> while_statement : {while_statement, '$1'}.
statements -> while_statement statements : [{while_statement, '$1', '$2'}].  %%WHILE语句
statements -> function ';' : [{function, '$1'}].
statements -> function ';' statements: [{function, '$1'} | '$3'].

%statements -> statement statements : ['$1' | '$2']. 

%%语句分析
%%IF子句
if_statement -> 'IF' '(' conditions ')' '{'  '}' : {none}.
if_statement -> 'IF' '(' conditions ')' '{'  statements '}' : {'IF', '$3', '$6'}.
if_statement -> 'IF' '(' conditions ')' '{' statements '}' 'ELSE' '{'  '}' :  {'IF','$3','$6'}.
if_statement -> 'IF' '(' conditions ')' '{'  '}' 'ELSE' '{' statements '}' :  {'IF',{'not','$3'},'$9'}.
if_statement -> 'IF' '(' conditions ')' '{' statements '}' 'ELSE' '{' statements '}' :  {'IF','$3','$6','$10'}.

while_statement -> 'WHILE' '(' conditions ')' '{' statements '}' : {'WHILE', '$3', '$6'}.

%%wait子句
wait_function -> 'WAIT' '(' args  ')' : {'WAIT', '$3'}.

%%条件分析
conditions -> condition : {condition, '$1'}.
conditions -> condition logic conditions : {'$1','$2','$3'}.

condition -> express : {express, '$1'}.
condition -> express compare condition: {express, '$1', '$2', '$3'}.
condition -> function : {function, '$1'}.
condition -> function compare condition: {function, '$1', '$2', '$3'}.

%%表达式
express -> vars : {vars, '$1'}.
express -> vars arithmetic express : {'$1', '$2', '$3'}.

%%函数
function -> atom '(' args ')' : {func, unwrap('$1'), '$3'}. 

%%参数
args -> '$empty' : [].
args -> arg : ['$1']. 
args -> arg ',' args : ['$1'| '$3'].

arg -> var : unwrap('$1').
arg -> integer : unwrap('$1').
arg -> atom : unwrap('$1').
arg -> function : {function, '$1'}.

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

%%算术运算符
arithmetic -> '+' : unwrap('$1').
arithmetic -> '-' : unwrap('$1').
arithmetic -> '*' : unwrap('$1').
arithmetic -> '/' : unwrap('$1').


Erlang code.
unwrap({_,_,V}) -> V;
unwrap({V,_}) -> V.
