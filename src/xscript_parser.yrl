%%非终结符
Nonterminals
scripts statements statement function args arg express conditions condition compare vars arithmetic logic.

%终结符
Terminals '+' '-' '*' '/' '&&' '||' '!' '>' '<' '==' ';' ',' '(' ')' '{' '}' 'IF' 'ELSE' 'WHILE'  atom integer float var.

Rootsymbol scripts.

%%语法分析器
scripts -> '$empty' : [].
scripts -> statements : '$1'.

%%段落分析
statements -> statement : ['$1'].
statements -> statement statements : ['$1' | '$2'].


%%语句分析
statement -> function ';' :  {function, '$1'}. 
statement -> 'IF' '(' conditions ')' '{' statements '}' : {'IF', '$3', '$5'}.
statement -> 'IF' '(' conditions ')' '{' statements '}' 'ELSE' '{' statements '}' :  {'IF','$3','$6','$10'}.
statement -> 'WHILE' '(' conditions ')' '{' statements '}' : {'WHILE', '$3', '$6'}.

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
