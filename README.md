# XScript
DSL(领域特定语言) for erlang,  适用于erlang的脚本语言。

目录说明
script: 示例脚本以及生成的代码
src: 源代码

文件说明
xscript_scanner.xrl: 词法分析文件
xscript_parser.yrl: 语法分析文件
xscript_function_map.erl: 脚本函数定义映射
xscript_function_define.erl: 脚本函数实现模块
xscript_utility.erl：公共模块
xscript_file.erl: 文件操作模块
xscript_compile.erl: 脚本语言生成模块

注意:
1. 如果修改了xscript_scanner.xrl文件，可以使用leex:file("src/xscript_scanner.xrl").重新生成对应的xscript_scanner.erl文件
1. 如果修改了xscript_parser.yrl文件，可以使用yecc:file("src/xscript_parser.yrl").重新生成对应的xscript_parser.erl文件,为了正确生成对应的erlang代码，可能需要同步修改xscript_compile.erl文件中对应的生成规则。


使用说明
xscript_compile:generate_file(ScriptFilename, Options):
  ScriptFilename: 脚本文件名(包含相对于工作目录的相对路径)
  Options: 选项,当前支持: {out_dir, Dir}: 指定文件生成目录


示例example(对应的生成文件，见script):
example1: 
脚本:
create_monster();
create_monster(1,500);
create_monster(2,400);

example2: 
if(level() > 3)
{
      apply(1,skill,1204);
      find_target(300,222,12);
}
else
{
    apply(2,skill,1205); 
    find_target(enemy_scope(1,0,0,0,1000,1000));     
}

example3:
while(random_find(1,num) > 0)
{
     attack_target(100);
     create_monster(1,500);
}

调用API:
script_脚本basename:execute().



