# ErlScript
DSL(领域特定语言) for erlang,  适用于erlang的脚本语言。<br/>
主要使用erlang的leex和yecc模块进行词法、语法分析，再转换成对应的erlang语言。

## 目录说明
script: 示例脚本以及生成的代码<br/>
src: 源代码<br/>

## 文件说明
xscript_scanner.xrl: 词法分析文件<br/>
xscript_parser.yrl: 语法分析文件<br/>
xscript_function_map.erl: 脚本函数定义映射<br/>
xscript_function_define.erl: 脚本函数实现模块<br/>
xscript_utility.erl：公共模块<br/>
xscript_file.erl: 文件操作模块<br/>
xscript_compile.erl: 脚本语言生成模块<br/>

### 注意:
1. 如果修改了xscript_scanner.xrl文件，可以使用leex:file("src/xscript_scanner.xrl").重新生成对应的xscript_scanner.erl文件<br/>
1. 如果修改了xscript_parser.yrl文件，可以使用yecc:file("src/xscript_parser.yrl").重新生成对应的xscript_parser.erl文件,为了正确生成对应的erlang代码，可能需要同步修改xscript_compile.erl文件中对应的生成规则。<br/>


## 使用说明
xscript_compile:generate_file(ScriptFilename, Options):<br/>
  ScriptFilename: 脚本文件名(包含相对于工作目录的相对路径)<br/>
  Options: 选项,当前支持: {out_dir, Dir}: 指定文件生成目录<br/>


## 示例example(对应的生成文件，见script目录):
        //example1: 
        
        create_monster();
        create_monster(1,500);
        create_monster(2,400);
        
        //example2: 
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
        
        //example3:
        while(random_find(1,num) > 0)
        {
             attack_target(100);
             create_monster(1,500);
        }

 

## 调用API:
script_脚本basename:execute().<br/>


## 扩展
尾函数(tail function): 如果一个函数在段落语句中是最后一个执行的函数，后面没有其他函数逻辑的函数。

结论：
1. 如果程序段落中存在阻塞操作，则阻塞操作可以通过异步调用尾函数的方式消除阻塞。

同步阻塞代码清除方法：
1. 将if子句转换为尾函数: if子句后的段落提取成if各条件分支的尾函数，则if子句变成整个段落的尾函数。<br/>
2. while子句本身看作一个尾函数<br/>
3. 将wait子句转换为尾函数： 将wait子句后的段落提取成一个尾函数，wait通过异步调用的方法调用该尾函数<br/>


## ToDo:
[planned] 扩展使用脚本虚拟机的方法清除阻塞

