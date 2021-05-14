
**CONCERN IN GRADING**

Sir, in the beginning of the course, It was mentioned that we don't need to include all the specifications mentioned in Epita and that our compiler would be simple integer-based. This is evident from one of the issues that were resolved too.
Now sir, actually what has happened is that, because no specific specifications were given, I tried to keep strings as simple ones consisting of only alphabets and comments not consisting of quotes (This lead to failure of major test cases). I had commited all the assignments well on time.


But as soon as test cases were released, I inculcated these too in my code.
Sir, now, the issue is that the test cases were released on 21 April, and now if I submit the hash corresponding to that week, I will face some heavy late submission penalties(this might reduce my marks even further). Only because of the absence of a lex rule to include characters like "." and "-" in strings have resulted in the failure of the major test cases (the last 4 test cases), which single-handedly has resulted in a huge depreciation of marks just because of one small thing(which cannot be even considered a mistake)


Sir, It would be very helpful if you can please take my request into consideration. I am not saying that you give me full marks but if you could reduce the penalty for my case, it would be really very helpful.
Sir, my commit after making these modifications is dated, 22 April 2021, 
commit hash 84f9f94c3f705580c9dde9ac15d90b50eeefd353 
Can be compiled as ```make all``` && ```./tc filename --pp``` for printing the pretty printer and ```./tc filename --ast``` for the ast.


Sir, I would request you to re-evaluate my pretty printer at the least with the above hash, and not ast in case there is heavy penalty being imposed on AST, which is depreciating my marks even further. (In case, the penalty is eased for my case please consider checking the ast also with the same above mentioned commit hash). The outputs are there in folders ```lab3-output``` and ```lab4-output```.


2021-02-17
- Made a compiler that prints "hello world"

2021-02-24
- Added support for () and / for Reverse Polish Compiler

2021-03-03
- Made AST for MIPS and Tiger Language

2021-03-10
- Made tiger.grm and tiger.lex files for Tiger Language

2021-03-17
- Tested and made some changes to the tiger.grm and tiger.lex files. Also, added a print function for printing AST.

2021-03-24
- Added file target/pp.sml for pretty printing

2021-03-31
- No Lab Assignment Given

2021-04-07
- Signature for Intermediate Trees

2021-04-14
- Read Canonisation

2021-04-21
- Extended support for CommandLine arguments before the filename. Support for commandLine Args following filename was already present before.
- Added Canonisation, only Linearise and Basic Blocks

2021-04-28
- Added Translation to TreeIR for BinOpExpr and IExpr
- Added environment for Variables

2021-05-05
- Added Translation to TreeIR for LValueExpr, SeqExpr, AssignExr, IfExpr, WhileExpr, ForExpr, BreakExpr and LetExpr.
- Connected Canonisation with TreeIR to represent Canonised TreeIR (only the Linearize function)
- Added file for printing the TreeIR, using --t option and its Canonised Version using --c option.