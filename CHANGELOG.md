

**Commands**

Making the Compiler   : ```make all```

Printing AST          : ```./tc filename --ast```

Pretty Printing       : ```./tc filename --pp```

IR Code Priting       : ```./tc filename --ir```

Canonised IR Printing : ```./tc filename --can```


- Without Support for functions

Commit Hash = 22220c1966a7a0c8f2e834497f3c30304efde255

Date of Commit = May 15, 2021

Commands are same as above



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

2021-05-20
- Added conversion from AST to Tree IR for functions also.
- Customised Error messages to show to the user.
- Modyfied the TreePrinting for specialised printing of sp, fp and rt.