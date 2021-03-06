Name        : Parul Sangwan

Roll Number : 111801053

The compiler works as follows. The Makefile for the tiger compiler is in the root folder itself.


```$ make all```

Makes the executable name *tc*. The compiler can then be used as ```./tc [option] filename [option]```.

There are 4 options available : 
- --pp  : pretty printing the code on the console
- --ast : for displaying the AST on the console
- --ir   : printing the Tree IR
- --can   : printing the Canonised Tree

The tests folder contains 5 test cases, to test the compiler.

```$ make tests_ast```

This command will run all the test cases, one at a time, and will print the AST on the console.

```$ make test1_ast```

This command will print the AST for test case1. Similarly for test2 ```$ make test2_ast```and test3 ```$ make test3_ast```.


```$ make tests_pp```

This command will run all the test cases, one at a time, and will pretty print the tiger code on the console.

```$ make test1_p```

This command will print the AST for test case1. Similarly for test2 ```$ make test2_p```and test3 ```$ make test3_p```.

```$ make clean```

Cleans the newly created files.