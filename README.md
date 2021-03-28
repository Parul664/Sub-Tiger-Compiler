Name        : Parul Sangwan

Roll Number : 111801053

The compiler works as follows. The Makefile for the tiger compiler is in the main folder itself.


```$ make all```

Makes the executable name compile. The compiler can then be used as ```./compile filename options```.

There are 2 options available : 
- --p : pretty printing the code on the console
- --s : for displaying the AST on the console

The tests folder contains 3 test cases, 2 from Apple's book and one is a small program, to test the compiler.

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