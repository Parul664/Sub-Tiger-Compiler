Name        : Parul Sangwan

Roll Number : 111801053

The compiler works as follows. The Makefile for the Tiger compiler is inside the folder 'tiger'.


```$ make all```

Makes the executable name compile. The compiler can then be used as ```./compile filename```.


The tests folder contains 3 test cases, 2 from Apple's book and one is a small program, to test the compiler.

```$ make tests```

This command will run all the test cases, one at a time, and prints the AST on the screen.

```$ make test1```

This command will run only test case1. Similarly for test2 ```$ make test2```and test3 ```$ make test3```.

```$ make clean```

Cleans the newly created files.