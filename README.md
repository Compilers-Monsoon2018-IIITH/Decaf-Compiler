# Decaf Compiler
## File Structure
- src
  - Makefile : 
    - use "make" to run
  - calc.y : 
    - Reading the code from a file. Creating nodes of AST tree
  - calc.l : 
    - Return tokens one by one to the parser file
  - ast.h : 
    - contains all the class and method declarations.
    - Has code for traversing the AST tree using vsitor design pattern
  - ast.cpp
    - All methods are written here.
    - generate code functions were used for creating LLVM-IR
- test-programs

  
## How to run
```
- make
- ./calc inputfile
- lli-3.8 outfile
```

## Description
The main parser code is in calc.y file. The various stages in the pipeline are lexical analysis, AST construction, IR generation. The written grammar is free of any shift-reduce conflicts. It has been tested on the programs in the test set. 
