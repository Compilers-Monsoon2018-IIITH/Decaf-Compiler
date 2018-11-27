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
The grammar writtem does not have any shift/reduce or reduce/reduce conflicts. The main code is in parser.ypp file. In this main function we first do yyparse(). In yyparse() the creation of AST, checking semantic errors. After that, dfs is done whose code is written in ast.h. Dfs is done using the visitor design pattern. Then after, LLVM-IR code is generated using generate code methods written for various classes. The LLVM-IR output is written in outputfile. We can lli interpreter to run the outputfile. Run-time checking is done, as we haven't done interpreter code evaluation.   
