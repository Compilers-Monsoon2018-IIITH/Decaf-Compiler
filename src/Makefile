calc: calc.y calc.l ast.h ast.cpp
		bison -d calc.y -v
		flex calc.l 
		g++ -std=c++11 -o calc calc.tab.c lex.yy.c ast.cpp -ll -g -ldl -lfl `llvm-config-3.8 --cppflags --libs --libfiles core --ldflags --system-libs`

clean: 
		rm *.tab.h calc.tab.c lex.yy.c calc 
