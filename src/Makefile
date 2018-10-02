calc: calc.y calc.l
		bison -d calc.y -v
		flex calc.l 
		g++ -std=c++11 -o calc calc.tab.c lex.yy.c -ll

clean: 
		rm *.tab.h calc.tab.c lex.yy.c calc 
