%{
#include <stdio.h>	
%}

%token ID
%left '+' '-'
%left '*' '/'

%%

expr : '(' expr ')'
	 |  expr '+' expr
	 |  expr '-' expr
	 |  expr '*' expr
	 |  expr '/' expr
	 |  ID
	 ;	

%%

main(int argc, char **argv)
{
	yyparse();
	printf("Parsing Over\n");
}

yyerror(char *s)
{
	fprintf(stderr, "error: %s\n", s);
}