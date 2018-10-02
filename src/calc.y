%{
#include <bits/stdc++.h>
extern FILE *yyin;
extern char* yytext;
extern "C" int yylex();
extern int yyparse();
extern FILE *yyin;
using namespace std;
extern FILE *yyin;
void yyerror(const char *s);

%}

%union{
  	int number;
	class Literal *lit;
	char* str;
}


%token PLUS
%token MINUS
%token MUL
%token DIV
%token MOD
%token GT
%token LT
%token GE
%token LE
%token NE
%token EE
%token AND
%token OR
%token EXCLAMATION
%token <str> ID
%token INT_LITERAL
%token CHAR
%token BOOL
%token '['
%token CALLOUT
%token STRING
%token FOR
%token IF
%token BREAK
%token CONTINUE
%token EQUALS
%token MEQUALS
%token PEQUALS
%token RETURN 
%token INT
%token BOOl
%token ELSE
%token PROGRAM
%token TYPE
%token VOID
%token CLASS
%token ']'

%left EE NE
%left AND OR
%left LT GT LE GE
%left PLUS MINUS
%left MUL DIV MOD
%right EXCLAMATION '='


%%


program : CLASS  PROGRAM '{' field_decls method_decls '}'  
		| CLASS  PROGRAM '{' field_decls '}' 
		| CLASS  PROGRAM '{' method_decls '}' 
		| CLASS  PROGRAM '{' '}' 

field_decls : field_decl 
			| field_decls field_decl 

field_decl : TYPE vars_decla ';' 

vars_decla : var_decla 
		  | vars_decla ',' var_decla 

var_decla : ID '[' INT_LITERAL ']' 
          | ID 

method_decls : method_decl 
			 | method_decls method_decl 

method_decl : TYPE ID '(' method_args ')' block 
			| TYPE ID '(' ')' block
			| VOID ID '(' method_args ')' block 
			| VOID ID '(' ')' block 

method_args : method_arg 
			| method_args ',' method_arg 

method_arg : TYPE ID 

block: '{' var_decl_list '}' 
     |  '{' var_decl_list statement_list '}' 
     | '{' '}'  
     |  '{' statement_list '}'  

var_decl_list : var_decl 
			  | var_decl_list var_decl 

var_decl : TYPE id_list ';' 
 	
id_list: ID 
	   | id_list ID 

statement_list : statement 
			   | statement_list statement 

statement : assignment 
		  | if_for 
		  | method_call ';'
		  | RETURN ';' 
		  | RETURN expr ';' 
		  | BREAK ';'  
		  | CONTINUE ';' 
		  | block 

assignment : location EQUALS expr ';' 
		   | location PEQUALS expr ';' 
		   | location MEQUALS expr ';' 

if_for :  IF '(' expr ')' block
		| IF '(' expr ')' block ELSE block 
		| FOR ID '=' expr ',' expr block 

expr : location 
	 | method_call 
	 | literal
	 | expr PLUS expr 
	 | expr MINUS expr 
	 | expr MUL expr 
	 | expr DIV expr 
	 | expr MOD expr 
	 | expr GT expr 
	 | expr LT expr 
	 | expr GE expr 
	 | expr LE expr 
	 | expr EE expr 
	 | expr NE expr 
	 | expr AND expr 
	 | expr OR expr 
	 | MINUS expr 
	 | EXCLAMATION expr 
	 | '(' expr ')' 

location : ID 
		 | ID '[' expr ']' 

method_call: ID '(' expressions ')' 
		   | ID '(' ')' 
		   | CALLOUT '(' STRING ')' 
		   | CALLOUT '(' STRING callout_args ')' 

expressions: expr 
			| expressions ',' expr 

callout_args: ',' callout_arg 
			| callout_args ',' callout_arg 

callout_arg: expr 
		   | STRING 

literal: INT_LITERAL 
		| CHAR 
		| BOOL 

%%

main(int argc, char **argv)
{
	printf("Starting to parse\n");

	yyparse();
	printf("Parsing over");
}

void yyerror(const char *s)
{
	fprintf(stderr, "error: %s\n", s);
}