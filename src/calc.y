%{
#include <bits/stdc++.h>
#include "ast.h"
extern FILE *yyin;
extern char* yytext;
extern "C" int yylex();
extern int yyparse();
extern FILE *yyin;
using namespace std;
extern FILE *yyin;
void yyerror(const char *s);
class Program * root; 
%}

%union{
  	int number;
	class Literal *lit;
	char* str;
	class Callout_arg *call_arg;
	class Callout_args *call_args;
	class Expression * expresso;
	class Expressions * expressos;
	class Method_call * met_call;
	class Location * loc;
	class Block * blo;
	class Var_decl * var_dec;
	class Var_decl_list * var_dec_lis;
	class Statement_list * stat_list;
	class Statement * stat;
	class Assignment * assgn;
	class If_for * if_fo;
	class Id_list * id_li;
	class Program * pro;
	class Field_decls* fie_decs; 
	class Field_decl * fie_dec;
	class Vars_decla * var_decs;
	class Var_decla * var_deca;
	class Method_decls * met_decs;
	class Method_decl * met_dec;
	class Method_args * met_args;
	class Method_arg * met_arg;
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

%type <lit> literal
%type <call_arg> callout_arg
%type <call_args> callout_args
%type <expresso> expr
%type <expressos> expressions
%type <met_call> method_call
%type <loc> location
%type <blo> block
%type <var_dec_lis> var_decl_list
%type <var_dec> var_decl
%type <stat_list> statement_list
%type <stat> statement
%type <assgn> assignment
%type <if_fo> if_for
%type <id_li> id_list
%type <pro> program
%type <fie_decs> field_decls
%type <fie_dec> field_decl
%type <var_decs> vars_decla
%type <var_deca> var_decla
%type <met_decs> method_decls
%type <met_dec> method_decl
%type <met_args> method_args
%type <met_arg> method_arg

%%


program : CLASS  PROGRAM '{' field_decls method_decls '}'  {$$ = new Program($4, $5); root = $$; }
		| CLASS  PROGRAM '{' field_decls '}' {$$ = new Program($4, NULL); root = $$;}
		| CLASS  PROGRAM '{' method_decls '}' {$$ = new Program(NULL, $4); root = $$; }
		| CLASS  PROGRAM '{' '}' {$$ = new Program(NULL, NULL); root = $$; }

field_decls : field_decl {$$ = new Field_decls($1);}
			| field_decls field_decl { $$->pushback($2);}

field_decl : TYPE vars_decla ';' {$$ = new Field_decl(1,$2);}

vars_decla : var_decla {$$ = new Vars_decla($1);}
		  | vars_decla ',' var_decla {$$->pushback($3);}

var_decla : ID '[' INT_LITERAL ']' {$$ = new Var_decla(NULL,1);}
          | ID {$$ = new Var_decla(NULL,1);}

method_decls : method_decl { $$=new Method_decls($1); }
			 | method_decls method_decl { $$->pushback($2); }

method_decl : TYPE ID '(' method_args ')' block { $$ = new Method_decl(NULL, NULL, $4, $6);}
			| TYPE ID '(' ')' block { $$ = new Method_decl(NULL, NULL, NULL, $5); }
			| VOID ID '(' method_args ')' block { $$ = new Method_decl(NULL, NULL, $4, $6); }
			| VOID ID '(' ')' block { $$ = new Method_decl(NULL, NULL, NULL, $5);}

method_args : method_arg { $$ = new Method_args($1); }
			| method_args ',' method_arg { $$->pushback($3);}

method_arg : TYPE ID {$$ = new Method_arg(NULL, NULL);}

block: '{' var_decl_list '}' {$$ = new Block($2, NULL);}
     |  '{' var_decl_list statement_list '}' {$$ = new Block($2,$3);}
     | '{' '}'  {$$ = new Block(NULL,NULL);}
     |  '{' statement_list '}'  {$$ = new Block(NULL,$2);}

var_decl_list : var_decl {$$ = new Var_decl_list($1);}
			  | var_decl_list var_decl {$$->pushback($2);}

var_decl : TYPE id_list ';' {$$ = new Var_decl(1,$2);}
 	
id_list: ID {$$ = new Id_list(yylval.str);}
	   | id_list ID {$$->pushback(yylval.str);}

statement_list : statement {$$ = new Statement_list($1);}
			   | statement_list statement {$$->pushback($2);}

statement : assignment {$$ = new Statement(NULL,NULL,NULL, $1, NULL, NULL);}
		  | if_for {$$ = new Statement(NULL,NULL,NULL, NULL, $1, NULL);}
		  | method_call ';' {$$ = new Statement(NULL,NULL,NULL, NULL, NULL, $1);}
		  | RETURN ';' {$$ = new Statement("return",NULL,NULL, NULL, NULL, NULL);}
		  | RETURN expr ';' {$$ = new Statement("return",$2,NULL, NULL, NULL, NULL);}
		  | BREAK ';'  {$$ = new Statement("break",NULL,NULL, NULL, NULL, NULL);}
		  | CONTINUE ';' {$$ = new Statement("continue",NULL,NULL, NULL, NULL, NULL);}
		  | block {$$ = new Statement(NULL,NULL,$1, NULL, NULL, NULL);}

assignment : location EQUALS expr ';'  {$$ = new Assignment($1, $3, 1);}
		   | location PEQUALS expr ';' {$$ = new Assignment($1, $3, 1);}
		   | location MEQUALS expr ';' {$$ = new Assignment($1, $3, 1);}

if_for :  IF '(' expr ')' block {$$ = new If_for($5, NULL, $3, NULL);}
		| IF '(' expr ')' block ELSE block {$$ = new If_for($5, $7, $3, NULL);}
		| FOR ID '=' expr ',' expr block {$$ = new If_for($7, NULL, $4, $6);}

expr : location {$$ = new Expression(NULL, NULL, 0, NULL);}
	 | method_call { $$ = new Expression(NULL, NULL, 0, NULL); }
	 | literal { $$ = new Expression(NULL, NULL, 0, $1); }
	 | expr PLUS expr {$$ = new Expression($1, $3, 1, NULL); }
	 | expr MINUS expr { $$ = new Expression($1, $3, 1, NULL); }
	 | expr MUL expr { $$ = new Expression($1, $3, 1, NULL); }
	 | expr DIV expr { $$ = new Expression($1, $3, 1, NULL); }
	 | expr MOD expr { $$ = new Expression($1, $3, 1, NULL); }
	 | expr GT expr { $$ = new Expression($1, $3, 1, NULL);  }
	 | expr LT expr { $$ = new Expression($1, $3, 1, NULL); }
	 | expr GE expr { $$ = new Expression($1, $3, 1, NULL); }
	 | expr LE expr { $$ = new Expression($1, $3, 1, NULL); }
	 | expr EE expr { $$ = new Expression($1, $3, 1, NULL); }
	 | expr NE expr { $$ = new Expression($1, $3, 1, NULL); }
	 | expr AND expr {  $$ = new Expression($1, $3, 1, NULL); }
	 | expr OR expr { $$ = new Expression($1, $3, 1, NULL); }
	 | MINUS expr {$$ = new Expression($2, NULL, 1, NULL);}
	 | EXCLAMATION expr { $$ = new Expression($2, NULL, 1, NULL); }
	 | '(' expr ')' { $$ = new Expression($2, NULL, 0, NULL); }

location : ID {$$ = new Location(NULL, yylval.str);}
		 | ID '[' expr ']' {$$ = new Location($3, yylval.str);}

method_call: ID '(' expressions ')' {$$ = new Method_call($3, yylval.str, NULL, NULL);}
		   | ID '(' ')' {$$ = new Method_call(NULL, yylval.str, NULL, NULL);}
		   | CALLOUT '(' STRING ')' {$$ = new Method_call(NULL,NULL, NULL, yylval.str);}
		   | CALLOUT '(' STRING callout_args ')' {$$ = new Method_call(NULL,NULL, $4, yylval.str);}

expressions: expr {$$ = new Expressions($1);}
			| expressions ',' expr { $$->pushback($3);}

callout_args: ',' callout_arg {$$ = new Callout_args($2);}
			| callout_args ',' callout_arg {$$->pushback($3);}

callout_arg: expr {$$ = new Callout_arg($1, NULL);}
		   | STRING {$$ = new Callout_arg(NULL, yylval.str);}

literal: INT_LITERAL {$$ = new Literal(1,yylval.number); }
		| CHAR {$$ = new Literal(2,yylval.number);}
		| BOOL {$$ = new Literal(3,yylval.number);}

%%

main(int argc, char **argv)
{
	printf("Starting to parse\n");
	if(argc < 1)
    {
		fprintf(stderr, "Please specify a file name\n");
		exit(1);
	}
	yyin = fopen(argv[1], "r");
	yyparse();
	printf("End of parsing");
}

void yyerror(const char *s)
{
	fprintf(stderr, "error: %s\n", s);
}