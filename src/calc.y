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
%token <number> INT_LITERAL
%token <number> CHAR
%token <number> BOOL
%token '['
%token '{'
%token '}'
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
%token ELSE
%token PROGRAM
%token <str> TYPE
%token VOID
%token CLASS
%token ']'
%token ';'
%left EE NE
%left AND OR
%left LT GT LE GE
%left PLUS MINUS
%left MUL DIV MOD
%right EXCLAMATION '='
%token ','

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


program : CLASS  PROGRAM '{' field_decls method_decls '}'  { $$ = new Program($4, $5); root = $$; }
		| CLASS  PROGRAM '{' field_decls '}' {$$ = new Program($4, NULL); root = $$;}
		| CLASS  PROGRAM '{' method_decls '}' {$$ = new Program(NULL, $4); root = $$; }
		| CLASS  PROGRAM '{' '}' {$$ = new Program(NULL, NULL); root = $$; }

field_decls : field_decl {$$ = new Field_decls($1);}
			| field_decls field_decl { $$->pushback($2);}

field_decl : TYPE vars_decla ';' {$$ = new Field_decl(string($1),$2);}

vars_decla : var_decla {$$ = new Vars_decla($1);}
		  | vars_decla ',' var_decla {$$->pushback($3);}

var_decla : ID '[' INT_LITERAL ']' {$$ = new Var_decla(string($1),1, $3);}
          | ID {$$ = new Var_decla(string($1),0,0);}

method_decls : method_decl { $$=new Method_decls($1); }
			 | method_decls method_decl { $$->pushback($2); }

method_decl : TYPE ID '(' method_args ')' block { $$ = new Method_decl(string($1), string($2), $4, $6);}
			| TYPE ID '(' ')' block { $$ = new Method_decl(string($1), string($2), NULL, $5); }
			| VOID ID '(' method_args ')' block { $$ = new Method_decl("void", string($2), $4, $6); }
			| VOID ID '(' ')' block { $$ = new Method_decl("void", string($2), NULL, $5);}

method_args : method_arg { $$ = new Method_args($1); }
			| method_args ',' method_arg { $$->pushback($3);}

method_arg : TYPE ID {$$ = new Method_arg(string($1), string($2));}

block: '{' var_decl_list '}' {$$ = new Block($2, NULL);}
     |  '{' var_decl_list statement_list '}' {$$ = new Block($2,$3);}
     | '{' '}'  {$$ = new Block(NULL,NULL);}
     |  '{' statement_list '}'  {$$ = new Block(NULL,$2);}

var_decl_list : var_decl {$$ = new Var_decl_list($1);}
			  | var_decl_list var_decl {$$->pushback($2);}

var_decl : TYPE id_list ';' {$$ = new Var_decl(string($1),$2);}
 	
id_list: ID {$$ = new Id_list(string($1));}
	   | id_list ID {$$->pushback(string($2));}

statement_list : statement {$$ = new Statement_list($1);}
			   | statement_list statement {$$->pushback($2);}

statement : assignment {$$ = new Statement("assignment",NULL,NULL, $1, NULL, NULL);}
		  | if_for {$$ = new Statement("if_for",NULL,NULL, NULL, $1, NULL);}
		  | method_call ';' {$$ = new Statement("str",NULL,NULL, NULL, NULL, $1);}
		  | RETURN ';' {$$ = new Statement("return",NULL,NULL, NULL, NULL, NULL);}
		  | RETURN expr ';' {$$ = new Statement("return",$2,NULL, NULL, NULL, NULL);}
		  | BREAK ';'  {$$ = new Statement("break",NULL,NULL, NULL, NULL, NULL);}
		  | CONTINUE ';' {$$ = new Statement("continue",NULL,NULL, NULL, NULL, NULL);}
		  | block {$$ = new Statement("block",NULL,$1, NULL, NULL, NULL);}

assignment : location EQUALS expr ';'  {$$ = new Assignment($1, $3, 1);}
		   | location PEQUALS expr ';' {$$ = new Assignment($1, $3, 2);}
		   | location MEQUALS expr ';' {$$ = new Assignment($1, $3, 3);}

if_for :  IF '(' expr ')' block {$$ = new If_for($5, NULL, $3, NULL,"str");}
		| IF '(' expr ')' block ELSE block {$$ = new If_for($5, $7, $3, NULL,"str");}
		| FOR ID EQUALS expr ',' expr block {$$ = new If_for($7, NULL, $4, $6,string($2));}

expr : location {$$ = new Expression(NULL, NULL, 0, NULL);}
	 | method_call { $$ = new Expression(NULL, NULL, 1, NULL); }
	 | literal { $$ = new Expression(NULL, NULL, 2, $1); }
	 | expr PLUS expr {$$ = new Expression($1, $3, 3, NULL); }
	 | expr MINUS expr { $$ = new Expression($1, $3, 4, NULL); }
	 | expr MUL expr { $$ = new Expression($1, $3, 5, NULL); }
	 | expr DIV expr { $$ = new Expression($1, $3, 6, NULL); }
	 | expr MOD expr { $$ = new Expression($1, $3, 7, NULL); }
	 | expr GT expr { $$ = new Expression($1, $3, 8, NULL);  }
	 | expr LT expr { $$ = new Expression($1, $3, 9, NULL); }
	 | expr GE expr { $$ = new Expression($1, $3, 10, NULL); }
	 | expr LE expr { $$ = new Expression($1, $3, 11, NULL); }
	 | expr EE expr { $$ = new Expression($1, $3, 12, NULL); }
	 | expr NE expr { $$ = new Expression($1, $3, 13, NULL); }
	 | expr AND expr {  $$ = new Expression($1, $3, 14, NULL); }
	 | expr OR expr { $$ = new Expression($1, $3, 15, NULL); }
	 | MINUS expr {$$ = new Expression($2, NULL, 16, NULL);}
	 | EXCLAMATION expr { $$ = new Expression($2, NULL, 17, NULL); }
	 | '(' expr ')' { $$ = new Expression($2, NULL, 18, NULL); }

location : ID {$$ = new Location(NULL, string($1));}
		 | ID '[' expr ']' {$$ = new Location($3, string($1));}

method_call: ID '(' expressions ')' {$$ = new Method_call($3, yylval.str, NULL, "str");}
		   | ID '(' ')' {$$ = new Method_call(NULL, yylval.str, NULL, "str");}
		   | CALLOUT '(' STRING ')' {$$ = new Method_call(NULL,"str", NULL, yylval.str);}
		   | CALLOUT '(' STRING callout_args ')' {$$ = new Method_call(NULL,"str", $4, yylval.str);}

expressions: expr {$$ = new Expressions($1);}
			| expressions ',' expr { $$->pushback($3);}

callout_args: ',' callout_arg {$$ = new Callout_args($2);}
			| callout_args ',' callout_arg {$$->pushback($3);}

callout_arg: expr {$$ = new Callout_arg($1, "str");}
		   | STRING {$$ = new Callout_arg(NULL, yylval.str);}

literal: INT_LITERAL {$$ = new Literal(1,$1); }
		| CHAR {$$ = new Literal(2,$1);}
		| BOOL {$$ = new Literal(3,$1);}

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
	root->generateCode();
	root->generateCodeDump();
}

void yyerror(const char *s)
{
	fprintf(stderr, "error: %s\n", s);
}