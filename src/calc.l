%{
using namespace std;
#include <bits/stdc++.h>
#include "calc.tab.h"
int lineno = 0;
#define YY_DECL extern "C" int yylex()

%}

num [0-9]+
id [a-zA-Z][0-9a-zA-Z]*
string "(.*?)"
char [ -~]
character   \'{char}\'
hex 0x[0-9a-fA-F][0-9a-fA-F]*

%%

"+"					{printf("+");return PLUS;}
"-"					{printf("-"); return MINUS;}	
"*"					{printf("*"); return MUL;}
"/"					{printf("/"); return DIV;}
"("					{printf("("); return '(';}
")"					{printf(")"); return ')';}
">"					{printf(">");return GT;}
"<"					{printf("<"); return LT;}	
">="				{printf(">="); return GE;}
"[" 				{return '[';}
"]" 				{return ']';}
"<="				{printf("<="); return LE;}
"=="				{printf("=="); return EE;}
"!="				{printf("!="); return NE;}
"callout"			{printf("callout"); return CALLOUT;}
"!"					{printf("!"); return EXCLAMATION;}
"&&"				{printf("&"); return AND;}
"||"				{printf("|"); return OR;}
"true"				{printf("true");yylval.number = 1;return BOOL;}
"false"				{printf("false"); yylval.number = 0; return BOOL;}
"int"				{return TYPE;}
"boolean" 			{return TYPE;}
"for"				{return FOR;}
"if"				{return IF;}
"else"				{return ELSE;}
"continue"			{return CONTINUE;}
"break"				{return BREAK;}	
"return"			{return RETURN;}
"program"			{return PROGRAM;}
"class"				{return CLASS;}
"void"				{return VOID;}
"="					{return EQUALS;}
"+="				{return PEQUALS;}
"-="				{return MEQUALS;}
{character}			{printf("char"); yylval.str  =strdup(yytext); return CHAR;}
{num}				{yylval.number = atoi(yytext);printf("found number\n");return INT_LITERAL;}
{id}				{printf("id"); yylval.str = strdup(yytext); return ID;}
{string}			{printf("string"); yylval.str = strdup(yytext); return STRING;}
"\n"				{printf("Line number %d\n",lineno+1);++lineno;return '\n';}
[ \t]				{}

%%

