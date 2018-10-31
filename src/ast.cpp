#include "ast.h" 

Literal::Literal(int type, int value)
{
	this->type = type;
	this->value = value;
}

Callout_arg::Callout_arg(class Expression* expression, string str)
{
	this->expression = expression;
	this->str = str;
}

Expression::Expression(class Expression * expression1, class Expression *expression2,int symbol,  class Literal* literal )
{
	this->expression1 = expression1;
	this->expression2 = expression2;
	this->symbol = symbol;
	this->literal = literal;
}

Expressions::Expressions(class Expression * expression)
{
	this->v.push_back(expression);
}

void Expressions::pushback(class Expression * expression)
{
	this->v.push_back(expression);
}

Location::Location(class Expression * expression, string Id)
{
	this->Id = Id;
	this->expression = expression;
}

Method_call::Method_call(class Expressions* expressions, string Id, class Callout_args * callout_args, string callout_str)
{
	this->expressions = expressions;
	this->Id  = Id;
	this->callout_args = callout_args;
	this->callout_str = callout_str;
}

Callout_args::Callout_args(class Callout_arg * callout_arg)
{
	this->v.push_back(callout_arg);
}

void Callout_args::pushback(class Callout_arg * callout_arg)
{
	this->v.push_back(callout_arg);
}

Block::Block(class Var_decl_list* var_decl_list, class Statement_list* statement_list)
{
 	this->var_decl_list = var_decl_list;
 	this->statement_list = statement_list;
}

Var_decl_list::Var_decl_list(class Var_decl * var_decl)
{
	this->v.push_back(var_decl);
}

void Var_decl_list::pushback(class Var_decl * var_decl)
{
	this->v.push_back(var_decl);
}

Statement_list::Statement_list(class Statement* statement)
{
	this->v.push_back(statement);
}

void Statement_list::pushback(class Statement* statement)
{
	this->v.push_back(statement);
}

Var_decl::Var_decl(int type, class Id_list* id_list)
{	
	this->type = type;
	this->id_list = id_list;
}

Statement::Statement(string type, class Expression* expression, class Block* block, class Assignment* assignment, class If_for* if_for, class Method_call* method_call)
{
	this->type = type;
	this->expression = expression;
	this->block = block;
	this->assignment = assignment;
	this->if_for = if_for;
	this->method_call = method_call;
}

Id_list::Id_list(string id)
{
	v.push_back(id);
}

void Id_list::pushback(string id)
{
	v.push_back(id);
}

Assignment::Assignment(class Location* location, class Expression* expression, int op)
{
	this->location = location;
	this->expression = expression;
	this->op = op;
}

If_for::If_for(class Block* block1, class Block* block2, class Expression * expression1, class Expression *expression2)
{
	this->block1 = block1;
	this->block2 = block2;
	this->expression1 = expression1;
	this->expression2 = expression2;
}

Program::Program(class Field_decls* field, class Method_decls* method)
{
	this->field = field;
	this->method = method;
}

Field_decls::Field_decls(class Field_decl * fie)
{
	v.push_back(fie);
}

void Field_decls::pushback(class Field_decl * fie)
{
	v.push_back(fie);
}


Field_decl::Field_decl(int type, class Vars_decla * vars)
{
	this->type = type;
	this->vars = vars;
}

Var_decla::Var_decla(string id, int lit)
{
	this->id = id;
	this->lit = lit;
}

Vars_decla::Vars_decla(class Var_decla * var)
{
 	v.push_back(var);
}

void Vars_decla::pushback(class Var_decla * var)
{
	v.push_back(var);
}

Method_decl::Method_decl(string type, string id, class Method_args* met, class Block* block)
{
	this->type = type;
	this->id = id;
	this->met = met;
	this->block = block;
}

Method_decls::Method_decls(class Method_decl * met)
{
	v.push_back(met);
}

void Method_decls::pushback(class Method_decl * met)
{
	v.push_back(met);
}

Method_args::Method_args(class Method_arg * met)
{
	v.push_back(met);
}

void Method_args::pushback(class Method_arg * met)
{
	v.push_back(met);
}

Method_arg::Method_arg(string type, string id)
{
	this->type = type;
	this->id = id;
}
