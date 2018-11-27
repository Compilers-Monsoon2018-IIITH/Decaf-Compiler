#include<bits/stdc++.h>
using namespace std;
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/IRBuilder.h>
#include "llvm/IR/Module.h"
using namespace llvm;



class loopInfo {
    BasicBlock *afterBB, *checkBB;
    llvm::Value *condition;
    std::string loopVariable;
    PHINode *phiVariable;
public:
    loopInfo(BasicBlock *afterBlock, BasicBlock *checkBlock, Value *cond, std::string var, PHINode *phiVar) {
        afterBB = afterBlock;
        checkBB = checkBlock;
        condition = cond;
        loopVariable = var;
        phiVariable = phiVar;
    }

    BasicBlock *getAfterBlock() { return afterBB; }

    BasicBlock *getCheckBlock() { return checkBB; }

    llvm::Value *getCondition() { return condition; }

    PHINode *getPHINode() { return phiVariable; }

    std::string getLoopVariable() { return loopVariable; }
};

class Constructs {
public:
    LLVMContext Context;
    

    Module *TheModule;
    

    IRBuilder<> *Builder;
    

    std::map<std::string, llvm::AllocaInst *> NamedValues;
    

    llvm::legacy::FunctionPassManager *TheFPM;
    
    int errors;

    std::stack<loopInfo *> *loops;

    Constructs();

    AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, std::string VarName, std::string type);

};

class AST
{
	public:
  	AST()=default;
  	
};


class Literal:public AST
{	public:
      	
      	int type;
      	int value;
  		Literal(int,int);

};

class Expression:public AST
{
public:
	class Expression * expression1;
	class Expression * expression2;
	int symbol;
	class Literal* literal;
	Expression(class Expression * ,class  Expression * , int, class Literal*);
	virtual Value *generateCode(Constructs *compilerConstructs);

};

class Callout_arg:public AST
{	public:
      	
      	string str;
      	class Expression* expression;
  		Callout_arg(class Expression*,string str);

};

class Expressions:public AST
{
public:
	 std::vector<class Expression*> v;
	 void pushback(class Expression *);
	 Expressions(class Expression *);

};

class Location:public AST
{
	public:
		class Expression* expression;
		string Id;
		int type;
		Location(class Expression *, string);
	virtual Value *generateCode(Constructs *compilerConstructs);
};

class Method_call:public AST
{
public:
	class Expressions* expressions;
	class Callout_args* callout_args;
	string Id;
	string callout_str;
	Method_call(class Expressions*, string, class Callout_args *, string);
};

class Callout_args:public AST
{
public:
	 std::vector<class Callout_arg*> v;
	 void pushback(class Callout_arg *);
	 Callout_args(class Callout_arg *);
};

class Block:public AST
{
public:
	class Var_decl_list* var_decl_list;
	class Statement_list* statement_list;
	Block(class Var_decl_list*, class Statement_list*);
	virtual Value *generateCode(Constructs *compilerConstructs);
};

class Var_decl_list:public AST
{
public:
	std::vector<class Var_decl*> v;
	void pushback(class Var_decl *);
	Var_decl_list(class Var_decl *);
	virtual Value *generateCode(std::map<std::string, llvm::AllocaInst *> &, Constructs *);
};

class Statement_list:public AST
{
public:
	std::vector<class Statement*> v;
	void pushback(class Statement*);
	virtual Value *generateCode(Constructs *compilerConstructs);
	Statement_list(class Statement*);
};

class Var_decl:public AST
{
public:
	string type;
	class Id_list* id_list;
	Var_decl(string, class Id_list*);
	virtual Value *generateCode(std::map<std::string, llvm::AllocaInst *> &, Constructs *);
};

class Statement:public AST
{
public:
	string type;
	class Expression* expression;
	class Block* block;
	class Assignment* assignment;
	class If_for* if_for;
	class Method_call* method_call;
	virtual Value *generateCode(Constructs *compilerConstructs);
	virtual Value *generateCode_return(Constructs *compilerConstructs);
	Statement(string, class Expression*, class Block*, class Assignment*, class If_for*, class Method_call*);
};

class Id_list:public AST
{
public:
	std::vector<string> v;
	void pushback(string );
	Id_list(string );
};

class Assignment:public AST
{
public:
	class Location* location;
	class Expression* expression;
	int op;
	virtual Value *generateCode(Constructs *compilerConstructs);
	Assignment(class Location*, class Expression*, int);
};

class If_for:public AST
{
public:
	class Block* block1;
	class Block* block2;
	class Expression* expr1;
	class Expression* expr2;
	string var_name;
	virtual Value *generateCode(Constructs *compilerConstructs);
	virtual Value *generateCode_If(Constructs *compilerConstructs);
	virtual Value *generateCode_For(Constructs *compilerConstructs);
	If_for(class Block*, class Block*, class Expression*, class Expression*, string var_name);
};

class Program:public AST
{
public:
	class Field_decls* field;
	class Method_decls* method;
	Program(class Field_decls* , class Method_decls* );
    Constructs *compilerConstructs;	
	virtual Value *generateCode();
	void generateCodeDump();
};


class Field_decl:public AST
{
public:
	string type;
	class Vars_decla * vars;
	Field_decl(string type, class Vars_decla *);
	std::vector<class Var_decla *> v;
	virtual Value *generateCode(Constructs *compilerConstructs);
};

class Field_decls:public AST
{
public:
	std::vector<class Field_decl * > v;
	void pushback(class Field_decl *);
	Field_decls(class Field_decl *);
	virtual Value *generateCode(Constructs *compilerConstructs);
};


class Var_decla:public AST
{
public:
	string id;
	int is_array;
	int array_size;
	Var_decla(string, int, int );
};

class Vars_decla:public AST
{
public:
	std::vector<class Var_decla *> v;
	Vars_decla(class Var_decla *);
	void pushback(class Var_decla *);
	std::vector<Var_decla *> getVars();
};



class Method_decl:public AST
{
public:
	string type;
	string id;
	class Method_args* met;
	class Block* block;
	Method_decl(string type, string id, class Method_args*, class Block*);
	virtual Function *generateCode(Constructs *compilerConstructs);
};

class Method_decls:public AST
{
public:
	std::vector<class Method_decl *> v;
	Method_decls(class Method_decl *);
	void pushback(class Method_decl *);
	virtual Value *generateCode(Constructs *compilerConstructs);

};

class Method_args:public AST
{
public:
	std::vector<class Method_arg *> v;
	Method_args(class Method_arg *);
	void pushback(class Method_arg *);

};

class Method_arg:public AST
{
public:
	string type;
	string id;
	Method_arg(string type, string id);
};
