#include "ast.h" 
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Analysis/Interval.h"
#include <llvm/IR/CFG.h>
#include "llvm/IR/BasicBlock.h"
#include <llvm/IR/Function.h>
#include <utility>
#include <llvm/IR/Value.h>


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

Expression::Expression(class Expression * expression1, class Expression *expression2,int symbol,  class Literal* literal, class Location * location, class Method_call * method_call )
{
	this->expression1 = expression1;
	this->expression2 = expression2;
	this->symbol = symbol;
	this->literal = literal;
	this->location = location;
	this->method_call = method_call;
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
	if(expression == NULL)
		type = 0;
	else
		type = 1;
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

Var_decl::Var_decl(string type, class Id_list* id_list)
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

If_for::If_for(class Block* block1, class Block* block2, class Expression * expression1, class Expression *expression2, string name)
{
	this->block1 = block1;
	this->block2 = block2;
	this->expr1 = expression1;
	this->expr2 = expression2;
	this->var_name = name;
}

Program::Program(class Field_decls* field, class Method_decls* method)
{
	this->field = field;
	this->method = method;
	this->compilerConstructs = new Constructs();
}

Field_decls::Field_decls(class Field_decl * fie)
{
	v.push_back(fie);
}

void Field_decls::pushback(class Field_decl * fie)
{
	v.push_back(fie);
}


Field_decl::Field_decl(string type, class Vars_decla * vars)
{
	this->type = type;
	this->vars = vars;
	this->v = vars->getVars();
}

Var_decla::Var_decla(string id, int lit, int array_size)
{
	this->id = id;
	this->is_array = lit;
	this->array_size = array_size;
}

std::vector<Var_decla *> Vars_decla::getVars()
{
 	return v;
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

Constructs::Constructs() {
    this->Builder = new IRBuilder<>(Context);
    this->loops = new std::stack<loopInfo*>();
    errors = 0;
    this->TheModule = new Module("Decaf compiler", Context);
    this->TheFPM = new llvm::legacy::FunctionPassManager(TheModule);
    TheFPM->doInitialization();
}

AllocaInst *Constructs::CreateEntryBlockAlloca(Function *TheFunction, std::string VarName, std::string type) {
    /* Get the builder for current context */
    IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
    AllocaInst *alloca_instruction = nullptr;
    if (type == "int") {
        alloca_instruction = TmpB.CreateAlloca(Type::getInt32Ty(this->Context), 0, VarName);
    } else if (type == "boolean") {
        alloca_instruction = TmpB.CreateAlloca(Type::getInt1Ty(this->Context), 0, VarName);
    }
    return alloca_instruction;
}


Value *Program::generateCode()
{
  Value *v;
  if(field!=NULL)
  {
    cout<<"Entered Fields\n";
    v=field->generateCode(this->compilerConstructs);
    if(v==nullptr)
    {
          return nullptr;
    }
  }
  if(method!=NULL)
  {
    v=method->generateCode(this->compilerConstructs);
    
    if(v==nullptr)
    {
      // reportError("Invalid method Declarations");
          return nullptr;
    }
  }
  return v;

  return v;
}

llvm::Value *reportError(string error_str) {
    cerr << "ERROR FOUND----->"<<error_str << endl;
    return nullptr;
}

raw_ostream &file_write() 
{
    FILE *fp=fopen("outfile","w");
    static raw_fd_ostream S(fileno(fp), true);
    return S;
}

void Program::generateCodeDump()
{
  cerr << "Generating LLVM IR Code\n";
    
    this->compilerConstructs->TheModule->print(file_write(), nullptr);
}


Value *Field_decls::generateCode(Constructs *compilerConstructs)
{
  cout<<"Entered field_declarations\n";
  for(auto &i:v)
  {
    cout<<"Iterating field_declarations\n";
    i->generateCode(compilerConstructs);
  }
  Value *v=ConstantInt::get(compilerConstructs->Context, APInt(32, 1));
  return v;
}


Value *Field_decl::generateCode(Constructs *compilerConstructs)
{
  cout<<"Entered Field_declaration\n";
  llvm::Type *ty = nullptr;
  cout<<"datatype "<<type<<endl;
  if(type=="int")
  {
    cout<<"entered Int\n";
    ty = Type::getInt32Ty(compilerConstructs->Context);
    cout<<"Exited Int\n";
  }
  else if(type=="bool")
  {
    cout<<"entered boolean\n";
    // cout<<compilerConstructs->Context<<endl;
    ty = Type::getInt1Ty(compilerConstructs->Context);
    cout<<"Exited boolean\n";
  }
  cout<<"Done with checking type\n";

  for(auto var: v)
  {
    cout<<"entered var_list loop\n";
    if(var->is_array)
    {
      ArrayType *arrType = ArrayType::get(ty, var->array_size);
      GlobalVariable *gv = new GlobalVariable(*(compilerConstructs->TheModule), arrType, false,
                                                    GlobalValue::ExternalLinkage, nullptr,
                                                    var->id);
            gv->setInitializer(ConstantAggregateZero::get(arrType));
    }
    else 
    {
            GlobalVariable *gv = new GlobalVariable(*(compilerConstructs->TheModule), ty, false,
                                                    GlobalValue::ExternalLinkage, nullptr,
                                                    var->id);
            gv->setInitializer(Constant::getNullValue(ty));
        }
  }
  Value *v = ConstantInt::get(compilerConstructs->Context, APInt(32, 1));
    return v;
}

Value *Method_decls::generateCode(Constructs *compilerConstructs)
{
  cout<<"Entered Method declarations\n";
  Value *V = ConstantInt::get(compilerConstructs->Context, APInt(32, 0));
  for(auto &i:v)
  {
    V=i->generateCode(compilerConstructs);
    if(V==nullptr)
      return V;
  }
  return V;
}

Function* Method_decl::generateCode(Constructs *compilerConstructs)
{
    cout<<"Entered Method declaration\n";
    std::vector<std::string> argNames;
    std::vector<std::string> argTypes;
    std::vector<class Method_arg *> args;
    if(met != NULL)
    {
      args = met->v;
    }

    std::vector<Type *> arguments;
    auto arg_size = args.size();
    for (auto &arg : args) {
        std::string arg_type = arg->type;
        std::string arg_name = arg->id;
        if (arg_type == "int") 
        {
            arguments.push_back(Type::getInt32Ty(compilerConstructs->Context));
        } else if (arg_type == "boolean") 
        {
            arguments.push_back(Type::getInt1Ty(compilerConstructs->Context));
        } 

        argTypes.emplace_back(arg_type);
        argNames.emplace_back(arg_name);
    }

    Type *returnType;
    /* Get the return Type */
    if (type == "int") {
        returnType = Type::getInt32Ty(compilerConstructs->Context);
    } else if (type == "boolean") {
        returnType = Type::getInt1Ty(compilerConstructs->Context);
    } else if (type == "void") {
        returnType = Type::getVoidTy(compilerConstructs->Context);
    } else {
        compilerConstructs->errors++;
        cerr<<"Invalid Return Type for " + type + ". Return Type can only be int or boolean or bool"<<endl;
        return nullptr;
    }

    FunctionType *FT = llvm::FunctionType::get(returnType, arguments, false);
    Function *F = llvm::Function::Create(FT, Function::ExternalLinkage, id, compilerConstructs->TheModule);

    unsigned Idx = 0;
    for (Function::arg_iterator AI = F->arg_begin(); Idx != arg_size; ++AI, ++Idx) {
        AI->setName(argNames[Idx]);
    }

    /* Create a New block for this Function */
    BasicBlock *BB = BasicBlock::Create(compilerConstructs->Context, "entry", F);
    compilerConstructs->Builder->SetInsertPoint(BB);
    Idx = 0;

    /* Allocate memory for the arguments passed */
    for (auto &Arg : F->args()) {
        AllocaInst *Alloca = compilerConstructs->CreateEntryBlockAlloca(F, argNames[Idx], argTypes[Idx]);
        compilerConstructs->Builder->CreateStore(&Arg, Alloca);
        compilerConstructs->NamedValues[argNames[Idx]] = Alloca;
        Idx++;
    }

    Value *RetVal = block->generateCode(compilerConstructs);
    if (RetVal) {
        if (type == "void")
            compilerConstructs->Builder->CreateRetVoid();
        verifyFunction(*F);
        compilerConstructs->TheFPM->run(*F);
        return F;
    }

    /* In case of errors remove the function */
    F->eraseFromParent();
    return nullptr;


}

Value* Block::generateCode(Constructs *compilerConstructs)
{
  cout<<"Block\n";
  Value *V;
    std::map<std::string, llvm::AllocaInst *> Old_vals;
    if(var_decl_list!=NULL)
    {
      V = var_decl_list->generateCode(Old_vals, compilerConstructs);
      if (V == nullptr) {
          return V;
      }
  }

  if(statement_list!=NULL)
  {
      V=statement_list->generateCode(compilerConstructs);
      for (auto it = Old_vals.begin(); it != Old_vals.end(); it++) {
          compilerConstructs->NamedValues[it->first] = Old_vals[it->first];
      }
  }
    return V;
}

Value* Var_decl_list::generateCode(map<string, AllocaInst *> &oldValues, Constructs *compilerConstructs)
{
  cout<<"Field_method_declarations\n";
  Value *v = ConstantInt::get(compilerConstructs->Context, APInt(32, 1));
    for (auto &decl : this->v) {
        /// Generate the code for each declaration
        v = decl->generateCode(oldValues, compilerConstructs);
        if (v == nullptr) {
            return v;
        }
    }
    return v;
}

Value* Var_decl::generateCode(map<string, llvm::AllocaInst *> &Old_vals, Constructs *compilerConstructs)
{
  cout<<"Field_method_declaration\n";
  llvm::Function *TheFunction = compilerConstructs->Builder->GetInsertBlock()->getParent();
    for (const auto &var : id_list->v) {
        string varName = var;
        llvm::Value *initval = nullptr;
        llvm::AllocaInst *Alloca = nullptr;
        if (type == "int") {
            initval = ConstantInt::get(compilerConstructs->Context, APInt(32, 0));
            Alloca = compilerConstructs->CreateEntryBlockAlloca(TheFunction, varName, "int");
        } else if (type == "boolean") {
            initval = ConstantInt::get(compilerConstructs->Context, APInt(1, 0));
            Alloca = compilerConstructs->CreateEntryBlockAlloca(TheFunction, varName, "boolean");
        }
        compilerConstructs->Builder->CreateStore(initval, Alloca);
        /* Store the old value to old_vals and new value to named values */
        Old_vals[varName] = compilerConstructs->NamedValues[varName];
        compilerConstructs->NamedValues[varName] = Alloca;
    }
    Value *v = ConstantInt::get(compilerConstructs->Context, APInt(32, 1));
    return v;
}

Value *Statement_list::generateCode(Constructs* compilerConstructs)
{
  cout<<"Entered Statements\n";
  Value *vi = ConstantInt::get(compilerConstructs->Context, llvm::APInt(32, 1));
  for (auto &stmt : v) {
        vi = stmt->generateCode(compilerConstructs);
    }
    return vi;
}

Value *Statement::generateCode(Constructs* compilerConstructs)
{
	cout<<"Entered Statements\n";
	if(type == "assignment"){
		return assignment->generateCode(compilerConstructs);
	}
	else if(type == "return"){
		return generateCode_return(compilerConstructs);
	}
	else if(type == "if_for"){
		return if_for->generateCode(compilerConstructs);
	}
	else if(type == "block"){
		return block->generateCode(compilerConstructs);
	}
	else if(type == "method_call"){
		printf("been here\n");
		return method_call->generateCode(compilerConstructs);
	}

}

Value *Statement::generateCode_return(Constructs *compilerConstructs)
{
  cout<<"Entered Return\n";
  llvm::Value *V = nullptr;
    if (expression != nullptr) {
        /// Generate IR for expression to be returned
        V = expression->generateCode(compilerConstructs);
        if (expression->symbol==0) {
            /// Generate IR for returning it
            V = compilerConstructs->Builder->CreateLoad(V);
        }
        compilerConstructs->Builder->CreateRet(V);
        return V;
    }
    compilerConstructs->Builder->CreateRetVoid();
    return V;
}


Value* Assignment::generateCode(Constructs *compilerConstructs)
{
  cout<<"Entered Assignment\n";
  Value *cur = compilerConstructs->NamedValues[location->Id];

    if (cur == nullptr) {
        cur = compilerConstructs->TheModule->getGlobalVariable(location->Id);
    }
    if (cur == nullptr) {
        compilerConstructs->errors++;
        return reportError("Unknown Variable Name " + location->Id);
    }

    Value *val = expression->generateCode(compilerConstructs);
    if (expression->symbol==0) {
        val = compilerConstructs->Builder->CreateLoad(val);
    }

    Value *lhs = location->generateCode(compilerConstructs);
    cur = compilerConstructs->Builder->CreateLoad(lhs);


    if (val == nullptr) {
        compilerConstructs->errors++;
        return reportError("Error in right hand side of the Assignment");
    }
    if (op == 2) {
        val = compilerConstructs->Builder->CreateAdd(cur, val, "addEqualToTmp");
    } else if (op == 3) {
        val = compilerConstructs->Builder->CreateSub(cur, val, "subEqualToTmp");
    }
    return compilerConstructs->Builder->CreateStore(val, lhs);

}

Value* If_for::generateCode(Constructs *compilerConstructs)
{
	if(expr2 == NULL)
	{
		return generateCode_If(compilerConstructs);
	}
	else
	{
		return generateCode_For(compilerConstructs);
	}
}

Value* If_for::generateCode_If(Constructs *compilerConstructs)
{
  cout<<"Entered If_else\n";
  Value *cond = expr1->generateCode(compilerConstructs);
    if (cond == nullptr) {
        // compilerConstructs->errors++;
        return reportError("Invalid Expression in the IF");
    }

    /* Create blocks for if, else and next part of the code */
    Function *TheFunction = compilerConstructs->Builder->GetInsertBlock()->getParent();
    BasicBlock *ifBlock = BasicBlock::Create(compilerConstructs->Context, "if", TheFunction);
    BasicBlock *elseBlock = BasicBlock::Create(compilerConstructs->Context, "else");
    BasicBlock *nextBlock = BasicBlock::Create(compilerConstructs->Context, "ifcont");
    BasicBlock *otherBlock = elseBlock;
    // bool ret_if = block1->has_return, ret_else = false;
    // bool break_if=block1->has_break;
    // bool continue_if=block1->has_continue;
    bool ret_if = false;
    bool break_if= false;
    bool continue_if = false;

    cout<<ret_if<<" rea "<<break_if<<" rea "<<continue_if<<" rea "<<endl;
    /// Create a conditional break and an insert point
    if (block2 == nullptr) {
        cout<<"Block2 is null pointer"<<endl;
        otherBlock = nextBlock;
    }

    compilerConstructs->Builder->CreateCondBr(cond, ifBlock, otherBlock);
    compilerConstructs->Builder->SetInsertPoint(ifBlock);

    /// generate the code for if block
    Value *if_val = block1->generateCode(compilerConstructs);
    if (if_val == nullptr) {
        return nullptr;
    }

    /// Create a break for next part of the code after else block

    bool flag = 0;
    if (!ret_if && !break_if && !continue_if) {
        compilerConstructs->Builder->CreateBr(nextBlock);
        flag = 1;
    }

    ifBlock = compilerConstructs->Builder->GetInsertBlock();
    /// Create insert point for else block

    Value *else_val = nullptr;
    bool ret_else = false;

    if (block2 != nullptr) {
        /// Generate code for else block
        TheFunction->getBasicBlockList().push_back(elseBlock);
        compilerConstructs->Builder->SetInsertPoint(elseBlock);
        else_val = block2->generateCode(compilerConstructs);
        if (else_val == nullptr) {
            return nullptr;
        }
        // bool break_else=block2->has_break();
        // bool continue_else=block2->has_continue();
        if (!ret_else)
            compilerConstructs->Builder->CreateBr(nextBlock);
    }

    // Create a break for the next part of the code
    TheFunction->getBasicBlockList().push_back(nextBlock);
    compilerConstructs->Builder->SetInsertPoint(nextBlock);
    if (ret_else && ret_if) {
        // if both if and else block have a return statement create a dummy instruction to hold a next block
        Type *retType = compilerConstructs->Builder->GetInsertBlock()->getParent()->getReturnType();
        if (retType == Type::getVoidTy(compilerConstructs->Context))
            compilerConstructs->Builder->CreateRetVoid();
        else {
            compilerConstructs->Builder->CreateRet(ConstantInt::get(compilerConstructs->Context, APInt(32, 0)));
        }
    }
    cout<<"hello5"<<endl;
    Value *V = ConstantInt::get(compilerConstructs->Context, APInt(32, 0));
    return V;
}

Value* If_for::generateCode_For(Constructs *compilerConstructs)
{
  cout<<"Entered For\n";
  Value *start = expr1->generateCode(compilerConstructs);
    if (start == nullptr) {
        return nullptr;
    }
    if (expr1->symbol==0) {
        start = compilerConstructs->Builder->CreateLoad(start);
    }
    /* Get the parent method of this for loop */
    Function *TheFunction = compilerConstructs->Builder->GetInsertBlock()->getParent();
    /* Create memory for the loop variable */
    llvm::AllocaInst *Alloca = compilerConstructs->CreateEntryBlockAlloca(TheFunction, var_name, string("int"));
    compilerConstructs->Builder->CreateStore(start, Alloca);

    Value *step_val = ConstantInt::get(compilerConstructs->Context, APInt(32, 1));
    BasicBlock *pre_header_basic_block = compilerConstructs->Builder->GetInsertBlock();
    BasicBlock *loop_body = BasicBlock::Create(compilerConstructs->Context, "loop", TheFunction);
    BasicBlock *afterBB = BasicBlock::Create(compilerConstructs->Context, "afterloop", TheFunction);
    compilerConstructs->Builder->CreateBr(loop_body);
    compilerConstructs->Builder->SetInsertPoint(loop_body);

    PHINode *Variable = compilerConstructs->Builder->CreatePHI(Type::getInt32Ty(compilerConstructs->Context), 2, var_name);
    Variable->addIncoming(start, pre_header_basic_block);
    /* Store the old value */
    Value *cond = expr2->generateCode(compilerConstructs);
    if (cond == nullptr) {
        compilerConstructs->errors++;
        return reportError("Invalid Condition");
    }

    // Check if condition is a location
    if (expr2->symbol==0) {
        cond = compilerConstructs->Builder->CreateLoad(cond);
    }
    compilerConstructs->loops->push(new loopInfo(afterBB, loop_body, cond, var_name, Variable));
    llvm::AllocaInst *OldVal = compilerConstructs->NamedValues[var_name];
    compilerConstructs->NamedValues[var_name] = Alloca;
    /* Generate the code for the body */
    if (block1->generateCode(compilerConstructs) == nullptr) {
        return nullptr;
    }

    Value *cur = compilerConstructs->Builder->CreateLoad(Alloca, var_name);
    Value *next_val = compilerConstructs->Builder->CreateAdd(cur, step_val, "NextVal");
    compilerConstructs->Builder->CreateStore(next_val, Alloca);
    cond = compilerConstructs->Builder->CreateICmpSLT(next_val, cond, "loopcondition");
    BasicBlock *loopEndBlock = compilerConstructs->Builder->GetInsertBlock();
    compilerConstructs->Builder->CreateCondBr(cond, loop_body, afterBB);
    compilerConstructs->Builder->SetInsertPoint(afterBB);
    Variable->addIncoming(next_val, loopEndBlock);

    if (OldVal) {
        compilerConstructs->NamedValues[var_name] = OldVal;
    } else {
        compilerConstructs->NamedValues.erase(var_name);
    }
    llvm::Value *V = ConstantInt::get(compilerConstructs->Context, APInt(32, 1));
    return V;
}

Value *Location::generateCode(Constructs *compilerConstructs)
{
  cout<<"Entered Location "<<type<<endl;
  Value *V = compilerConstructs->NamedValues[Id];
  if (V == nullptr) 
  {
        V = compilerConstructs->TheModule->getNamedGlobal(Id);
    }
    if (V == nullptr) 
    {
        compilerConstructs->errors++;
        return reportError("Unknown Variable name " + Id);
    }
    /* If location is variable return the code generated */
    if (this->type == 0) {
        return V;
    }
    /* Check if we have an index for array */
    if (this->expression == nullptr) {
        return reportError("Invalid array index");
    }
    /* Generate the code for index of the array */
    Value *index = expression->generateCode(compilerConstructs);
    if (expression->symbol == 0) {
        index = compilerConstructs->Builder->CreateLoad(index);
    }
    /* If index is invalid then report error */
    if (index == nullptr) {
        return reportError("Invalid array index");
    }
    /* Generate the code required for accessing the array at the given index */
    vector<Value *> array_index;
    array_index.push_back(compilerConstructs->Builder->getInt32(0));
    array_index.push_back(index);
    V = compilerConstructs->Builder->CreateGEP(V, array_index, Id + "_Index");
    return V;

}

Value *Expression::generateCode(Constructs *compilerConstructs)
{
  	cout<<"Entered Expression"<<symbol<<endl;

    if(expression1!= NULL and expression2!=NULL)
    {
    	 	Value *left = expression1->generateCode(compilerConstructs);
   	 Value *right = expression2->generateCode(compilerConstructs);
	    if (expression1->symbol == 0) {
	        left = compilerConstructs->Builder->CreateLoad(left);
	    }
	    if (expression2->symbol == 0) {
	        right = compilerConstructs->Builder->CreateLoad(right);
	    }
	    if (left == 0) {
	        compilerConstructs->errors++;
	        return reportError("Error in left operand");
	    } else if (right == 0) {
	        compilerConstructs->errors++;
	        return reportError("Error in right operand");
	    }
	    Value *v = nullptr;
	    if (symbol == 3) {
	        v = compilerConstructs->Builder->CreateAdd(left, right, "addtmp");
	    } else if (symbol == 4) {
	        v = compilerConstructs->Builder->CreateSub(left, right, "subtmp");
	    } else if (symbol == 5) {
	        v = compilerConstructs->Builder->CreateMul(left, right, "multmp");
	    } else if (symbol == 6) {
	        v = compilerConstructs->Builder->CreateSDiv(left, right, "divtmp");
	    } else if (symbol == 7) {
	        v = compilerConstructs->Builder->CreateSRem(left, right, "modtmp");
	    } else if (symbol == 9) {
	        v = compilerConstructs->Builder->CreateICmpSLT(left, right, "ltcomparetmp");
	    } else if (symbol == 8) {
	        v = compilerConstructs->Builder->CreateICmpSGT(left, right, "gtcomparetmp");
	    } else if (symbol == 11) {
	        v = compilerConstructs->Builder->CreateICmpSLE(left, right, "lecomparetmp");
	    } else if (symbol == 10) {
	        v = compilerConstructs->Builder->CreateICmpSGE(left, right, "gecomparetmp");
	    } else if (symbol == 12) {
	        v = compilerConstructs->Builder->CreateICmpEQ(left, right, "equalcomparetmp");
	    } else if (symbol == 13) {
	        v = compilerConstructs->Builder->CreateICmpNE(left, right, "notequalcomparetmp");
	    }
	    else if (symbol== 14){
			v = compilerConstructs->Builder->CreateAnd(left, right, "andtmp");
		}
		else if (symbol== 15){
			v = compilerConstructs->Builder->CreateOr(left, right, "ortmp");
		}
	    return v;
	}
	else
	{

		Value *v;
		if(symbol == 18){
			return expression1->generateCode(compilerConstructs);
		}

		else if(symbol == 2){
			return ConstantInt::get(compilerConstructs->Context, llvm::APInt(32, static_cast<uint64_t>(literal->value)));
		}

		else if(symbol == 0){
			return location->generateCode(compilerConstructs);
		}
		else if(symbol == 16){
			v = expression1->generateCode(compilerConstructs);
			v = compilerConstructs->Builder->CreateNeg(v, "negtmp");
		}
		else if(symbol == 17){
			v = expression1->generateCode(compilerConstructs);
			v = compilerConstructs->Builder->CreateNot(v, "negtmp");
		}

		else if(symbol == 1){
			return method_call->generateCode(compilerConstructs);
		}

		return v;
	}

}



Value *Callout_arg::generateCode(Constructs *compilerConstructs)
{
    cout<<"Entered Callout_arg "<<str<<endl;
    if (expression == nullptr && str=="") {
        compilerConstructs->errors++;
        return reportError("Invalid Callout Arg");
    }
    Value *v;
    if(expression != nullptr)
    {
      cout<<"NOT NULL EXPR\n";
      v = expression->generateCode(compilerConstructs);
      if (expression->symbol==0) {
          v = compilerConstructs->Builder->CreateLoad(v);
      }
    }
    else
    {
      v = compilerConstructs->Builder->CreateGlobalStringPtr(str);
    }
    cout<<"Exited from Call_out_arg\n";
    return v;
}

Value* Method_call::generateCode(Constructs *compilerConstructs)
{
	if(Id == "str")
	{
		return generateCode_callout(compilerConstructs);
	}
	else
	{
		return generateCode_methodcall(compilerConstructs);
	}
}

Value* Method_call::generateCode_callout(Constructs *compilerConstructs)
{
  cout<<"Entered Call_out "<<endl;
  std::vector<llvm::Type *> argTypes;
    std::vector<Value *> Args;
    std::vector<class Callout_arg *> args_list;
    if(callout_args!=NULL) 
      args_list= callout_args->v;

    /**
     * Iterate through the arguments and generate the code required for each one of them
     */
    for (auto &i : args_list) {
        Value *tmp = i->generateCode(compilerConstructs);
        if (tmp == nullptr) {
            return nullptr;
        }
        Args.push_back(tmp);
        argTypes.push_back(tmp->getType());
    }
    /* Generate the code for the function execution */
    llvm::ArrayRef<llvm::Type *> argsRef(argTypes);
    llvm::ArrayRef<llvm::Value *> funcargs(Args);
    llvm::FunctionType *FType = FunctionType::get(Type::getInt32Ty(compilerConstructs->Context), argsRef, false);
    Constant *func = compilerConstructs->TheModule->getOrInsertFunction(callout_str, FType);
    if (!func) {
        return reportError("Error in inbuilt function. Unknown Function name " + callout_str);
    }
    Value *v = compilerConstructs->Builder->CreateCall(func, funcargs);
    return v;
}

Value* Method_call::generateCode_methodcall(Constructs *compilerConstructs)
{
  cout<<"Entered Method_call\n";
  Function *calle = compilerConstructs->TheModule->getFunction(Id);
    if (calle == nullptr) {
        compilerConstructs->errors++;
        return reportError("Unknown Function name" + Id);
    }
    /* Check if required number of parameters are passed */
    vector<class Expression*> args_list;
    if( expressions!=NULL)
       args_list = expressions->v;
    if (calle->arg_size() != args_list.size()) {
        compilerConstructs->errors++;
        return reportError("Incorrect Number of Parameters Passed");
    }
    /// Generate the code for the arguments
    vector<Value *> Args;
    for (auto &arg : args_list) {
        Value *argVal = arg->generateCode(compilerConstructs);
        if (arg->symbol == 0) {
            argVal = compilerConstructs->Builder->CreateLoad(argVal);
        }
        if (argVal == nullptr) {
            compilerConstructs->errors++;
            // reportError("Argument is not valid");
            return nullptr;
        }
        Args.push_back(argVal);
    }
    // Reverse the order of arguments as the parser parses in the reverse order
    // std::reverse(Args.begin(), Args.end());
    // Generate the code for the function call
    Value *v = compilerConstructs->Builder->CreateCall(calle, Args);
    return v;
}
