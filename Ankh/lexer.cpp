/*!
	Plan for dealing with types:
		- Change lexer
		- Change parser
		- Change codegen

*/
#ifdef _WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif
#include <string>
#include <iostream>
#include <cstdio>
#include <vector>
#include <map>

#pragma warning(push, 0) //these headers have a million warnings (sloppily written?)
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#pragma warning(pop) //stop hiding warnings for *our* code


using namespace llvm;



// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token {
	tok_eof = -1,

	// commands
	tok_fun = -2,
	tok_extern = -3,
	tok_var = -4,

	// primary
	tok_identifier = -5,
	tok_num = -6,
};

enum LocalType {
	type_unsupported = 0,
	type_int = -1,
	type_char = -2,
	type_double = -3,
	type_infint =  -4,
};

static std::map<std::string, LocalType> g_type_map;

static void initialize_type_map() {
	g_type_map["int"] = LocalType::type_int;
	g_type_map["char"] = LocalType::type_char;
	g_type_map["double"] = LocalType::type_double;
	g_type_map["infint"] = LocalType::type_infint;
}


FILE* g_file;							// .ank file that we will lex, parse, and compile to LLVM IR.

static std::string g_line;				// Full line of standard input
static unsigned long g_line_idx;		// Index into g_line
static unsigned long g_line_count = 0;	// Line counter
bool g_seen_errors = false;				// Whether any errors were encountered while parsing tokens

static std::string g_identifier_str;	// Filled in for tok_identifier
static std::string g_number_str;		// Filled in for tok_num, stored as string to enable infinite precision


//global variables for creating LLVM bytecode
static std::unique_ptr<LLVMContext> g_llvm_context;
static std::unique_ptr<IRBuilder<>> g_builder;
static std::unique_ptr<Module> g_module;
static std::map<std::string, Value*> g_named_values;

// initialize_llvm()
//	Initializes the LLVM context, module, and builder to allow
//	for IR generation.
static void initialize_llvm_module() {
	g_llvm_context = std::make_unique<LLVMContext>();
	g_module = std::make_unique<Module>("Ankh IR Module", *g_llvm_context);
	g_builder = std::make_unique<IRBuilder<>>(*g_llvm_context);
	return;
}

// TODO: function definitions should have the arguments types
// TODO: this implies that we need to separate calls from definitions

// log_compiler_error(str)
//	Logs a compilation error and returns a nullptr of type Value.
//	Also sets the g_seen_errors flag to true.
Value* log_compiler_error(const char* str) {
	g_seen_errors = true;
	fprintf(stderr, "[%lu, %lu]: CompilerError: %s\n", g_line_count, g_line_idx, str);
	return nullptr;
}

//======================================================================================================
// Abstract Syntax Tree (AST)
//======================================================================================================
namespace AST {
	// ExprAST - Base class for all expression nodes.
	class ExprAST {
		// TODO: change all of these to have type enum
		// TODO: trace all the mentions of the variable `ExprAST::type` to make 
		// TODO: sure that they're handled properly as enums
		LocalType type;
		//! In a more aggressive and realistic language, the “ExprAST” class would probably have a type field.

	public:
		ExprAST(const LocalType type) : type(type) {}

		const LocalType get_type() const {
			return type;
		}

		virtual ~ExprAST() = default;
		virtual Value* codegen() = 0;
	};

	// DoubleAST - Expression class for floating point numbers, at double precision.
	class DoubleAST : public ExprAST {
		double val;

	public:
		DoubleAST(const LocalType type, double val) : ExprAST(type), val(val) {}
		Value* codegen() override;
	};

	Value* DoubleAST::codegen() {
		return ConstantFP::get(*g_llvm_context, APFloat(val));
	}

	// IntegerAST - Expression class for integers, taking four bytes of memory.
	class IntegerAST : public ExprAST {
		int32_t val;

	public:
		IntegerAST(const LocalType type, int32_t val) : ExprAST(type), val(val) {} //! If we know that the type is integer, why do we pass the type as an argument?
		Value* codegen() override;
	};
	// TODO: add codegen()

	// InfIntegerAST - Expression class for integers, with infinite precision.
	class InfIntegerAST : public ExprAST {
		//this needs to be fixed in the future
		std::string val;

	public:
		InfIntegerAST(const LocalType type, const std::string& val) : ExprAST(type), val(val) {}
		Value* codegen() override;
	};
	// TODO: add codegen()

	/// VariableExprAST - Expression class for referencing a variable, like "a".
	class VariableExprAST : public ExprAST {
		std::string name;

	public:
		VariableExprAST(const LocalType type, const std::string& name) : ExprAST(type), name(name) {} //! Should the type be the type of the variable?
		Value* codegen() override;
	};

	Value* VariableExprAST::codegen() {
		//assumes the variable has already been emitted somewhere and its value is available.
		Value* V = g_named_values[name];
		if (!V)
			log_compiler_error("Unknown variable name");
		return V;
	}

	// BinaryExprAST - Expression class for a binary operator.
	class BinaryExprAST : public ExprAST {
		char op;
		std::unique_ptr<ExprAST> lhs, rhs;

	public:
	 	// TODO: deal with the type here
		BinaryExprAST(const LocalType type, char op, std::unique_ptr<ExprAST> lhs, std::unique_ptr<ExprAST> rhs)
			: ExprAST(type), op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {
		}
		Value* codegen() override;
	}; 
	
	Value* BinaryExprAST::codegen() {
		//L and R **MUST** have the same type OR we must do type conversions
		Value* L = lhs->codegen();
		Value* R = rhs->codegen();
		if (!L || !R)
			return nullptr;

		// TODO: need to deal with different types when creating operations
		switch (op) {
		case '+':
			return g_builder->CreateFAdd(L, R, "addtmp"); //can omit the "addtmp" arg in all cases...
		case '-':
			return g_builder->CreateFSub(L, R, "subtmp");
		case '*':
			return g_builder->CreateFMul(L, R, "multmp");
		case '/':
			return g_builder->CreateFDiv(L, R, "multmp");
		case '<':
			L = g_builder->CreateFCmpULT(L, R, "cmptmp");
			// Convert bool 0/1 to double 0.0 or 1.0. This SHOULD be unnecessary in the future
			// TODO: change to be integer or bool
			return g_builder->CreateUIToFP(L, Type::getDoubleTy(*g_llvm_context),
				"booltmp");
		case '>':
			L = g_builder->CreateFCmpULT(R, L, "cmptmp");
			// Convert bool 0/1 to double 0.0 or 1.0. This SHOULD be unnecessary in the future
			// TODO: change to be integer or bool
			return g_builder->CreateUIToFP(L, Type::getDoubleTy(*g_llvm_context),
				"booltmp");
		default:
			return log_compiler_error("invalid binary operator");
		}
	}
	// TODO: check https://llvm.org/docs/LangRef.html#type-system

	// CallExprAST - Expression class for function calls.
	class CallExprAST : public ExprAST {
		std::string callee;
		std::vector<std::unique_ptr<ExprAST>> args;

	public:
		CallExprAST(const LocalType type, const std::string& callee, std::vector<std::unique_ptr<ExprAST>> args)
			: ExprAST(type), callee(callee), args(std::move(args)) {
		}
		Value* codegen() override;
	};

	Value* CallExprAST::codegen() {
		// Look up the name in the global module table.
		Function* callee_f = g_module->getFunction(callee);
		if (!callee_f)
			return log_compiler_error("Unknown function referenced");

		// If argument mismatch error.
		if (callee_f->arg_size() != args.size())
			return log_compiler_error("Incorrect # arguments passed");

		std::vector<Value*> args_v;
		for (size_t i = 0, e = args.size(); i != e; ++i) {
			args_v.push_back(args[i]->codegen());
			if (!args_v.back())
				return nullptr;
		}

		return g_builder->CreateCall(callee_f, args_v, "calltmp");
	}

	// PrototypeAST - This class represents the "prototype" for a function,
	// which captures its name, type, and argument names (thus implicitly the number
	// of arguments the function takes).
	class PrototypeAST : public ExprAST {
		std::string name;
		std::vector<std::string> args; //this will likely need to contain more than just the name of arguments

	public:
		PrototypeAST(const LocalType type, const std::string& name, std::vector<std::string> args)
			: ExprAST(type), name(name), args(std::move(args)) {
		}

		Function* codegen() override;

		const std::string& get_name() const { return name; }
		const size_t get_nargs() const { return args.size(); }
		const std::string& get_arg_by_idx(size_t i) const { assert(i < get_nargs()); return args[i]; }
	};

	Function* PrototypeAST::codegen() {
		// Make the function type:  double(double,double) etc.
		// TODO: appropriate argument types
		//! I need a way to keep track of the types in order to call them here
		//! maybe a map?
		//! actually using the type field in the ExprAst class would suffice
		std::vector<Type*> Doubles(args.size(), Type::getDoubleTy(*g_llvm_context));

		// TODO: approrpirate function type
		FunctionType* FT = FunctionType::get(Type::getDoubleTy(*g_llvm_context), Doubles, false);

		Function* F = Function::Create(FT, Function::ExternalLinkage, name, g_module.get());

		// Set names for all arguments.
		unsigned idx = 0;
		for (auto& arg : F->args())
			arg.setName(args[idx++]);

		return F;
	}

	// FunctionAST - This class represents a function definition itself.
	class FunctionAST {
		std::unique_ptr<PrototypeAST> proto;
		std::unique_ptr<ExprAST> body;

	public:
	 	// TODO: figure out if I want to give this a type
		FunctionAST(std::unique_ptr<PrototypeAST> proto, std::unique_ptr<ExprAST> body)
			: proto(std::move(proto)), body(std::move(body)) {
		}

		Function* codegen();
	};

	Function* FunctionAST::codegen() {
		// First, check for an existing function from a previous 'extern' declaration.
		Function* f = g_module->getFunction(proto->get_name());

		if (!f)
			f = proto->codegen();

		if (!f)
			return nullptr;

		if (!f->empty())
			return (Function*)log_compiler_error("Function cannot be redefined.");


		//verify that the signature of f is the same as the prototype
		if (f->arg_size() != proto->get_nargs()) {
			fprintf(stderr, "f->arg_size(): %i, proto->get_nargs(): %i\n", f->arg_size(), proto->get_nargs());
			return (Function*)log_compiler_error("Signature does not match forward definition");
		}
		size_t proto_iter = 0;
		for (auto f_iter = f->args().begin(); f_iter != f->args().end(); ++f_iter) {
			//f and proto have the same number of args; check if they also have the same names
			// TODO: need to edit this to check types too
			if (f_iter->getName().str() != proto->get_arg_by_idx(proto_iter)) {
				fprintf(stderr, "f_iter->getName().str(): %s, proto->get_arg_by_idx(proto_iter): %s\n", f_iter->getName().str().c_str(), proto->get_arg_by_idx(proto_iter).c_str());
				return (Function*)log_compiler_error("Signature does not match forward definition");
			}
			++proto_iter;
		}
		
		// Create a new basic block to start insertion into.
		BasicBlock* bb = BasicBlock::Create(*g_llvm_context, "entry", f);
		g_builder->SetInsertPoint(bb);

		// Record the function arguments in the NamedValues map.
		g_named_values.clear();
		for (auto& arg : f->args())
			g_named_values[std::string(arg.getName())] = &arg;
		
		if (Value* retval = body->codegen()) {
			// Finish off the function.
			g_builder->CreateRet(retval);

			// Validate the generated code, checking for consistency.
			verifyFunction(*f);

			return f;
		}

		// Error reading body, remove function.
		f->eraseFromParent();
		return nullptr;
	}
}

using namespace AST;

// log_syntax_error()
//	Helper function for error handling. Returns nullptr.
//	Also sets the g_seen_errors flag to true.
std::unique_ptr<ExprAST> log_syntax_error(const char* str) {
	g_seen_errors = true;
	fprintf(stderr, "[%lu, %lu]: SyntaxError: %s\n", g_line_count, g_line_idx, str);
	return nullptr;
}

// log_syntax_error_p()
//	Helper function for error handling. Returns nullptr.
std::unique_ptr<PrototypeAST> log_syntax_error_p(const char* str) {
	log_syntax_error(str);
	return nullptr;
}


//======================================================================================================
// Lexer
//======================================================================================================

// read_line()
//	Reads a line from a file into g_line 
//	and sets g_line_idx to 0.
// TODO: make this use ; rather than `\n` to match C syntax
static void read_line() {
	++g_line_count;
	g_line = fgetc(g_file);
	g_line_idx = 0;
	while (g_line[g_line_idx] != '\n' && g_line[g_line_idx] != EOF) {
		g_line += fgetc(g_file);
		++g_line_idx;
	}
	g_line_idx = 0;
}

static bool check_for_parentheses() {
	int i = 0;
	char current_char = g_line[g_line_idx + i];
	while (
		isalnum(current_char) || current_char == '_' || current_char == ' ' 
		|| current_char == '\t' || current_char == '\r'
	) {
		// Eat all alphanumeric characters
		++i;
		current_char = g_line[g_line_idx + i];
	}
	fprintf(stderr, "g_line[g_line_idx + i]: %i\n", g_line[g_line_idx + i]);
	return g_line[g_line_idx + i] == '(';
}


// gettok()
//	Returns the next token from the globally defined file.
static int get_tok() {

	if (g_line.empty()) {
		//read a new line
		read_line();
		return get_tok();
	}

	while (g_line[g_line_idx] == ' ' || g_line[g_line_idx] == '\t' || g_line[g_line_idx] == '\r') {
		//skip whitespace
		++g_line_idx;
	}

	if (g_line[g_line_idx] == EOF) {
		return tok_eof;
	}

	if (g_line[g_line_idx] == '\n') {
		//read a new line and try again
		read_line();
		return get_tok();
	}

	if (isalpha(g_line[g_line_idx]) || g_line[g_line_idx] == '_') {
		//token starts with a letter or an underscore
		g_identifier_str = g_line[g_line_idx]; 
		++g_line_idx;
		while (isalnum(g_line[g_line_idx]) || g_line[g_line_idx] == '_') {
			g_identifier_str += g_line[g_line_idx];
			++g_line_idx;
		}

		//check if identifier string is a keyword
		//TODO: edit based on type 
		if (g_type_map.empty()) {
			initialize_type_map();
		}
		//! need to differentiate between variable definition and token
		//! I should look ahead to see if there are parentheses
		//! Actually, looking to the next char want solve it as regardless of 
		//! whether it is a var or a function, I one more token to look through
		//! before parentheses
		
		//! Not sure if removing tok_def may create complexity as here the 
		//! function name is stll defined later as (probably) a `tok_identifier`
		if (g_identifier_str == "extern")
			return tok_extern;

		for (auto type:g_type_map) {
			//! Currently, `g_identifier_str` is set to the type when parsing 
			//! the type, maybe this is sub-optimal
			if (g_identifier_str == type.first) {
				if (check_for_parentheses()) {
					return tok_fun; //! was tok_def
				}
				else {
					return tok_var;
				}
			}
		}

		//not a keyword, indicate that g_identifier_str is filled
		//! Should check all places where `tok_identifier` or `tok_def` is used
		//! Should also check where `g_identifiter_str` is used to see if I need
		//! to reference `g_definition_type` there. Edit: `g_definition_type`
		//! was removed; using `g_identifier_str` for both purposes now
		//* Going through `tok_def`
		return tok_identifier;
	}

	if (isdigit(g_line[g_line_idx]) || g_line[g_line_idx] == '.') {   // Number: [0-9.]+
		//token starts with a period or a number

		//we can only have a single period
		bool seen_period = false;
		if (g_line[g_line_idx] == '.') {
			seen_period = true;
		}

		g_number_str = g_line[g_line_idx];
		++g_line_idx;
		while (isdigit(g_line[g_line_idx]) || g_line[g_line_idx] == '.') {

			if (g_line[g_line_idx] == '.' && seen_period) {
				log_syntax_error("Invalid number of decimals in number.");
				read_line();
				return get_tok();
			}
			else if (g_line[g_line_idx] == '.') {
				seen_period = true;
			}

			g_number_str += g_line[g_line_idx];
			++g_line_idx;
		}
		//! It seems to me that this will identify 123gfadaf as a num
		//! Actually no as it would end this token and the dfadaf part would be
		//! another token

		//indicate that g_identifier_str is filled
		return tok_num;
	}

	if (g_line[g_line_idx] == '/') {
		//either a comment or a division

		if (g_line.size() > g_line_idx + 1 && g_line[g_line_idx + 1] == '/') {
			//comment, ignore rest of line
			read_line();
			return get_tok();
		}

		//division operator
		++g_line_idx;
		//! Interesting. so if we have a line with just / we will consider it a
		//! division operator. This kinda makes sense.
		return '/';
	}

	//special or unknown character, such as a binary operator or paretheses
	++g_line_idx;
	return g_line[g_line_idx - 1];
}



//======================================================================================================
// Parsing the AST
//======================================================================================================

static int g_cur_tok; //current token
static std::unique_ptr<ExprAST> parse_expression();
static std::unique_ptr<ExprAST> parse_binop_rhs(int expr_prec, std::unique_ptr<ExprAST> lhs);


// get_next_tok()
//	Update the current token g_cur_tok using the get_tok() function
static int get_next_tok() {
	g_cur_tok = get_tok();
	fprintf(stderr, "get_next_tok: g_cur_token: %i\n", g_cur_tok);
	return g_cur_tok;
}


// parse_double_expr()
//	Parse a double precision float number expression
static std::unique_ptr<ExprAST> parse_double_expr() {
	double d = strtod(g_number_str.c_str(), 0); // `g_number_str` is set by lexer
	auto result = std::make_unique<DoubleAST>(LocalType::type_double, d);
	get_next_tok(); // consume the number
	return std::move(result);
}

// parse_paren_expr()
//	Parse a parenthetical expression
static std::unique_ptr<ExprAST> parse_paren_expr() {
	// parenexpr := '(' expression ')'
	get_next_tok(); // eat (.

	//handle whatever expression is within the parentheses
	auto v = parse_expression();
	if (!v) {
		return nullptr;
	}

	//should be impossible to reach
	if (g_cur_tok != ')')
		return log_syntax_error("expected ')'");

	get_next_tok(); // eat ).

	return v;
}

// parse_identifier_expr()
//	Parse an identifier expression
static std::unique_ptr<ExprAST> parse_identifier_expr() {
	// identifierexpr
	//   := identifier
	//   := identifier '(' expression* ')'

	std::string id_name = g_identifier_str; // Set by lexer

	get_next_tok();  //eat identifier.

	if (g_cur_tok != '(') {
		// TODO: Need to decide on what type this would be
		return std::make_unique<VariableExprAST>(LocalType::type_unsupported, id_name); 
	}

	get_next_tok();  // eat (
	std::vector<std::unique_ptr<ExprAST>> args;
	if (g_cur_tok != ')') {
		while (true) {
			std::unique_ptr<ExprAST> arg = parse_expression(); //! I'm not sure how parse expression knows how to parse just enough
			if (arg) {
				args.push_back(std::move(arg));
			}
			else {
				return nullptr;
			}

			// We expect this to be `)` or `,` because `parse_expression()` ate 
			// some tokens
			if (g_cur_tok == ')')
				break;

			if (g_cur_tok != ',')
				return log_syntax_error("Expected ')' or ',' in argument list");
			
			get_next_tok();
		}
	}

	// Eat the ')'.
	get_next_tok();

	// TODO: deal with type
	return std::make_unique<CallExprAST>(LocalType::type_unsupported, id_name, std::move(args));
}

// parse_primary()
//	Determines the type of expression to parse and calls the appropriate handler
static std::unique_ptr<ExprAST> parse_primary() {
	// primary
	//   ::= identifierexpr
	//   ::= numberexpr
	//   ::= parenexpr
	switch (g_cur_tok) {
	case tok_identifier:
		return parse_identifier_expr();
	case tok_num:
		//TODO
		//THIS ONLY WORKS FOR DOUBLES CURRENTLY!!!
		return parse_double_expr();
	case '(':
		return parse_paren_expr();
	// TODO: to have C-style functions, we need to add a case for `{`
	default:
		return log_syntax_error("unknown token when expecting an expression");
	}
}

//mapping from binary operator to precedence value
//! Maybe do a g_binop_precedence?
static std::map<char, int> binop_precedence;

// set_binop_precedence()
//	Sets the precedence of all binary operators, such as +, -, *, and /.
static void set_binop_precedence() {
	//higher precedence is performed first
	binop_precedence['<'] = 10;
	binop_precedence['>'] = 10;
	binop_precedence['+'] = 20;
	binop_precedence['-'] = 20;
	binop_precedence['*'] = 40;
	binop_precedence['/'] = 40;
}

#ifndef isascii
static unsigned int isascii(unsigned int ch) {
	return (ch < 128);
}
#endif

// get_tok_precedence() 
//	Get the precedence of the pending binary operator token.
static int get_tok_precedence() {
	if (!isascii(g_cur_tok)) {
		return -1;
	}

	if (binop_precedence.empty()) {
		set_binop_precedence();
	}

	// Will automatically deal with invalid tokens because they're not in the
	// map
	int tok_prec = binop_precedence[g_cur_tok];
	if (tok_prec <= 0) {
		//undefined operator
		return -1;
	}
	return tok_prec;
}

// parse_expression()
//	Parses an expression into a left-hand-side and a right-hand-side
static std::unique_ptr<ExprAST> parse_expression() {
	// expression
	//   := primary binop_rhs
	auto lhs = parse_primary();
	if (!lhs)
		return nullptr;

	return parse_binop_rhs(0, std::move(lhs));
}

// parse_binop_rhs(expr_prec, lhs)
//	Parses the right-hand-side of a binary expression using precedence.
//	This is functionally an extremely important part of the parser.
static std::unique_ptr<ExprAST> parse_binop_rhs(int expr_prec, std::unique_ptr<ExprAST> lhs) {
	// binop_rhs
	//   := ('+' primary)*
	while (true) {
		int tok_prec = get_tok_precedence();

		//if this is a binop that binds at least as tightly as the current binop,
		//consume it, otherwise we are done.
		if (tok_prec < expr_prec)
		 	// We're already inside another `parse_binop_rhs` call so it will
			// deal with the current token
			// This will also return when the next token is not a binop
			return lhs;

		int binop = g_cur_tok;
		get_next_tok();  // eat binop
		auto rhs = parse_primary();
		if (!rhs)
			return nullptr;

		// Now, `g_curr_tok` is the token after rhs as `parse_primary()` ate 
		// the ones before
		//if binop binds less tightly with rhs than the operator after rhs, let
		//the pending operator take rhs as its lhs.
		int next_prec = get_tok_precedence();
		if (tok_prec < next_prec) {
			//use tok_prec + 1 because we know that the rhs must be evaluated with 
			//minimally higher precedence than the current binop.
			rhs = parse_binop_rhs(tok_prec + 1, std::move(rhs));
			if (!rhs)
				return nullptr;
		}

		//merge lhs and rhs.
		lhs = std::make_unique<BinaryExprAST>(lhs->get_type(), binop, std::move(lhs), std::move(rhs));
	}
}

// parse_prototype
//	Parse a function prototype, i.e. its name and arguments
// TODO: Should be edited to reflect the type of the prototype
// TODO: Have to distinguish between function calling and definition (one 
// TODO: does not have type before) but maybe already dealt with as before we 
// TODO: had `def` keyword
static std::unique_ptr<PrototypeAST> parse_prototype() {
	// prototype
	//   := id '(' id* ')'
	// if (g_cur_tok != tok_identifier)
	// 	return log_syntax_error_p("Expected function name in prototype");

	// Handling the type of the funciton
	if (g_cur_tok != tok_fun) {
		return log_syntax_error_p("Expected function type in prototype");
	}
	
	if (g_type_map.find(g_identifier_str) == g_type_map.end()) {
		return log_syntax_error_p("Unsupported function type");
	}
	LocalType type = g_type_map[g_identifier_str];
	// TODO: trace `tok_def`

	// Parsing the identifier of the function
	get_next_tok();
	std::string fn_name = g_identifier_str;

	get_next_tok();
	if (g_cur_tok != '(')
		return log_syntax_error_p("Expected '(' in prototype");

	//read the list of argument names.
	// TODO: maybe also add arg_types?
	std::vector<std::string> arg_names;
	int next_tok = get_next_tok();
	while (true) {
		//* Dealing with types of variables in function prototype
		if (next_tok != tok_var) {
			break;	
		}
		if (get_next_tok() != tok_identifier) {
			return log_syntax_error_p("Variable name was not specified");
		}
		arg_names.push_back(g_identifier_str);

		next_tok = get_next_tok();
		if (next_tok == ')') {
			break;
		}
		if (next_tok != ',') {
			return log_syntax_error_p("Invalid function argument");
		}
		next_tok = get_next_tok();
	}
	
	// while (get_next_tok() == tok_identifier)
	// 	arg_names.push_back(g_identifier_str);
	//! I think this does not support having math operations in the arguments
	//! of the function as if I have (x + 5) this will raise an error
	if (g_cur_tok != ')')
		return log_syntax_error_p("Expected ')' in prototype");

	//success.
	get_next_tok();  // eat ')'.

	return std::make_unique<PrototypeAST>(type, fn_name, std::move(arg_names));
}

// parse_function()
//	Parses a function by getting its protoype and its body expression
// TODO: should be edited to reflect the type of the function
static std::unique_ptr<FunctionAST> parse_function() {
	// definition := 'def' prototype expression
	// TODO: might want to encode the type of the function here
	//! I'll comment this as the prototype needs to have the type of the
	//! function
	// get_next_tok();  // eat def.
	auto proto = parse_prototype();
	if (!proto) {
		return nullptr;
	}

	//! I think this may not deal with C-style functions properly; how are we
	//! dealing with the `{` and `}`?
	auto expr = parse_expression();
	if (expr) {
		return std::make_unique<FunctionAST>(std::move(proto), std::move(expr));
	}
	return nullptr;
}

// parse_extern()
//	Parses an external import.
static std::unique_ptr<PrototypeAST> parse_extern() {
	//! At least according to the manual, forward declaration requirese extern
	//! Might be able to change that by parsing a prototype only without extern
	// external ::= 'extern' prototype
	get_next_tok();  // eat extern.
	return parse_prototype();
}

static std::unique_ptr<FunctionAST> parse_top_level_expression() {
	/// toplevelexpr := expression
	if (auto expr = parse_expression()) {
		// Make an anonymous proto.
		//! I think this creates a function with no name. this deals with cases
		//! like {int x = 10; x+10;} in global scope
		// TODO: might want to add support for `{` and `}` here
		// TODO: deal with type
		auto proto = std::make_unique<PrototypeAST>(LocalType::type_unsupported, "", std::vector<std::string>());
		return std::make_unique<FunctionAST>(std::move(proto), std::move(expr));
	}
	return nullptr;
}

//======================================================================================================
// Top level parsing
//======================================================================================================


static void handle_function() {
	auto fn_ptr = parse_function();
	if (!fn_ptr) {
		//skip token for error recovery
		fprintf(stderr, "[%lu, %lu]: SyntaxError: Attempted and failed to parse function.\n", g_line_count, g_line_idx);
		get_next_tok();
		return;
	}
	fn_ptr->codegen();
}

// TODO: need to deal with the fact that function type will be after extern
static void handle_extern() {
	auto extern_ptr = parse_extern();
	if (!extern_ptr) {
		//skip token for error recovery
		fprintf(stderr, "[%lu, %lu]: SyntaxError: Attempted and failed to parse an external function.\n", g_line_count, g_line_idx);
		get_next_tok();
		return;
	}
	extern_ptr->codegen();
}

static void handle_top_level_expression() {
	// Evaluate a top-level expression into an anonymous function.
	auto tle_ptr = parse_top_level_expression();
	if (!tle_ptr) {
		//skip token for error recovery
		fprintf(stderr, "[%lu, %lu]: SyntaxError: Attempted and failed to parse a top level expression.\n", g_line_count, g_line_idx);
		get_next_tok();
		return;
	}
	tle_ptr->codegen();
}


// parse_file()
//	Parses an entire input file by moving through the file
//	line-by-line and parsing individual definitions, imports,
//	and top-level expressions.
static void parse_file() {
	// top := definition | external | expression | ';'
	while (true) {
		switch (g_cur_tok) {
			case tok_eof:
				return;
			case ';': // ignore top-level semicolons.
				//! Not sure if we want to ignore for C-style code?
				//! Maybe yes because we eat whitespace anyway
				get_next_tok();
				break;
			// For defining function
			case tok_fun: //! was tok_def
				handle_function();
				break;
			case tok_extern:
				handle_extern();
				break;
			// TODO: if we want forward definition with prototypes, we edit here
			// TODO: add case for `tok_var`
			default:
				handle_top_level_expression();
				break;
			}
	}
}



int main(int argc, char** argv) {
	if (argc != 2) {
		fprintf(stderr, "Usage: No .ank file provided to lexer.\n");
		return 1;
	}

	const char* filename = argv[1];  // Get filename from arguments

	if (
		strlen(filename) < 5 ||
		filename[strlen(filename) - 4] != '.' ||
		filename[strlen(filename) - 3] != 'a' ||
		filename[strlen(filename) - 2] != 'n' ||
		filename[strlen(filename) - 1] != 'k'
	) {
		fprintf(stderr, "Usage: Incorrect file type. Lexer parses .ank files.\n");
		return 1;
	}

	//can safely open the file read-only
	g_file = fopen(filename, "r");

	if (!g_file) {
		fprintf(stderr, "Error: Could not open file %s.\n", filename);
		return 1;
	}

	//test lexer
	//int x = get_tok();
	//while (x != tok_eof) {
	//	std::cout << x << ", " << g_identifier_str << ", " << g_number_str << ", " << std::endl;
	//	x = get_tok();
	//}

	
	//initialize by setting up the LLVM IR tools and getting the first token
	initialize_llvm_module();
	get_next_tok();
	
	//parse all code
	parse_file();

	if (g_seen_errors) {
		fprintf(stderr, "\nParsing failed due to listed errors.\n\n");
		return 1;
	}

	//parsing was successful, print generated code
	g_module->print(errs(), nullptr);

	return 0;
}