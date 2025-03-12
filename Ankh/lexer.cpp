#ifdef _WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif
#include <string>
#include <iostream>
#include <cstdio>
#include <vector>
#include <map>
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

using namespace llvm;

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token {
	tok_eof = -1,

	// commands
	tok_def = -2,
	tok_extern = -3,

	// primary
	tok_identifier = -4,
	tok_num = -5,
};

static std::string g_line;				// Full line of standard input
static unsigned long g_line_idx;		// Index into g_line
static unsigned long g_line_count = 0;	// Line counter
bool g_seen_errors = false;				// Whether any errors were encountered while parsing tokens

static std::string g_identifier_str;	// Filled in for tok_identifier
static std::string g_number_str;		// Filled in for tok_num

FILE* g_file;							// Global file


//======================================================================================================
// Abstract Syntax Tree (AST)
//======================================================================================================
namespace AST {
	// ExprAST - Base class for all expression nodes.
	class ExprAST {
		std::string type;

	public:
		ExprAST(const std::string& type) : type(type) {}

		const std::string get_type() const {
			return type;
		}

		virtual ~ExprAST() = default;
	};

	// DoubleAST - Expression class for floating point numbers, at double precision.
	class DoubleAST : public ExprAST {
		double val;

	public:
		DoubleAST(const std::string& type, double val) : ExprAST(type), val(val) {}
	};

	// IntegerAST - Expression class for integers, taking four bytes of memory.
	class IntegerAST : public ExprAST {
		int32_t val;

	public:
		IntegerAST(const std::string& type, int32_t val) : ExprAST(type), val(val) {}
	};

	// InfIntegerAST - Expression class for integers, with infinite precision.
	class InfIntegerAST : public ExprAST {
		//this needs to be fixed in the future
		std::string val;

	public:
		InfIntegerAST(const std::string& type, const std::string& val) : ExprAST(type), val(val) {}
	};

	/// VariableExprAST - Expression class for referencing a variable, like "a".
	class VariableExprAST : public ExprAST {
		std::string name;

	public:
		VariableExprAST(const std::string& type, const std::string& name) : ExprAST(type), name(name) {}
	};

	/// BinaryExprAST - Expression class for a binary operator.
	class BinaryExprAST : public ExprAST {
		char op;
		std::unique_ptr<ExprAST> lhs, rhs;

	public:
		BinaryExprAST(const std::string& type, char op, std::unique_ptr<ExprAST> lhs, std::unique_ptr<ExprAST> rhs)
			: ExprAST(type), op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {
		}
	};

	/// CallExprAST - Expression class for function calls.
	class CallExprAST : public ExprAST {
		std::string callee;
		std::vector<std::unique_ptr<ExprAST>> args;

	public:
		CallExprAST(const std::string& type, const std::string& callee, std::vector<std::unique_ptr<ExprAST>> args)
			: ExprAST(type), callee(callee), args(std::move(args)) {
		}
	};

	// PrototypeAST - This class represents the "prototype" for a function,
	// which captures its name, type, and argument names (thus implicitly the number
	// of arguments the function takes).
	class PrototypeAST : public ExprAST {
		std::string name;
		std::vector<std::string> args;

	public:
		PrototypeAST(const std::string& type, const std::string& name, std::vector<std::string> args)
			: ExprAST(type), name(name), args(std::move(args)) {
		}

		const std::string& getName() const { return name; }
	};

	// FunctionAST - This class represents a function definition itself.
	class FunctionAST {
		std::unique_ptr<PrototypeAST> proto;
		std::unique_ptr<ExprAST> body;

	public:
		FunctionAST(std::unique_ptr<PrototypeAST> proto, std::unique_ptr<ExprAST> body)
			: proto(std::move(proto)), body(std::move(body)) {
		}
	};

}


// log_error()
//	Helper function for error handling. Returns nullptr.
std::unique_ptr<AST::ExprAST> log_error(const char* str) {
	g_seen_errors = true;
	fprintf(stderr, "[%lu, %lu]: SyntaxError: %s\n", g_line_count, g_line_idx, str);
	return nullptr;
}

// log_error_p()
//	Helper function for error handling. Returns nullptr.
std::unique_ptr<AST::PrototypeAST> log_error_p(const char* str) {
	log_error(str);
	return nullptr;
}


//======================================================================================================
// Lexer
//======================================================================================================

// read_line()
//	Reads a line from a file into g_line 
//	and sets g_line_idx to 0.
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
		if (g_identifier_str == "keyword1")
			return tok_def;
		if (g_identifier_str == "keyword2")
			return tok_extern;

		//not a keyword, indicate that g_identifier_str is filled
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
				log_error("Invalid number of decimals in number.");
				read_line();
				return get_tok();
			}
			else if (g_line[g_line_idx] == '.') {
				seen_period = true;
			}

			g_number_str += g_line[g_line_idx];
			++g_line_idx;
		}

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
static std::unique_ptr<AST::ExprAST> parse_expression();
static std::unique_ptr<AST::ExprAST> parse_binop_rhs(int expr_prec, std::unique_ptr<AST::ExprAST> lhs);


// get_next_tok()
//	Update the current token g_cur_tok using the get_tok() function
static int get_next_tok() {
	return g_cur_tok = get_tok();
}


// parse_double_expr()
//	Parse a double precision float number expression
static std::unique_ptr<AST::ExprAST> parse_double_expr() {
	double d = strtod(g_number_str.c_str(), 0);
	auto result = std::make_unique<AST::DoubleAST>("double", d);
	get_next_tok(); // consume the number
	return std::move(result);
}

// parse_paren_expr()
//	Parse a parenthetical expression
static std::unique_ptr<AST::ExprAST> parse_paren_expr() {
	// parenexpr := '(' expression ')'
	get_next_tok(); // eat (.

	//handle whatever expression is within the parentheses
	auto v = parse_expression();
	if (!v) {
		return nullptr;
	}

	//should be impossible to reach
	if (g_cur_tok != ')')
		return log_error("expected ')'");

	get_next_tok(); // eat ).

	return v;
}

// parse_identifier_expr()
//	Parse an identifier expression
static std::unique_ptr<AST::ExprAST> parse_identifier_expr() {
	// identifierexpr
	//   := identifier
	//   := identifier '(' expression* ')'

	std::string id_name = g_identifier_str;

	get_next_tok();  //eat identifier.

	if (g_cur_tok != '(') {
		return std::make_unique<AST::VariableExprAST>("TODO", id_name);
	}

	get_next_tok();  // eat (
	std::vector<std::unique_ptr<AST::ExprAST>> args;
	if (g_cur_tok != ')') {
		while (true) {
			std::unique_ptr<AST::ExprAST> arg = parse_expression();
			if (arg) {
				args.push_back(std::move(arg));
			}
			else {
				return nullptr;
			}

			if (g_cur_tok == ')')
				break;

			if (g_cur_tok != ',')
				return log_error("Expected ')' or ',' in argument list");
			
			get_next_tok();
		}
	}

	// Eat the ')'.
	get_next_tok();

	return std::make_unique<AST::CallExprAST>("none", id_name, std::move(args));
}

// parse_primary()
//	Determines the type of expression to parse and calls the appropriate handler
static std::unique_ptr<AST::ExprAST> parse_primary() {
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
	default:
		return log_error("unknown token when expecting an expression");
	}
}

//mapping from binary operator to precedence value
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


// get_tok_precedence() 
//	Get the precedence of the pending binary operator token.
static int get_tok_precedence() {
	if (!isascii(g_cur_tok)) {
		return -1;
	}

	if (binop_precedence.empty()) {
		set_binop_precedence();
	}

	int tok_prec = binop_precedence[g_cur_tok];
	if (tok_prec <= 0) {
		//undefined operator
		return -1;
	}
	return tok_prec;
}

// parse_expression()
//	Parses an expression into a left-hand-side and a right-hand-side
static std::unique_ptr<AST::ExprAST> parse_expression() {
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
static std::unique_ptr<AST::ExprAST> parse_binop_rhs(int expr_prec, std::unique_ptr<AST::ExprAST> lhs) {
	// binop_rhs
	//   := ('+' primary)*
	while (true) {
		int tok_prec = get_tok_precedence();

		//if this is a binop that binds at least as tightly as the current binop,
		//consume it, otherwise we are done.
		if (tok_prec < expr_prec)
			return lhs;

		int binop = g_cur_tok;
		get_next_tok();  // eat binop
		auto rhs = parse_primary();
		if (!rhs)
			return nullptr;

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
		lhs = std::make_unique<AST::BinaryExprAST>(lhs->get_type(), binop, std::move(lhs), std::move(rhs));
	}
}

// parse_prototype
//	Parse a function prototype, i.e. its name and arguments
static std::unique_ptr<AST::PrototypeAST> parse_prototype() {
	// prototype
	//   := id '(' id* ')'
	if (g_cur_tok != tok_identifier)
		return log_error_p("Expected function name in prototype");

	std::string fn_name = g_identifier_str;
	get_next_tok();

	if (g_cur_tok != '(')
		return log_error_p("Expected '(' in prototype");

	//read the list of argument names.
	std::vector<std::string> arg_names;
	while (get_next_tok() == tok_identifier)
		arg_names.push_back(g_identifier_str);
	if (g_cur_tok != ')')
		return log_error_p("Expected ')' in prototype");

	//success.
	get_next_tok();  // eat ')'.

	return std::make_unique<AST::PrototypeAST>("TODO", fn_name, std::move(arg_names));
}

// parse_function()
//	Parses a function by getting its protoype and its body expression
static std::unique_ptr<AST::FunctionAST> parse_function() {
	// definition := 'def' prototype expression
	get_next_tok();  // eat def.
	auto proto = parse_prototype();
	if (!proto) {
		return nullptr;
	}

	auto expr = parse_expression();
	if (expr) {
		return std::make_unique<AST::FunctionAST>(std::move(proto), std::move(expr));
	}
	return nullptr;
}

// parse_extern()
//	Parses an external import.
static std::unique_ptr<AST::PrototypeAST> parse_extern() {
	// external ::= 'extern' prototype
	get_next_tok();  // eat extern.
	return parse_prototype();
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
	}
}

static void handle_extern() {
	auto extern_ptr = parse_extern();
	if (!extern_ptr) {
		//skip token for error recovery
		fprintf(stderr, "[%lu, %lu]: SyntaxError: Attempted and failed to parse an external function.\n", g_line_count, g_line_idx);
		get_next_tok();
	}
}

/// top ::= definition | external | expression | ';'
static void main_loop() {
	while (true) {
		switch (g_cur_tok) {
		case tok_eof:
			return;
		case ';': // ignore top-level semicolons.
			get_next_tok();
			break;
		case tok_def:
			handle_function();
			break;
		case tok_extern:
			handle_extern();
			break;
		default:
			fprintf(stderr, "[%lu, %lu]: SyntaxError: Unexpected symbol.\n", g_line_count, g_line_idx);
			get_next_tok();
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
	/*int x = get_tok();
	while (x != tok_eof) {
		std::cout << x << ", " << g_identifier_str << ", " << g_number_str << ", " << std::endl;
		x = get_tok();
	}*/

	//test parser
	main_loop();

	if (g_seen_errors) {
		fprintf(stderr, "\nParsing failed due to listed errors.\n\n");
		return 1;
	}

	return 0;
}