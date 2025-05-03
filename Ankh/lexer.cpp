//TODO Problems to fix
//TODO - no_opt with while loops doens't work, look into it
//TODO - scoped block flushing is causing problems, look into it
#ifdef _WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif
#include <string>
#include <iostream>
#include <cstdio>
#include <vector>
#include <map>
#include <cstdarg>

#pragma warning(push, 0) //these headers have a million warnings (sloppily written?)
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include "llvm/Transforms/Utils/Mem2Reg.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#pragma warning(pop) //stop hiding warnings for *our* code

//#define DEBUG

#ifdef DEBUG
void debug_log(const char* format, ...) {
	va_list args;
	va_start(args, format);
	vprintf(format, args);
	va_end(args);
}

#endif

#ifndef DEBUG
void debug_log(const char* format, ...) {
	(void) format;
}
#endif


#ifndef isascii
static unsigned int isascii(unsigned int ch) {
	return (ch < 128);
}
#endif

typedef uint8_t sectype;

sectype constexpr SECURITY_MIN = 0;
sectype constexpr SECURITY_MAX = UINT8_MAX;
uint8_t constexpr SCOPE_MAX = UINT8_MAX;


using namespace llvm;
// TODO: consider changing doubles to floats
// We could remove double support altogether, but I don't see why we would change doubles to floats


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

	// control flow
	tok_if = -7,
	tok_else = -8, //we don't use the tok_then that kaleidoscope uses, because... that's stupid
	tok_while = -9,

	// special keywords
	tok_setnat = -10,
};

enum LocalType {
	type_unsupported = 0,
	type_int = -1,
	type_char = -2,
	type_double = -3,
	type_nat =  -4,
};


static std::map<std::string, LocalType> g_type_map;

static void initialize_type_map() {
	g_type_map["int"] = LocalType::type_int;
	g_type_map["char"] = LocalType::type_char;
	g_type_map["double"] = LocalType::type_double;
	g_type_map["nat"] = LocalType::type_nat;
}

static std::string g_no_function_optimization_tag = "no_opt";


FILE* g_file;							// .ank file that we will lex, parse, and compile to LLVM IR.

static std::string g_line;				// Full line of standard input
static unsigned long g_line_idx;		// Index into g_line
static unsigned long g_line_count = 0;	// Line counter
bool g_seen_errors = false;				// Whether any errors were encountered while parsing tokens

static std::string g_identifier_str;	// Filled in for tok_identifier
static std::string g_prev_identifier_str;
static bool g_disable_function_optimization;
static std::string g_number_str;			// Filled in for tok_num, stored as string to enable infinite precision
static sectype g_sectype = SECURITY_MIN;	// Changes only when a new security identifier is seen or used
static bool g_number_has_period; 			// Helps distinguish between floats and ints
static bool g_number_is_nat;				// Helps distinguish between nats and ints
static LocalType g_type = type_unsupported;	// Used when a variable or function is defined


static uint32_t g_max_nat_bits = 256;	//maximum number of bits in a big unsigned integer defaults to 256
static bool g_seen_nat = false;		//has a nat been parsed yet?



//contains everything you could possibly want to know about a name.
//that means if you want to know something about a variable and you don't, add it to this struct.
struct NameKeywords {
	LocalType type = type_unsupported;
	sectype security = SECURITY_MIN;
	uint8_t scope = 0; //default is global scope
	bool is_fun = false; //if false, the name corresponds to a variable
};

struct FunctionKeywords {
	NameKeywords fn;
	std::vector<NameKeywords> args;
};



static std::map<std::string, NameKeywords> g_var_names; //variable names and info
static std::map<std::string, FunctionKeywords> g_fun_names; //function names and info
static uint8_t g_scope = 0; //the current operating scope. 0 is global

struct AllocaProperties {
	AllocaInst* alloca;
	uint8_t scope = 0; //default is global scope
	sectype security = SECURITY_MIN;
	LocalType type = type_unsupported;
	Value* val;
};

void print_map_keys(std::map<std::string, NameKeywords> mp) {
	printf("printing map: \n");
	for (auto it = mp.begin(); it != mp.end(); ++it) {
		printf("\tit->first: %s\n", it->first.c_str());
	}
}

void print_map_keys(std::map<std::string, AllocaProperties> mp) {
	printf("printing map: \n");
	for (auto it = mp.begin(); it != mp.end(); ++it) {
		printf("\tit->first: %s, it->second.scope: %i\n", it->first.c_str(), it->second.scope);
	}
}

//global variables for creating LLVM bytecode
static std::unique_ptr<LLVMContext> g_llvm_context;
static std::unique_ptr<IRBuilder<>> g_builder;
static std::unique_ptr<Module> g_module;
static std::map<std::string, AllocaProperties> g_named_values;
static std::unique_ptr<FunctionPassManager> g_fpm;
static std::unique_ptr<LoopAnalysisManager> g_lam;
static std::unique_ptr<FunctionAnalysisManager> g_fam;
static std::unique_ptr<CGSCCAnalysisManager> g_cgam;
static std::unique_ptr<ModuleAnalysisManager> g_mam;
static std::unique_ptr<PassInstrumentationCallbacks> g_pic;
static std::unique_ptr<StandardInstrumentations> g_si;


// initialize_llvm()
//	Initializes the LLVM context, module, and builder to allow
//	for IR generation.
static void initialize_llvm_module() {
	// Open a new context and module
	std::cerr << "\nInitializing LLVM\n" << std::endl;
	g_llvm_context = std::make_unique<LLVMContext>();
	g_module = std::make_unique<Module>("Ankh IR Module", *g_llvm_context);
	// g_module->setDataLayout(g_jit->getDataLayout());

	// Builder
	g_builder = std::make_unique<IRBuilder<>>(*g_llvm_context);

	// Optimization
	// Pass and analysis managers
	g_fpm = std::make_unique<FunctionPassManager>();
	g_lam = std::make_unique<LoopAnalysisManager>();
	g_fam = std::make_unique<FunctionAnalysisManager>();
	g_cgam = std::make_unique<CGSCCAnalysisManager>();
	g_mam = std::make_unique<ModuleAnalysisManager>();
	g_pic = std::make_unique<PassInstrumentationCallbacks>();
	g_si = std::make_unique<StandardInstrumentations>(
		*g_llvm_context, /*DebugLogging*/ true
	);
	g_si->registerCallbacks(*g_pic, g_mam.get());

	// Add transform passes.
	// promote allocas to registers 
	g_fpm->addPass(PromotePass());
	// Do simple "peephole" optimizations and bit-twiddling optzns.
	g_fpm->addPass(InstCombinePass());
	// Reassociate expressions.
	g_fpm->addPass(ReassociatePass());
	// Eliminate Common SubExpressions.
	g_fpm->addPass(GVNPass());
	// Simplify the control flow graph (deleting unreachable blocks, etc).
	g_fpm->addPass(SimplifyCFGPass());

	// Register analysis passes used in these transform passes.
	PassBuilder PB;
	PB.registerModuleAnalyses(*g_mam);
	PB.registerFunctionAnalyses(*g_fam);
	PB.crossRegisterProxies(*g_lam, *g_fam, *g_cgam, *g_mam);
	return;
}

// log_compiler_error(str)
//	Logs a compilation error and returns a nullptr of type Value.
//	Also sets the g_seen_errors flag to true, preventing code output
Value* log_compiler_error(const char* str) {
	g_seen_errors = true;
	fprintf(stderr, "[%lu, %lu]: CompilerError: %s\n", g_line_count, g_line_idx, str);
	return nullptr;
}


//======================================================================================================
// Security
//======================================================================================================

// Return the least upper bound between two security types.
// This is equivalent to max(s1, s2), but follows the language of noninterference literature.
static inline sectype security_LUB(sectype s1, sectype s2) {
	return (s1 > s2) ? s1 : s2;
}


//======================================================================================================
// Memory Management
//======================================================================================================

// Removes any variable that has gone out of scope; i.e. the variable scope is greater than the global scope.
static void flush_vars() {
	for (auto it = g_var_names.begin(); it != g_var_names.end(); ) {
		//std::cout << "Flushing: " << it->first << "\tScope: " << static_cast<int>(it->second.scope) << std::endl;
		if (it->second.scope > g_scope) {
			it = g_var_names.erase(it);
		}
		else {
			++it;
		}
	}
}

// Removes variables from `g_named_values` with scope higher than `cur_scope`.
static void flush_named_values_map(uint8_t cur_scope) {
	for (auto it = g_named_values.begin(); it != g_named_values.end(); ) {
		if (it->second.scope > cur_scope) {
			it = g_named_values.erase(it);
		}
		else {
			++it;
		}
	}
}

// Checks whether a name is already being used for another variable
static bool is_named_var(std::string& name) {
	if (g_var_names.find(name) == g_var_names.end()) {
		return false;
	}
	return true;
}

// Checks whether a name is already being used for another function
static bool is_named_fun(std::string& name) {
	if (g_fun_names.find(name) == g_fun_names.end()) {
		return false;
	}
	return true;
}

// Checks whether a name is already being used for a function or variable
static bool is_named(std::string& name) {
	if (is_named_var(name) || is_named_fun(name)) {
		return true;
	}
	return false;
}

//======================================================================================================
// Abstract Syntax Tree (AST)
//======================================================================================================

namespace AST {

	static Type* local_type_to_llvm(LocalType type) {
		switch (type) {
			case LocalType::type_int:
				return Type::getInt32Ty(*g_llvm_context);
			case LocalType::type_double:
				return Type::getDoubleTy(*g_llvm_context);
			//! Maybe we don't need chars
			case LocalType::type_char:
				return Type::getInt8Ty(*g_llvm_context);
			case LocalType::type_nat: {
				Type* i32Ty = Type::getInt32Ty(*g_llvm_context);
				ArrayType* limbsArrayTy = ArrayType::get(i32Ty, g_max_nat_bits / 32);
				StructType* natTy = StructType::get(*g_llvm_context, { limbsArrayTy }, false);
				return natTy;
			}
			default:
				log_compiler_error("Unsupported type\n");
				return Type::getVoidTy(*g_llvm_context);
				break;
		}
	}


	static Value* cast_int_to_nat(Value* int_val=nullptr) {
		Type* i32Ty = Type::getInt32Ty(*g_llvm_context);
		Value* nat_ptr = g_builder->CreateAlloca(local_type_to_llvm(type_nat), nullptr, "nat_var");

		// Zero-initialize the entire struct manually
		g_builder->CreateMemSet(
			nat_ptr,
			ConstantInt::get(Type::getInt8Ty(*g_llvm_context), 0),
			g_max_nat_bits / 8,
			llvm::MaybeAlign(4)
		);
		if (!int_val) {
			return nat_ptr;
		}

		// Get pointer to the first limb (limbs[0])
		Value* limb_array_ptr = g_builder->CreateStructGEP(local_type_to_llvm(type_nat), nat_ptr, 0, "limb_array_ptr");
		Value* limb0_ptr = g_builder->CreateGEP(
			ArrayType::get(i32Ty, g_max_nat_bits / 32),
			limb_array_ptr,
			{ ConstantInt::get(i32Ty, 0), ConstantInt::get(i32Ty, 0) }
		);

		// Store the 32-bit integer value into limbs[0]
		g_builder->CreateStore(int_val, limb0_ptr);
		return nat_ptr;
	}


	// get_default_type_value(type)
	//	Gets the default type for all supported types. Generally this is 0.
	static Value* get_default_type_value(Type* type) {
		if (type->isIntegerTy()) {
			return ConstantInt::get(type, 0);
		}
		else if (type->isFloatingPointTy()) {
			return ConstantFP::get(type, 0.0);
		}
		else if (type == local_type_to_llvm(type_nat)) {
			return cast_int_to_nat();
		}
		log_compiler_error("Unsupported type for default type value");
		return nullptr;
	}

	/// create_entry_block_alloca - Create an alloca instruction in the entry block of
	/// the function.  This is used for mutable variables etc.
	static AllocaInst* create_entry_block_alloca(
		Function* f, StringRef var_name, Type* type
	) {
		IRBuilder<> temp_builder(
				&f->getEntryBlock(), f->getEntryBlock().begin()
		);
		return temp_builder.CreateAlloca(type, nullptr, var_name);
	}

	// ExprAST - Base class for all expression nodes.
	class ExprAST {
		LocalType type;
		sectype security;


	public:
		ExprAST(const LocalType type, sectype security) : type(type), security(security) {}

		const LocalType get_type() const {
			return type;
		}

		const sectype get_security() const {
			return security;
		}

		virtual ~ExprAST() = default;
		virtual Value* codegen() = 0;
	};

	// DoubleAST - Expression class for floating point numbers, at double precision.
	class DoubleAST : public ExprAST {
		double val;

	public:
		DoubleAST(const LocalType type, double val, const sectype security=SECURITY_MIN) : ExprAST(type, security), val(val) {}
		Value* codegen() override;
	};

	Value* DoubleAST::codegen() {
		return ConstantFP::get(*g_llvm_context, APFloat(val));
	}

	// IntegerAST - Expression class for integers, taking four bytes of memory.
	class IntegerAST : public ExprAST {
		int32_t val;

	public:
		IntegerAST(const LocalType type, int32_t val, const sectype security=SECURITY_MIN) : ExprAST(type, security), val(val) {} 
		Value* codegen() override;
	};

	Value* IntegerAST::codegen() {
		return ConstantInt::get(*g_llvm_context, APInt(32, val));
	}

	// NatAST - Expression class for natural numbers, with precision up to g_max_nat_bits bits.
	class NatAST : public ExprAST {
		//this vector will always have g_max_nat_bits / 32 elements
		std::unique_ptr<uint32_t[]> val;
		uint32_t num_limbs;
		bool valid = true;

	public:
		NatAST(const LocalType type, std::string digits, const sectype security=SECURITY_MIN) : 
			ExprAST(type, security), val(new uint32_t[g_max_nat_bits / 32]), num_limbs(g_max_nat_bits / 32) {
			std::memset(val.get(), 0, g_max_nat_bits/8);

			for (char ch : digits) {
				uint8_t digit = ch - '0';
				//std::cout << int(digit) << std::endl;
				if (!mul_by_10() || !add_digit(digit)) {
					log_compiler_error("Overflow encountered when constructing natural number literal.");
					valid = false;
					break;
				}
			}
			//for (int i = 0; i < g_max_nat_bits / 32; ++i) {
			//	std::cout << val[i] << std::endl;
			//}
		}
		Value* codegen() override;

	private:
		// returns true if multiplied by 10 successfully, false if overflow.
		bool mul_by_10() {
			uint64_t carry = 0;
			for (uint32_t i = 0; i < num_limbs; ++i) {
				uint64_t prod = static_cast<uint64_t>(val[i]) * 10 + carry;
				val[i] = static_cast<uint32_t>(prod);
				carry = prod >> 32;
			}
			if (carry) {
				//overflow
				return false;
			}
			return true;
		}

		// returns true if added digit successfully, false if overflow.
		bool add_digit(uint8_t digit) {
			uint64_t carry = digit;
			for (uint32_t i = 0; i < num_limbs; ++i) {
				uint64_t sum = static_cast<uint64_t>(val[i]) + carry;
				val[i] = static_cast<uint32_t>(sum);
				carry = sum >> 32;
				if (!carry) {
					return true;
				}
			}
			if (carry) {
				//overflow
				return false;
			}
			return true;
		}
	};

	Value* NatAST::codegen() {

		if (!valid) {
			return nullptr;
		}

		Type* i32Ty = Type::getInt32Ty(*g_llvm_context);
		ArrayType* limbsArrayTy = ArrayType::get(i32Ty, num_limbs);
		StructType* natTy = StructType::get(*g_llvm_context, { limbsArrayTy }, false);

		// Convert each limb to a Constant
		std::vector<Constant*> limb_constants;
		limb_constants.reserve(num_limbs);
		for (uint32_t i = 0; i < num_limbs; ++i) {
			limb_constants.push_back(ConstantInt::get(i32Ty, val[i]));
		}

		// Create the [N x i32] array constant
		Constant* limbs = ConstantArray::get(limbsArrayTy, limb_constants);

		// Wrap it in the struct { [N x i32] }
		Constant* nat_literal = ConstantStruct::get(natTy, { limbs });

		Value* nat_ptr = g_builder->CreateAlloca(natTy, nullptr, "nat_literal");

		// Create a global constant or constant memory block
		GlobalVariable* const_nat = new GlobalVariable(
			*g_module,
			natTy,
			/*isConstant=*/true,
			GlobalValue::PrivateLinkage,
			nat_literal,
			"nat_literal_const"
		);

		// Use memcpy to copy the constant into the local variable
		g_builder->CreateMemCpy(nat_ptr, llvm::MaybeAlign(4), const_nat, llvm::MaybeAlign(4), g_max_nat_bits/8);
		return nat_ptr;
	}

	/// VariableExprAST - Expression class for referencing a variable, like "a".
	class VariableExprAST : public ExprAST {
		std::string name;
		uint8_t scope;

	public:
		VariableExprAST(
			const LocalType type,
			const std::string& name,
			bool new_var, uint8_t scope, 
			const sectype security=SECURITY_MIN
		) : 
			ExprAST(type, security), 
			name(name),
			scope(scope) 
		{
			if (!new_var) {
				//no need to do anything for a variable already defined
				return;
			}

			//add new variable to the global names map at the current scope
			NameKeywords nk;
			nk.is_fun = false;
			// g_scope is the current scope the code is in
			nk.scope = g_scope;
			nk.security = this->get_security();
			nk.type = this->get_type();
			g_var_names[this->name] = nk;
		}
		Value* codegen() override;
		std::string get_name() {
			return name;
		}
	};


	Value* VariableExprAST::codegen() {
		//assumes the variable has already been emitted somewhere and its value is available.
		// debug_log("just enterd var codeg\n");
		AllocaProperties props = g_named_values[name];
		// print_map_keys(g_named_values);
		// debug_log("after accessing name in g_named_values\n");

		AllocaInst* alloca = props.alloca;

		if (!alloca && get_type() == type_nat) {
			//the variable is already a pointer
			return props.val;
		}

		if (!alloca) {
			debug_log("About to fail, var name: %s\n, g_named_values: \n", name.c_str());
			print_map_keys(g_named_values);
			return log_compiler_error("Unknown variable name");
		}

		if (get_type() == type_nat) {
			return alloca;
		}

		Value* loaded_value = g_builder->CreateLoad(alloca->getAllocatedType(), alloca, name.c_str());
		return loaded_value;
	}

	class LocalVariableExprAST : public ExprAST {
		std::string name;
		uint8_t scope;

	public:
		LocalVariableExprAST(
			const LocalType type, 
			const std::string& name, 
			const sectype security
		) : 
			ExprAST(type, security), 
			name(name) 
		{
			//add new variable to the global names map at the current scope
			NameKeywords nk;
			nk.is_fun = false;
			// g_scope is the current scope the code is in
			//std::cout << name << " scope: " << int(g_scope) << std::endl;
			nk.scope = g_scope;
			nk.security = this->get_security();
			nk.type = this->get_type();
			g_var_names[this->name] = nk;

			scope = g_scope;
		}

		Value* codegen() override;

		std::string get_name() {
			return name;
		}
	};

	Value* LocalVariableExprAST::codegen() {
		Function* fun = g_builder->GetInsertBlock()->getParent();
		Type* llvm_type = local_type_to_llvm(get_type());
		AllocaInst* alloca = create_entry_block_alloca(fun, name, llvm_type);
		Value* default_val = get_default_type_value(llvm_type);
		if (get_type() == type_nat) {
			g_builder->CreateMemCpy(alloca, MaybeAlign(4), default_val, MaybeAlign(4), g_max_nat_bits / 8);
		}
		else {
			g_builder->CreateStore(default_val, alloca);
		}

		AllocaProperties alloca_prop;
		alloca_prop.alloca = alloca;
		if (!alloca) {
			debug_log("in localvar codegen, !alloca is True");
		}
		else {
			debug_log("in localvar codegen, !alloca is False");
		}
		alloca_prop.scope = scope;
		alloca_prop.type = get_type();
		alloca_prop.val = default_val;
		g_named_values[name] = alloca_prop;

		if (get_type() == type_nat) {
			return alloca;
		}

		return default_val;
	}

	class IfExprAST : public ExprAST {
		std::unique_ptr<ExprAST> cond_expr, then_expr, else_expr;
		bool valid = true;

	public:
		IfExprAST (
			std::unique_ptr<ExprAST> cond_expr, 
			std::unique_ptr<ExprAST> then_expr,
			std::unique_ptr<ExprAST> else_expr
		) : 
			ExprAST(then_expr->get_type(), then_expr->get_security()), 
			cond_expr(std::move(cond_expr)), 
			then_expr(std::move(then_expr)), 
			else_expr(std::move(else_expr)) 
		{
			if (this->cond_expr->get_security() > SECURITY_MIN) {
				valid = false;
				log_compiler_error("The security level of a condition must not be higher than public.");
			}
			if (this->else_expr && this->else_expr->get_type() != this->then_expr->get_type()) {
				//If there is an "else" expression and it has a different type than the "then" expression
				//You can think of an if statement in Ankh as being like a ternary operator.
				//An if expression without an else expression creates a default else expression that always returns 0.
				valid = false;
				log_compiler_error("Both branches of the if expression must have the same return type.");
			}
			if (this->else_expr && this->else_expr->get_security() != this->then_expr->get_security()) {
				//we enforce that both branches have the same security type. This is overly restrictive,
				//but it simplifies codegen and verification significantly.
				valid = false;
				log_compiler_error("Both branches of the if expression must have the same security type.");
			}
		}

		Value* codegen() override;
	};

	Value* IfExprAST::codegen() {

		if (!valid) {
			return nullptr;
		}

		Value* cond_v = cond_expr->codegen();
		if (!cond_v) {
			return nullptr;
		}

		// Convert condition to a bool by comparing non-equal to 0.
		if (cond_v->getType()->isIntegerTy()) {
			// Integer comparison: cond_v != 0
			cond_v = g_builder->CreateICmpNE(
				cond_v,
				ConstantInt::get(cond_v->getType(), 0),
				"ifcond"
			);
		}
		else if (cond_v->getType()->isFloatingPointTy()) {
			// Floating point comparison: cond_v != 0.0
			cond_v = g_builder->CreateFCmpONE(
				cond_v,
				ConstantFP::get(*g_llvm_context, APFloat(0.0)),
				"ifcond"
			);
		}
		else {
			log_compiler_error("Unknown condition value type");
		}

		Function* fun = g_builder->GetInsertBlock()->getParent();

		// Create blocks for the then and else cases.  Insert the 'then' block at the
		// end of the function.
		BasicBlock* then_bb = BasicBlock::Create(*g_llvm_context, "then", fun);
		BasicBlock* else_bb = BasicBlock::Create(*g_llvm_context, "else");
		BasicBlock* merge_bb = BasicBlock::Create(*g_llvm_context, "ifcont");

		g_builder->CreateCondBr(cond_v, then_bb, else_bb);
		
		// Emit then value.
		g_builder->SetInsertPoint(then_bb);

		Value* then_v = then_expr->codegen();
		if (!then_v) {
			//should be impossible to reach...
			return nullptr;
		}

		g_builder->CreateBr(merge_bb);
		// Codegen of 'Then' can change the current block, update ThenBB for the PHI.
		then_bb = g_builder->GetInsertBlock();

		// Emit else block.
		fun->insert(fun->end(), else_bb);
		g_builder->SetInsertPoint(else_bb);

		Value* else_v;
		if (else_expr) {
			else_v = else_expr->codegen();
		}
		else {
			//THIS IS THE DEFAULT VALUE OF AN ELSE BLOCK
			else_v = get_default_type_value(then_v->getType());
		}
		
		if (!else_v) {
			//should be impossible to reach...
			return nullptr;
		}

		if (then_v->getType() != else_v->getType()) {
			return log_compiler_error("During codegen, the types of the then and else expressions became different.");
		}

		g_builder->CreateBr(merge_bb);
		// codegen of 'else' can change the current block, update else_bb for the PHI.
		else_bb = g_builder->GetInsertBlock();

		// emit merge block.
		fun->insert(fun->end(), merge_bb);
		g_builder->SetInsertPoint(merge_bb);

		PHINode* PN = g_builder->CreatePHI(then_v->getType(), 2, "iftmp");
		PN->addIncoming(then_v, then_bb);
		PN->addIncoming(else_v, else_bb);

		return PN;
	}

	class WhileExprAST : public ExprAST {
		std::unique_ptr<ExprAST> cond_expr, body_expr;
		bool valid = true;

	public:
		WhileExprAST (
			std::unique_ptr<ExprAST> cond_expr, 
			std::unique_ptr<ExprAST> body_expr
		) : 
			ExprAST(body_expr->get_type(), body_expr->get_security()),
			cond_expr(std::move(cond_expr)), 
			body_expr(std::move(body_expr))
		{
			if (this->cond_expr->get_security() > SECURITY_MIN) {
				valid = false;
				log_compiler_error("The security level of a condition must not be higher than public.");
			}
		}

		Value* codegen() override;
	};


	Value* WhileExprAST::codegen() {
		if (!valid) {
			return nullptr;
		}
		// Make the new basic block for the loop header, inserting after current block.
		Function* fun = g_builder->GetInsertBlock()->getParent();
		BasicBlock* preheader_bb = g_builder->GetInsertBlock();
		BasicBlock* cond_bb = BasicBlock::Create(*g_llvm_context, "loop", fun);
		BasicBlock* body_bb = BasicBlock::Create(*g_llvm_context, "body", fun);
		BasicBlock* exit_bb = BasicBlock::Create(*g_llvm_context, "exit", fun);

		//tell LLVM we're branching to cond_bb
		g_builder->CreateBr(cond_bb);

		//at cond_bb, generate code from the while condition
		g_builder->SetInsertPoint(cond_bb);
		//add PHI node to track loop result
		PHINode* phi_result = g_builder->CreatePHI(local_type_to_llvm(this->get_type()), 2, "loopres");
		//initial default value, in case the condition starts as false.
		phi_result->addIncoming(get_default_type_value(local_type_to_llvm(this->get_type())), preheader_bb);
		phi_result->addIncoming(get_default_type_value(local_type_to_llvm(this->get_type())), g_builder->GetInsertBlock());

		Value* cond_val = cond_expr->codegen();
		if (!cond_val) {
			return nullptr;
		}

		// Convert condition to a bool by comparing non-equal to 0.
		if (cond_val->getType()->isIntegerTy()) {
			// Integer comparison: cond_val != 0
			cond_val = g_builder->CreateICmpNE(
				cond_val,
				ConstantInt::get(cond_val->getType(), 0),
				"ifcond"
			);
		}
		else if (cond_val->getType()->isFloatingPointTy()) {
			// Floating point comparison: cond_val != 0.0
			cond_val = g_builder->CreateFCmpONE(
				cond_val,
				ConstantFP::get(*g_llvm_context, APFloat(0.0)),
				"ifcond"
			);
		}
		else {
			log_compiler_error("Unknown condition value type");
		}

		//if the condition is true, go to body_bb. Otherwise, go to exit_bb
		g_builder->CreateCondBr(cond_val, body_bb, exit_bb);

		//generate body code
		g_builder->SetInsertPoint(body_bb);
		Value* body_val = body_expr->codegen();
		if (!body_val) {
			return nullptr;
		}
		phi_result->addIncoming(body_val, g_builder->GetInsertBlock());
		//once body is done, go right back to the condition
		g_builder->CreateBr(cond_bb);

		//exit the loop
		g_builder->SetInsertPoint(exit_bb);

		return phi_result;
	}


	// BinaryExprAST - Expression class for a binary operator.
	class BinaryExprAST : public ExprAST {
		char op;
		std::unique_ptr<ExprAST> lhs, rhs;

	public:
		BinaryExprAST (
			const LocalType type, 
			char op, 
			std::unique_ptr<ExprAST> lhs, 
			std::unique_ptr<ExprAST> rhs
		) : 
			ExprAST(type, security_LUB(lhs->get_security(), rhs->get_security())),
			op(op), 
			lhs(std::move(lhs)), 
			rhs(std::move(rhs))
		{}
		Value* codegen_less_nats(Value* L, Value* R);
		Value* codegen_less(Value* L, Value* R, LocalType type);
		Value* codegen_add(Value* L, Value* R, LocalType type);
		Value* codegen_add_nats(Value* L, Value* R);
		Value* codegen_sub(Value* L, Value* R, LocalType type);
		Value* codegen_sub_nats(Value* L, Value* R);
		Value* codegen_mul(Value* L, Value* R, LocalType type);
		Value* codegen_mul_nats(Value* L, Value* R);
		Value* codegen_mod(Value* L, Value* R, LocalType type);
		Value* codegen_mod_nats(Value* L, Value* R);
		Value* codegen_div(Value* L, Value* R, LocalType type);
		Value* codegen_div_nats(Value* L, Value* R);
		Value* codegen_assign();
		Value* codegen() override;
	}; 

	Value* BinaryExprAST::codegen_less_nats(Value* L, Value* R) {
		// Returns an i1 Value* representing whether L < R.
		Type* i32Ty = g_builder->getInt32Ty();
		Type* i1Ty = g_builder->getInt1Ty();

		Value* result = ConstantInt::getFalse(*g_llvm_context);
		Value* decided = ConstantInt::getFalse(*g_llvm_context);

		// Get pointers to the array field (index 0 in the struct)
		Value* L_ptr = g_builder->CreateStructGEP(local_type_to_llvm(type_nat), L, 0, "limb_array_ptr_L");
		Value* R_ptr = g_builder->CreateStructGEP(local_type_to_llvm(type_nat), R, 0, "limb_array_ptr_R");

		for (int i = (g_max_nat_bits / 32) - 1; i >= 0; --i) {
			// Get pointers to L.limbs[i] and R.limbs[i]
			Value* idx_i = ConstantInt::get(i32Ty, i);
			Value* limb_ptr_L = g_builder->CreateGEP(i32Ty, L_ptr, { idx_i }, "limb_ptr_L");
			Value* limb_ptr_R = g_builder->CreateGEP(i32Ty, R_ptr, { idx_i }, "limb_ptr_R");
			
			// Load the values at those pointers
			Value* l_i = g_builder->CreateLoad(i32Ty, limb_ptr_L, "limb_L");
			Value* r_i = g_builder->CreateLoad(i32Ty, limb_ptr_R, "limb_R");

			// Compare limbs
			Value* lt = g_builder->CreateICmpULT(l_i, r_i); // L < R at this limb
			Value* gt = g_builder->CreateICmpUGT(l_i, r_i); // L > R at this limb

			// Only update result if not yet decided
			Value* less_now = g_builder->CreateAnd(lt, g_builder->CreateNot(decided));
			Value* greater_now = g_builder->CreateAnd(gt, g_builder->CreateNot(decided));

			// Update result and decided
			result = g_builder->CreateSelect(less_now, ConstantInt::getTrue(*g_llvm_context), result);
			result = g_builder->CreateSelect(greater_now, ConstantInt::getFalse(*g_llvm_context), result);

			Value* diff_found = g_builder->CreateOr(lt, gt);
			decided = g_builder->CreateOr(decided, diff_found);
		}

		return result;
	}

	Value* BinaryExprAST::codegen_less(
		Value* L, Value* R, LocalType type
	) {
		if (type == type_int) {
			return g_builder->CreateICmpULT(L, R, "cmptmp");
		} 
		else if (type == type_double) {
			return g_builder->CreateFCmpULT(L, R, "cmptmp");
		}
		else if (
			type == type_nat && 
			lhs->get_security() == SECURITY_MIN && 
			rhs->get_security() == SECURITY_MIN
		) {
			return codegen_less_nats(L, R);
		}
		log_compiler_error("Comparison is not defined for this type:");
		fprintf(stderr, "\tType: %i\n", type);
		return nullptr;
	}

	Value* BinaryExprAST::codegen_add_nats(Value* L, Value* R) {
		Type* i32Ty = g_builder->getInt32Ty();
		Type* i1Ty = g_builder->getInt1Ty();

		Value* carry = ConstantInt::get(i1Ty, 0);
		Value* result = UndefValue::get(local_type_to_llvm(type_nat));


		// Get pointers to the array field (index 0 in the struct)
		Value* L_ptr = g_builder->CreateStructGEP(local_type_to_llvm(type_nat), L, 0, "limb_array_ptr_L");
		Value* R_ptr = g_builder->CreateStructGEP(local_type_to_llvm(type_nat), R, 0, "limb_array_ptr_R");

		Value* nat_ptr = g_builder->CreateAlloca(local_type_to_llvm(type_nat), nullptr, "nat_result");
		Value* result_limbs_ptr = g_builder->CreateStructGEP(local_type_to_llvm(type_nat), nat_ptr, 0, "limb_array_ptr");

		for (unsigned i = 0; i < g_max_nat_bits / 32; ++i) {
			// Get pointers to L.limbs[i] and R.limbs[i]
			Value* idx_i = ConstantInt::get(i32Ty, i);
			Value* limb_ptr_L = g_builder->CreateGEP(i32Ty, L_ptr, { idx_i }, "limb_ptr_L");
			Value* limb_ptr_R = g_builder->CreateGEP(i32Ty, R_ptr, { idx_i }, "limb_ptr_R");

			// Load the values at those pointers
			Value* l_i = g_builder->CreateLoad(i32Ty, limb_ptr_L, "limb_L");
			Value* r_i = g_builder->CreateLoad(i32Ty, limb_ptr_R, "limb_R");

			// Sum: temp_sum = l + r
			Value* temp_sum = g_builder->CreateAdd(l_i, r_i, "sum_no_carry");

			// Final sum with carry
			Value* sum = g_builder->CreateAdd(temp_sum, g_builder->CreateZExt(carry, i32Ty), "sum_with_carry");

			// carry = (temp_sum < l) || (sum < temp_sum)
			Value* carry1 = g_builder->CreateICmpULT(temp_sum, l_i);
			Value* carry2 = g_builder->CreateICmpULT(sum, temp_sum);
			carry = g_builder->CreateOr(carry1, carry2, "carry_out");

			Value* result_limb_ptr = g_builder->CreateGEP(
				ArrayType::get(i32Ty, g_max_nat_bits / 32),
				result_limbs_ptr,
				{ ConstantInt::get(i32Ty, 0), idx_i }
			);
			g_builder->CreateStore(sum, result_limb_ptr);
		}

		return nat_ptr;
	}

	// Generates an LLVM addition operation between two operators
	// with type `type`
	Value* BinaryExprAST::codegen_add(Value* L, Value* R, LocalType type) {
		switch (type) {
		case type_int:
			return g_builder->CreateAdd(L, R, "addtmp");
		case type_double:
			return g_builder->CreateFAdd(L, R, "addtmp");
		case type_nat:
			return codegen_add_nats(L, R);
		default:
			log_compiler_error("Addition is not defined for this type:");
			fprintf(stderr, "\tType: %i\n", type);
			return nullptr;
		}
	}


	Value* BinaryExprAST::codegen_sub_nats(Value* L, Value* R) {
		Type* i32Ty = g_builder->getInt32Ty();
		Type* i1Ty = g_builder->getInt1Ty();

		Value* borrow = ConstantInt::get(i1Ty, 0);
		Value* result = UndefValue::get(local_type_to_llvm(type_nat));

		// Get pointers to the array field (index 0 in the struct)
		Value* L_ptr = g_builder->CreateStructGEP(local_type_to_llvm(type_nat), L, 0, "limb_array_ptr_L");
		Value* R_ptr = g_builder->CreateStructGEP(local_type_to_llvm(type_nat), R, 0, "limb_array_ptr_R");

		Value* nat_ptr = g_builder->CreateAlloca(local_type_to_llvm(type_nat), nullptr, "nat_literal");
		Value* result_limbs_ptr = g_builder->CreateStructGEP(local_type_to_llvm(type_nat), nat_ptr, 0, "limb_array_ptr");

		for (unsigned i = 0; i < g_max_nat_bits / 32; ++i) {

			// Get pointers to L.limbs[i] and R.limbs[i]
			Value* idx_i = ConstantInt::get(i32Ty, i);
			Value* limb_ptr_L = g_builder->CreateGEP(i32Ty, L_ptr, { idx_i }, "limb_ptr_L");
			Value* limb_ptr_R = g_builder->CreateGEP(i32Ty, R_ptr, { idx_i }, "limb_ptr_R");

			// Load the values at those pointers
			Value* l_i = g_builder->CreateLoad(i32Ty, limb_ptr_L, "limb_L");
			Value* r_i = g_builder->CreateLoad(i32Ty, limb_ptr_R, "limb_R");

			// r_with_borrow = r_limb + borrow
			Value* borrow_ext = g_builder->CreateZExt(borrow, i32Ty);
			Value* r_with_borrow = g_builder->CreateAdd(r_i, borrow_ext, "r_with_borrow");

			// Compute if new borrow is needed before doing the subtraction
			// borrow_out = (l_limb < r_with_borrow)
			borrow = g_builder->CreateICmpULT(l_i, r_with_borrow, "borrow_out");

			// Subtract: l_limb - r_with_borrow
			Value* diff = g_builder->CreateSub(l_i, r_with_borrow, "limb_diff");

			Value* result_limb_ptr = g_builder->CreateGEP(
				ArrayType::get(i32Ty, g_max_nat_bits / 32),
				result_limbs_ptr,
				{ ConstantInt::get(i32Ty, 0), idx_i }
			);
			g_builder->CreateStore(diff, result_limb_ptr);
		}

		return nat_ptr;
	}



	// Generates an LLVM subtraction operation between two operators
	// with type `type`
	Value* BinaryExprAST::codegen_sub(Value* L, Value* R, LocalType type) {
		switch (type) {
		case type_int:
			return g_builder->CreateSub(L, R, "subtmp");
		case type_double:
			return g_builder->CreateFSub(L, R, "subtmp");
		case type_nat:
			return codegen_sub_nats(L, R);
		default:
			log_compiler_error("Subtraction is not defined for this type:");
			fprintf(stderr, "\tType: %i\n", type);
			return nullptr;
		}
	}

	// Generates an LLVM multiplication operation between two operators
	// with type `type`
	Value* BinaryExprAST::codegen_mul(Value* L, Value* R, LocalType type) {
		switch (type) {
		case type_int:
			return g_builder->CreateMul(L, R, "multmp");
		case type_double:
			return g_builder->CreateFMul(L, R, "multmp");
		default:
			log_compiler_error("Multiplication is not defined for this type:");
			fprintf(stderr, "\tType: %i\n", type);
			return nullptr;
		}
	}

	// Generates an LLVM division operation between two operators
	// with type `type`
	Value* BinaryExprAST::codegen_div(Value* L, Value* R, LocalType type) {
		//It is possible to generate code giving division by 0. Maybe this is okay since it fails at runtime

		if (this->get_security() > SECURITY_MIN) {
			return log_compiler_error("Division is only permitted on low security types.");
		}

		switch (type) {
		case type_int:
			//signed integer division
			return g_builder->CreateSDiv(L, R, "divtmp");
		case type_double:
			return g_builder->CreateFDiv(L, R, "divtmp");
		default:
			log_compiler_error("Division is not defined for this type:");
			fprintf(stderr, "\tType: %i\n", type);
			return nullptr;
		}
	}

	// Generates an LLVM modulus operation between two operators
	// with type `type`
	Value* BinaryExprAST::codegen_mod(Value* L, Value* R, LocalType type) {
		if (type != type_int) {
			log_compiler_error("Modulus is only defined for integer types:");
			fprintf(stderr, "\tType: %i\n", type);
			return nullptr;
		}
		return g_builder->CreateSRem(L, R, "modtmp");
	}


	Value* BinaryExprAST::codegen_assign() {

		if (this->get_security() != lhs->get_security()) {
			return log_compiler_error("Security level of a variable must be at least the security level of the assigned value.");
		}

		VariableExprAST* lhse = static_cast<VariableExprAST*>(lhs.get());
		if (!lhse) {
			return log_compiler_error(
				"destination of '=' must be a variable"
			);
		}

		Value* val = rhs->codegen();
		if (!val) {
			return log_compiler_error("Invalid assignment: unable to parse right-hand-side value of assignment.");
		}

		Value* variable = g_named_values[lhse->get_name()].alloca;
		if (!variable && get_type() != type_nat) {
			return log_compiler_error("Assignment for undefined variable.");
		}


		Type* val_type = val->getType();

		Type* var_type;
		if (get_type() == type_nat) {
			if (!variable) {
				variable = g_named_values[lhse->get_name()].val;
			}
			var_type = variable->getType();
		}
		else {
			AllocaInst* allocaInst = dyn_cast<AllocaInst>(variable);
			if (!allocaInst) {
				return log_compiler_error("Variable was not correctly created using alloca.");
			}
			var_type = allocaInst->getAllocatedType();
		}

		//std::cout << "Assign Check: " << var_type->getTypeID() << '\t' << val->getType()->getTypeID() << int(val->getType()->isPointerTy()) << std::endl;
		std::cout << var_type->getTypeID() << std::endl;
		std::cout << val_type->getTypeID() << std::endl;
		bool var_nat = (var_type->isPointerTy() && lhs->get_type() == type_nat);
		bool val_nat = (val_type->isPointerTy() && rhs->get_type() == type_nat);

		if (!(var_type->isIntegerTy() || var_type->isFloatingPointTy() || var_nat)) {
			return log_compiler_error("Invalid assignment: variable is not a valid type.");
		}

		if (!(val_type->isIntegerTy() || val_type->isFloatingPointTy() || val_nat)) {
			return log_compiler_error("Invalid assignment: value is not a valid type.");
		}

		if (
			(var_type->isIntegerTy() && !(val_type->isIntegerTy())) ||
			(var_type->isFloatingPointTy() && !(val_type->isFloatingPointTy())) ||
			(var_nat && !val_nat)
		) {
			return log_compiler_error("Invalid assignment: type of value is different from type of variable.");
		}

		if (var_nat) {
			//nat requires a memcpy
			g_builder->CreateMemCpy(
				variable,
				llvm::MaybeAlign(4),
				val,
				llvm::MaybeAlign(4),
				g_max_nat_bits/8
			);
		}
		else {
			//primitive requires a store
			g_builder->CreateStore(val, variable);
		}

		g_named_values[lhse->get_name()].val = val;
		return val;
	}

	
	Value* BinaryExprAST::codegen() {
		// Deal with assignment as a special case since we don't want to emit
		// LHS as an expression.
		if (op == '=') {
			return codegen_assign();
		}
		//L and R **MUST** have the same type OR we must do type conversions
		LocalType type_lhs = lhs->get_type();
		LocalType type_rhs = rhs->get_type();

		Value* L = lhs->codegen();
		Value* R = rhs->codegen();

		if (!L || !R) {
			return nullptr;
		}

		//if types are different, try to typecast them if they are compatible.
		if (type_lhs != type_rhs) {
			/* 
			// This code forces the programmer to do their own typecasting
			log_compiler_error("Inconsistent types: ");
			fprintf(stderr, "LHS: %i, RHS: %i\n", type_lhs, type_rhs);
			return nullptr;
			*/

			//instead, we can typecast for them
			if (type_lhs == type_double && type_rhs == type_int) {
				type_rhs = type_double;
				R = g_builder->CreateSIToFP(R, g_builder->getDoubleTy(), "casttmp");
			}
			else if (type_lhs == type_int && type_rhs == type_double) {
				type_lhs = type_double;
				L = g_builder->CreateSIToFP(L, g_builder->getDoubleTy(), "casttmp");
			}
			else if (type_lhs == type_int && type_rhs == type_nat) {
				type_lhs = type_nat;
				L = cast_int_to_nat(L);
			}
			else if (type_lhs == type_nat && type_rhs == type_int) {
				type_rhs = type_nat;
				//if R is a negative integer, this will cast R to an unsigned 32-bit unsigned integer (i.e. -1 becomes 0xffffffff)
				//and then it will perform the extension.
				R = cast_int_to_nat(R);
			}
			else {
				//cannot infer typecast
				//in particular, note that we can't typecast doubles to nats, nats to doubles, or nats to ints.
				log_compiler_error("Incompatible types: ");
				fprintf(stderr, "\tLHS: %i, RHS: %i\n", type_lhs, type_rhs);
				return nullptr;
			}
		}
		//Now, L and R **must** have the same type
		if (L->getType()->getTypeID() != R->getType()->getTypeID()) {
			log_compiler_error("Typecasting was unsuccessful. Helpful information is provided below.");
			fprintf(
				stderr,
				"\tLHS Name: %s, Type: Ours: %i, LLVMs: %i, \t RHS Name: %s, Type: Ours: %i, LLVMs: %i\n",
				L->getName().str().c_str(),
				type_lhs, 
				L->getType()->getTypeID(),
				R->getName().str().c_str(),
				type_rhs, 
				R->getType()->getTypeID()
			);
			//for (auto& it : g_fun_names) {
			//	std::cout << it.first << " has type: " << it.second[fn].type << std::endl;
			//}
			return nullptr;
		}

		switch (op) {
		case '+':
			return codegen_add(L, R, type_lhs);
		case '-':
			return codegen_sub(L, R, type_lhs);
		case '*':
			return codegen_mul(L, R, type_lhs);
		case '/':
			return codegen_div(L, R, type_lhs);
		case '%':
			return codegen_mod(L, R, type_lhs);
		case '<':
			return codegen_less(L, R, type_lhs);
		case '>':
			return codegen_less(R, L, type_lhs);
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
		CallExprAST(
			const LocalType type, 
			const std::string& callee, 
			std::vector<std::unique_ptr<ExprAST>> args, 
			const sectype security=SECURITY_MIN
		) : 
			ExprAST(type, security), 
			callee(callee), 
			args(std::move(args)) 
		{}
		Value* codegen() override;
	};

	Value* CallExprAST::codegen() {
		// Look up the name in the global module table.
		Function* callee_f = g_module->getFunction(callee);

		if (!callee_f || g_fun_names.find(callee) == g_fun_names.end()) {
			//the callee could not be found in the symbol table or it could not be found in the named function list 
			return log_compiler_error("Unknown function referenced");
		}

		FunctionKeywords fun_signature = g_fun_names[callee];

		bool uses_sret = (get_type() == type_nat);
		Type* return_ty = local_type_to_llvm(get_type());

		// If argument mismatch error.
		if (callee_f->arg_size() - uses_sret != args.size() || callee_f->arg_size() - uses_sret != fun_signature.args.size()) {
			return log_compiler_error("Incorrect number of arguments passed");
		}

		std::vector<Value*> args_v;
		Value* sret_result = nullptr;
		if (uses_sret) {
			sret_result = g_builder->CreateAlloca(return_ty, nullptr, "sret_result");
			args_v.push_back(sret_result);
		}

		for (size_t i = 0; i < args.size(); ++i) {
			//verify that function call matches function signature at argument i
			NameKeywords expected = fun_signature.args[i];
			if (args[i]->get_type() != expected.type) {
				return log_compiler_error("Argument type mismatch between function and call");
			}
			if (args[i]->get_security() != expected.security) {
				return log_compiler_error("Argument security mismatch between function and call");
			}

			//argument i matches, we can codegen it and add it to the call
			Value* arg_v = args[i]->codegen();
			if (!arg_v) {
				return nullptr;
			}
			args_v.push_back(arg_v);
			if (!args_v.back()) {
				return nullptr;
			}
		}

		Value* call = g_builder->CreateCall(callee_f, args_v, "calltmp");
		//if the function expects to return a nat, then we need to load the return value stored in sret_result
		//return uses_sret ? g_builder->CreateLoad(return_ty, sret_result, "load_ret") : call;
		return uses_sret ? sret_result : call;
	}

	// PrototypeAST - This class represents the "prototype" for a function,
	// which captures its name, type, and argument names and type (thus
	// implicitly the number of arguments the function takes).
	class PrototypeAST : public ExprAST {
		std::string name;
		std::map<std::string, NameKeywords> arg_types;
		uint8_t scope;

	public:
		PrototypeAST(
			const LocalType type, 
			const std::string& name, 
			std::vector<std::string> args, 
			std::map<std::string, NameKeywords> arg_types, 
			sectype security=SECURITY_MIN
		) : 
			ExprAST(type, security), 
			name(name), 
			args(std::move(args))
		{
			//add the function name to the global names
			FunctionKeywords fk;
			NameKeywords nk;
			nk.is_fun = false;
			nk.scope = g_scope;
			nk.security = this->get_security();
			nk.type = this->get_type();
			fk.fn = nk;
			std::vector<NameKeywords> arg_keywords;

			//add the argument names to the local variable names
			for (const auto& [name_i, nk_i] : arg_types) {
				g_var_names[name_i] = nk_i;
				arg_keywords.push_back(nk_i);
			}

			fk.args = std::move(arg_keywords);
			g_fun_names[this->name] = fk;

			this->arg_types = std::move(arg_types);
			scope = g_scope;
		}

		std::vector<std::string> args; //this will likely need to contain more than just the name of arguments

		Function* codegen() override;

		const std::string& get_name() const { return name; }
		const size_t get_nargs() const { return args.size(); }
		const std::string& get_arg_by_idx(size_t i) const { assert(i < get_nargs()); return args[i]; }
		const LocalType get_arg_type(std::string& arg) { return arg_types[arg].type; }
		const uint8_t get_scope() const { return scope; }
	};

	Function* PrototypeAST::codegen() {
		// Make the function type:  double(double,double) etc.
		std::vector<Type*> Args;

		// Handle struct return types with sret
		bool uses_sret = (get_type() == type_nat);
		Type* llvm_type = uses_sret ? Type::getVoidTy(*g_llvm_context) : local_type_to_llvm(get_type());

		if (uses_sret) {
			Type* sret_ty = local_type_to_llvm(get_type());
			Args.push_back(PointerType::getUnqual(sret_ty));  // hidden sret param at beginning of function
		}

		for (int i = 0; i < args.size(); ++i) {
			std::string arg = args[i];
			LocalType arg_type = arg_types[arg].type;
			Type* llvm_ty = local_type_to_llvm(arg_type);
			if (arg_type == type_nat) {
				Args.push_back(PointerType::getUnqual(llvm_ty));  // pass struct by pointer
			}
			else {
				Args.push_back(llvm_ty);  // primitive types
			}
		}

		FunctionType* FT = FunctionType::get(llvm_type, Args, false);
		Function* F = Function::Create(FT, Function::ExternalLinkage, name, g_module.get());

		// Set names for all arguments.
		unsigned idx = 0;
		if (uses_sret) {
			// first argument is the result pointer
			F->addParamAttr(0, Attribute::StructRet);
			F->getArg(0)->setName("sret_result");
			idx = 1;
		}
		for (size_t i = 0; i < args.size(); ++i) {
			F->getArg(idx++)->setName(args[i]);
		}

		return F;
	}

	// FunctionAST - This class represents a function definition itself.
	class FunctionAST {
		std::unique_ptr<PrototypeAST> proto;
		std::unique_ptr<ExprAST> body;

	public:
		FunctionAST(std::unique_ptr<PrototypeAST> proto, std::unique_ptr<ExprAST> body)
			: proto(std::move(proto)), body(std::move(body)) {
		}

		Function* codegen();
	};

	Function* FunctionAST::codegen() {
		// First, check for an existing function from a previous 'extern' declaration.
		Function* f = g_module->getFunction(proto->get_name());

		if (!f) {
			f = proto->codegen();
		}

		if (!f) {
			return nullptr;
		}

		if (!f->empty()) {
			return (Function*)log_compiler_error("Function cannot be redefined.");
		}

		bool uses_sret = (proto->get_type() == type_nat);

		//verify that the signature of f is the same as the prototype
		if (f->arg_size() - uses_sret != proto->get_nargs()) {
			return (Function*)log_compiler_error("Signature does not match forward definition");
		}
		size_t proto_iter = 0;
		for (auto f_iter = f->args().begin(); f_iter != f->args().end(); ++f_iter) {
			//f and proto have the same number of args; check if they also have the same names
			// TODO: need to edit this to check types too
			// printf("f_iter->getType(): %s \n", f_iter->getType());
			if (uses_sret) {
				//the first argument of f is a return pointer sret.
				continue;
			}
			if (f_iter->getName().str() != proto->get_arg_by_idx(proto_iter)) {
				return (Function*)log_compiler_error("Signature does not match forward definition");
			}
			++proto_iter;
		}
		
		// Create a new basic block to start insertion into.
		BasicBlock* bb = BasicBlock::Create(*g_llvm_context, "entry", f);
		g_builder->SetInsertPoint(bb);

		flush_named_values_map(proto->get_scope());
		// Store the g_named_values in the current block
		// Why are we bending over backwards to support globals
		for (auto it = g_named_values.begin(); it != g_named_values.end(); ++it) {
			debug_log("just enterd 1st for\n");
			Type* llvm_type = local_type_to_llvm(it->second.type);
			debug_log("after 1st line\n");
			AllocaInst* alloca = create_entry_block_alloca(f, it->first, llvm_type);
			debug_log("after 2 line\n");
			Value* val = it->second.val;
			debug_log("after 3 line\n");
			g_builder->CreateStore(val, alloca);
			debug_log("after 4 line\n");

			AllocaProperties alloca_prop;
			alloca_prop.alloca = alloca;
			alloca_prop.scope = it->second.scope;
			alloca_prop.type = it->second.type;
			alloca_prop.val = val;
			g_named_values[it->first] = alloca_prop;
		}

		// Add function arguments to the store 
		Argument* sret_arg = uses_sret ? f->getArg(0) : nullptr;
		unsigned idx = uses_sret ? 1 : 0;
		for (const auto& arg_name : proto->args) {
			Argument& arg = *(f->getArg(idx++));
			//TODO: ensure that the type here works well (is it correct type?)

			AllocaProperties alloca_prop;
			if (arg.getType()->isPointerTy()) {
				//nat, which is already a pointer.
				alloca_prop.alloca = nullptr;
			}
			else {
				AllocaInst* alloca = create_entry_block_alloca(f, arg.getName(), arg.getType());
				g_builder->CreateStore(&arg, alloca);

				alloca_prop.alloca = alloca;
			}
			alloca_prop.scope = proto->get_scope() + 1;
			std::string arg_name = arg.getName().str();
			alloca_prop.type = proto->get_arg_type(arg_name);
			alloca_prop.val = &arg;
			g_named_values[arg.getName().str()] = alloca_prop;
		}
		
		if (Value* retval = body->codegen()) {
			if (uses_sret) {
				// Store return value to sret pointer
				g_builder->CreateMemCpy(sret_arg, MaybeAlign(4), retval, MaybeAlign(4), g_max_nat_bits/8);
				g_builder->CreateRetVoid();
			}
			else {
				g_builder->CreateRet(retval);
			}

			verifyFunction(*f);

			// Optimize the function if necessary
			if (!g_disable_function_optimization) {
				g_fpm->run(*f, *g_fam);
			}

			// Remove out of scope variables from `g_named_values`. Out of scope is
			// determined based on the scope the function is in (as opposed to the 
			// scope inside the function) which is determined by the scope the 
			// prototype is in.

			// Should be equivalent to flush_named_values_map(0) since all functions are global scope.
			flush_named_values_map(proto->get_scope());
			return f;
		}

		// Error reading body, remove function.
		f->eraseFromParent();
		flush_named_values_map(proto->get_scope());
		return nullptr;
	}


	// BlockExprAST - This class represents a scoped block
	class BlockExprAST : public ExprAST {
		std::vector<std::unique_ptr<ExprAST>> body;
		uint8_t scope; //this is the scope that the block lives in, not the scope that the block's body lives in

	public:
		BlockExprAST(LocalType type, std::vector<std::unique_ptr<ExprAST>> body, const uint8_t scope, const sectype security=SECURITY_MIN)
			: ExprAST(type, security), body(std::move(body)), scope(scope)
		{}

		Value* codegen() override;
		uint8_t get_scope() { return scope; }
	};

	Value* BlockExprAST::codegen() {
		//TODO: need to add the ability to return early from a block
		Value* last = nullptr;
		for (auto& expr : body) {
			last = expr->codegen();
		}

		//flush_named_values_map(this->scope + 1);

		//the following checks assume the return value is the last expression in the block.
		if (body.size() > 0 && body[body.size() - 1]->get_security() != this->get_security()) {
			return log_compiler_error("Invalid security level generated from block.");
		}
		if (body.size() > 0 && body[body.size() - 1]->get_type() != this->get_type()) {
			return log_compiler_error("Invalid type generated from block.");
		}

		//debug_log("printing before flushing in scope block, g_scope: %i, this->scope: %i\n", g_scope, this->scope);
		//print_map_keys(g_named_values);
		//flush_named_values_map(scope);
		//debug_log("printing after flushing in scope block\n");
		//print_map_keys(g_named_values);
		return last; // this is the return value from the block, and thus also the function if the block is around a function.
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
	char current_char = g_line[g_line_idx];
	while (
		isalnum(current_char) || current_char == '_' || current_char == ' ' 
		|| current_char == '\t' || current_char == '\r'
	) {
		// Eat all alphanumeric characters
		++i;
		current_char = g_line[g_line_idx + i];
	}
	return g_line[g_line_idx + i] == '(';
}


// Gets the nth future identifier without 
// increasing g_line_idx or changing any other global variables (except for
// possibly populating g_type_map by initializing)
static std::string get_next_identifier(int n = 1) {

	//generates the next keyword, starting by ignoring whitespace
	int i = 0;
	std::string identifier_str; //the next identifier, starting with current_char
	for (int j = 0; j < n; ++j) {
		char current_char = g_line[g_line_idx];
		while (current_char == ' ' || current_char == '\t' || current_char == '\r') {
			// Eat all whitespace
			++i;
			current_char = g_line[g_line_idx + i];
		}

		identifier_str = g_line[g_line_idx + i];
		++i;
		while (isalnum(g_line[g_line_idx + i]) || g_line[g_line_idx + i] == '_') {
			identifier_str += g_line[g_line_idx + i];
			++i;
		}
	}

	return identifier_str;
}

static bool check_for_valid_name() {
	if (g_type_map.empty()) {
		initialize_type_map();
	}

	std::string identifier_str = get_next_identifier();

	if (identifier_str == g_no_function_optimization_tag) {
		identifier_str = get_next_identifier(2);
	}
	
	if (!isalpha(identifier_str[0]) && identifier_str[0] != '_') {
		return false;
	}

	for (auto& type : g_type_map) {
		if (identifier_str == type.first) {
			return false;
		}
	}

	if (is_named(identifier_str) || identifier_str == "extern") {
		return false;
	}

	return true;
}

static inline size_t get_num_digits(int x) {
	return static_cast<size_t>(floor(log10(static_cast<double>(x)))) + 1;
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

	if (isalpha(g_line[g_line_idx]) || g_line[g_line_idx] == '_') {
		//token starts with a letter or an underscore
		g_identifier_str = g_line[g_line_idx]; 
		++g_line_idx;
		while (isalnum(g_line[g_line_idx]) || g_line[g_line_idx] == '_') {
			g_identifier_str += g_line[g_line_idx];
			++g_line_idx;
		}

		//check if identifier string is a keyword
		if (g_type_map.empty()) {
			initialize_type_map();
		}

		if (g_identifier_str == "extern") {
			return tok_extern;
		}
		if (g_identifier_str == "if") {
			return tok_if;
		}
		if (g_identifier_str == "else") {
			return tok_else;
		}
		if (g_identifier_str == "while") {
			return tok_while;
		}
		if (g_identifier_str == "set_max_nat_bits") {
			return tok_setnat;
		}

		for (auto& type:g_type_map) {
			if (g_identifier_str == type.first) {
				//identifier is a type keyword

				//check that the next keyword is a valid variable or function name
				if (!check_for_valid_name()) {
					//the next token is not a valid function identifier.
					log_syntax_error("Invalid identifier name.");
					read_line();
					return get_tok();
				}

				//valid typing
				g_type = type.second;
				if (check_for_parentheses()) {
					return tok_fun;
				}
				else {
					return tok_var;
				}
			}
		}

		//! For some reason, having secret in front of int made the parser treat
		//! int as an identifier.
		// not a keyword, indicate that g_identifier_str is filled
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
		bool is_nat = false;
		if (g_line[g_line_idx] == 'n' && !seen_period) {
			//natural number! Defined as an integer
			is_nat = true;
			++g_line_idx;
		}

		g_number_has_period = seen_period;
		g_number_is_nat = is_nat;
		//indicate that g_identifier_str is filled
		return tok_num;
	}

	if (g_line[g_line_idx] == '$') {
		std::string num_str = "";
		++g_line_idx;
		while (isdigit(g_line[g_line_idx])) {
			num_str += g_line[g_line_idx];
			++g_line_idx;
		}
		if (num_str.size() == 0 || num_str.size() > get_num_digits(SECURITY_MAX)) {
			log_syntax_error("Invalid security number provided. Must be in [SECURITY_MIN, SECURITY_MAX].");
			read_line();
			return get_tok();
		}
		int sectype_guess = std::stoi(num_str);
		if (sectype_guess < SECURITY_MIN || sectype_guess > SECURITY_MAX) {
			log_syntax_error("Invalid security number provided. Must be in [SECURITY_MIN, SECURITY_MAX].");
			read_line();
			return get_tok();
		}
		//set the security level since the provided code was valid
		g_sectype = static_cast<sectype>(sectype_guess);

		//the next token must be a function or variable
		int cur_tok = get_tok();
		if (cur_tok != tok_var && cur_tok != tok_fun) {
			log_syntax_error("Invalid use of security type. Must be followed by a variable or function identifier.");
			read_line();
			return get_tok();
		}
		return cur_tok;
	}

	//special or unknown character, such as a binary operator or paretheses
	++g_line_idx;
	return g_line[g_line_idx - 1];
}



//======================================================================================================
// Parsing the AST
//======================================================================================================

static int g_cur_tok; //current token
static int g_prev_tok = tok_eof; //previous token
static std::unique_ptr<ExprAST> parse_expression();
static std::unique_ptr<ExprAST> parse_binop_rhs(int expr_prec, std::unique_ptr<ExprAST> lhs);


// get_next_tok()
//	Update the current token g_cur_tok using the get_tok() function
static int get_next_tok() {
	g_prev_tok = g_cur_tok;
	g_prev_identifier_str = g_identifier_str;
	g_cur_tok = get_tok();
	debug_log(
		"`get_next_tok`. g_cur_tok: %i,\n \t\tg_identifier_str: %s\n",
		g_cur_tok, g_identifier_str.c_str()
	);
	return g_cur_tok;
}


// parse_double_expr()
//	Parse a double precision float number expression
static std::unique_ptr<ExprAST> parse_double_expr() {
	double d = strtod(g_number_str.c_str(), 0); // `g_number_str` is set by lexer
	auto result = std::make_unique<DoubleAST>(LocalType::type_double, d, g_sectype);
	g_sectype = SECURITY_MIN; //used up the security type, reset
	get_next_tok(); // consume the number
	return result;
}

// parse_nat_expr()
//	Parse a natural number expression
static std::unique_ptr<ExprAST> parse_nat_expr() {
	auto result = std::make_unique<NatAST>(LocalType::type_nat, g_number_str, g_sectype);
	g_sectype = SECURITY_MIN; //used up the security type, reset
	get_next_tok(); // consume the number
	return result;
}

// parse_int_expr()
//	Parse an integer expression
static std::unique_ptr<ExprAST> parse_int_expr() {
	//* I may need to see how many digits the number is and based on that may
	//* parse using more sophisticated function (e.g., create a vector and
	//* parse as an InfIntAST class)
	int d = std::stoi(g_number_str); // `g_number_str` is set by lexer
	auto result = std::make_unique<IntegerAST>(LocalType::type_int, d, g_sectype);
	g_sectype = SECURITY_MIN; //used up the security type, reset
	get_next_tok(); // consume the number
	return result;
}

// parse_num_expr()
//	Parse a number as either double or int
static std::unique_ptr<ExprAST> parse_num_expr() {
	if (g_number_has_period) {
		return parse_double_expr();
	}
	if (g_number_is_nat) {
		return parse_nat_expr();
	}
	return parse_int_expr();
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
	if (g_cur_tok != ')') {
		return log_syntax_error("expected ')'");
	}

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
	LocalType type = g_type;
	sectype security = g_sectype;
	g_sectype = SECURITY_MIN; //used up the security type, reset

	get_next_tok();  //eat identifier.
	

	if (g_cur_tok != '(') {
		// The identifier is a variable name. If it is not already defined, then its type is g_type
		bool new_var = true; //whether the identifier is new or already defined
		uint8_t scope = g_scope;
		if (is_named_var(id_name)) {
			type = g_var_names[id_name].type;
			new_var = false;
			scope = g_var_names[id_name].scope;
			security = g_var_names[id_name].security;
		}
		return std::make_unique<VariableExprAST>(type, id_name, new_var, scope, security); 
	}

	// The identifier is a function name. If it is not already defined, then its type is g_type
	if (is_named_fun(id_name)) {
		type = g_fun_names[id_name].fn.type;
		security = g_fun_names[id_name].fn.security;
	}

	get_next_tok();  // eat (
	std::vector<std::unique_ptr<ExprAST>> args;
	if (g_cur_tok != ')') {
		while (true) {
			std::unique_ptr<ExprAST> arg = parse_expression();
			if (arg) {
				args.push_back(std::move(arg));
			}
			else {
				return nullptr;
			}

			// We expect this to be `)` or `,` because `parse_expression()` ate 
			// ate a tokens
			if (g_cur_tok == ')')
				break;

			if (g_cur_tok != ',')
				return log_syntax_error("Expected ')' or ',' in argument list");
			
			get_next_tok();
		}
	}

	// Eat the ')'.
	get_next_tok();

	return std::make_unique<CallExprAST>(type, id_name, std::move(args), security);
}

// parse_scoped_block()
//	Parse block of code contained within curly braces
static std::unique_ptr<ExprAST> parse_scoped_block() {
	// scoped_block ::= 
	//   := '{' expression* '}'

	get_next_tok();  //eat '{'.

	if (g_scope == SCOPE_MAX) {
		return log_syntax_error("Too many nested blocks/scopes. All following compiler output is likely garbage.");
	}

	++g_scope;

	std::vector<std::unique_ptr<ExprAST>> exprs;
	while (g_cur_tok != '}') {
		std::unique_ptr<ExprAST> expr = parse_expression(); //automatically loads the next token
		if (expr) {
			exprs.push_back(std::move(expr));
		}
	}

	// Eat the '}'.
	get_next_tok();
	--g_scope;

	
	if (exprs.size() == 0) {
		//no expressions were generated
		return nullptr;
	}

	// Note: `flush_vars()` and `g_var_names` should only be used when parsing
	// and using it in `codegen` can cause unexpected behavior
	std::unique_ptr<BlockExprAST> block_code = std::make_unique<BlockExprAST>(
		exprs.back()->get_type(), std::move(exprs), g_scope, exprs.back()->get_security()
	);

	flush_vars();

	//the type of the block is the type of its return. CURRENTLY that is the final expression.
	return block_code;
}

static std::unique_ptr<ExprAST> parse_var_expr() {

	// if (g_scope == 0) {
	// 	get_next_tok();
	// 	return log_syntax_error("Scope must be nonzero to define a variable.");
	// }

	// Determine type of variable
	if (g_cur_tok != tok_var) {
		return log_syntax_error_p(
			"Expected variable type in variable definition"
		);
	}
	if (g_type_map.find(g_identifier_str) == g_type_map.end()) {
		return log_syntax_error_p("Unsupported variable type");
	}

	LocalType type = g_type;
	sectype security = g_sectype;
	g_sectype = SECURITY_MIN; //used up the security type, reset

	// Eat the type and get variable name
	get_next_tok();
	std::string var_name = g_identifier_str;
	//std::cout << var_name << std::endl;

	// Eat the variable name
	get_next_tok();

	return std::make_unique<LocalVariableExprAST>(
		g_type, var_name, security
	);
}

// parse_conditional_expr()
//	Parses an if statement, in the form: conditional expr ::= if condition {} else {}
static std::unique_ptr<ExprAST> parse_conditional_expr() {

	//if (g_scope == 0) {
	//	get_next_tok();
	//	return log_syntax_error("Scope must be nonzero to define an if statement.");
	//}

	get_next_tok();  // eat the if.

	// condition.
	auto cond_expr = parse_expression();
	if (!cond_expr) {
		return log_syntax_error("No condition was provided to the if expression.");
	}

	if (g_cur_tok != '{') {
		return log_syntax_error("Expected scoped block ('{') following condition expression.");
	}
	 
	auto then_expr = parse_scoped_block();
	if (!then_expr) {
		return log_syntax_error("Expected non-empty scoped block ('{') following condition expression.");
	}

	std::unique_ptr<ExprAST> else_expr;
	if (g_cur_tok == tok_else) {
		//there is an else command to run
		get_next_tok(); //eat the else

		if (g_cur_tok != '{') {
			return log_syntax_error("Expected scoped block ('{') following else expression.");
		}

		else_expr = parse_scoped_block();
		if (!else_expr) {
			return log_syntax_error("Expected non-empty scoped block ('{') following else expression.");
		}
	} 
	else {
		//there is no else command to run
		else_expr = nullptr;
	}

	return std::make_unique<IfExprAST>(std::move(cond_expr), std::move(then_expr), std::move(else_expr));
}


static std::unique_ptr<ExprAST> parse_while_expr() {
	// while ::= while ( cond_expr ) {}

	get_next_tok();  // eat the "while".

	std::unique_ptr<ExprAST> condition = parse_expression();
	if (!condition) {
		return log_syntax_error("Expected condition after while loop.");
	}

	if (g_cur_tok != '{') {
		return log_syntax_error("Expected scoped block ('{') following while condition.");
	}

	std::unique_ptr<ExprAST> body = parse_scoped_block();
	if (!body) {
		return log_syntax_error("Expected non-empty scoped block ('{') following while condition.");
	}

	return std::make_unique<WhileExprAST>(std::move(condition), std::move(body));
}

static std::unique_ptr<ExprAST> parse_setnat_expr() {
	// setnat ::= set_max_nat_bits ( int >= 32 )

	get_next_tok();  // eat the "set_max_nat_bits"
	if (g_scope != 0) {
		return log_syntax_error("Maximum nat size may only be adjusted in global scope.");
	}
	if (g_seen_nat) {
		return log_syntax_error("Maximum nat size may only be adjusted before any nat definition.");
	}

	if (g_cur_tok != '(') {
		return log_syntax_error("Expected parenthesis '(' after set_max_nat_bits built-in function.");
	}
	get_next_tok(); // eat '('
	if (g_cur_tok != tok_num || g_number_has_period || g_number_is_nat) {
		return log_syntax_error("Only integer literals may be passed to set_max_nat_bits.");
	}

	int d = std::stoi(g_number_str); // `g_number_str` is set by lexer
	get_next_tok(); // eat integer

	if (d < 32) {
		return log_syntax_error("The maximum number of bits in a nat must be at least 32.");
	}
	if (d % 32 != 0) {
		return log_syntax_error("The maximum number of bits in a nat must be divisible by 32.");
	}

	if (g_cur_tok != ')') {
		return log_syntax_error("Expected parenthesis ')' after set_max_nat_bits argument.");
	}
	get_next_tok(); //eat ')'

	//successful setnat function call
	g_max_nat_bits = d;

	//returning nullptr causes an error because it flags the top level expression as having failed.
	//it makes reasonable sense to just return the new maximum nat bit size.
	return std::make_unique<IntegerAST>(LocalType::type_int, d, SECURITY_MIN);
}

// parse_primary()
//	Determines the type of expression to parse and calls the appropriate handler
static std::unique_ptr<ExprAST> parse_primary() {
	// primary
	//   ::= identifierexpr
	//   ::= numberexpr
	//   ::= parenexpr
	
	//std::cout << g_cur_tok << std::endl;
	switch (g_cur_tok) {
	case tok_identifier:
		return parse_identifier_expr();
	case tok_num:
		return parse_num_expr();
	case '(':
		return parse_paren_expr();
	case tok_var:
		return parse_var_expr();
	case '{':
		return parse_scoped_block();
	case tok_if:
		return parse_conditional_expr();
	case tok_while:
		return parse_while_expr();
	case tok_setnat:
		return parse_setnat_expr();
	case ';':
		//ignore semicolons
		get_next_tok(); //eat ';'
		return nullptr;
	default:

		debug_log("g_curr_token: %i\n", g_cur_tok);
		return log_syntax_error("unknown token when expecting an expression");
	}
}

//mapping from binary operator to precedence value
static std::map<char, int> g_binop_precedence;

// set_binop_precedence()
//	Sets the precedence of all binary operators, such as +, -, *, and /.
static void set_binop_precedence() {
	//higher precedence is performed first
	g_binop_precedence['='] = 2;
	g_binop_precedence['<'] = 10;
	g_binop_precedence['>'] = 10;
	g_binop_precedence['%'] = 15;
	g_binop_precedence['+'] = 20;
	g_binop_precedence['-'] = 20;
	g_binop_precedence['*'] = 40;
	g_binop_precedence['/'] = 40;
}


// get_tok_precedence() 
//	Get the precedence of the pending binary operator token.
static int get_tok_precedence() {
	if (!isascii(g_cur_tok)) {
		return -1;
	}

	if (g_binop_precedence.empty()) {
		set_binop_precedence();
	}

	// Will automatically deal with invalid tokens because they're not in the
	// map
	int tok_prec = g_binop_precedence[g_cur_tok];
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
	std::unique_ptr<ExprAST> lhs = parse_primary();
	if (!lhs) {
		return nullptr;
	}
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
		if (tok_prec < expr_prec) {
			// We're already inside another `parse_binop_rhs` call so it will
			// deal with the current token
			// This will also return when the next token is not a binop
			return lhs;
		}

		int binop = g_cur_tok;
		get_next_tok();  // eat binop
		auto rhs = parse_primary();
		if (!rhs) {
			return nullptr;
		}

		// Now, `g_cur_tok` is the token after rhs as `parse_primary()` ate 
		// the ones before
		//if binop binds less tightly with rhs than the operator after rhs, let
		//the pending operator take rhs as its lhs.
		int next_prec = get_tok_precedence();
		if (tok_prec < next_prec) {
			//use tok_prec + 1 because we know that the rhs must be evaluated with 
			//minimally higher precedence than the current binop.
			rhs = parse_binop_rhs(tok_prec + 1, std::move(rhs));
			if (!rhs) {
				return nullptr;
			}
		}

		//merge lhs and rhs.
		LocalType use_type = lhs->get_type();
		if (lhs->get_type() != rhs->get_type()) {
			if (lhs->get_type() == type_double && rhs->get_type() == type_int) {
				use_type = type_double;
			}
			else if (lhs->get_type() == type_int && rhs->get_type() == type_double) {
				use_type = type_double;
			}
			else if (lhs->get_type() == type_int && rhs->get_type() == type_nat) {
				use_type = type_nat;
			}
			else if (lhs->get_type() == type_nat && rhs->get_type() == type_int) {
				use_type = type_nat;
			}
			else {
				//in particular, we should not do operations between doubles and nats
				log_compiler_error("Operand mismatch could not be resolved.");
				use_type = type_unsupported;
			}
		}
		lhs = std::make_unique<BinaryExprAST>(use_type, binop, std::move(lhs), std::move(rhs));
	}
}

// handle_function_optimization
//	Checks if the current identifier string is a string for turning off 
//  optimization and if so triggers the appropriate flag. Returns a boolean
//	indicating whether the current token is an optimization token.
bool handle_function_optimization() {
	if (g_identifier_str == g_no_function_optimization_tag) {
		g_disable_function_optimization = true;
		return true;
	}
	else {
		g_disable_function_optimization = false;
		return false;
	}
}

// parse_prototype
//	Parse a function prototype, i.e. its name and arguments
// TODO: Should be edited to reflect the type of the prototype
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

	LocalType type = g_type;
	sectype fn_security = g_sectype;
	g_sectype = SECURITY_MIN; //used security type, reset

	// Parsing the identifier of the function
	get_next_tok();
	if (handle_function_optimization()) {
		get_next_tok();
	}
	std::string fn_name = g_identifier_str;

	get_next_tok();
	if (g_cur_tok != '(')
		return log_syntax_error_p("Expected '(' in prototype");

	//read the list of argument names.
	std::vector<std::string> args;
	std::map<std::string, NameKeywords> arg_types;
	while (true) {
		//* Dealing with types of variables in function prototype
		get_next_tok();
		if (g_cur_tok != tok_var) {
			break;
		}
		LocalType arg_type = g_type_map[g_identifier_str];
		sectype security = g_sectype;
		g_sectype = SECURITY_MIN; //used security type, reset
		if (get_next_tok() != tok_identifier) {
			return log_syntax_error_p("Variable name was not specified");
		}
		args.push_back(g_identifier_str);
		NameKeywords nk;
		nk.type = arg_type;
		nk.is_fun = false;
		nk.scope = g_scope + 1;
		nk.security = security;
		arg_types[g_identifier_str] = nk;

		// Adding to `g_var_names`
		NameKeywords nk_copy = nk;
		g_var_names[g_identifier_str] = nk_copy;

		int next_tok = get_next_tok();
		if (next_tok == ')') {
			break;
		}
		if (next_tok != ',') {
			return log_syntax_error_p("Invalid function argument");
		}
	}
	
	if (g_cur_tok != ')')
		return log_syntax_error_p("Expected ')' in prototype");

	//success.
	get_next_tok();  // eat ')'.

	return std::make_unique<PrototypeAST>(type, fn_name, std::move(args), std::move(arg_types), fn_security);
}

// parse_function()
//	Parses a function by getting its protoype and its body expression
// TODO: should be edited to reflect the type of the function
static std::unique_ptr<FunctionAST> parse_function() {
	// TODO: might want to encode the type of the function here
	auto proto = parse_prototype();
	if (!proto) {
		return nullptr;
	}

	std::unique_ptr<AST::ExprAST> expr = parse_expression();

	if (!expr) {
		return nullptr;
	}

	if (proto->get_type() != expr->get_type()) {
		log_compiler_error("Function type does not match return type.");
		return nullptr;
	}
	if (proto->get_security() != expr->get_security()) {
		log_compiler_error("Function security does not match return security.");
		return nullptr;
	}

	std::unique_ptr<FunctionAST> fun_code = std::make_unique<FunctionAST>(std::move(proto), std::move(expr));

	// Moved inside the codegen as we want the variables to be availble 
	// inside the codegen
	flush_vars();

	return fun_code;
}

// parse_extern()
//	Parses an external import.
static std::unique_ptr<PrototypeAST> parse_extern() {
	//! At least according to the manual, forward declaration requirese extern
	//! Might be able to change that by parsing a prototype only without extern
	// external ::= 'extern' prototype
	get_next_tok();  // eat extern.
	std::unique_ptr<AST::PrototypeAST> proto = parse_prototype();
	flush_vars(); //remove argument names from the global variable list
	return proto;
}

static std::unique_ptr<FunctionAST> parse_top_level_expression() {
	if (g_scope != 0) {
		log_compiler_error("Scope was unexpectedly nonzero for a top level expression.");
		return nullptr;
	}
	/// toplevelexpr := expression
	sectype security = g_sectype;
	// We DO NOT set g_sectype = SECURITY_MIN since a top-level expression starting with a sectype needs that type.
	if (auto expr = parse_expression()) {
		// Make an anonymous proto.
		auto proto = std::make_unique<PrototypeAST>(
			expr->get_type(), "", std::vector<std::string>(),
			std::map<std::string, NameKeywords>(), security
		);
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
		log_syntax_error("Attempted and failed to parse a function.");
		get_next_tok();
		return;
	}
	fn_ptr->codegen();
}

static void handle_extern() {
	auto extern_ptr = parse_extern();
	if (!extern_ptr) {
		//skip token for error recovery
		log_syntax_error("Attempted and failed to parse an external function.");
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
		log_syntax_error("Attempted and failed to parse a top level expression.");
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
		//// These are helpful for checking that variables are out of scope after certain functions are defined
		//std::cout << "Current Names:" << std::endl;
		//for (const auto& [name, val] : g_named_values) {
		//	std::cout << name << std::endl;
		//}
		//std::cout << "Current Vars:" << std::endl;
		//for (const auto& [name, val] : g_var_names) {
		//	std::cout << name << std::endl;
		//}
		//std::cout << "Current Functions:" << std::endl;
		//for (const auto& [name, val] : g_fun_names) {
		//	std::cout << name << std::endl;
		//}
		switch (g_cur_tok) {
			case tok_eof:
				return;
			case ';': // ignore top-level semicolons.
				get_next_tok();
				break;
			// For defining function
			case tok_fun:
				handle_function();
				break;
			case tok_extern:
				// TODO: if we want forward definition with prototypes, we edit here
				handle_extern();
				break;
			default:
				handle_top_level_expression();
				break;
			}
	}
}



int main(int argc, char** argv) {
	debug_log("Debug log is active");
	if (argc < 2) {
		fprintf(stderr, "Usage: No .ank file provided to lexer.\n");
		return 1;
	}

	const char* filename = argv[1];  // Get filename from arguments
	
	bool return_llvm_ir = false;
	std::string snd_argument;
	std::string flag;
	if (argc > 2) {
	 	// std::string snd_argument(argv[2]);
		// std::string flag("--llvm-ir");
	 	snd_argument = argv[2];
		flag = "--llvm-ir";
		return_llvm_ir = snd_argument == flag;
	}

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

	
	//initialize by setting up the LLVM IR tools and getting the first token
	initialize_llvm_module();
	get_next_tok();
	
	//parse all code
	parse_file();

	if (g_seen_errors) {
		fprintf(stderr, "\nParsing failed due to listed errors.\n\n");
		return 1;
	}


	std::cerr << "\nFile parsed successfully.\n" << std::endl;

	if (return_llvm_ir) {
		//parsing was successful, print generated code
		g_module->print(errs(), nullptr);
		return 0;
	}

	auto target_spec = sys::getDefaultTargetTriple();

	InitializeAllTargetInfos();
	InitializeAllTargets();
	InitializeAllTargetMCs();
	InitializeAllAsmParsers();
	InitializeAllAsmPrinters();

	std::string error;
	llvm::Triple triple(target_spec);
	auto target = TargetRegistry::lookupTarget(triple.str(), error);
	// Print an error and exit if we couldn't find the requested target.
	// This generally occurs if we've forgotten to initialise the
	// TargetRegistry or we have a bogus target triple.
	if (!target) {
		errs() << error;
		return 1;
	}

	auto cpu_type = "generic";
	// no features (e.g. of featuers, SSE)
	auto features = "";

	TargetOptions target_options;
	auto target_machine = target->createTargetMachine(triple.str(), cpu_type, features, target_options, Reloc::PIC_);

	// TODO: check if this messes up optimizations
	g_module->setDataLayout(target_machine->createDataLayout());
	g_module->setTargetTriple(triple); //does this need to be triple.str() for Aghyad? If so, we need to do some ifdef nonsense.

	std::string input_filename(filename);
	auto output_filename = (
		input_filename.substr(0, input_filename.find(".")) + ".o"
	);
	std::error_code error_code;
	raw_fd_ostream dest(output_filename, error_code, llvm::sys::fs::OF_None);

	if (error_code) {
		errs() << "Could not open output file: " << error_code.message();
		return 1;
	}

	legacy::PassManager pass;
	auto file_type = CodeGenFileType::ObjectFile;

	if (target_machine->addPassesToEmitFile(pass, dest, nullptr, file_type)) {
	  errs() << "TargetMachine can't emit a file of this type";
		return 1;
	}

	pass.run(*g_module);
	dest.flush();

	return 0;
}