#ifdef _WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif
#include <string>
#include <iostream>
#include <cstdio>

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
	tok_special = -6,
};

static std::string g_line;				// Full line of standard input
static unsigned long g_line_idx;		// Index into g_line
static unsigned long g_line_count = 0;	// Line counter
bool g_seen_errors = false;				// Whether any errors were encountered while parsing tokens

static std::string g_identifier_str;	// Filled in for tok_identifier
static std::string g_number_str;		// Filled in for tok_num
static uint8_t g_special_char;			// Filled in if tok_special
FILE* g_file;							// Global file



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


// isspecial(c)
//	Checks whether c is a special character
static bool isspecial(char c) {
	const char* specials = "+-/*=()!%^&[]{}|\'\";:,.<>?";
	for (int i = 0; i < strlen(specials); ++i) {
		if (c == specials[i]) {
			return true;
		}
	}
	return false;
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
				fprintf(stderr, "[%lu, %lu]: SyntaxError: Invalid number of decimals in number.\n", g_line_count, g_line_idx);
				g_seen_errors = true;
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
		g_special_char = '/';
		++g_line_idx;
		return tok_special;
	}

	if (isspecial(g_line[g_line_idx])) {
		g_special_char = g_line[g_line_idx];
		++g_line_idx;
		return tok_special;
	}

	//unknown character
	fprintf(stderr, "[%lu, %lu]: SyntaxError: Unknown symbol \"%c\".\n", g_line_count, g_line_idx, g_line[g_line_idx]);
	g_seen_errors = true;
	++g_line_idx;
	return g_line[g_line_idx - 1];
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

	g_file = fopen(filename, "r");	 // Open file in read mode

	if (!g_file) {
		fprintf(stderr, "Error: Could not open file %s.\n", filename);
		return 1;
	}

	int x = get_tok();
	while (x != tok_eof) {
		std::cout << x << ", " << g_identifier_str << ", " << g_number_str << ", " << g_special_char << ", " << std::endl;
		x = get_tok();
	}
	return 0;
}