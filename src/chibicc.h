#define _POSIX_C_SOURCE 200809L                 ///< Defines POSIX version to 200809L for compliance
#include <assert.h>                             ///< For assert() macro to perform runtime assertions
#include <ctype.h>                              ///< For character classification and conversion
#include <errno.h>                              ///< For handling error numbers
#include <glob.h>                               ///< For pattern matching (glob)
#include <libgen.h>                             ///< For manipulating filenames
#include <stdarg.h>                             ///< For handling variable arguments in functions
#include <stdbool.h>                            ///< For boolean type support
#include <stdint.h>                             ///< For fixed-width integer types
#include <stdio.h>                              ///< For input/output functions
#include <stdlib.h>                             ///< For memory allocation, process control, etc.
#include <stdnoreturn.h>                        ///< For noreturn function attribute
#include <string.h>                             ///< For string manipulation functions
#include <strings.h>                            ///< For string comparison and manipulation
#include <sys/stat.h>                           ///< For file status operations
#include <sys/types.h>                          ///< For system data types
#include <sys/wait.h>                           ///< For process termination and status
#include <time.h>                               ///< For time manipulation
#include <unistd.h>                             ///< For POSIX operating system API

#define MAX(x, y) ((x) < (y) ? (y) : (x))       ///< Macro to return the maximum of two values
#define MIN(x, y) ((x) < (y) ? (x) : (y))       ///< Macro to return the minimum of two values

#ifndef __GNUC__
# define __attribute__(x) ///< Macro to avoid errors if GCC is not defined
#endif

/**
 * @brief Forward declarations of custom types used throughout the program.
 */
typedef struct Type Type;               ///< Forward declaration for Type structure
typedef struct Node Node;               ///< Forward declaration for Node structure
typedef struct Member Member;           ///< Forward declaration for Member structure
typedef struct Relocation Relocation;   ///< Forward declaration for Relocation structure
typedef struct Hideset Hideset;         ///< Forward declaration for Hideset structure

//
// strings.c
//

/**
 * @brief Structure to hold an array of strings with dynamic resizing.
 */
typedef struct {
	char ** data;           ///< Pointer to an array of string pointers
	int	capacity;       ///< Maximum capacity of the array
	int	len;            ///< Current length (number of elements) in the array
} StringArray;

/**
 * @brief Push a new string into the string array.
 *
 * @param arr The StringArray to which the string will be added
 * @param s The string to be added to the array
 */
void strarray_push(StringArray *arr, char *s);

/**
 * @brief A formatted string generator function, similar to printf.
 *
 * @param fmt The format string
 * @return A formatted string
 */
char *format(char *fmt, ...) __attribute__((format(printf, 1, 2))); ///< Function for formatted string creation

//
// tokenize.c
//

/**
 * @brief Enumeration of different token kinds used in the tokenizer.
 */
typedef enum {
	TK_IDENT,       ///< Identifiers
	TK_PUNCT,       ///< Punctuators (e.g., operators, separators)
	TK_KEYWORD,     ///< Keywords (e.g., "if", "while", etc.)
	TK_STR,         ///< String literals
	TK_NUM,         ///< Numeric literals
	TK_PP_NUM,      ///< Preprocessing numbers
	TK_EOF,         ///< End-of-file markers
} TokenKind;

/**
 * @brief Structure to represent a source file.
 */
typedef struct {
	char *	name;           ///< File name
	int	file_no;        ///< File number (used for error reporting)
	char *	contents;       ///< The contents of the file

	// For #line directive
	char *	display_name;   ///< Display name for the file in error reporting
	int	line_delta;     ///< Line number offset (used for #line directive)
} File;

/**
 * @brief Structure representing a token in the tokenizer.
 */
typedef struct Token Token;
struct Token {
	TokenKind	kind;           ///< The kind of the token
	Token *		next;           ///< Pointer to the next token
	int64_t		val;            ///< Integer value (used for TK_NUM)
	long double	fval;           ///< Floating point value (used for TK_NUM)
	char *		loc;            ///< Location in the source code (pointer to the token in the source)
	int		len;            ///< Length of the token (in characters)
	Type *		ty;             ///< Type associated with the token (for TK_NUM or TK_STR)
	char *		str;            ///< The string content of a string literal, including the terminating '\0'

	File *		file;           ///< Source file where the token was found
	char *		filename;       ///< Filename of the source file
	int		line_no;        ///< Line number where the token was found
	int		line_delta;     ///< Line number delta (for #line directive)
	bool		at_bol;         ///< True if this token is at the beginning of the line
	bool		has_space;      ///< True if this token is followed by a space character
	Hideset *	hideset;        ///< Used for macro expansion
	Token *		origin;         ///< Pointer to the original token if this is from macro expansion
};

/**
 * @brief Reports a fatal error and terminates the program.
 *
 * This function takes a format string and additional arguments similar to printf.
 * It prints the error message and terminates the program without returning.
 *
 * @param fmt The format string for the error message
 * @param ... The arguments for the format string
 */
noreturn void error(char *fmt, ...) __attribute__((format(printf, 1, 2)));

/**
 * @brief Reports a fatal error with a specified location and terminates the program.
 *
 * This function takes a location string (e.g., filename or line number) and a format string.
 * It prints the error message along with the location and then terminates the program without returning.
 *
 * @param loc The location where the error occurred (e.g., filename, line number)
 * @param fmt The format string for the error message
 * @param ... The arguments for the format string
 */
noreturn void error_at(char *loc, char *fmt, ...) __attribute__((format(printf, 2, 3)));

/**
 * @brief Reports a fatal error related to a specific token and terminates the program.
 *
 * This function takes a token and a format string, printing the error message associated with the token
 * before terminating the program without returning.
 *
 * @param tok The token associated with the error
 * @param fmt The format string for the error message
 * @param ... The arguments for the format string
 */
noreturn void error_tok(Token *tok, char *fmt, ...) __attribute__((format(printf, 2, 3)));

/**
 * @brief Issues a warning related to a specific token.
 *
 * This function takes a token and a format string, printing a warning message associated with the token.
 *
 * @param tok The token associated with the warning
 * @param fmt The format string for the warning message
 * @param ... The arguments for the format string
 */
void warn_tok(Token *tok, char *fmt, ...) __attribute__((format(printf, 2, 3)));

/**
 * @brief Compares a token with a specific operator string.
 *
 * This function checks if the given token matches the operator string.
 *
 * @param tok The token to compare
 * @param op The operator string to compare the token against
 * @return true if the token matches the operator string, false otherwise
 */
bool equal(Token *tok, char *op);

/**
 * @brief Skips a token if it matches a specific operator string.
 *
 * This function checks if the token matches the operator string. If it does, it returns the next token;
 * otherwise, it returns the original token.
 *
 * @param tok The current token
 * @param op The operator string to match the token against
 * @return The next token if the operator matches, the original token otherwise
 */
Token *skip(Token *tok, char *op);

/**
 * @brief Consumes a token if it matches a specific string and returns the rest of the tokens.
 *
 * This function checks if the current token matches the provided string. If it does, it advances the token
 * and returns the rest of the tokens.
 *
 * @param rest A pointer to the token list to store the remaining tokens
 * @param tok The current token
 * @param str The string to match the token against
 * @return true if the token matches the string, false otherwise
 */
bool consume(Token **rest, Token *tok, char *str);

/**
 * @brief Converts preprocessing tokens in the token list.
 *
 * This function processes preprocessing tokens and prepares them for further tokenization.
 *
 * @param tok The first token in the list to be processed
 */
void convert_pp_tokens(Token *tok);

/**
 * @brief Retrieves the list of input files for tokenization.
 *
 * This function returns a list of files that are currently being processed by the tokenizer.
 *
 * @return A list of File pointers representing the input files
 */
File **get_input_files(void);

/**
 * @brief Creates a new file object representing a source file.
 *
 * This function initializes a new file object with a given name, file number, and contents.
 *
 * @param name The name of the file
 * @param file_no The file number for error reporting
 * @param contents The contents of the file as a string
 * @return A pointer to the newly created File object
 */
File *new_file(char *name, int file_no, char *contents);

/**
 * @brief Tokenizes a string literal and returns the next token.
 *
 * This function tokenizes a string literal based on the specified base type and returns the token representing the string.
 *
 * @param tok The current token being processed
 * @param basety The base type for the string literal
 * @return The next token after tokenizing the string literal
 */
Token *tokenize_string_literal(Token *tok, Type *basety);

/**
 * @brief Tokenizes the contents of a file.
 *
 * This function reads and tokenizes the contents of the given file.
 *
 * @param file The file to tokenize
 * @return A list of tokens representing the file's contents
 */
Token *tokenize(File *file);

/**
 * @brief Tokenizes a file based on its filename.
 *
 * This function tokenizes a file based on its filename, reading its contents and returning the corresponding tokens.
 *
 * @param filename The name of the file to tokenize
 * @return A list of tokens representing the file's contents
 */
Token *tokenize_file(char *filename);

/**
 * @brief Marks a location as unreachable in the code.
 *
 * This macro reports an internal error and stops the execution of the program.
 * It is used in places that should never be reached.
 */
#define unreachable() \
	error("internal error at %s:%d", __FILE__, __LINE__) ///< Macro for marking unreachable code

/**
 * @brief Searches for an include file in the specified include paths.
 *
 * This function takes a filename and searches for the file in the configured include paths.
 * If the file is found, its path is returned; otherwise, NULL is returned.
 *
 * @param filename The name of the file to search for
 * @return The path to the file if found, or NULL if not found
 */
char *search_include_paths(char *filename);

/**
 * @brief Initializes the macros for the preprocessing phase.
 *
 * This function sets up the initial state for macro handling by defining standard macros
 * or performing any necessary setup before macro expansion begins.
 */
void init_macros(void);

/**
 * @brief Defines a new macro with the given name and body.
 *
 * This function defines a macro by associating it with a name and a corresponding body.
 * The body is a string that will be used during macro expansion.
 *
 * @param name The name of the macro
 * @param buf The body of the macro, typically a string
 */
void define_macro(char *name, char *buf);

/**
 * @brief Undefines a previously defined macro.
 *
 * This function removes a macro definition by its name, effectively making it unavailable
 * for the remainder of the preprocessing phase.
 *
 * @param name The name of the macro to undefine
 */
void undef_macro(char *name);

/**
 * @brief Preprocesses a list of tokens.
 *
 * This function takes a list of tokens and performs preprocessing steps such as macro expansion,
 * file inclusion, or conditional compilation. It returns the processed token list.
 *
 * @param tok The first token in the list to preprocess
 * @return The processed list of tokens after preprocessing
 */
Token *preprocess(Token *tok);

/**
 * @brief Represents an object (variable, function, etc.) in the program.
 *
 * This structure stores information about an object, including its type, name, and other attributes
 * that determine whether the object is a variable, function, or another entity.
 */
typedef struct Obj Obj;

/**
 * @brief Represents an object, such as a variable or function, in the program.
 *
 * This structure contains details about an object, including its type, alignment, scope,
 * and other characteristics depending on whether it is a local or global variable, function,
 * or special types like static or TLS (Thread-Local Storage).
 */
struct Obj {
	Obj *		next;           ///< Pointer to the next object in the list
	char *		name;           ///< Name of the object (e.g., variable or function)
	Type *		ty;             ///< Type of the object
	Token *		tok;            ///< The representative token of the object
	bool		is_local;       ///< Indicates whether the object is local or global/function
	int		align;          ///< Alignment of the object (in bytes)

	// Local variable
	int		offset; ///< Offset of the local variable within the stack frame

	// Global variable or function
	bool		is_function;    ///< Indicates whether the object is a function
	bool		is_definition;  ///< Indicates whether the object is a definition
	bool		is_static;      ///< Indicates whether the object is a static variable or function

	// Global variable
	bool		is_tentative;   ///< Indicates whether the global variable is tentative
	bool		is_tls;         ///< Indicates whether the object is a Thread-Local Storage (TLS) variable
	char *		init_data;      ///< Initialization data for the object (if applicable)
	Relocation *	rel;            ///< List of relocation entries for the object

	// Function
	bool		is_inline;      ///< Indicates whether the function is inline
	Obj *		params;         ///< List of function parameters
	Node *		body;           ///< Function body (for non-inline functions)
	Obj *		locals;         ///< Local variables within the function
	Obj *		va_area;        ///< Pointer to the variable argument area (for variadic functions)
	Obj *		alloca_bottom;  ///< Bottom of the stack for alloca-based allocations
	int		stack_size;     ///< Total stack size required by the function

	// Static inline function
	bool		is_live;        ///< Indicates whether the static inline function is live (i.e., actually used)
	bool		is_root;        ///< Indicates whether the function is the root of a call graph
	StringArray	refs;           ///< References to the function or object (used for static inline functions)
};

/**
 * @brief Represents a relocation entry for a global variable.
 *
 * Global variables can be initialized either by a constant expression or
 * by a pointer to another global variable. This structure represents the latter
 * case, where a relocation entry is needed to correctly link the global variable
 * to the referenced global variable.
 */
typedef struct Relocation Relocation;

/**
 * @brief Structure to represent a relocation entry.
 *
 * This structure holds information about the relocation of a global variable, including
 * its offset, a label (name of the referenced global variable), and an addend for
 * relocation calculations.
 */
struct Relocation {
	Relocation *	next;   ///< Pointer to the next relocation entry (if any)
	int		offset; ///< Offset where the relocation occurs
	char **		label;  ///< Pointer to the label (name) of the referenced global variable
	long		addend; ///< Addend to be applied during relocation
};

/**
 * @brief Enumeration of different kinds of AST nodes.
 *
 * This enumeration defines the various types of AST nodes that represent different
 * kinds of expressions, statements, and operations in the abstract syntax tree.
 */
typedef enum {
	ND_NULL_EXPR,   ///< Represents a no-op expression (does nothing)
	ND_ADD,         ///< Addition operator: "+"
	ND_SUB,         ///< Subtraction operator: "-"
	ND_MUL,         ///< Multiplication operator: "*"
	ND_DIV,         ///< Division operator: "/"
	ND_NEG,         ///< Unary negation operator: "-"
	ND_MOD,         ///< Modulo operator: "%"
	ND_BITAND,      ///< Bitwise AND operator: "&"
	ND_BITOR,       ///< Bitwise OR operator: "|"
	ND_BITXOR,      ///< Bitwise XOR operator: "^"
	ND_SHL,         ///< Bitwise left shift operator: "<<"
	ND_SHR,         ///< Bitwise right shift operator: ">>"
	ND_EQ,          ///< Equality comparison: "=="
	ND_NE,          ///< Inequality comparison: "!="
	ND_LT,          ///< Less than comparison: "<"
	ND_LE,          ///< Less than or equal to comparison: "<="
	ND_ASSIGN,      ///< Assignment operator: "="
	ND_COND,        ///< Conditional expression (ternary): "? :"
	ND_COMMA,       ///< Comma operator: ","
	ND_MEMBER,      ///< Member access operator: "." (used for struct member access)
	ND_ADDR,        ///< Address-of operator: "&" (unary)
	ND_DEREF,       ///< Dereference operator: "*" (unary)
	ND_NOT,         ///< Logical NOT operator: "!"
	ND_BITNOT,      ///< Bitwise NOT operator: "~"
	ND_LOGAND,      ///< Logical AND operator: "&&"
	ND_LOGOR,       ///< Logical OR operator: "||"
	ND_RETURN,      ///< "return" statement
	ND_IF,          ///< "if" statement
	ND_FOR,         ///< "for" or "while" loop
	ND_DO,          ///< "do" statement (do-while loop)
	ND_SWITCH,      ///< "switch" statement
	ND_CASE,        ///< "case" statement (in switch)
	ND_BLOCK,       ///< Block of statements: "{ ... }"
	ND_GOTO,        ///< "goto" statement
	ND_GOTO_EXPR,   ///< "goto" labels-as-values
	ND_LABEL,       ///< Labeled statement
	ND_LABEL_VAL,   ///< [GNU] Labels-as-values
	ND_FUNCALL,     ///< Function call expression
	ND_EXPR_STMT,   ///< Expression statement
	ND_STMT_EXPR,   ///< Statement expression (expression used as a statement)
	ND_VAR,         ///< Variable expression
	ND_VLA_PTR,     ///< Variable Length Array (VLA) designator
	ND_NUM,         ///< Integer constant
	ND_CAST,        ///< Type cast expression
	ND_MEMZERO,     ///< Zero-clear a stack variable
	ND_ASM,         ///< Assembly instruction: "asm"
	ND_CAS,         ///< Atomic compare-and-swap operation
	ND_EXCH         ///< Atomic exchange operation
} NodeKind;

/**
 * @brief Structure representing an AST node.
 *
 * Each node represents a distinct expression, statement, or operation in the program.
 * The `NodeKind` enumeration defines the type of node, and the `Node` structure holds
 * various data fields depending on the kind of node it represents.
 */
struct Node {
	NodeKind	kind;   ///< The type of the node (e.g., addition, variable, function call)
	Node *		next;   ///< Pointer to the next node in a sequence (used for lists of nodes)
	Type *		ty;     ///< Type of the expression (e.g., integer, pointer)
	Token *		tok;    ///< Representative token for the node (token from the lexer)

	Node *		lhs;    ///< Left-hand side for binary operators or assignments
	Node *		rhs;    ///< Right-hand side for binary operators or assignments

	// Control flow nodes (e.g., "if", "for")
	Node *		cond;   ///< Condition expression for "if" or "for" statements
	Node *		then;   ///< "Then" branch for "if" statements
	Node *		els;    ///< "Else" branch for "if" statements
	Node *		init;   ///< Initialization expression for "for" loops
	Node *		inc;    ///< Increment expression for "for" loops

	// Labels for "break" and "continue"
	char *		brk_label;      ///< Label for break statements
	char *		cont_label;     ///< Label for continue statements

	// Block or statement expression
	Node *		body; ///< Body of a block or statement expression

	// Struct member access
	Member *	member; ///< Struct member accessed by "." operator

	// Function call
	Type *		func_ty;        ///< Type of the function being called
	Node *		args;           ///< Arguments passed to the function
	bool		pass_by_stack;  ///< Whether the function arguments are passed via the stack
	Obj *		ret_buffer;     ///< Buffer to store return value (if needed)

	// Goto or labeled statement
	char *		label;          ///< Label name for goto or labeled statement
	char *		unique_label;   ///< Unique label for labels-as-values
	Node *		goto_next;      ///< Next node after a "goto" statement

	// Switch statement
	Node *		case_next;      ///< Next case node for the "switch"
	Node *		default_case;   ///< Default case for the "switch"

	// Case statement
	long		begin;  ///< Start value for a "case" statement
	long		end;    ///< End value for a "case" statement

	// "asm" string literal
	char *		asm_str; ///< Assembly code string for "asm" node

	// Atomic compare-and-swap (CAS) operation
	Node *		cas_addr;       ///< Address for CAS operation
	Node *		cas_old;        ///< Old value for CAS operation
	Node *		cas_new;        ///< New value for CAS operation

	// Atomic op= operators
	Obj *		atomic_addr;    ///< Address of the atomic operation
	Node *		atomic_expr;    ///< Expression for the atomic operation

	// Variable node
	Obj *		var; ///< The variable associated with this node

	// Numeric literal
	int64_t		val;    ///< Integer value for numeric literal nodes
	long double	fval;   ///< Floating-point value for numeric literal nodes
};

/**
 * @brief Creates a new cast expression node.
 *
 * This function creates a new AST node that represents a type cast. It takes
 * an existing expression node and a target type to cast the expression to.
 *
 * @param expr The expression to cast.
 * @param ty The target type to cast the expression to.
 * @return A new AST node representing the type cast.
 */
Node *new_cast(Node *expr, Type *ty);

/**
 * @brief Evaluates a constant expression.
 *
 * This function evaluates a constant expression and returns its value. It processes
 * the token stream to determine the result of the expression.
 *
 * @param rest A pointer to the rest of the token stream after processing the expression.
 * @param tok The token representing the start of the expression.
 * @return The evaluated constant expression value.
 */
int64_t const_expr(Token **rest, Token *tok);

/**
 * @brief Parses a token stream to generate an object.
 *
 * This function parses a token stream to produce an object representing a variable,
 * function, or other entities in the program.
 *
 * @param tok The token stream to parse.
 * @return A pointer to the object created from the token stream.
 */
Obj *parse(Token *tok);

/**
 * @brief Enumeration of type kinds.
 *
 * This enumeration defines various kinds of types supported by the compiler, including
 * basic types (e.g., integer, float), user-defined types (e.g., struct, union), and
 * type modifiers (e.g., pointer, array, function).
 */
typedef enum {
	TY_VOID,        ///< Void type (no value)
	TY_BOOL,        ///< Boolean type (true/false)
	TY_CHAR,        ///< Character type
	TY_SHORT,       ///< Short integer type
	TY_INT,         ///< Integer type
	TY_LONG,        ///< Long integer type
	TY_FLOAT,       ///< Floating-point type (single precision)
	TY_DOUBLE,      ///< Double precision floating-point type
	TY_LDOUBLE,     ///< Long double floating-point type
	TY_ENUM,        ///< Enum type
	TY_PTR,         ///< Pointer type
	TY_FUNC,        ///< Function type
	TY_ARRAY,       ///< Array type
	TY_VLA,         ///< Variable-length array type
	TY_STRUCT,      ///< Struct type
	TY_UNION,       ///< Union type
} TypeKind;

/**
 * @brief Structure representing a type.
 *
 * The `Type` structure is used to represent types in the compiler. It supports various
 * types including basic types (e.g., `int`, `float`), compound types (e.g., `struct`,
 * `union`), pointers, arrays, and functions.
 */
struct Type {
	TypeKind	kind;           ///< The type kind (e.g., `TY_INT`, `TY_PTR`)
	int		size;           ///< The size of the type in bytes (e.g., `sizeof()`)
	int		align;          ///< The alignment of the type in bytes
	bool		is_unsigned;    ///< Whether the type is unsigned (e.g., `unsigned int`)
	bool		is_atomic;      ///< Whether the type is atomic (e.g., `_Atomic int`)
	Type *		origin;         ///< Original type for compatibility checking

	// Pointer-to or array-of type. A pointer type and an array type share the same
	// member to represent their dual nature in C. This helps to simplify handling
	// of pointer/array types in many contexts.
	Type *		base; ///< The base type for pointers or arrays

	// Declaration details
	Token *		name;           ///< The name of the type (for debugging or error messages)
	Token *		name_pos;       ///< The position of the type name in the source code

	// Array-specific data
	int		array_len; ///< The length of the array (number of elements)

	// Variable-length array (VLA) specifics
	Node *		vla_len;        ///< The expression representing the number of elements in the VLA
	Obj *		vla_size;       ///< The `sizeof()` value of the variable-length array

	// Struct-specific data
	Member *	members;        ///< The members of the struct (a linked list of `Member`)
	bool		is_flexible;    ///< Whether the struct has flexible array members
	bool		is_packed;      ///< Whether the struct is packed (without padding)

	// Function-specific data
	Type *		return_ty;      ///< The return type of the function
	Type *		params;         ///< The parameters of the function (a linked list of types)
	bool		is_variadic;    ///< Whether the function is variadic (accepts variable arguments)
	Type *		next;           ///< The next type (for chaining types, used in function parameters)
};

/**
 * @brief Structure representing a member of a struct or union.
 *
 * The `Member` structure holds information about a member of a `struct` or `union`, including
 * its type, name, and offsets. It also contains special fields for bitfields, which allow
 * storing integers in a more compact way within a struct.
 */
struct Member {
	Member *next;   ///< Pointer to the next member in the list
	Type *	ty;     ///< The type of the member (e.g., `int`, `char`)
	Token * tok;    ///< The token representing the member (used for error reporting)
	Token * name;   ///< The name of the member
	int	idx;    ///< The index of the member in the struct/union
	int	align;  ///< The alignment of the member
	int	offset; ///< The offset of the member within the struct/union

	// Bitfield-specific data
	bool	is_bitfield;    ///< Whether the member is a bitfield
	int	bit_offset;     ///< The bit offset within the bitfield
	int	bit_width;      ///< The width of the bitfield in bits
};

/**
 * @brief Declared basic types for the compiler.
 *
 * These external variables represent the basic types used in the compiler. They
 * are defined elsewhere in the code and are used throughout the compiler to represent
 * specific types.
 */
extern Type *ty_void;           ///< Void type (no value)
extern Type *ty_bool;           ///< Boolean type (true/false)
extern Type *ty_char;           ///< Character type
extern Type *ty_short;          ///< Short integer type
extern Type *ty_int;            ///< Integer type
extern Type *ty_long;           ///< Long integer type

extern Type *ty_uchar;          ///< Unsigned character type
extern Type *ty_ushort;         ///< Unsigned short integer type
extern Type *ty_uint;           ///< Unsigned integer type
extern Type *ty_ulong;          ///< Unsigned long integer type

extern Type *ty_float;          ///< Single precision floating-point type
extern Type *ty_double;         ///< Double precision floating-point type
extern Type *ty_ldouble;        ///< Long double floating-point type

/**
 * @brief Checks if a type is an integer.
 *
 * This function checks whether a given type is an integer type, which includes
 * both signed and unsigned integer types (e.g., `int`, `short`, `long`, `unsigned int`).
 *
 * @param ty The type to check.
 * @return `true` if the type is an integer, `false` otherwise.
 */
bool is_integer(Type *ty);

/**
 * @brief Checks if a type is a floating-point number.
 *
 * This function checks whether a given type is a floating-point number, including
 * types such as `float`, `double`, and `long double`.
 *
 * @param ty The type to check.
 * @return `true` if the type is a floating-point number, `false` otherwise.
 */
bool is_flonum(Type *ty);

/**
 * @brief Checks if a type is numeric.
 *
 * This function checks whether a given type is numeric, which includes both integer
 * and floating-point types.
 *
 * @param ty The type to check.
 * @return `true` if the type is numeric, `false` otherwise.
 */
bool is_numeric(Type *ty);

/**
 * @brief Checks if two types are compatible.
 *
 * This function checks whether two types are compatible, which is useful for type
 * coercion or type compatibility in operations and assignments.
 *
 * @param t1 The first type.
 * @param t2 The second type.
 * @return `true` if the types are compatible, `false` otherwise.
 */
bool is_compatible(Type *t1, Type *t2);

/**
 * @brief Creates a copy of a type.
 *
 * This function creates a copy of the given type. This is used when a new type
 * needs to be created based on an existing type, preserving the original properties
 * of the type.
 *
 * @param ty The type to copy.
 * @return A new type that is a copy of the given type.
 */
Type *copy_type(Type *ty);

/**
 * @brief Creates a pointer type from a base type.
 *
 * This function creates a pointer type to the given base type. This is used to
 * represent pointer types in the type system.
 *
 * @param base The base type to create a pointer to.
 * @return A new type representing a pointer to the base type.
 */
Type *pointer_to(Type *base);

/**
 * @brief Creates a function type with a return type.
 *
 * This function creates a function type with the specified return type. The function
 * type can be used to represent function signatures.
 *
 * @param return_ty The return type of the function.
 * @return A new type representing the function with the given return type.
 */
Type *func_type(Type *return_ty);

/**
 * @brief Creates an array type with a specified size.
 *
 * This function creates an array type with the given base type and size. This is
 * used to represent arrays in the type system.
 *
 * @param base The base type of the array elements.
 * @param size The number of elements in the array.
 * @return A new type representing the array.
 */
Type *array_of(Type *base, int size);

/**
 * @brief Creates a variable-length array (VLA) type.
 *
 * This function creates a variable-length array (VLA) type. VLAs allow arrays to
 * have a size that is determined at runtime.
 *
 * @param base The base type of the array elements.
 * @param expr The expression representing the number of elements in the VLA.
 * @return A new type representing the VLA.
 */
Type *vla_of(Type *base, Node *expr);

/**
 * @brief Creates an enum type.
 *
 * This function creates a new enum type. Enums are used to represent sets of
 * named integer constants.
 *
 * @return A new enum type.
 */
Type *enum_type(void);

/**
 * @brief Creates a struct type.
 *
 * This function creates a new struct type. Structs are used to represent
 * collections of different types of data grouped together under a single name.
 *
 * @return A new struct type.
 */
Type *struct_type(void);

/**
 * @brief Adds a type to a node.
 *
 * This function adds a type to the given AST node. This is typically used during
 * the type checking phase, where types are assigned to various expressions and
 * statements.
 *
 * @param node The AST node to add the type to.
 */
void add_type(Node *node);

/**
 * @brief Generates machine code for the given program.
 *
 * This function generates the final machine code for the program and writes it
 * to the specified output file.
 *
 * @param prog The program to generate code for.
 * @param out The output file to write the generated machine code to.
 */
void codegen(Obj *prog, FILE *out);

/**
 * @brief Aligns a number to a specified alignment boundary.
 *
 * This function aligns the number `n` to the next multiple of `align`.
 *
 * @param n The number to be aligned.
 * @param align The alignment boundary.
 * @return The aligned number.
 */
int align_to(int n, int align);

/**
 * @brief Encodes a Unicode character as a UTF-8 sequence.
 *
 * This function encodes a single Unicode character (`c`) into a UTF-8 sequence
 * and writes it to the provided buffer `buf`.
 *
 * @param buf The buffer to write the encoded UTF-8 sequence to.
 * @param c The Unicode character to encode.
 * @return The number of bytes written to the buffer.
 */
int encode_utf8(char *buf, uint32_t c);

/**
 * @brief Decodes a UTF-8 sequence into a Unicode character.
 *
 * This function decodes a UTF-8 sequence from the provided pointer `p` and
 * returns the decoded Unicode character. It also updates the pointer `new_pos`
 * to point to the next byte after the decoded character.
 *
 * @param new_pos Pointer to the position in the string after the decoded character.
 * @param p The UTF-8 sequence to decode.
 * @return The decoded Unicode character.
 */
uint32_t decode_utf8(char **new_pos, char *p);

/**
 * @brief Checks if a Unicode character is a valid first character for an identifier.
 *
 * This function checks whether the character `c` can be used as the first
 * character of an identifier (e.g., variable name).
 *
 * @param c The Unicode character to check.
 * @return `true` if the character can be the first character of an identifier, `false` otherwise.
 */
bool is_ident1(uint32_t c);

/**
 * @brief Checks if a Unicode character is a valid subsequent character for an identifier.
 *
 * This function checks whether the character `c` can be used as any subsequent
 * character in an identifier (e.g., variable name).
 *
 * @param c The Unicode character to check.
 * @return `true` if the character can be a subsequent character of an identifier, `false` otherwise.
 */
bool is_ident2(uint32_t c);

/**
 * @brief Computes the display width of a string.
 *
 * This function computes the visual display width of the string `p` with a given
 * length `len`. This is useful for formatting purposes (e.g., column alignment).
 *
 * @param p The string to check.
 * @param len The length of the string.
 * @return The display width of the string.
 */
int display_width(char *p, int len);

/**
 * @brief Hash entry for use in a hashmap.
 *
 * This structure represents an entry in a hashmap, storing a key-value pair.
 */
typedef struct {
	char *	key;    ///< The key of the entry.
	int	keylen; ///< The length of the key.
	void *	val;    ///< The value associated with the key.
} HashEntry;

/**
 * @brief Hashmap for storing key-value pairs.
 *
 * This structure represents a hashmap used to store a collection of key-value pairs.
 */
typedef struct {
	HashEntry *	buckets;        ///< Array of hash entries (buckets).
	int		capacity;       ///< The total number of buckets in the hashmap.
	int		used;           ///< The number of buckets currently used (i.e., number of entries).
} HashMap;

/**
 * @brief Retrieves the value associated with a key in a hashmap.
 *
 * This function looks up the value associated with the given key in the provided
 * hashmap. It returns a pointer to the value if the key is found, or `NULL` otherwise.
 *
 * @param map The hashmap to search.
 * @param key The key to look up.
 * @return A pointer to the value associated with the key, or `NULL` if the key is not found.
 */
void *hashmap_get(HashMap *map, char *key);

/**
 * @brief Retrieves the value associated with a key in a hashmap (key length variant).
 *
 * This function is similar to `hashmap_get`, but it allows for specifying the length
 * of the key, rather than assuming it is null-terminated.
 *
 * @param map The hashmap to search.
 * @param key The key to look up.
 * @param keylen The length of the key.
 * @return A pointer to the value associated with the key, or `NULL` if the key is not found.
 */
void *hashmap_get2(HashMap *map, char *key, int keylen);

/**
 * @brief Inserts a key-value pair into a hashmap.
 *
 * This function inserts a new key-value pair into the hashmap. If the key already
 * exists, its value will be updated.
 *
 * @param map The hashmap to insert the key-value pair into.
 * @param key The key to insert.
 * @param val The value to associate with the key.
 */
void hashmap_put(HashMap *map, char *key, void *val);

/**
 * @brief Inserts a key-value pair into a hashmap (key length variant).
 *
 * This function is similar to `hashmap_put`, but it allows for specifying the length
 * of the key.
 *
 * @param map The hashmap to insert the key-value pair into.
 * @param key The key to insert.
 * @param keylen The length of the key.
 * @param val The value to associate with the key.
 */
void hashmap_put2(HashMap *map, char *key, int keylen, void *val);

/**
 * @brief Deletes a key-value pair from a hashmap.
 *
 * This function removes the key-value pair associated with the given key from the hashmap.
 *
 * @param map The hashmap to delete the key-value pair from.
 * @param key The key to delete.
 */
void hashmap_delete(HashMap *map, char *key);

/**
 * @brief Deletes a key-value pair from a hashmap (key length variant).
 *
 * This function is similar to `hashmap_delete`, but it allows for specifying the length
 * of the key.
 *
 * @param map The hashmap to delete the key-value pair from.
 * @param key The key to delete.
 * @param keylen The length of the key.
 */
void hashmap_delete2(HashMap *map, char *key, int keylen);

/**
 * @brief Runs a test on the hashmap.
 *
 * This function runs tests to ensure that the hashmap operations (insert, get, delete)
 * are functioning correctly.
 */
void hashmap_test(void);

/**
 * @brief Checks if a file exists at the specified path.
 *
 * This function checks whether a file exists at the specified `path`. It returns
 * `true` if the file exists, and `false` otherwise.
 *
 * @param path The path to check.
 * @return `true` if the file exists, `false` otherwise.
 */
bool file_exists(char *path);

/**
 * @brief External variables for compiler configuration.
 *
 * These external variables are used to store configuration values such as
 * include paths, compiler flags, and the base filename.
 */
extern StringArray include_paths;       ///< List of include paths for the compiler.
extern bool opt_fpic;                   ///< Flag for position-independent code (PIC) generation.
extern bool opt_fcommon;                ///< Flag for handling common symbols.
extern char *base_file;                 ///< The base filename of the program being compiled.
