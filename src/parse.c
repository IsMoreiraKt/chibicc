#include "chibicc.h"

/**
 * @struct VarScope
 * @brief Scope for local variables, global variables, typedefs, or enum constants.
 *
 * This structure represents the scope of variables, including any typedefs or
 * enum constants. It contains the object representing the variable, its type,
 * and any related enum type or value.
 */
typedef struct {
	Obj *	var;            /**< The variable object. */
	Type *	type_def;       /**< The typedef definition, if applicable. */
	Type *	enum_ty;        /**< The enum type, if applicable. */
	int	enum_val;       /**< The enum value, if applicable. */
} VarScope;

/**
 * @struct Scope
 * @brief Represents a block scope in the program.
 *
 * A scope represents a block of code, where variables and typedefs are
 * stored. There are two kinds of block scopes in C: one for variables/typedefs
 * and the other for struct/union/enum tags. This structure contains maps for
 * both kinds of entities.
 */
typedef struct Scope Scope;
struct Scope {
	Scope * next;   /**< Pointer to the next scope in the chain. */

	HashMap vars;   /**< HashMap storing variables and typedefs. */
	HashMap tags;   /**< HashMap storing struct/union/enum tags. */
};

/**
 * @struct VarAttr
 * @brief Represents the attributes of a variable.
 *
 * This structure contains attributes for a variable such as whether it's a
 * typedef, static, extern, inline, or TLS, and its alignment.
 */
typedef struct {
	bool	is_typedef;     /**< True if the variable is a typedef. */
	bool	is_static;      /**< True if the variable is static. */
	bool	is_extern;      /**< True if the variable is extern. */
	bool	is_inline;      /**< True if the variable is inline. */
	bool	is_tls;         /**< True if the variable is thread-local storage (TLS). */
	int	align;          /**< The alignment of the variable. */
} VarAttr;

/**
 * @struct Initializer
 * @brief Represents a variable initializer.
 *
 * This structure is used to represent the initializer of a variable. It supports
 * nested initializers, such as in the case of arrays or structs.
 * The structure is tree-like to accommodate aggregate types.
 */
typedef struct Initializer Initializer;
struct Initializer {
	Initializer *	next;           /**< Pointer to the next initializer. */
	Type *		ty;             /**< The type of the variable being initialized. */
	Token *		tok;            /**< The token representing the initializer. */
	bool		is_flexible;    /**< True if the initializer is flexible. */
	Node *		expr;           /**< The initialization expression, if it's not an aggregate type. */
	Initializer **	children;       /**< Children initializers for aggregate types. */
	Member *	mem;            /**< The member of a union being initialized, if applicable. */
};

/**
 * @struct InitDesg
 * @brief Represents a local variable initializer descriptor.
 *
 * This structure helps manage initializers for local variables, including
 * handling arrays, structs, and other complex initializations.
 */
typedef struct InitDesg InitDesg;
struct InitDesg {
	InitDesg *	next;   /**< Pointer to the next initialization descriptor. */
	int		idx;    /**< Index of the initializer in case of an array. */
	Member *	member; /**< The member being initialized in a struct. */
	Obj *		var;    /**< The variable being initialized. */
};

/**
 * @var locals
 * @brief List of all local variable instances created during parsing.
 *
 * This static list accumulates all the local variables encountered during
 * the parsing phase.
 */
static Obj *locals;

/**
 * @var globals
 * @brief List of all global variables encountered during parsing.
 *
 * This static list accumulates all global variables encountered during
 * the parsing phase.
 */
static Obj *globals;

/**
 * @var scope
 * @brief Pointer to the current scope being parsed.
 *
 * This variable points to the current scope being parsed. It represents
 * the block-level scope of the program.
 */
static Scope *scope = &(Scope){};

/**
 * @var current_fn
 * @brief Pointer to the function object being parsed.
 *
 * This variable holds the function object that the parser is currently
 * parsing, allowing tracking of the current function's context.
 */
static Obj *current_fn;

/**
 * @var gotos
 * @brief List of all goto statements in the current function.
 *
 * This list accumulates all the goto statements found within the current
 * function.
 */
static Node *gotos;

/**
 * @var labels
 * @brief List of all labels in the current function.
 *
 * This list accumulates all the labels found within the current function.
 */
static Node *labels;

/**
 * @var brk_label
 * @brief The label for the current "break" statement target.
 *
 * This label indicates the target for the current "break" statement within
 * the function.
 */
static char *brk_label;

/**
 * @var cont_label
 * @brief The label for the current "continue" statement target.
 *
 * This label indicates the target for the current "continue" statement within
 * the function.
 */
static char *cont_label;

/**
 * @var current_switch
 * @brief Pointer to the switch node if parsing a switch statement.
 *
 * This pointer indicates the node representing the switch statement
 * being parsed. If not parsing a switch, it will be NULL.
 */
static Node *current_switch;

/**
 * @var builtin_alloca
 * @brief Represents the built-in `alloca` function.
 *
 * This variable points to the object representing the built-in `alloca` function,
 * used for dynamic memory allocation on the stack.
 */
static Obj *builtin_alloca;

/**
 * @brief Checks if the token represents a typename.
 * @param tok The token to check.
 * @return True if the token is a typename, false otherwise.
 */
static bool is_typename(Token *tok);

/**
 * @brief Processes a type declaration and returns the corresponding type.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @param attr Variable attributes to apply during processing.
 * @return The resulting type after processing the declaration.
 */
static Type *declspec(Token **rest, Token *tok, VarAttr *attr);

/**
 * @brief Processes a typename specifier and returns the corresponding type.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting type after processing the typename.
 */
static Type *typename(Token **rest, Token *tok);

/**
 * @brief Processes an enum specifier and returns the corresponding type.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting type after processing the enum specifier.
 */
static Type *enum_specifier(Token **rest, Token *tok);

/**
 * @brief Processes a typeof specifier and returns the corresponding type.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting type after processing the typeof specifier.
 */
static Type *typeof_specifier(Token **rest, Token *tok);

/**
 * @brief Processes a type suffix and returns the corresponding type.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @param ty The base type to which the suffix is applied.
 * @return The resulting type after applying the suffix.
 */
static Type *type_suffix(Token **rest, Token *tok, Type *ty);

/**
 * @brief Processes a declarator and returns the resulting type.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @param ty The base type to which the declarator is applied.
 * @return The resulting type after processing the declarator.
 */
static Type *declarator(Token **rest, Token *tok, Type *ty);

/**
 * @brief Processes a variable declaration and returns a corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @param basety The base type for the declaration.
 * @param attr Variable attributes to apply during processing.
 * @return The resulting node representing the declaration.
 */
static Node *declaration(Token **rest, Token *tok, Type *basety, VarAttr *attr);

/**
 * @brief Processes an array initializer and recursively initializes elements.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @param init The initializer to process.
 * @param i The index of the element to initialize.
 */
static void array_initializer2(Token **rest, Token *tok, Initializer *init, int i);

/**
 * @brief Processes a struct initializer and recursively initializes members.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @param init The initializer to process.
 * @param mem The struct member to initialize.
 */
static void struct_initializer2(Token **rest, Token *tok, Initializer *init, Member *mem);

/**
 * @brief Processes a general initializer and recursively initializes.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @param init The initializer to process.
 */
static void initializer2(Token **rest, Token *tok, Initializer *init);

/**
 * @brief Initializes a variable or aggregate type.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @param ty The type of the variable or aggregate.
 * @param new_ty A pointer to store the new type after processing.
 * @return The resulting initializer.
 */
static Initializer *initializer(Token **rest, Token *tok, Type *ty, Type **new_ty);

/**
 * @brief Processes the initialization of a local variable.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @param var The local variable being initialized.
 * @return The resulting node representing the initializer.
 */
static Node *lvar_initializer(Token **rest, Token *tok, Obj *var);

/**
 * @brief Processes the initialization of a global variable.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @param var The global variable being initialized.
 */
static void gvar_initializer(Token **rest, Token *tok, Obj *var);

/**
 * @brief Processes a compound statement and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the compound statement.
 */
static Node *compound_stmt(Token **rest, Token *tok);

/**
 * @brief Processes a statement and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the statement.
 */
static Node *stmt(Token **rest, Token *tok);

/**
 * @brief Processes an expression statement and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the expression statement.
 */
static Node *expr_stmt(Token **rest, Token *tok);

/**
 * @brief Processes an expression and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the expression.
 */
static Node *expr(Token **rest, Token *tok);

/**
 * @brief Evaluates an expression and returns its integer value.
 * @param node The node representing the expression.
 * @return The evaluated value of the expression.
 */
static int64_t eval(Node *node);

/**
 * @brief Evaluates an expression with label tracking and returns its value.
 * @param node The node representing the expression.
 * @param label Pointer to track labels.
 * @return The evaluated value of the expression.
 */
static int64_t eval2(Node *node, char ***label);

/**
 * @brief Evaluates the right-hand value of an expression.
 * @param node The node representing the expression.
 * @param label Pointer to track labels.
 * @return The evaluated right-hand value.
 */
static int64_t eval_rval(Node *node, char ***label);

/**
 * @brief Checks if an expression is a constant expression.
 * @param node The node representing the expression.
 * @return True if the expression is constant, false otherwise.
 */
static bool is_const_expr(Node *node);

/**
 * @brief Processes an assignment expression and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the assignment.
 */
static Node *assign(Token **rest, Token *tok);

/**
 * @brief Processes a logical OR expression and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the logical OR expression.
 */
static Node *logor(Token **rest, Token *tok);

/**
 * @brief Evaluates a double expression and returns its value.
 * @param node The node representing the expression.
 * @return The evaluated double value of the expression.
 */
static double eval_double(Node *node);

/**
 * @brief Processes a conditional expression and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the conditional expression.
 */
static Node *conditional(Token **rest, Token *tok);

/**
 * @brief Processes a logical AND expression and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the logical AND expression.
 */
static Node *logand(Token **rest, Token *tok);

/**
 * @brief Processes a bitwise OR expression and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the bitwise OR expression.
 */
static Node * bitor (Token * *rest, Token *tok);

/**
 * @brief Processes a bitwise XOR expression and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the bitwise XOR expression.
 */
static Node *bitxor(Token **rest, Token *tok);

/**
 * @brief Processes a bitwise AND expression and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the bitwise AND expression.
 */
static Node * bitand (Token * *rest, Token *tok);

/**
 * @brief Processes an equality expression and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the equality expression.
 */
static Node *equality(Token **rest, Token *tok);

/**
 * @brief Processes a relational expression and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the relational expression.
 */
static Node *relational(Token **rest, Token *tok);

/**
 * @brief Processes a shift expression and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the shift expression.
 */
static Node *shift(Token **rest, Token *tok);

/**
 * @brief Processes an addition expression and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the addition expression.
 */
static Node *add(Token **rest, Token *tok);

/**
 * @brief Creates a new addition node.
 * @param lhs The left-hand side node.
 * @param rhs The right-hand side node.
 * @param tok The current token.
 * @return The resulting addition node.
 */
static Node *new_add(Node *lhs, Node *rhs, Token *tok);

/**
 * @brief Creates a new subtraction node.
 * @param lhs The left-hand side node.
 * @param rhs The right-hand side node.
 * @param tok The current token.
 * @return The resulting subtraction node.
 */
static Node *new_sub(Node *lhs, Node *rhs, Token *tok);

/**
 * @brief Processes a multiplication expression and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the multiplication expression.
 */
static Node *mul(Token **rest, Token *tok);

/**
 * @brief Processes a type cast expression and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the cast expression.
 */
static Node *cast(Token **rest, Token *tok);

/**
 * @brief Retrieves a member from a struct type.
 * @param ty The struct type.
 * @param tok The token representing the member.
 * @return The struct member corresponding to the token.
 */
static Member *get_struct_member(Type *ty, Token *tok);

/**
 * @brief Processes a struct declaration and returns the corresponding type.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting struct type after processing the declaration.
 */
static Type *struct_decl(Token **rest, Token *tok);

/**
 * @brief Processes a union declaration and returns the corresponding type.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting union type after processing the declaration.
 */
static Type *union_decl(Token **rest, Token *tok);

/**
 * @brief Processes a postfix expression and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the postfix expression.
 */
static Node *postfix(Token **rest, Token *tok);

/**
 * @brief Processes a function call expression and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @param node The node representing the function being called.
 * @return The resulting node representing the function call.
 */
static Node *funcall(Token **rest, Token *tok, Node *node);

/**
 * @brief Processes a unary expression and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the unary expression.
 */
static Node *unary(Token **rest, Token *tok);

/**
 * @brief Processes a primary expression and returns the corresponding node.
 * @param rest Pointer to the rest of the tokens to process.
 * @param tok The current token.
 * @return The resulting node representing the primary expression.
 */
static Node *primary(Token **rest, Token *tok);

/**
 * @brief Parses a typedef and returns the resulting token.
 * @param tok The current token.
 * @param basety The base type for the typedef.
 * @return The resulting token after parsing the typedef.
 */
static Token *parse_typedef(Token *tok, Type *basety);

/**
 * @brief Checks if the token represents a function.
 * @param tok The token to check.
 * @return True if the token represents a function, false otherwise.
 */
static bool is_function(Token *tok);

/**
 * @brief Parses a function definition and returns the resulting token.
 * @param tok The current token.
 * @param basety The base type for the function.
 * @param attr The attributes for the function.
 * @return The resulting token after parsing the function.
 */
static Token *function(Token *tok, Type *basety, VarAttr *attr);

/**
 * @brief Parses a global variable definition and returns the resulting token.
 * @param tok The current token.
 * @param basety The base type for the global variable.
 * @param attr The attributes for the variable.
 * @return The resulting token after parsing the global variable.
 */
static Token *global_variable(Token *tok, Type *basety, VarAttr *attr);

/**
 * @brief Aligns a value down to the nearest multiple of `align`.
 *
 * This function ensures that the value `n` is rounded down to the nearest
 * multiple of the specified `align` value. This is typically used for memory
 * alignment purposes.
 *
 * @param n The value to align.
 * @param align The alignment boundary.
 * @return The value aligned down to the nearest multiple of `align`.
 */
static int align_down(int n, int align)
{
	return align_to(n - align + 1, align);
}

/**
 * @brief Enters a new scope, adding it to the scope stack.
 *
 * This function creates a new scope and places it on top of the current scope
 * stack, making it the current scope. The scope will be removed when
 * `leave_scope` is called.
 */
static void enter_scope(void)
{
	Scope *sc = calloc(1, sizeof(Scope));

	sc->next = scope;
	scope = sc;
}

/**
 * @brief Leaves the current scope, removing it from the scope stack.
 *
 * This function pops the topmost scope from the current scope stack, effectively
 * leaving the scope. The previous scope becomes the current scope.
 */
static void leave_scope(void)
{
	scope = scope->next;
}

/**
 * @brief Finds a variable by name in the current and all outer scopes.
 *
 * This function searches for a variable with the name specified in the token
 * `tok` in the current scope and all outer scopes. It returns the variable scope
 * if found, or NULL if the variable does not exist.
 *
 * @param tok The token representing the variable name.
 * @return A pointer to the variable scope if found, or NULL if not found.
 */
static VarScope *find_var(Token *tok)
{
	for (Scope *sc = scope; sc; sc = sc->next) {
		VarScope *sc2 = hashmap_get2(&sc->vars, tok->loc, tok->len);
		if (sc2)
			return sc2;
	}
	return NULL;
}

/**
 * @brief Finds a tag (struct, union, or enum) by its name in the current and all outer scopes.
 *
 * This function searches for a tag (e.g., struct, union, or enum) with the name
 * specified in the token `tok` in the current scope and all outer scopes. It
 * returns the type of the tag if found, or NULL if the tag does not exist.
 *
 * @param tok The token representing the tag name.
 * @return A pointer to the type of the tag if found, or NULL if not found.
 */
static Type *find_tag(Token *tok)
{
	for (Scope *sc = scope; sc; sc = sc->next) {
		Type *ty = hashmap_get2(&sc->tags, tok->loc, tok->len);
		if (ty)
			return ty;
	}
	return NULL;
}

/**
 * @brief Creates a new node with the given kind and associated token.
 *
 * This function allocates memory for a new node, initializes its kind and
 * associates it with the provided token.
 *
 * @param kind The kind of the node.
 * @param tok The token associated with the node.
 * @return A pointer to the newly created node.
 */
static Node *new_node(NodeKind kind, Token *tok)
{
	Node *node = calloc(1, sizeof(Node));

	node->kind = kind;
	node->tok = tok;
	return node;
}

/**
 * @brief Creates a new binary node with the given kind, left-hand side,
 *        right-hand side, and associated token.
 *
 * This function creates a binary node, associating it with the provided kind,
 * left-hand side (`lhs`), right-hand side (`rhs`), and token (`tok`). The
 * resulting node represents an expression with two operands.
 *
 * @param kind The kind of the binary node.
 * @param lhs The left-hand side node.
 * @param rhs The right-hand side node.
 * @param tok The token associated with the binary node.
 * @return A pointer to the newly created binary node.
 */
static Node *new_binary(NodeKind kind, Node *lhs, Node *rhs, Token *tok)
{
	Node *node = new_node(kind, tok);

	node->lhs = lhs;
	node->rhs = rhs;
	return node;
}

/**
 * @brief Creates a new unary node with the given kind, expression, and token.
 *
 * This function creates a unary node where the node type is specified by `kind`,
 * and the single operand (expression) is specified by `expr`. The token `tok`
 * is associated with the node.
 *
 * @param kind The kind of the unary node.
 * @param expr The expression that serves as the operand for the unary operation.
 * @param tok The token associated with the unary node.
 * @return A pointer to the newly created unary node.
 */
static Node *new_unary(NodeKind kind, Node *expr, Token *tok)
{
	Node *node = new_node(kind, tok);

	node->lhs = expr;
	return node;
}

/**
 * @brief Creates a new number node with the given value and token.
 *
 * This function creates a node representing a numerical value with the value `val`
 * and associates it with the token `tok`. The node kind is set to `ND_NUM`.
 *
 * @param val The numerical value to be stored in the node.
 * @param tok The token associated with the number node.
 * @return A pointer to the newly created number node.
 */
static Node *new_num(int64_t val, Token *tok)
{
	Node *node = new_node(ND_NUM, tok);

	node->val = val;
	return node;
}

/**
 * @brief Creates a new long number node with the given value and token.
 *
 * This function creates a node representing a long numerical value with the value `val`
 * and associates it with the token `tok`. The node kind is set to `ND_NUM` and its type
 * is set to `ty_long`.
 *
 * @param val The long numerical value to be stored in the node.
 * @param tok The token associated with the long number node.
 * @return A pointer to the newly created long number node.
 */
static Node *new_long(int64_t val, Token *tok)
{
	Node *node = new_node(ND_NUM, tok);

	node->val = val;
	node->ty = ty_long;
	return node;
}

/**
 * @brief Creates a new unsigned long number node with the given value and token.
 *
 * This function creates a node representing an unsigned long numerical value with
 * the value `val` and associates it with the token `tok`. The node kind is set to `ND_NUM`
 * and its type is set to `ty_ulong`.
 *
 * @param val The unsigned long numerical value to be stored in the node.
 * @param tok The token associated with the unsigned long number node.
 * @return A pointer to the newly created unsigned long number node.
 */
static Node *new_ulong(long val, Token *tok)
{
	Node *node = new_node(ND_NUM, tok);

	node->val = val;
	node->ty = ty_ulong;
	return node;
}

/**
 * @brief Creates a new variable node with the given variable and token.
 *
 * This function creates a node representing a variable with the provided `var`
 * and associates it with the token `tok`. The node kind is set to `ND_VAR`.
 *
 * @param var The variable represented by the node.
 * @param tok The token associated with the variable node.
 * @return A pointer to the newly created variable node.
 */
static Node *new_var_node(Obj *var, Token *tok)
{
	Node *node = new_node(ND_VAR, tok);

	node->var = var;
	return node;
}

/**
 * @brief Creates a new variable-length array (VLA) pointer node with the given variable and token.
 *
 * This function creates a node representing a pointer to a variable-length array
 * with the provided `var` and associates it with the token `tok`. The node kind is set to `ND_VLA_PTR`.
 *
 * @param var The variable representing the VLA.
 * @param tok The token associated with the VLA pointer node.
 * @return A pointer to the newly created VLA pointer node.
 */
static Node *new_vla_ptr(Obj *var, Token *tok)
{
	Node *node = new_node(ND_VLA_PTR, tok);

	node->var = var;
	return node;
}

/**
 * @brief Creates a new cast node with the given expression and target type.
 *
 * This function creates a cast node that represents casting an expression `expr`
 * to the specified target type `ty`. The node kind is set to `ND_CAST` and the token
 * is copied from the expression's token.
 *
 * @param expr The expression to be cast.
 * @param ty The target type of the cast.
 * @return A pointer to the newly created cast node.
 */
Node *new_cast(Node *expr, Type *ty)
{
	add_type(expr);

	Node *node = calloc(1, sizeof(Node));
	node->kind = ND_CAST;
	node->tok = expr->tok;
	node->lhs = expr;
	node->ty = copy_type(ty);
	return node;
}

/**
 * @brief Pushes a new variable scope onto the current scope stack.
 *
 * This function creates a new variable scope and adds it to the current scope
 * stack, associating the scope with the provided `name`.
 *
 * @param name The name associated with the variable scope.
 * @return A pointer to the newly created variable scope.
 */
static VarScope *push_scope(char *name)
{
	VarScope *sc = calloc(1, sizeof(VarScope));

	hashmap_put(&scope->vars, name, sc);
	return sc;
}

/**
 * @brief Creates a new initializer for a type, optionally marking it as flexible.
 *
 * This function creates an initializer for the specified type `ty`. If the type
 * is an array, struct, or union, it will recursively initialize the children as well.
 * If `is_flexible` is true, it indicates that the initializer is for a flexible array
 * or member.
 *
 * @param ty The type for which the initializer is being created.
 * @param is_flexible A flag indicating whether the initializer is for a flexible type.
 * @return A pointer to the newly created initializer.
 */
static Initializer *new_initializer(Type *ty, bool is_flexible)
{
	Initializer *init = calloc(1, sizeof(Initializer));

	init->ty = ty;

	if (ty->kind == TY_ARRAY) {
		if (is_flexible && ty->size < 0) {
			init->is_flexible = true;
			return init;
		}

		init->children = calloc(ty->array_len, sizeof(Initializer *));
		for (int i = 0; i < ty->array_len; i++)
			init->children[i] = new_initializer(ty->base, false);
		return init;
	}

	if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
		// Count the number of struct members.
		int len = 0;
		for (Member *mem = ty->members; mem; mem = mem->next)
			len++;

		init->children = calloc(len, sizeof(Initializer *));

		for (Member *mem = ty->members; mem; mem = mem->next) {
			if (is_flexible && ty->is_flexible && !mem->next) {
				Initializer *child = calloc(1, sizeof(Initializer));
				child->ty = mem->ty;
				child->is_flexible = true;
				init->children[mem->idx] = child;
			} else {
				init->children[mem->idx] = new_initializer(mem->ty, false);
			}
		}
		return init;
	}

	return init;
}

/**
 * @brief Creates a new variable with the given name and type.
 *
 * This function creates a new variable object and associates it with the given
 * name and type. It also sets the alignment of the variable based on its type
 * and pushes the variable onto the current scope.
 *
 * @param name The name of the variable.
 * @param ty The type of the variable.
 * @return A pointer to the newly created variable object.
 */
static Obj *new_var(char *name, Type *ty)
{
	Obj *var = calloc(1, sizeof(Obj));

	var->name = name;
	var->ty = ty;
	var->align = ty->align;
	push_scope(name)->var = var;
	return var;
}

/**
 * @brief Creates a new local variable with the given name and type.
 *
 * This function creates a new local variable by calling `new_var()` to create
 * the base variable and then marks it as local. It also adds the variable to
 * the list of local variables.
 *
 * @param name The name of the local variable.
 * @param ty The type of the local variable.
 * @return A pointer to the newly created local variable object.
 */
static Obj *new_lvar(char *name, Type *ty)
{
	Obj *var = new_var(name, ty);

	var->is_local = true;
	var->next = locals;
	locals = var;
	return var;
}

/**
 * @brief Creates a new global variable with the given name and type.
 *
 * This function creates a new global variable by calling `new_var()` to create
 * the base variable and then marks it as static and a definition. It also adds
 * the variable to the list of global variables.
 *
 * @param name The name of the global variable.
 * @param ty The type of the global variable.
 * @return A pointer to the newly created global variable object.
 */
static Obj *new_gvar(char *name, Type *ty)
{
	Obj *var = new_var(name, ty);

	var->next = globals;
	var->is_static = true;
	var->is_definition = true;
	globals = var;
	return var;
}

/**
 * @brief Creates a unique name for a new variable.
 *
 * This function generates a unique name for a variable by incrementing a static
 * counter and formatting the counter value into a string.
 *
 * @return A string representing a unique name for a new variable.
 */
static char *new_unique_name(void)
{
	static int id = 0;

	return format(".L..%d", id++);
}

/**
 * @brief Creates a new anonymous global variable with the given type.
 *
 * This function creates a new anonymous global variable by calling `new_gvar()`
 * with a unique name and the specified type.
 *
 * @param ty The type of the anonymous global variable.
 * @return A pointer to the newly created anonymous global variable object.
 */
static Obj *new_anon_gvar(Type *ty)
{
	return new_gvar(new_unique_name(), ty);
}

/**
 * @brief Creates a new string literal global variable.
 *
 * This function creates a new anonymous global variable of the specified type
 * and sets its initialization data to the provided string literal `p`.
 *
 * @param p The string literal to be used as initialization data.
 * @param ty The type of the global variable.
 * @return A pointer to the newly created string literal global variable object.
 */
static Obj *new_string_literal(char *p, Type *ty)
{
	Obj *var = new_anon_gvar(ty);

	var->init_data = p;
	return var;
}

/**
 * @brief Extracts the identifier string from a token.
 *
 * This function checks if the token is of type `TK_IDENT` and then returns
 * the identifier string represented by the token. It throws an error if
 * the token is not an identifier.
 *
 * @param tok The token representing the identifier.
 * @return A string representing the identifier.
 * @throws Error if the token is not of kind `TK_IDENT`.
 */
static char *get_ident(Token *tok)
{
	if (tok->kind != TK_IDENT)
		error_tok(tok, "expected an identifier");
	return strndup(tok->loc, tok->len);
}

/**
 * @brief Finds the typedef associated with a token.
 *
 * This function checks if the token is an identifier and searches for a
 * typedef definition associated with the token in the current scope. If found,
 * it returns the associated type definition.
 *
 * @param tok The token representing the identifier to search for.
 * @return The type definition associated with the token, or NULL if not found.
 */
static Type *find_typedef(Token *tok)
{
	if (tok->kind == TK_IDENT) {
		VarScope *sc = find_var(tok);
		if (sc)
			return sc->type_def;
	}
	return NULL;
}

/**
 * @brief Pushes a new tag scope onto the current scope stack.
 *
 * This function adds a new type tag (such as a struct or enum) to the current
 * scope under the provided token `tok` and the associated type `ty`.
 *
 * @param tok The token representing the tag.
 * @param ty The type associated with the tag.
 */
static void push_tag_scope(Token *tok, Type *ty)
{
	hashmap_put2(&scope->tags, tok->loc, tok->len, ty);
}

/**
 * @brief Parses the declaration specifier to create the appropriate type.
 *
 * This function processes a sequence of tokens that represent a declaration
 * specifier in C, which can include keywords, storage class specifiers,
 * and user-defined types. It processes each token in the sequence, updates
 * the type object accordingly, and returns the final type.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after the declaration specifier.
 * @param tok The current token that represents the first token of the
 *            declaration specifier.
 * @param attr A pointer to a variable attributes structure, which will be
 *             updated with storage class information (e.g., `static`, `extern`).
 * @return The type object that represents the declaration specifier.
 *
 * @details The declaration specifier can contain a combination of the following:
 *
 * - Primitive types like `void`, `int`, `char`, etc.
 * - Storage class specifiers like `static`, `extern`, `inline`, etc.
 * - Type modifiers like `signed`, `unsigned`, `const`, `volatile`, etc.
 * - User-defined types like `struct`, `union`, `enum`, and typedef names.
 * - Special keywords like `_Atomic`, `_Alignas`, and `typeof`.
 *
 * The function parses the specifiers in a way that accounts for the order of
 * the typenames, which may appear in any order (e.g., `int long static` is valid).
 *
 * @throws Error if an invalid type or combination of specifiers is found.
 */
static Type *declspec(Token **rest, Token *tok, VarAttr *attr)
{
	// We use a single integer as counters for all typenames.
	// For example, bits 0 and 1 represents how many times we saw the
	// keyword "void" so far. With this, we can use a switch statement
	// as you can see below.
	enum {
		VOID		= 1 << 0,
		BOOL		= 1 << 2,
		CHAR		= 1 << 4,
		SHORT		= 1 << 6,
		INT		= 1 << 8,
		LONG		= 1 << 10,
		FLOAT		= 1 << 12,
		DOUBLE		= 1 << 14,
		OTHER		= 1 << 16,
		SIGNED		= 1 << 17,
		UNSIGNED	= 1 << 18,
	};

	Type *ty = ty_int;
	int counter = 0;
	bool is_atomic = false;

	while (is_typename(tok)) {
		// Handle storage class specifiers.
		if (equal(tok, "typedef") || equal(tok, "static") || equal(tok, "extern") ||
		    equal(tok, "inline") || equal(tok, "_Thread_local") || equal(tok, "__thread")) {
			if (!attr)
				error_tok(tok, "storage class specifier is not allowed in this context");

			if (equal(tok, "typedef"))
				attr->is_typedef = true;
			else if (equal(tok, "static"))
				attr->is_static = true;
			else if (equal(tok, "extern"))
				attr->is_extern = true;
			else if (equal(tok, "inline"))
				attr->is_inline = true;
			else
				attr->is_tls = true;

			if (attr->is_typedef &&
			    attr->is_static + attr->is_extern + attr->is_inline + attr->is_tls > 1)
				error_tok(tok, "typedef may not be used together with static,"
					  " extern, inline, __thread or _Thread_local");
			tok = tok->next;
			continue;
		}

		// These keywords are recognized but ignored.
		if (consume(&tok, tok, "const") || consume(&tok, tok, "volatile") ||
		    consume(&tok, tok, "auto") || consume(&tok, tok, "register") ||
		    consume(&tok, tok, "restrict") || consume(&tok, tok, "__restrict") ||
		    consume(&tok, tok, "__restrict__") || consume(&tok, tok, "_Noreturn"))
			continue;

		if (equal(tok, "_Atomic")) {
			tok = tok->next;
			if (equal(tok, "(")) {
				ty = typename(&tok, tok->next);
				tok = skip(tok, ")");
			}
			is_atomic = true;
			continue;
		}

		if (equal(tok, "_Alignas")) {
			if (!attr)
				error_tok(tok, "_Alignas is not allowed in this context");
			tok = skip(tok->next, "(");

			if (is_typename(tok))
				attr->align = typename(&tok, tok)->align;
			else
				attr->align = const_expr(&tok, tok);
			tok = skip(tok, ")");
			continue;
		}

		// Handle user-defined types.
		Type *ty2 = find_typedef(tok);
		if (equal(tok, "struct") || equal(tok, "union") || equal(tok, "enum") ||
		    equal(tok, "typeof") || ty2) {
			if (counter)
				break;

			if (equal(tok, "struct")) {
				ty = struct_decl(&tok, tok->next);
			} else if (equal(tok, "union")) {
				ty = union_decl(&tok, tok->next);
			} else if (equal(tok, "enum")) {
				ty = enum_specifier(&tok, tok->next);
			} else if (equal(tok, "typeof")) {
				ty = typeof_specifier(&tok, tok->next);
			} else {
				ty = ty2;
				tok = tok->next;
			}

			counter += OTHER;
			continue;
		}

		// Handle built-in types.
		if (equal(tok, "void"))
			counter += VOID;
		else if (equal(tok, "_Bool"))
			counter += BOOL;
		else if (equal(tok, "char"))
			counter += CHAR;
		else if (equal(tok, "short"))
			counter += SHORT;
		else if (equal(tok, "int"))
			counter += INT;
		else if (equal(tok, "long"))
			counter += LONG;
		else if (equal(tok, "float"))
			counter += FLOAT;
		else if (equal(tok, "double"))
			counter += DOUBLE;
		else if (equal(tok, "signed"))
			counter |= SIGNED;
		else if (equal(tok, "unsigned"))
			counter |= UNSIGNED;
		else
			unreachable();

		switch (counter) {
		case VOID:
			ty = ty_void;
			break;
		case BOOL:
			ty = ty_bool;
			break;
		case CHAR:
		case SIGNED + CHAR:
			ty = ty_char;
			break;
		case UNSIGNED + CHAR:
			ty = ty_uchar;
			break;
		case SHORT:
		case SHORT + INT:
		case SIGNED + SHORT:
		case SIGNED + SHORT + INT:
			ty = ty_short;
			break;
		case UNSIGNED + SHORT:
		case UNSIGNED + SHORT + INT:
			ty = ty_ushort;
			break;
		case INT:
		case SIGNED:
		case SIGNED + INT:
			ty = ty_int;
			break;
		case UNSIGNED:
		case UNSIGNED + INT:
			ty = ty_uint;
			break;
		case LONG:
		case LONG + INT:
		case LONG + LONG:
		case LONG + LONG + INT:
		case SIGNED + LONG:
		case SIGNED + LONG + INT:
		case SIGNED + LONG + LONG:
		case SIGNED + LONG + LONG + INT:
			ty = ty_long;
			break;
		case UNSIGNED + LONG:
		case UNSIGNED + LONG + INT:
		case UNSIGNED + LONG + LONG:
		case UNSIGNED + LONG + LONG + INT:
			ty = ty_ulong;
			break;
		case FLOAT:
			ty = ty_float;
			break;
		case DOUBLE:
			ty = ty_double;
			break;
		case LONG + DOUBLE:
			ty = ty_ldouble;
			break;
		default:
			error_tok(tok, "invalid type");
		}

		tok = tok->next;
	}

	if (is_atomic) {
		ty = copy_type(ty);
		ty->is_atomic = true;
	}

	*rest = tok;
	return ty;
}

/**
 * @brief Parses the function parameters in a function declaration.
 *
 * This function processes the tokens representing the parameters of a function
 * declaration, determining the types and names of each parameter. It handles
 * both normal and variadic parameter lists, including the special case of
 * `void` as the parameter list.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after the function parameters.
 * @param tok The current token that represents the first token of the function
 *            parameter list.
 * @param ty The base type that will be applied to the function's return type.
 * @return The final type object representing the function's return type, with
 *         the parameter list and variadic flag set.
 *
 * @details This function handles the following cases:
 * - A single parameter list with multiple parameters separated by commas.
 * - The case of a variadic function, indicated by `...`.
 * - The special case of a function declaration with `void` parameters (e.g., `void func(void)`).
 * It also handles the conversion of arrays and function types to pointers in the parameter list.
 */
static Type *func_params(Token **rest, Token *tok, Type *ty)
{
	if (equal(tok, "void") && equal(tok->next, ")")) {
		*rest = tok->next->next;
		return func_type(ty);
	}

	Type head = {};
	Type *cur = &head;
	bool is_variadic = false;

	while (!equal(tok, ")")) {
		if (cur != &head)
			tok = skip(tok, ",");

		if (equal(tok, "...")) {
			is_variadic = true;
			tok = tok->next;
			skip(tok, ")");
			break;
		}

		Type *ty2 = declspec(&tok, tok, NULL);
		ty2 = declarator(&tok, tok, ty2);

		Token *name = ty2->name;

		if (ty2->kind == TY_ARRAY) {
			// "array of T" is converted to "pointer to T" only in the parameter
			// context. For example, *argv[] is converted to **argv by this.
			ty2 = pointer_to(ty2->base);
			ty2->name = name;
		} else if (ty2->kind == TY_FUNC) {
			// Likewise, a function is converted to a pointer to a function
			// only in the parameter context.
			ty2 = pointer_to(ty2);
			ty2->name = name;
		}

		cur = cur->next = copy_type(ty2);
	}

	if (cur == &head)
		is_variadic = true;

	ty = func_type(ty);
	ty->params = head.next;
	ty->is_variadic = is_variadic;
	*rest = tok->next;
	return ty;
}

/**
 * @brief Parses the array dimensions in a type declaration.
 *
 * This function handles the array dimensions syntax in a type declaration,
 * including the handling of "static" and "restrict" keywords, constant expressions,
 * and type suffixes.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after parsing the array dimensions.
 * @param tok The current token that represents the start of the array dimensions.
 * @param ty The base type of the array.
 * @return The type object representing the array type, either a regular array,
 *         a variable-length array (VLA), or a pointer type.
 *
 * @details This function processes the following:
 * - The optional `static` and `restrict` keywords before the array dimension.
 * - The constant expression representing the size of the array, if present.
 * - The handling of variable-length arrays (VLAs) using the `vla_of` function.
 * - The case when no size is specified (indicating an incomplete type), using `array_of`.
 */
static Type *array_dimensions(Token **rest, Token *tok, Type *ty)
{
	while (equal(tok, "static") || equal(tok, "restrict"))
		tok = tok->next;

	if (equal(tok, "]")) {
		ty = type_suffix(rest, tok->next, ty);
		return array_of(ty, -1);
	}

	Node *expr = conditional(&tok, tok);
	tok = skip(tok, "]");
	ty = type_suffix(rest, tok, ty);

	if (ty->kind == TY_VLA || !is_const_expr(expr))
		return vla_of(ty, expr);
	return array_of(ty, eval(expr));
}

/**
 * @brief Parses the type suffix in a type declaration.
 *
 * This function handles the parsing of type suffixes, such as function parameters
 * in function declarations and array dimensions in array declarations.
 * It processes the token stream and returns the appropriate type object.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after parsing the type suffix.
 * @param tok The current token representing the type suffix.
 * @param ty The base type to which the suffix will be applied.
 * @return The type object representing the resulting type after applying the suffix.
 *
 * @details This function handles two cases:
 * - A function parameter list, indicated by the token `(`, which is parsed by
 *   calling `func_params`.
 * - Array dimensions, indicated by the token `[`, which are parsed by calling
 *   `array_dimensions`.
 * If neither case is present, it returns the base type as is.
 */
static Type *type_suffix(Token **rest, Token *tok, Type *ty)
{
	if (equal(tok, "("))
		return func_params(rest, tok->next, ty);

	if (equal(tok, "["))
		return array_dimensions(rest, tok->next, ty);

	*rest = tok;
	return ty;
}

/**
 * @brief Parses pointer declarations, including the handling of type qualifiers.
 *
 * This function processes the token stream for pointer declarations, including
 * handling the `*` symbol and any associated type qualifiers like `const`,
 * `volatile`, and `restrict`.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after parsing the pointers.
 * @param tok The current token representing a pointer (`*`).
 * @param ty The base type to which the pointers will be applied.
 * @return The resulting type object after applying the pointers.
 *
 * @details This function loops through the token stream, looking for the
 * `*` symbol, which denotes a pointer. It also processes any type qualifiers
 * like `const`, `volatile`, or `restrict` that may follow the `*` symbol.
 */
static Type *pointers(Token **rest, Token *tok, Type *ty)
{
	while (consume(&tok, tok, "*")) {
		ty = pointer_to(ty);
		while (equal(tok, "const") || equal(tok, "volatile") || equal(tok, "restrict") ||
		       equal(tok, "__restrict") || equal(tok, "__restrict__"))
			tok = tok->next;
	}
	*rest = tok;
	return ty;
}

/**
 * @brief Parses the declarator, which can include pointers, names, and type suffixes.
 *
 * This function parses the declarator portion of a variable or function declaration,
 * handling pointers, names, and type suffixes like function parameters or array dimensions.
 * It processes the token stream and returns the resulting type object.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after parsing the declarator.
 * @param tok The current token representing the declarator.
 * @param ty The base type to which the declarator will be applied.
 * @return The resulting type object after applying the declarator.
 *
 * @details This function first parses any pointer symbols using `pointers`. It then
 * checks for specific declarator syntax such as function parameters in parentheses
 * or a simple identifier. It also handles the application of type suffixes, such as
 * arrays or function types, using `type_suffix`.
 */
static Type *declarator(Token **rest, Token *tok, Type *ty)
{
	ty = pointers(&tok, tok, ty);

	if (equal(tok, "(")) {
		Token *start = tok;
		Type dummy = {};
		declarator(&tok, start->next, &dummy);
		tok = skip(tok, ")");
		ty = type_suffix(rest, tok, ty);
		return declarator(&tok, start->next, ty);
	}

	Token *name = NULL;
	Token *name_pos = tok;

	if (tok->kind == TK_IDENT) {
		name = tok;
		tok = tok->next;
	}

	ty = type_suffix(rest, tok, ty);
	ty->name = name;
	ty->name_pos = name_pos;
	return ty;
}

/**
 * @brief Parses an abstract declarator, which can include pointers and type suffixes.
 *
 * This function parses an abstract declarator, which includes pointer symbols
 * and an optional set of parentheses around another abstract declarator. It then
 * applies type suffixes like function parameters or array dimensions.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after parsing the abstract declarator.
 * @param tok The current token representing the abstract declarator.
 * @param ty The base type to which the abstract declarator will be applied.
 * @return The resulting type object after applying the abstract declarator.
 *
 * @details This function first processes any pointer symbols with `pointers`. If parentheses
 * are encountered, it recursively parses the abstract declarator inside the parentheses.
 * Finally, the function applies any type suffixes to the resulting type and returns it.
 */
static Type *abstract_declarator(Token **rest, Token *tok, Type *ty)
{
	ty = pointers(&tok, tok, ty);

	if (equal(tok, "(")) {
		Token *start = tok;
		Type dummy = {};
		abstract_declarator(&tok, start->next, &dummy);
		tok = skip(tok, ")");
		ty = type_suffix(rest, tok, ty);
		return abstract_declarator(&tok, start->next, ty);
	}

	return type_suffix(rest, tok, ty);
}

/**
 * @brief Parses a typename, which consists of a declaration specifier followed by an abstract declarator.
 *
 * This function parses a typename, which is a combination of a declaration specifier and
 * an abstract declarator. It processes the token stream and returns the corresponding type.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after parsing the typename.
 * @param tok The current token representing the typename.
 * @return The type object corresponding to the parsed typename.
 *
 * @details This function calls `declspec` to parse the declaration specifier and then
 * passes the resulting type to `abstract_declarator` to handle the abstract declarator.
 */
static Type *typename(Token **rest, Token *tok)
{
	Type *ty = declspec(&tok, tok, NULL);

	return abstract_declarator(rest, tok, ty);
}

/**
 * @brief Checks if the token represents the end of a block.
 *
 * This function checks whether the current token is the end of a block, which can be either
 * a closing brace (`}`) or a comma followed by a closing brace (`,}`).
 *
 * @param tok The current token.
 * @return `true` if the token represents the end of a block, `false` otherwise.
 */
static bool is_end(Token *tok)
{
	return equal(tok, "}") || (equal(tok, ",") && equal(tok->next, "}"));
}

/**
 * @brief Consumes a token if it represents the end of a block.
 *
 * This function consumes the token representing the end of a block, which can be either
 * a closing brace (`}`) or a comma followed by a closing brace (`,}`). It updates the
 * `rest` pointer to point to the next token after the block end.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after the block end.
 * @param tok The current token.
 * @return `true` if the token was consumed, `false` otherwise.
 */
static bool consume_end(Token **rest, Token *tok)
{
	if (equal(tok, "}")) {
		*rest = tok->next;
		return true;
	}

	if (equal(tok, ",") && equal(tok->next, "}")) {
		*rest = tok->next->next;
		return true;
	}

	return false;
}

/**
 * @brief Parses an enum specifier, which defines an enumeration type.
 *
 * This function parses an enum specifier, which can either be a tagged enum or an anonymous
 * enum. It processes the token stream to build the enum type and its associated values.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after parsing the enum specifier.
 * @param tok The current token representing the enum specifier.
 * @return The type object representing the parsed enum.
 *
 * @details This function handles both tagged and anonymous enums. It processes the token stream,
 * handles enum lists, assigns values to enum members, and pushes the enum tag into the scope.
 */
static Type *enum_specifier(Token **rest, Token *tok)
{
	Type *ty = enum_type();

	// Read a struct tag.
	Token *tag = NULL;

	if (tok->kind == TK_IDENT) {
		tag = tok;
		tok = tok->next;
	}

	if (tag && !equal(tok, "{")) {
		Type *ty = find_tag(tag);
		if (!ty)
			error_tok(tag, "unknown enum type");
		if (ty->kind != TY_ENUM)
			error_tok(tag, "not an enum tag");
		*rest = tok;
		return ty;
	}

	tok = skip(tok, "{");

	// Read an enum-list.
	int i = 0;
	int val = 0;
	while (!consume_end(rest, tok)) {
		if (i++ > 0)
			tok = skip(tok, ",");

		char *name = get_ident(tok);
		tok = tok->next;

		if (equal(tok, "="))
			val = const_expr(&tok, tok->next);

		VarScope *sc = push_scope(name);
		sc->enum_ty = ty;
		sc->enum_val = val++;
	}

	if (tag)
		push_tag_scope(tag, ty);
	return ty;
}

/**
 * @brief Parses a typeof specifier, which can be either an expression or a typename inside parentheses.
 *
 * This function parses a typeof specifier, which is a type expression enclosed in parentheses.
 * The specifier can either be an expression or a typename, and the function returns the type
 * corresponding to the specifier.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after parsing the typeof specifier.
 * @param tok The current token representing the typeof specifier.
 * @return The type object corresponding to the parsed typeof specifier.
 *
 * @details This function first skips the opening parenthesis and then checks if the next token
 * is a typename or an expression. It parses the appropriate part, either using `typename` or `expr`,
 * and returns the resulting type.
 */
static Type *typeof_specifier(Token **rest, Token *tok)
{
	tok = skip(tok, "(");

	Type *ty;
	if (is_typename(tok)) {
		ty = typename(&tok, tok);
	} else {
		Node *node = expr(&tok, tok);
		add_type(node);
		ty = node->ty;
	}
	*rest = skip(tok, ")");
	return ty;
}

/**
 * @brief Generates code for computing the size of a variable-length array (VLA).
 *
 * This function generates the code necessary to compute the size of a VLA. If the type is
 * a VLA, the size is calculated by multiplying the VLA length by the size of the base type.
 * If the type is a pointer to a VLA, the function generates code for the base size and VLA length.
 *
 * @param ty The type of the variable-length array.
 * @param tok The current token representing the VLA.
 * @return A Node representing the computation for the VLA size.
 *
 * @details The function recursively computes the size of the VLA by traversing the type and
 * generating the corresponding code. It handles both direct and pointer-to-VLA types.
 */
static Node *compute_vla_size(Type *ty, Token *tok)
{
	Node *node = new_node(ND_NULL_EXPR, tok);

	if (ty->base)
		node = new_binary(ND_COMMA, node, compute_vla_size(ty->base, tok), tok);

	if (ty->kind != TY_VLA)
		return node;

	Node *base_sz;
	if (ty->base->kind == TY_VLA)
		base_sz = new_var_node(ty->base->vla_size, tok);
	else
		base_sz = new_num(ty->base->size, tok);

	ty->vla_size = new_lvar("", ty_ulong);
	Node *expr = new_binary(ND_ASSIGN, new_var_node(ty->vla_size, tok),
				new_binary(ND_MUL, ty->vla_len, base_sz, tok),
				tok);
	return new_binary(ND_COMMA, node, expr, tok);
}

/**
 * @brief Generates code for an `alloca` call to allocate space on the stack.
 *
 * This function generates code to allocate space on the stack using the `alloca` function.
 * The size of the allocation is provided by the argument `sz`, which is a node representing
 * the size expression.
 *
 * @param sz The size of the allocation.
 * @return A Node representing the `alloca` call.
 *
 * @details The function creates a unary node representing a call to the `alloca` function
 * and assigns the correct type for the allocation. It also adds the type information for
 * the size node.
 */
static Node *new_alloca(Node *sz)
{
	Node *node = new_unary(ND_FUNCALL, new_var_node(builtin_alloca, sz->tok), sz->tok);

	node->func_ty = builtin_alloca->ty;
	node->ty = builtin_alloca->ty->return_ty;
	node->args = sz;
	add_type(sz);
	return node;
}

/**
 * @brief Parses a variable declaration.
 *
 * This function parses a declaration, which can include one or more declarators and
 * optional initializers. It handles static variables, local variables, and variable-length
 * arrays (VLAs). For VLAs, the function generates code for their size computation and allocation.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after parsing the declaration.
 * @param tok The current token representing the declaration.
 * @param basety The base type for the declaration.
 * @param attr Additional attributes for the declaration, such as `static`.
 * @return A Node representing the parsed declaration.
 *
 * @details The function parses the declaration and handles various cases such as static
 * variables, VLA size computations, and variable initializations. It generates the corresponding
 * code for each case and returns the resulting node representing the declaration.
 */
static Node *declaration(Token **rest, Token *tok, Type *basety, VarAttr *attr)
{
	Node head = {};
	Node *cur = &head;
	int i = 0;

	while (!equal(tok, ";")) {
		if (i++ > 0)
			tok = skip(tok, ",");

		Type *ty = declarator(&tok, tok, basety);
		if (ty->kind == TY_VOID)
			error_tok(tok, "variable declared void");
		if (!ty->name)
			error_tok(ty->name_pos, "variable name omitted");

		if (attr && attr->is_static) {
			// static local variable
			Obj *var = new_anon_gvar(ty);
			push_scope(get_ident(ty->name))->var = var;
			if (equal(tok, "="))
				gvar_initializer(&tok, tok->next, var);
			continue;
		}

		// Generate code for computing a VLA size. We need to do this
		// even if ty is not VLA because ty may be a pointer to VLA
		// (e.g. int (*foo)[n][m] where n and m are variables.)
		cur = cur->next = new_unary(ND_EXPR_STMT, compute_vla_size(ty, tok), tok);

		if (ty->kind == TY_VLA) {
			if (equal(tok, "="))
				error_tok(tok, "variable-sized object may not be initialized");

			// Variable length arrays (VLAs) are translated to alloca() calls.
			// For example, `int x[n+2]` is translated to `tmp = n + 2,
			// x = alloca(tmp)`.
			Obj *var = new_lvar(get_ident(ty->name), ty);
			Token *tok = ty->name;
			Node *expr = new_binary(ND_ASSIGN, new_vla_ptr(var, tok),
						new_alloca(new_var_node(ty->vla_size, tok)),
						tok);

			cur = cur->next = new_unary(ND_EXPR_STMT, expr, tok);
			continue;
		}

		Obj *var = new_lvar(get_ident(ty->name), ty);
		if (attr && attr->align)
			var->align = attr->align;

		if (equal(tok, "=")) {
			Node *expr = lvar_initializer(&tok, tok->next, var);
			cur = cur->next = new_unary(ND_EXPR_STMT, expr, tok);
		}

		if (var->ty->size < 0)
			error_tok(ty->name, "variable has incomplete type");
		if (var->ty->kind == TY_VOID)
			error_tok(ty->name, "variable declared void");
	}

	Node *node = new_node(ND_BLOCK, tok);
	node->body = head.next;
	*rest = tok->next;
	return node;
}

/**
 * @brief Skips excess elements in a token stream.
 *
 * This function recursively processes tokens to skip over unnecessary elements.
 * If a token represents an opening brace `{`, the function will recursively
 * skip to its corresponding closing brace `}`. Otherwise, the function will
 * move to the next token.
 *
 * @param tok The current token in the token stream.
 * @return The next token after skipping excess elements.
 *
 * @details This function is useful when dealing with complex token streams,
 * especially in the context of handling block structures or other elements
 * that are nested or have excess tokens to be ignored.
 */
static Token *skip_excess_element(Token *tok)
{
	if (equal(tok, "{")) {
		tok = skip_excess_element(tok->next);
		return skip(tok, "}");
	}

	assign(&tok, tok);
	return tok;
}

/**
 * @brief Initializes a string literal in an array initializer.
 *
 * This function handles the initialization of a string literal into an array.
 * The string is assigned element-by-element into the array based on the type
 * size of the base type, either 1 byte, 2 bytes, or 4 bytes.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after parsing the string initializer.
 * @param tok The current token representing the string literal.
 * @param init The initializer object to which the string literal is assigned.
 *
 * @details This function handles initialization of string literals in various
 * forms (e.g., UTF-8, UTF-16, UTF-32) depending on the size of the base type.
 * It ensures that the string is correctly placed within the array.
 */
static void string_initializer(Token **rest, Token *tok, Initializer *init)
{
	if (init->is_flexible)
		*init = *new_initializer(array_of(init->ty->base, tok->ty->array_len), false);

	int len = MIN(init->ty->array_len, tok->ty->array_len);

	switch (init->ty->base->size) {
	case 1: {
		char *str = tok->str;
		for (int i = 0; i < len; i++)
			init->children[i]->expr = new_num(str[i], tok);
		break;
	}
	case 2: {
		uint16_t *str = (uint16_t *)tok->str;
		for (int i = 0; i < len; i++)
			init->children[i]->expr = new_num(str[i], tok);
		break;
	}
	case 4: {
		uint32_t *str = (uint32_t *)tok->str;
		for (int i = 0; i < len; i++)
			init->children[i]->expr = new_num(str[i], tok);
		break;
	}
	default:
		unreachable();
	}

	*rest = tok->next;
}

/**
 * @brief Processes an array designator in an initializer.
 *
 * This function processes the array designator, which allows for specifying
 * an index or a range of indices for initializing specific elements of an array.
 * It handles both simple indices and ranges, and validates that the index/range
 * is within bounds of the array.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after processing the array designator.
 * @param tok The current token representing the array designator.
 * @param ty The type of the array being initialized.
 * @param begin A pointer to an integer where the start of the designator range
 *              will be stored.
 * @param end A pointer to an integer where the end of the designator range
 *            will be stored.
 *
 * @details The function parses the array designator, which can be a simple index
 * (e.g., `[5]`) or a range (e.g., `[5...10]`). It ensures that the indices are
 * valid and within the bounds of the array. If the range is invalid, an error is thrown.
 */
static void array_designator(Token **rest, Token *tok, Type *ty, int *begin, int *end)
{
	*begin = const_expr(&tok, tok->next);
	if (*begin >= ty->array_len)
		error_tok(tok, "array designator index exceeds array bounds");

	if (equal(tok, "...")) {
		*end = const_expr(&tok, tok->next);
		if (*end >= ty->array_len)
			error_tok(tok, "array designator index exceeds array bounds");
		if (*end < *begin)
			error_tok(tok, "array designator range [%d, %d] is empty", *begin, *end);
	} else {
		*end = *begin;
	}

	*rest = skip(tok, "]");
}

/**
 * @brief Processes a struct field designator.
 *
 * This function processes a struct field designator, which is used to access
 * members of a struct. It searches through the struct's members to find the
 * correct field based on the identifier provided in the token.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after processing the struct designator.
 * @param tok The current token representing the struct designator.
 * @param ty The type of the struct.
 * @return A pointer to the struct member being accessed.
 *
 * @details The function checks if the token after the dot (`"."`) is an
 * identifier, and then searches the struct's member list for a matching field.
 * It handles both regular and anonymous struct members.
 */
static Member *struct_designator(Token **rest, Token *tok, Type *ty)
{
	Token *start = tok;

	tok = skip(tok, ".");
	if (tok->kind != TK_IDENT)
		error_tok(tok, "expected a field designator");

	for (Member *mem = ty->members; mem; mem = mem->next) {
		// Anonymous struct member
		if (mem->ty->kind == TY_STRUCT && !mem->name) {
			if (get_struct_member(mem->ty, tok)) {
				*rest = start;
				return mem;
			}
			continue;
		}

		// Regular struct member
		if (mem->name->len == tok->len && !strncmp(mem->name->loc, tok->loc, tok->len)) {
			*rest = tok->next;
			return mem;
		}
	}

	error_tok(tok, "struct has no such member");
}

/**
 * @brief Processes a designation in an initializer.
 *
 * A designation is used for designating specific elements of an array or
 * struct in an initializer. This function handles array indices, struct
 * field names, and union members in initializer expressions.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after processing the designation.
 * @param tok The current token representing the designation.
 * @param init The initializer to which the designation will be applied.
 *
 * @details This function supports both array designators (e.g., `[5]`) and
 * struct/unions field designators (e.g., `.field`). It recursively processes
 * these designators and calls the appropriate initializer functions.
 */
static void designation(Token **rest, Token *tok, Initializer *init)
{
	if (equal(tok, "[")) {
		if (init->ty->kind != TY_ARRAY)
			error_tok(tok, "array index in non-array initializer");

		int begin, end;
		array_designator(&tok, tok, init->ty, &begin, &end);

		Token *tok2;
		for (int i = begin; i <= end; i++)
			designation(&tok2, tok, init->children[i]);
		array_initializer2(rest, tok2, init, begin + 1);
		return;
	}

	if (equal(tok, ".") && init->ty->kind == TY_STRUCT) {
		Member *mem = struct_designator(&tok, tok, init->ty);
		designation(&tok, tok, init->children[mem->idx]);
		init->expr = NULL;
		struct_initializer2(rest, tok, init, mem->next);
		return;
	}

	if (equal(tok, ".") && init->ty->kind == TY_UNION) {
		Member *mem = struct_designator(&tok, tok, init->ty);
		init->mem = mem;
		designation(rest, tok, init->children[mem->idx]);
		return;
	}

	if (equal(tok, "."))
		error_tok(tok, "field name not in struct or union initializer");

	if (equal(tok, "="))
		tok = tok->next;
	initializer2(rest, tok, init);
}

/**
 * @brief Counts the number of elements in an array initializer.
 *
 * This function counts how many elements are provided in the initializer
 * for an array. If the array length is omitted, it calculates the number
 * of elements based on the initializer's content.
 *
 * @param tok The current token representing the start of the array initializer.
 * @param ty The type of the array being initialized.
 * @return The number of elements in the initializer.
 *
 * @details This function handles the case where an array length is omitted
 * (e.g., `int x[] = {1, 2, 3}`). It counts the number of initializer elements
 * and determines the appropriate array size.
 */
static int count_array_init_elements(Token *tok, Type *ty)
{
	bool first = true;
	Initializer *dummy = new_initializer(ty->base, true);

	int i = 0, max = 0;

	while (!consume_end(&tok, tok)) {
		if (!first)
			tok = skip(tok, ",");
		first = false;

		if (equal(tok, "[")) {
			i = const_expr(&tok, tok->next);
			if (equal(tok, "..."))
				i = const_expr(&tok, tok->next);
			tok = skip(tok, "]");
			designation(&tok, tok, dummy);
		} else {
			initializer2(&tok, tok, dummy);
		}

		i++;
		max = MAX(max, i);
	}
	return max;
}

/**
 * @brief Processes an array initializer with flexible array members.
 *
 * This function processes the array initializer, handling array elements and
 * their initialization. It supports flexible array members, where the length
 * of the array is determined dynamically based on the initializer content.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after processing the array initializer.
 * @param tok The current token representing the start of the array initializer.
 * @param init The initializer to be applied to the array.
 *
 * @details This function processes the initializers for array elements,
 * including handling array designators (e.g., `[5]`) and flexible array
 * members. It iterates over the elements, applying the appropriate initializers
 * and managing the layout of elements in the array.
 */
static void array_initializer1(Token **rest, Token *tok, Initializer *init)
{
	tok = skip(tok, "{");

	if (init->is_flexible) {
		int len = count_array_init_elements(tok, init->ty);
		*init = *new_initializer(array_of(init->ty->base, len), false);
	}

	bool first = true;

	for (int i = 0; !consume_end(rest, tok); i++) {
		if (!first)
			tok = skip(tok, ",");
		first = false;

		if (equal(tok, "[")) {
			int begin, end;
			array_designator(&tok, tok, init->ty, &begin, &end);

			Token *tok2;
			for (int j = begin; j <= end; j++)
				designation(&tok2, tok, init->children[j]);
			tok = tok2;
			i = end;
			continue;
		}

		if (i < init->ty->array_len)
			initializer2(&tok, tok, init->children[i]);
		else
			tok = skip_excess_element(tok);
	}
}

/**
 * @brief Processes an array initializer without flexible array members.
 *
 * This function processes a simpler array initializer, where the array size
 * is known. It handles the initialization of array elements, and ensures
 * that excess elements are properly skipped.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after processing the array initializer.
 * @param tok The current token representing the start of the array initializer.
 * @param init The initializer to be applied to the array.
 * @param i The index of the current element being initialized.
 *
 * @details This function handles array initialization by iterating over the
 * elements in the initializer list, skipping excess elements if needed, and
 * ensuring the correct application of the initializer expressions.
 */
static void array_initializer2(Token **rest, Token *tok, Initializer *init, int i)
{
	if (init->is_flexible) {
		int len = count_array_init_elements(tok, init->ty);
		*init = *new_initializer(array_of(init->ty->base, len), false);
	}

	for (; i < init->ty->array_len && !is_end(tok); i++) {
		Token *start = tok;
		if (i > 0)
			tok = skip(tok, ",");

		if (equal(tok, "[") || equal(tok, ".")) {
			*rest = start;
			return;
		}

		initializer2(&tok, tok, init->children[i]);
	}
	*rest = tok;
}

/**
 * @brief Processes a struct initializer with flexible struct members.
 *
 * This function processes the struct initializer, initializing struct fields
 * and handling flexible struct members. It supports field designators (e.g., `.field`)
 * and processes the struct elements sequentially.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after processing the struct initializer.
 * @param tok The current token representing the start of the struct initializer.
 * @param init The initializer to be applied to the struct.
 *
 * @details This function processes the initializers for struct fields. It handles
 * both regular fields and flexible array members. It processes each field, checks
 * for field designators, and applies the initializer expressions to the struct.
 */
static void struct_initializer1(Token **rest, Token *tok, Initializer *init)
{
	tok = skip(tok, "{");

	Member *mem = init->ty->members;
	bool first = true;

	while (!consume_end(rest, tok)) {
		if (!first)
			tok = skip(tok, ",");
		first = false;

		if (equal(tok, ".")) {
			mem = struct_designator(&tok, tok, init->ty);
			designation(&tok, tok, init->children[mem->idx]);
			mem = mem->next;
			continue;
		}

		if (mem) {
			initializer2(&tok, tok, init->children[mem->idx]);
			mem = mem->next;
		} else {
			tok = skip_excess_element(tok);
		}
	}
}

/**
 * @brief Processes a struct initializer without flexible struct members.
 *
 * This function processes the struct initializer in a simpler case where
 * the struct's fields are already defined, and flexible struct members
 * are not being used. It initializes each field of the struct accordingly.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after processing the struct initializer.
 * @param tok The current token representing the start of the struct initializer.
 * @param init The initializer to be applied to the struct.
 * @param mem The current struct member being initialized.
 *
 * @details This function processes the struct initializer by iterating over
 * the struct's members and applying the appropriate initializer expression
 * to each field.
 */
static void struct_initializer2(Token **rest, Token *tok, Initializer *init, Member *mem)
{
	bool first = true;

	for (; mem && !is_end(tok); mem = mem->next) {
		Token *start = tok;

		if (!first)
			tok = skip(tok, ",");
		first = false;

		if (equal(tok, "[") || equal(tok, ".")) {
			*rest = start;
			return;
		}

		initializer2(&tok, tok, init->children[mem->idx]);
	}
	*rest = tok;
}

/**
 * @brief Processes the initialization of a union type.
 *
 * Unlike structs, union initializers take only one initializer, and this initializer
 * initializes the first union member by default. Other members can be initialized
 * using designated initializers.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after processing the union initializer.
 * @param tok The current token representing the start of the union initializer.
 * @param init The initializer to be applied to the union.
 *
 * @details This function handles the union initialization process. If the initializer
 * uses a designated initializer (e.g., `{.field = value}`), it applies the initializer
 * to the specified union member. If the initializer uses regular initialization (e.g., `{value}`),
 * it initializes the first union member.
 */
static void union_initializer(Token **rest, Token *tok, Initializer *init)
{
	// If the union initializer is using a designated initializer (e.g., {.field = value}).
	if (equal(tok, "{") && equal(tok->next, ".")) {
		Member *mem = struct_designator(&tok, tok->next, init->ty);
		init->mem = mem;
		designation(&tok, tok, init->children[mem->idx]);
		*rest = skip(tok, "}");
		return;
	}

	// If the initializer is not using a designated initializer, initialize the first member.
	init->mem = init->ty->members;

	if (equal(tok, "{")) {
		initializer2(&tok, tok->next, init->children[0]);
		consume(&tok, tok, ",");
		*rest = skip(tok, "}");
	} else {
		initializer2(rest, tok, init->children[0]);
	}
}

/**
 * @brief Processes the initialization of a variable or type.
 *
 * This function handles various types of initializers including string, array, struct,
 * and union initializers. It determines the type of initialization based on the token
 * and applies the appropriate initialization logic.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after processing the initializer.
 * @param tok The current token representing the start of the initializer.
 * @param init The initializer to be applied to the variable or type.
 *
 * @details This function distinguishes between different kinds of initializers:
 * - String initializers for arrays of characters.
 * - Array initializers for arrays of various types.
 * - Struct initializers for struct types.
 * - Union initializers for union types.
 * Additionally, it handles scalar initializers where the value is surrounded by braces.
 */
static void initializer2(Token **rest, Token *tok, Initializer *init)
{
	if (init->ty->kind == TY_ARRAY && tok->kind == TK_STR) {
		string_initializer(rest, tok, init);
		return;
	}

	if (init->ty->kind == TY_ARRAY) {
		if (equal(tok, "{"))
			array_initializer1(rest, tok, init);
		else
			array_initializer2(rest, tok, init, 0);
		return;
	}

	if (init->ty->kind == TY_STRUCT) {
		if (equal(tok, "{")) {
			struct_initializer1(rest, tok, init);
			return;
		}

		// Handle struct initialization by assignment, if applicable.
		Node *expr = assign(rest, tok);
		add_type(expr);
		if (expr->ty->kind == TY_STRUCT) {
			init->expr = expr;
			return;
		}

		struct_initializer2(rest, tok, init, init->ty->members);
		return;
	}

	if (init->ty->kind == TY_UNION) {
		union_initializer(rest, tok, init);
		return;
	}

	if (equal(tok, "{")) {
		// Handle scalar variable initialization surrounded by braces (e.g., `int x = {3};`).
		initializer2(&tok, tok->next, init);
		*rest = skip(tok, "}");
		return;
	}

	init->expr = assign(rest, tok);
}

/**
 * @brief Copies a struct type and its members.
 *
 * This function creates a deep copy of a struct type, including its members, so that
 * the original struct type is not modified. The members are copied by iterating over
 * the original struct's members and creating a new member for each one.
 *
 * @param ty The original struct type to be copied.
 *
 * @return A new struct type that is a copy of the original type.
 *
 * @details The function ensures that each member of the struct is copied independently
 * by allocating memory for each member and linking them into a new list of members.
 * This is necessary to avoid modifying the original type and its members.
 */
static Type *copy_struct_type(Type *ty)
{
	ty = copy_type(ty);

	Member head = {};
	Member *cur = &head;
	for (Member *mem = ty->members; mem; mem = mem->next) {
		Member *m = calloc(1, sizeof(Member));
		*m = *mem;
		cur = cur->next = m;
	}

	ty->members = head.next;
	return ty;
}

/**
 * @brief Processes and creates an initializer for a given type.
 *
 * This function creates an initializer object for a given type. If the type is
 * a flexible array (for structs or unions), it creates a copy of the struct type,
 * adjusting the size of the type and assigning the initializer's value to the last
 * member of the struct.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after processing the initializer.
 * @param tok The current token representing the start of the initializer.
 * @param ty The type to initialize.
 * @param new_ty A pointer to a type pointer that will be updated to the new type
 *               after processing the initializer.
 *
 * @return A pointer to the created initializer.
 *
 * @details If the type is a struct or union and is flexible, this function
 * will copy the struct type and adjust the size of the last member based on the
 * initializer. The function returns the newly created initializer, with the new type
 * being updated in `new_ty`.
 */
static Initializer *initializer(Token **rest, Token *tok, Type *ty, Type **new_ty)
{
	Initializer *init = new_initializer(ty, true);

	initializer2(rest, tok, init);

	// If the type is a flexible struct or union, copy the struct type
	if ((ty->kind == TY_STRUCT || ty->kind == TY_UNION) && ty->is_flexible) {
		ty = copy_struct_type(ty);

		// Update the last member's type and size for flexible types
		Member *mem = ty->members;
		while (mem->next)
			mem = mem->next;
		mem->ty = init->children[mem->idx]->ty;
		ty->size += mem->ty->size;

		*new_ty = ty;
		return init;
	}

	*new_ty = init->ty;
	return init;
}

/**
 * @brief Creates an expression for an initializer designation.
 *
 * This function generates a node that represents the expression for a designated
 * initializer, where it handles the variable or member to be initialized.
 *
 * @param desg The initialization designation containing the information
 *             about the variable or member being initialized.
 * @param tok The token associated with the initializer expression.
 *
 * @return A pointer to the node representing the designated initializer.
 *
 * @details This function recursively traverses the designated initializer structure
 * and creates the appropriate expression node for either a variable, struct member,
 * or array element.
 */
static Node *init_desg_expr(InitDesg *desg, Token *tok)
{
	if (desg->var)
		return new_var_node(desg->var, tok);

	if (desg->member) {
		Node *node = new_unary(ND_MEMBER, init_desg_expr(desg->next, tok), tok);
		node->member = desg->member;
		return node;
	}

	Node *lhs = init_desg_expr(desg->next, tok);
	Node *rhs = new_num(desg->idx, tok);
	return new_unary(ND_DEREF, new_add(lhs, rhs, tok), tok);
}

/**
 * @brief Creates the initialization expression for a local variable.
 *
 * This function recursively creates an expression for initializing a local
 * variable, handling array, struct, union, and scalar initializers. It also
 * handles the case of nested initializations for arrays and structs.
 *
 * @param init The initializer to apply.
 * @param ty The type of the variable being initialized.
 * @param desg The initialization designation containing the target information.
 * @param tok The token associated with the initialization expression.
 *
 * @return A pointer to the node representing the initialization expression.
 *
 * @details For array and struct types, the function recursively initializes
 * the elements or members by calling `create_lvar_init` for each child.
 * For unions, it initializes the specified union member. For scalar types,
 * the initialization is directly done using an assignment expression.
 */
static Node *create_lvar_init(Initializer *init, Type *ty, InitDesg *desg, Token *tok)
{
	if (ty->kind == TY_ARRAY) {
		Node *node = new_node(ND_NULL_EXPR, tok);
		for (int i = 0; i < ty->array_len; i++) {
			InitDesg desg2 = { desg, i };
			Node *rhs = create_lvar_init(init->children[i], ty->base, &desg2, tok);
			node = new_binary(ND_COMMA, node, rhs, tok);
		}
		return node;
	}

	if (ty->kind == TY_STRUCT && !init->expr) {
		Node *node = new_node(ND_NULL_EXPR, tok);

		for (Member *mem = ty->members; mem; mem = mem->next) {
			InitDesg desg2 = { desg, 0, mem };
			Node *rhs = create_lvar_init(init->children[mem->idx], mem->ty, &desg2, tok);
			node = new_binary(ND_COMMA, node, rhs, tok);
		}
		return node;
	}

	if (ty->kind == TY_UNION) {
		Member *mem = init->mem ? init->mem : ty->members;
		InitDesg desg2 = { desg, 0, mem };
		return create_lvar_init(init->children[mem->idx], mem->ty, &desg2, tok);
	}

	if (!init->expr)
		return new_node(ND_NULL_EXPR, tok);

	Node *lhs = init_desg_expr(desg, tok);
	return new_binary(ND_ASSIGN, lhs, init->expr, tok);
}

/**
 * @brief Initializes a local variable with an initializer expression.
 *
 * This function generates assignment expressions for a variable definition
 * with an initializer. For example, an initializer like `int x[2][2] = {{6, 7}, {8, 9}}`
 * will be converted into separate assignment expressions for each element:
 *
 *   x[0][0] = 6;
 *   x[0][1] = 7;
 *   x[1][0] = 8;
 *   x[1][1] = 9;
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after processing the initializer.
 * @param tok The current token representing the start of the initializer.
 * @param var The object (variable) being initialized.
 *
 * @return A pointer to the node representing the initialization expressions.
 *
 * @details This function first zero-initializes the entire memory region for
 * the variable (if any partial initializers are given), and then applies
 * the user-supplied values for the initialization. The result is a series
 * of assignment expressions for each element of the array or struct being initialized.
 */
static Node *lvar_initializer(Token **rest, Token *tok, Obj *var)
{
	Initializer *init = initializer(rest, tok, var->ty, &var->ty);
	InitDesg desg = { NULL, 0, NULL, var };

	// Zero-initialize the memory region for the variable
	Node *lhs = new_node(ND_MEMZERO, tok);

	lhs->var = var;

	// Create the initialization expressions
	Node *rhs = create_lvar_init(init, var->ty, &desg, tok);
	return new_binary(ND_COMMA, lhs, rhs, tok);
}

/**
 * @brief Reads a value from a buffer based on the specified size.
 *
 * This function reads a value of size 1, 2, 4, or 8 bytes from a given
 * buffer and returns the corresponding value.
 *
 * @param buf A pointer to the buffer containing the data.
 * @param sz The size of the value to read (1, 2, 4, or 8).
 *
 * @return The value read from the buffer.
 *
 * @details The function checks the specified size and casts the buffer
 * appropriately to read the correct value type. If the size is unsupported,
 * the function calls `unreachable()`.
 */
static uint64_t read_buf(char *buf, int sz)
{
	if (sz == 1)
		return *buf;
	if (sz == 2)
		return *(uint16_t *)buf;
	if (sz == 4)
		return *(uint32_t *)buf;
	if (sz == 8)
		return *(uint64_t *)buf;
	unreachable();
}

/**
 * @brief Writes a value to a buffer based on the specified size.
 *
 * This function writes a value of size 1, 2, 4, or 8 bytes to a given
 * buffer.
 *
 * @param buf A pointer to the buffer where the data will be written.
 * @param val The value to write to the buffer.
 * @param sz The size of the value to write (1, 2, 4, or 8).
 *
 * @details The function checks the specified size and casts the value to
 * the appropriate type before writing it to the buffer. If the size is unsupported,
 * the function calls `unreachable()`.
 */
static void write_buf(char *buf, uint64_t val, int sz)
{
	if (sz == 1)
		*buf = val;
	else if (sz == 2)
		*(uint16_t *)buf = val;
	else if (sz == 4)
		*(uint32_t *)buf = val;
	else if (sz == 8)
		*(uint64_t *)buf = val;
	else
		unreachable();
}

/**
 * @brief Writes data for a global variable initializer to memory, handling
 *        arrays, structs, and bitfields.
 *
 * This function processes an initializer for a global variable, writing
 * the data to the provided buffer and handling different types such as
 * arrays, structs, unions, and bitfields.
 *
 * @param cur The current relocation (or NULL if there is no relocation).
 * @param init The initializer containing the data to write.
 * @param ty The type of the global variable being initialized.
 * @param buf The buffer to which the data will be written.
 * @param offset The offset within the buffer where the data should be written.
 *
 * @return A pointer to the updated relocation (if any).
 *
 * @details The function recursively processes the initializer for arrays,
 * structs, and unions. It checks for bitfields and applies bitwise operations
 * to correctly combine the new and old values for bitfields. The function
 * returns the updated relocation information (if any).
 */
static Relocation *
write_gvar_data(Relocation *cur, Initializer *init, Type *ty, char *buf, int offset)
{
	if (ty->kind == TY_ARRAY) {
		int sz = ty->base->size;
		for (int i = 0; i < ty->array_len; i++)
			cur = write_gvar_data(cur, init->children[i], ty->base, buf, offset + sz * i);
		return cur;
	}

	if (ty->kind == TY_STRUCT) {
		for (Member *mem = ty->members; mem; mem = mem->next) {
			if (mem->is_bitfield) {
				Node *expr = init->children[mem->idx]->expr;
				if (!expr)
					break;

				char *loc = buf + offset + mem->offset;
				uint64_t oldval = read_buf(loc, mem->ty->size);
				uint64_t newval = eval(expr);
				uint64_t mask = (1L << mem->bit_width) - 1;
				uint64_t combined = oldval | ((newval & mask) << mem->bit_offset);
				write_buf(loc, combined, mem->ty->size);
			} else {
				cur = write_gvar_data(cur, init->children[mem->idx], mem->ty, buf,
						      offset + mem->offset);
			}
		}
		return cur;
	}

	if (ty->kind == TY_UNION) {
		if (!init->mem)
			return cur;
		return write_gvar_data(cur, init->children[init->mem->idx],
				       init->mem->ty, buf, offset);
	}

	if (!init->expr)
		return cur;

	if (ty->kind == TY_FLOAT) {
		*(float *)(buf + offset) = eval_double(init->expr);
		return cur;
	}

	if (ty->kind == TY_DOUBLE) {
		*(double *)(buf + offset) = eval_double(init->expr);
		return cur;
	}

	char **label = NULL;
	uint64_t val = eval2(init->expr, &label);

	if (!label) {
		write_buf(buf + offset, val, ty->size);
		return cur;
	}

	Relocation *rel = calloc(1, sizeof(Relocation));
	rel->offset = offset;
	rel->label = label;
	rel->addend = val;
	cur->next = rel;
	return cur->next;
}

/**
 * @brief Initializes a global variable with a compile-time initializer.
 *
 * This function serializes an `Initializer` object into a flat byte array,
 * which is then embedded into the `.data` section during the compilation.
 * It is a compile-time error if an initializer list contains a non-constant expression.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after processing the initializer.
 * @param tok The current token representing the start of the initializer.
 * @param var The global variable being initialized.
 *
 * @details The function first evaluates the initializer at compile time and then
 * serializes it into a byte buffer. The initializer data is stored in `var->init_data`,
 * and the relocation information (if any) is stored in `var->rel`.
 */
static void gvar_initializer(Token **rest, Token *tok, Obj *var)
{
	Initializer *init = initializer(rest, tok, var->ty, &var->ty);

	Relocation head = {};
	char *buf = calloc(1, var->ty->size);

	write_gvar_data(&head, init, var->ty, buf, 0);
	var->init_data = buf;
	var->rel = head.next;
}

/**
 * @brief Checks if a token represents a type name.
 *
 * This function checks whether a given token corresponds to a valid type,
 * such as basic types (`int`, `float`, etc.) or more complex types like
 * `struct` and `union`.
 *
 * @param tok The token to check.
 *
 * @return `true` if the token represents a type name, `false` otherwise.
 *
 * @details The function uses a hashmap to store common type names and checks
 * if the token matches any of those types. It also considers typedefs by
 * calling `find_typedef`.
 */
static bool is_typename(Token *tok)
{
	static HashMap map;

	if (map.capacity == 0) {
		static char *kw[] = {
			"void",		 "_Bool",     "char",	 "short",    "int",	 "long",       "struct",   "union",
			"typedef",	 "enum",      "static",	 "extern",   "_Alignas", "signed",     "unsigned",
			"const",	 "volatile",  "auto",	 "register", "restrict", "__restrict",
			"__restrict__",	 "_Noreturn", "float",	 "double",   "typeof",	 "inline",
			"_Thread_local", "__thread",  "_Atomic",
		};

		for (int i = 0; i < sizeof(kw) / sizeof(*kw); i++)
			hashmap_put(&map, kw[i], (void *)1);
	}

	return hashmap_get2(&map, tok->loc, tok->len) || find_typedef(tok);
}

/**
 * @brief Parses an assembly statement in the form of `asm` with optional `volatile` or `inline` modifiers.
 *
 * This function parses an assembly statement of the form `"asm" ("volatile" | "inline")* "(" string-literal ")"`.
 * It expects a string literal representing the assembly code.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after processing the statement.
 * @param tok The current token representing the start of the assembly statement.
 *
 * @return A `Node` representing the parsed assembly statement.
 *
 * @details The function checks for optional `volatile` or `inline` keywords before
 * extracting the string literal containing the assembly code. It returns a node
 * representing the assembly statement, which can later be processed or translated.
 */
static Node *asm_stmt(Token **rest, Token *tok)
{
	Node *node = new_node(ND_ASM, tok);

	tok = tok->next;

	// Skip optional "volatile" or "inline"
	while (equal(tok, "volatile") || equal(tok, "inline"))
		tok = tok->next;

	tok = skip(tok, "(");
	if (tok->kind != TK_STR || tok->ty->base->kind != TY_CHAR)
		error_tok(tok, "expected string literal");
	node->asm_str = tok->str;
	*rest = skip(tok->next, ")");
	return node;
}

/**
 * @brief Parses a statement in the source code.
 *
 * This function parses a wide variety of statements in C, including control
 * flow statements (e.g., `if`, `for`, `while`), jump statements (e.g., `goto`, `break`),
 * and expressions. It recursively processes each statement type and returns a `Node`
 * representing the parsed statement.
 *
 * @param rest A pointer to a token pointer that will be updated to point to
 *             the next token after processing the statement.
 * @param tok The current token representing the start of the statement.
 *
 * @return A `Node` representing the parsed statement.
 *
 * @details The function handles different types of statements including:
 * - `return` statement with an optional expression.
 * - `if`, `else`, and `switch` statements.
 * - `for`, `while`, and `do-while` loops.
 * - `asm` for inline assembly.
 * - Control flow statements like `goto`, `break`, and `continue`.
 * - Labels and compound statements.
 */
static Node *stmt(Token **rest, Token *tok)
{
	if (equal(tok, "return")) {
		Node *node = new_node(ND_RETURN, tok);
		if (consume(rest, tok->next, ";"))
			return node;

		Node *exp = expr(&tok, tok->next);
		*rest = skip(tok, ";");

		add_type(exp);
		Type *ty = current_fn->ty->return_ty;
		if (ty->kind != TY_STRUCT && ty->kind != TY_UNION)
			exp = new_cast(exp, current_fn->ty->return_ty);

		node->lhs = exp;
		return node;
	}

	if (equal(tok, "if")) {
		Node *node = new_node(ND_IF, tok);
		tok = skip(tok->next, "(");
		node->cond = expr(&tok, tok);
		tok = skip(tok, ")");
		node->then = stmt(&tok, tok);
		if (equal(tok, "else"))
			node->els = stmt(&tok, tok->next);
		*rest = tok;
		return node;
	}

	if (equal(tok, "switch")) {
		Node *node = new_node(ND_SWITCH, tok);
		tok = skip(tok->next, "(");
		node->cond = expr(&tok, tok);
		tok = skip(tok, ")");

		Node *sw = current_switch;
		current_switch = node;

		char *brk = brk_label;
		brk_label = node->brk_label = new_unique_name();

		node->then = stmt(rest, tok);

		current_switch = sw;
		brk_label = brk;
		return node;
	}

	if (equal(tok, "case")) {
		if (!current_switch)
			error_tok(tok, "stray case");

		Node *node = new_node(ND_CASE, tok);
		int begin = const_expr(&tok, tok->next);
		int end;

		if (equal(tok, "...")) {
			// [GNU] Case ranges, e.g. "case 1 ... 5:"
			end = const_expr(&tok, tok->next);
			if (end < begin)
				error_tok(tok, "empty case range specified");
		} else {
			end = begin;
		}

		tok = skip(tok, ":");
		node->label = new_unique_name();
		node->lhs = stmt(rest, tok);
		node->begin = begin;
		node->end = end;
		node->case_next = current_switch->case_next;
		current_switch->case_next = node;
		return node;
	}

	if (equal(tok, "default")) {
		if (!current_switch)
			error_tok(tok, "stray default");

		Node *node = new_node(ND_CASE, tok);
		tok = skip(tok->next, ":");
		node->label = new_unique_name();
		node->lhs = stmt(rest, tok);
		current_switch->default_case = node;
		return node;
	}

	if (equal(tok, "for")) {
		Node *node = new_node(ND_FOR, tok);
		tok = skip(tok->next, "(");

		enter_scope();

		char *brk = brk_label;
		char *cont = cont_label;
		brk_label = node->brk_label = new_unique_name();
		cont_label = node->cont_label = new_unique_name();

		if (is_typename(tok)) {
			Type *basety = declspec(&tok, tok, NULL);
			node->init = declaration(&tok, tok, basety, NULL);
		} else {
			node->init = expr_stmt(&tok, tok);
		}

		if (!equal(tok, ";"))
			node->cond = expr(&tok, tok);
		tok = skip(tok, ";");

		if (!equal(tok, ")"))
			node->inc = expr(&tok, tok);
		tok = skip(tok, ")");

		node->then = stmt(rest, tok);

		leave_scope();
		brk_label = brk;
		cont_label = cont;
		return node;
	}

	if (equal(tok, "while")) {
		Node *node = new_node(ND_FOR, tok);
		tok = skip(tok->next, "(");
		node->cond = expr(&tok, tok);
		tok = skip(tok, ")");

		char *brk = brk_label;
		char *cont = cont_label;
		brk_label = node->brk_label = new_unique_name();
		cont_label = node->cont_label = new_unique_name();

		node->then = stmt(rest, tok);

		brk_label = brk;
		cont_label = cont;
		return node;
	}

	if (equal(tok, "do")) {
		Node *node = new_node(ND_DO, tok);

		char *brk = brk_label;
		char *cont = cont_label;
		brk_label = node->brk_label = new_unique_name();
		cont_label = node->cont_label = new_unique_name();

		node->then = stmt(&tok, tok->next);

		brk_label = brk;
		cont_label = cont;

		tok = skip(tok, "while");
		tok = skip(tok, "(");
		node->cond = expr(&tok, tok);
		tok = skip(tok, ")");
		*rest = skip(tok, ";");
		return node;
	}

	if (equal(tok, "asm"))
		return asm_stmt(rest, tok);

	if (equal(tok, "goto")) {
		if (equal(tok->next, "*")) {
			// [GNU] `goto *ptr` jumps to the address specified by `ptr`.
			Node *node = new_node(ND_GOTO_EXPR, tok);
			node->lhs = expr(&tok, tok->next->next);
			*rest = skip(tok, ";");
			return node;
		}

		Node *node = new_node(ND_GOTO, tok);
		node->label = get_ident(tok->next);
		node->goto_next = gotos;
		gotos = node;
		*rest = skip(tok->next->next, ";");
		return node;
	}

	if (equal(tok, "break")) {
		if (!brk_label)
			error_tok(tok, "stray break");
		Node *node = new_node(ND_GOTO, tok);
		node->unique_label = brk_label;
		*rest = skip(tok->next, ";");
		return node;
	}

	if (equal(tok, "continue")) {
		if (!cont_label)
			error_tok(tok, "stray continue");
		Node *node = new_node(ND_GOTO, tok);
		node->unique_label = cont_label;
		*rest = skip(tok->next, ";");
		return node;
	}

	if (tok->kind == TK_IDENT && equal(tok->next, ":")) {
		Node *node = new_node(ND_LABEL, tok);
		node->label = strndup(tok->loc, tok->len);
		node->unique_label = new_unique_name();
		node->lhs = stmt(rest, tok->next->next);
		node->goto_next = labels;
		labels = node;
		return node;
	}

	if (equal(tok, "{"))
		return compound_stmt(rest, tok->next);

	return expr_stmt(rest, tok);
}

/**
 * @brief Parses a compound statement (block of statements enclosed in `{}`).
 *
 * A compound statement can contain variable declarations, typedefs, function
 * definitions, global variables (with `extern`), or other statements.
 * This function processes each statement inside the block, adds it to a
 * linked list of `Node` objects, and returns the block as a `ND_BLOCK` node.
 *
 * @param rest A pointer to a token pointer that will be updated to the next token after `}`.
 * @param tok The token representing the start of the compound statement.
 * @return A `Node` representing the parsed compound statement.
 */
static Node *compound_stmt(Token **rest, Token *tok)
{
	Node *node = new_node(ND_BLOCK, tok);
	Node head = {};
	Node *cur = &head;

	enter_scope();

	while (!equal(tok, "}")) {
		if (is_typename(tok) && !equal(tok->next, ":")) {
			VarAttr attr = {};
			Type *basety = declspec(&tok, tok, &attr);

			if (attr.is_typedef) {
				tok = parse_typedef(tok, basety);
				continue;
			}

			if (is_function(tok)) {
				tok = function(tok, basety, &attr);
				continue;
			}

			if (attr.is_extern) {
				tok = global_variable(tok, basety, &attr);
				continue;
			}

			cur = cur->next = declaration(&tok, tok, basety, &attr);
		} else {
			cur = cur->next = stmt(&tok, tok);
		}
		add_type(cur);
	}

	leave_scope();

	node->body = head.next;
	*rest = tok->next;
	return node;
}

/**
 * @brief Parses an expression statement.
 *
 * An expression statement is an optional expression followed by a semicolon.
 * If the statement consists of only `;`, it is treated as an empty block.
 *
 * @param rest A pointer to a token pointer that will be updated to the next token after `;`.
 * @param tok The token representing the start of the expression statement.
 * @return A `Node` representing the parsed expression statement.
 */
static Node *expr_stmt(Token **rest, Token *tok)
{
	if (equal(tok, ";")) {
		*rest = tok->next;
		return new_node(ND_BLOCK, tok);
	}

	Node *node = new_node(ND_EXPR_STMT, tok);
	node->lhs = expr(&tok, tok);
	*rest = skip(tok, ";");
	return node;
}

/**
 * @brief Parses an expression.
 *
 * An expression consists of an assignment, optionally followed by a comma-separated
 * sequence of expressions. This function handles the parsing of such expressions and
 * constructs the corresponding `Node` structure.
 *
 * @param rest A pointer to a token pointer that will be updated to the next token.
 * @param tok The token representing the start of the expression.
 * @return A `Node` representing the parsed expression.
 */
static Node *expr(Token **rest, Token *tok)
{
	Node *node = assign(&tok, tok);

	if (equal(tok, ","))
		return new_binary(ND_COMMA, node, expr(rest, tok->next), tok);

	*rest = tok;
	return node;
}

/**
 * @brief Evaluates an expression node as a constant.
 *
 * This function is a wrapper for `eval2`, calling it without a label.
 *
 * @param node The node of the expression to be evaluated.
 * @return The integer value resulting from the evaluation.
 */
static int64_t eval(Node *node)
{
	return eval2(node, NULL);
}

/**
 * @brief Evaluates an expression as a compile-time constant.
 *
 * This function tries to evaluate the expression represented by `node` as a constant.
 * A constant can be a literal number or an expression involving pointers and offsets.
 * If the expression is not a valid compile-time constant, an error is generated.
 *
 * @param node The node of the expression to be evaluated.
 * @param label Pointer for storing labels of global variables used in the evaluation.
 * @return The integer value resulting from the evaluation.
 */
static int64_t eval2(Node *node, char ***label)
{
	add_type(node);

	if (is_flonum(node->ty))
		return eval_double(node);

	switch (node->kind) {
	case ND_ADD:
		return eval2(node->lhs, label) + eval(node->rhs);
	case ND_SUB:
		return eval2(node->lhs, label) - eval(node->rhs);
	case ND_MUL:
		return eval(node->lhs) * eval(node->rhs);
	case ND_DIV:
		if (node->ty->is_unsigned)
			return (uint64_t)eval(node->lhs) / eval(node->rhs);
		return eval(node->lhs) / eval(node->rhs);
	case ND_NEG:
		return -eval(node->lhs);
	case ND_MOD:
		if (node->ty->is_unsigned)
			return (uint64_t)eval(node->lhs) % eval(node->rhs);
		return eval(node->lhs) % eval(node->rhs);
	case ND_BITAND:
		return eval(node->lhs) & eval(node->rhs);
	case ND_BITOR:
		return eval(node->lhs) | eval(node->rhs);
	case ND_BITXOR:
		return eval(node->lhs) ^ eval(node->rhs);
	case ND_SHL:
		return eval(node->lhs) << eval(node->rhs);
	case ND_SHR:
		if (node->ty->is_unsigned && node->ty->size == 8)
			return (uint64_t)eval(node->lhs) >> eval(node->rhs);
		return eval(node->lhs) >> eval(node->rhs);
	case ND_EQ:
		return eval(node->lhs) == eval(node->rhs);
	case ND_NE:
		return eval(node->lhs) != eval(node->rhs);
	case ND_LT:
		if (node->lhs->ty->is_unsigned)
			return (uint64_t)eval(node->lhs) < eval(node->rhs);
		return eval(node->lhs) < eval(node->rhs);
	case ND_LE:
		if (node->lhs->ty->is_unsigned)
			return (uint64_t)eval(node->lhs) <= eval(node->rhs);
		return eval(node->lhs) <= eval(node->rhs);
	case ND_COND:
		return eval(node->cond) ? eval2(node->then, label) : eval2(node->els, label);
	case ND_COMMA:
		return eval2(node->rhs, label);
	case ND_NOT:
		return !eval(node->lhs);
	case ND_BITNOT:
		return ~eval(node->lhs);
	case ND_LOGAND:
		return eval(node->lhs) && eval(node->rhs);
	case ND_LOGOR:
		return eval(node->lhs) || eval(node->rhs);
	case ND_CAST: {
		int64_t val = eval2(node->lhs, label);
		if (is_integer(node->ty)) {
			switch (node->ty->size) {
			case 1: return node->ty->is_unsigned ? (uint8_t)val : (int8_t)val;
			case 2: return node->ty->is_unsigned ? (uint16_t)val : (int16_t)val;
			case 4: return node->ty->is_unsigned ? (uint32_t)val : (int32_t)val;
			}
		}
		return val;
	}
	case ND_ADDR:
		return eval_rval(node->lhs, label);
	case ND_LABEL_VAL:
		*label = &node->unique_label;
		return 0;
	case ND_MEMBER:
		if (!label)
			error_tok(node->tok, "not a compile-time constant");
		if (node->ty->kind != TY_ARRAY)
			error_tok(node->tok, "invalid initializer");
		return eval_rval(node->lhs, label) + node->member->offset;
	case ND_VAR:
		if (!label)
			error_tok(node->tok, "not a compile-time constant");
		if (node->var->ty->kind != TY_ARRAY && node->var->ty->kind != TY_FUNC)
			error_tok(node->tok, "invalid initializer");
		*label = &node->var->name;
		return 0;
	case ND_NUM:
		return node->val;
	}

	error_tok(node->tok, "not a compile-time constant");
}

/**
 * @brief Evaluates a node as a compile-time constant value.
 *
 * This function evaluates a node that represents a compile-time constant,
 * such as a global variable or a struct member offset.
 *
 * @param node The node to evaluate.
 * @param label Pointer to store the label of the associated global variable.
 * @return The resulting integer value.
 */
static int64_t eval_rval(Node *node, char ***label)
{
	switch (node->kind) {
	case ND_VAR:
		if (node->var->is_local)
			error_tok(node->tok, "not a compile-time constant");
		*label = &node->var->name;
		return 0;
	case ND_DEREF:
		return eval2(node->lhs, label);
	case ND_MEMBER:
		return eval_rval(node->lhs, label) + node->member->offset;
	}

	error_tok(node->tok, "invalid initializer");
}

/**
 * @brief Checks if a node represents a constant expression.
 *
 * This function determines whether an expression can be evaluated at compile-time.
 *
 * @param node The expression node to evaluate.
 * @return `true` if the expression is constant, `false` otherwise.
 */
static bool is_const_expr(Node *node)
{
	add_type(node);

	switch (node->kind) {
	case ND_ADD:
	case ND_SUB:
	case ND_MUL:
	case ND_DIV:
	case ND_BITAND:
	case ND_BITOR:
	case ND_BITXOR:
	case ND_SHL:
	case ND_SHR:
	case ND_EQ:
	case ND_NE:
	case ND_LT:
	case ND_LE:
	case ND_LOGAND:
	case ND_LOGOR:
		return is_const_expr(node->lhs) && is_const_expr(node->rhs);
	case ND_COND:
		if (!is_const_expr(node->cond))
			return false;
		return is_const_expr(eval(node->cond) ? node->then : node->els);
	case ND_COMMA:
		return is_const_expr(node->rhs);
	case ND_NEG:
	case ND_NOT:
	case ND_BITNOT:
	case ND_CAST:
		return is_const_expr(node->lhs);
	case ND_NUM:
		return true;
	}

	return false;
}

/**
 * @brief Evaluates a conditional expression as a constant.
 *
 * This function evaluates an expression and returns its value as a compile-time constant.
 *
 * @param rest Pointer to store the remaining token after evaluation.
 * @param tok The starting token of the expression.
 * @return The resulting integer value.
 */
int64_t const_expr(Token **rest, Token *tok)
{
	Node *node = conditional(rest, tok);

	return eval(node);
}

/**
 * @brief Evaluates an expression and returns its value as a floating-point number.
 *
 * This function evaluates an expression and converts it to a floating-point value,
 * if necessary.
 *
 * @param node The expression node to evaluate.
 * @return The resulting floating-point value.
 */
static double eval_double(Node *node)
{
	add_type(node);

	if (is_integer(node->ty)) {
		if (node->ty->is_unsigned)
			return (unsigned long)eval(node);
		return eval(node);
	}

	switch (node->kind) {
	case ND_ADD:
		return eval_double(node->lhs) + eval_double(node->rhs);
	case ND_SUB:
		return eval_double(node->lhs) - eval_double(node->rhs);
	case ND_MUL:
		return eval_double(node->lhs) * eval_double(node->rhs);
	case ND_DIV:
		return eval_double(node->lhs) / eval_double(node->rhs);
	case ND_NEG:
		return -eval_double(node->lhs);
	case ND_COND:
		return eval_double(node->cond) ? eval_double(node->then) : eval_double(node->els);
	case ND_COMMA:
		return eval_double(node->rhs);
	case ND_CAST:
		if (is_flonum(node->lhs->ty))
			return eval_double(node->lhs);
		return eval(node->lhs);
	case ND_NUM:
		return node->fval;
	}

	error_tok(node->tok, "not a compile-time constant");
}

/**
 * @brief Converts `op=` operators into equivalent assignment expressions.
 *
 * This function transforms `A op= C` expressions into explicit assignments.
 * The general transformation follows:
 * - `A op= C` → `tmp = &A, *tmp = *tmp op C`
 * - If `A` is a struct member (e.g., `A.x op= C`), it transforms into:
 *   `tmp = &A, (*tmp).x = (*tmp).x op C`, ensuring bitfields are handled properly.
 *
 * Additionally, if `A` is an atomic type, the transformation uses
 * `atomic_compare_exchange_strong` to ensure atomicity.
 *
 * @param binary The binary operation node representing `A op= C`.
 * @return A new node representing the transformed assignment expression.
 */
static Node *to_assign(Node *binary)
{
	add_type(binary->lhs);
	add_type(binary->rhs);
	Token *tok = binary->tok;

	// Convert `A.x op= C` to `tmp = &A, (*tmp).x = (*tmp).x op C`
	if (binary->lhs->kind == ND_MEMBER) {
		Obj *var = new_lvar("", pointer_to(binary->lhs->lhs->ty));

		Node *expr1 = new_binary(ND_ASSIGN, new_var_node(var, tok),
					 new_unary(ND_ADDR, binary->lhs->lhs, tok), tok);

		Node *expr2 = new_unary(ND_MEMBER,
					new_unary(ND_DEREF, new_var_node(var, tok), tok),
					tok);
		expr2->member = binary->lhs->member;

		Node *expr3 = new_unary(ND_MEMBER,
					new_unary(ND_DEREF, new_var_node(var, tok), tok),
					tok);
		expr3->member = binary->lhs->member;

		Node *expr4 = new_binary(ND_ASSIGN, expr2,
					 new_binary(binary->kind, expr3, binary->rhs, tok),
					 tok);

		return new_binary(ND_COMMA, expr1, expr4, tok);
	}

	// If A is an atomic type, convert `A op= B` to:
	//
	// ({
	//   T1 *addr = &A; T2 val = (B); T1 old = *addr; T1 new;
	//   do {
	//     new = old op val;
	//   } while (!atomic_compare_exchange_strong(addr, &old, new));
	//   new;
	// })
	if (binary->lhs->ty->is_atomic) {
		Node head = {};
		Node *cur = &head;

		Obj *addr = new_lvar("", pointer_to(binary->lhs->ty));
		Obj *val = new_lvar("", binary->rhs->ty);
		Obj *old = new_lvar("", binary->lhs->ty);
		Obj *new = new_lvar("", binary->lhs->ty);

		cur = cur->next =
			new_unary(ND_EXPR_STMT,
				  new_binary(ND_ASSIGN, new_var_node(addr, tok),
					     new_unary(ND_ADDR, binary->lhs, tok), tok),
				  tok);

		cur = cur->next =
			new_unary(ND_EXPR_STMT,
				  new_binary(ND_ASSIGN, new_var_node(val, tok), binary->rhs, tok),
				  tok);

		cur = cur->next =
			new_unary(ND_EXPR_STMT,
				  new_binary(ND_ASSIGN, new_var_node(old, tok),
					     new_unary(ND_DEREF, new_var_node(addr, tok), tok), tok),
				  tok);

		Node *loop = new_node(ND_DO, tok);
		loop->brk_label = new_unique_name();
		loop->cont_label = new_unique_name();

		Node *body = new_binary(ND_ASSIGN,
					new_var_node(new, tok),
					new_binary(binary->kind, new_var_node(old, tok),
						   new_var_node(val, tok), tok),
					tok);

		loop->then = new_node(ND_BLOCK, tok);
		loop->then->body = new_unary(ND_EXPR_STMT, body, tok);

		Node *cas = new_node(ND_CAS, tok);
		cas->cas_addr = new_var_node(addr, tok);
		cas->cas_old = new_unary(ND_ADDR, new_var_node(old, tok), tok);
		cas->cas_new = new_var_node(new, tok);
		loop->cond = new_unary(ND_NOT, cas, tok);

		cur = cur->next = loop;
		cur = cur->next = new_unary(ND_EXPR_STMT, new_var_node(new, tok), tok);

		Node *node = new_node(ND_STMT_EXPR, tok);
		node->body = head.next;
		return node;
	}

	// Convert `A op= B` to `tmp = &A, *tmp = *tmp op B`
	Obj *var = new_lvar("", pointer_to(binary->lhs->ty));

	Node *expr1 = new_binary(ND_ASSIGN, new_var_node(var, tok),
				 new_unary(ND_ADDR, binary->lhs, tok), tok);

	Node *expr2 =
		new_binary(ND_ASSIGN,
			   new_unary(ND_DEREF, new_var_node(var, tok), tok),
			   new_binary(binary->kind,
				      new_unary(ND_DEREF, new_var_node(var, tok), tok),
				      binary->rhs,
				      tok),
			   tok);

	return new_binary(ND_COMMA, expr1, expr2, tok);
}

/**
 * @brief Parses an assignment expression.
 *
 * Grammar:
 * ```
 * assign    = conditional (assign-op assign)?
 * assign-op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
 *           | "<<=" | ">>="
 * ```
 *
 * This function first parses a conditional expression, which can be the left-hand side (LHS) of an assignment.
 * If an assignment operator (`=`, `+=`, etc.) follows, it recursively parses the right-hand side (RHS)
 * and constructs the appropriate assignment node.
 *
 * If the operator is a compound assignment (e.g., `+=`), the function rewrites it into an explicit assignment
 * using `to_assign()`. Example transformations:
 *
 * - `a += b` → `a = a + b`
 * - `a *= b` → `a = a * b`
 *
 * @param rest  A pointer to store the next token after parsing.
 * @param tok   The current token being processed.
 * @return A pointer to the parsed AST node representing the assignment expression.
 */
static Node *assign(Token **rest, Token *tok)
{
	Node *node = conditional(&tok, tok);

	if (equal(tok, "="))
		return new_binary(ND_ASSIGN, node, assign(rest, tok->next), tok);

	if (equal(tok, "+="))
		return to_assign(new_add(node, assign(rest, tok->next), tok));

	if (equal(tok, "-="))
		return to_assign(new_sub(node, assign(rest, tok->next), tok));

	if (equal(tok, "*="))
		return to_assign(new_binary(ND_MUL, node, assign(rest, tok->next), tok));

	if (equal(tok, "/="))
		return to_assign(new_binary(ND_DIV, node, assign(rest, tok->next), tok));

	if (equal(tok, "%="))
		return to_assign(new_binary(ND_MOD, node, assign(rest, tok->next), tok));

	if (equal(tok, "&="))
		return to_assign(new_binary(ND_BITAND, node, assign(rest, tok->next), tok));

	if (equal(tok, "|="))
		return to_assign(new_binary(ND_BITOR, node, assign(rest, tok->next), tok));

	if (equal(tok, "^="))
		return to_assign(new_binary(ND_BITXOR, node, assign(rest, tok->next), tok));

	if (equal(tok, "<<="))
		return to_assign(new_binary(ND_SHL, node, assign(rest, tok->next), tok));

	if (equal(tok, ">>="))
		return to_assign(new_binary(ND_SHR, node, assign(rest, tok->next), tok));

	*rest = tok;
	return node;
}

/**
 * @brief Parses a conditional (ternary) expression.
 *
 * Grammar:
 * ```
 * conditional = logor ("?" expr? ":" conditional)?
 * ```
 *
 * This function parses a logical OR expression as the condition (`logor`).
 * If a `?` token follows, it parses a ternary conditional expression.
 *
 * - If the ternary expression is of the form `a ? b : c`, it constructs an `ND_COND` AST node.
 * - If the expression is `a ?: b` (GNU extension), it rewrites it as:
 *   ```
 *   tmp = a, tmp ? tmp : b
 *   ```
 *   This ensures `a` is only evaluated once.
 *
 * @param rest  A pointer to store the next token after parsing.
 * @param tok   The current token being processed.
 * @return A pointer to the parsed AST node representing the conditional expression.
 */
static Node *conditional(Token **rest, Token *tok)
{
	Node *cond = logor(&tok, tok);

	if (!equal(tok, "?")) {
		*rest = tok;
		return cond;
	}

	if (equal(tok->next, ":")) {
		// [GNU] Compile `a ?: b` as `tmp = a, tmp ? tmp : b`.
		add_type(cond);
		Obj *var = new_lvar("", cond->ty);
		Node *lhs = new_binary(ND_ASSIGN, new_var_node(var, tok), cond, tok);
		Node *rhs = new_node(ND_COND, tok);
		rhs->cond = new_var_node(var, tok);
		rhs->then = new_var_node(var, tok);
		rhs->els = conditional(rest, tok->next->next);
		return new_binary(ND_COMMA, lhs, rhs, tok);
	}

	Node *node = new_node(ND_COND, tok);
	node->cond = cond;
	node->then = expr(&tok, tok->next);
	tok = skip(tok, ":");
	node->els = conditional(rest, tok);
	return node;
}

/**
 * @brief Parses a logical OR (`||`) expression.
 *
 * Grammar:
 * ```
 * logor = logand ("||" logand)*
 * ```
 *
 * This function parses a left-associative sequence of logical AND (`logand`) expressions
 * separated by `||`. Each `||` operator creates an `ND_LOGOR` node in the AST.
 *
 * Example:
 * ```
 * a || b || c
 * ```
 * Parses as:
 * ```
 * (a || (b || c))
 * ```
 *
 * @param rest  A pointer to store the next token after parsing.
 * @param tok   The current token being processed.
 * @return A pointer to the parsed AST node representing the logical OR expression.
 */
static Node *logor(Token **rest, Token *tok)
{
	Node *node = logand(&tok, tok);

	while (equal(tok, "||")) {
		Token *start = tok;
		node = new_binary(ND_LOGOR, node, logand(&tok, tok->next), start);
	}
	*rest = tok;
	return node;
}

/**
 * @brief Parses a logical AND (`&&`) expression.
 *
 * Grammar:
 * ```
 * logand = bitor ("&&" bitor)*
 * ```
 *
 * Similar to `logor`, this function parses a sequence of bitwise OR (`bitor`) expressions
 * separated by `&&`. Each `&&` operator creates an `ND_LOGAND` node in the AST.
 *
 * Example:
 * ```
 * a && b && c
 * ```
 * Parses as:
 * ```
 * (a && (b && c))
 * ```
 *
 * @param rest  A pointer to store the next token after parsing.
 * @param tok   The current token being processed.
 * @return A pointer to the parsed AST node representing the logical AND expression.
 */
static Node *logand(Token **rest, Token *tok)
{
	Node *node = bitor (&tok, tok);

	while (equal(tok, "&&")) {
		Token *start = tok;
		node = new_binary(ND_LOGAND, node, bitor (&tok, tok->next), start);
	}
	*rest = tok;
	return node;
}

/**
 * @brief Parses a bitwise OR (`|`) expression.
 *
 * Grammar:
 * ```
 * bitor = bitxor ("|" bitxor)*
 * ```
 *
 * This function parses a sequence of bitwise XOR (`bitxor`) expressions separated by `|`.
 * Each `|` operator creates an `ND_BITOR` node in the AST.
 *
 * Example:
 * ```
 * a | b | c
 * ```
 * Parses as:
 * ```
 * (a | (b | c))
 * ```
 *
 * @param rest  A pointer to store the next token after parsing.
 * @param tok   The current token being processed.
 * @return A pointer to the parsed AST node representing the bitwise OR expression.
 */
static Node * bitor (Token * *rest, Token *tok) {
	Node *node = bitxor(&tok, tok);
	while (equal(tok, "|")) {
		Token *start = tok;
		node = new_binary(ND_BITOR, node, bitxor(&tok, tok->next), start);
	}
	*rest = tok;
	return node;
}

/**
 * @brief Parses a bitwise XOR (`^`) expression.
 *
 * Grammar:
 * ```
 * bitxor = bitand ("^" bitand)*
 * ```
 *
 * This function parses a sequence of bitwise AND (`bitand`) expressions separated by `^`.
 * Each `^` operator creates an `ND_BITXOR` node in the AST.
 *
 * Example:
 * ```
 * a ^ b ^ c
 * ```
 * Parses as:
 * ```
 * (a ^ (b ^ c))
 * ```
 *
 * @param rest  A pointer to store the next token after parsing.
 * @param tok   The current token being processed.
 * @return A pointer to the parsed AST node representing the bitwise XOR expression.
 */
static Node *bitxor(Token **rest, Token *tok)
{
	Node *node = bitand (&tok, tok);

	while (equal(tok, "^")) {
		Token *start = tok;
		node = new_binary(ND_BITXOR, node, bitand (&tok, tok->next), start);
	}
	*rest = tok;
	return node;
}

/**
 * @brief Parses a bitwise AND (`&`) expression.
 *
 * Grammar:
 * ```
 * bitand = equality ("&" equality)*
 * ```
 *
 * This function parses a sequence of equality expressions separated by `&`.
 * Each `&` operator creates an `ND_BITAND` node in the AST.
 *
 * Example:
 * ```
 * a & b & c
 * ```
 * Parses as:
 * ```
 * (a & (b & c))
 * ```
 *
 * @param rest  A pointer to store the next token after parsing.
 * @param tok   The current token being processed.
 * @return A pointer to the parsed AST node representing the bitwise AND expression.
 */
static Node * bitand (Token * *rest, Token *tok) {
	Node *node = equality(&tok, tok);
	while (equal(tok, "&")) {
		Token *start = tok;
		node = new_binary(ND_BITAND, node, equality(&tok, tok->next), start);
	}
	*rest = tok;
	return node;
}

/**
 * @brief Parses an equality expression (`==`, `!=`).
 *
 * Grammar:
 * ```
 * equality = relational ("==" relational | "!=" relational)*
 * ```
 *
 * This function parses a sequence of relational expressions separated by `==` or `!=`.
 * Each `==` operator creates an `ND_EQ` node, and each `!=` creates an `ND_NE` node.
 *
 * Example:
 * ```
 * a == b != c
 * ```
 * Parses as:
 * ```
 * ((a == b) != c)
 * ```
 *
 * @param rest  A pointer to store the next token after parsing.
 * @param tok   The current token being processed.
 * @return A pointer to the parsed AST node representing the equality expression.
 */
static Node *equality(Token **rest, Token *tok)
{
	Node *node = relational(&tok, tok);

	for (;;) {
		Token *start = tok;

		if (equal(tok, "==")) {
			node = new_binary(ND_EQ, node, relational(&tok, tok->next), start);
			continue;
		}

		if (equal(tok, "!=")) {
			node = new_binary(ND_NE, node, relational(&tok, tok->next), start);
			continue;
		}

		*rest = tok;
		return node;
	}
}

/**
 * @brief Parses a relational expression (`<`, `<=`, `>`, `>=`).
 *
 * Grammar:
 * ```
 * relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
 * ```
 *
 * This function parses a sequence of shift expressions separated by `<`, `<=`, `>`, or `>=`.
 * Each comparison operator creates the corresponding AST node (`ND_LT`, `ND_LE`, etc.).
 *
 * Example:
 * ```
 * a < b >= c
 * ```
 * Parses as:
 * ```
 * ((a < b) >= c)
 * ```
 *
 * @param rest  A pointer to store the next token after parsing.
 * @param tok   The current token being processed.
 * @return A pointer to the parsed AST node representing the relational expression.
 */
static Node *relational(Token **rest, Token *tok)
{
	Node *node = shift(&tok, tok);

	for (;;) {
		Token *start = tok;

		if (equal(tok, "<")) {
			node = new_binary(ND_LT, node, shift(&tok, tok->next), start);
			continue;
		}

		if (equal(tok, "<=")) {
			node = new_binary(ND_LE, node, shift(&tok, tok->next), start);
			continue;
		}

		if (equal(tok, ">")) {
			node = new_binary(ND_LT, shift(&tok, tok->next), node, start);
			continue;
		}

		if (equal(tok, ">=")) {
			node = new_binary(ND_LE, shift(&tok, tok->next), node, start);
			continue;
		}

		*rest = tok;
		return node;
	}
}

/**
 * @brief Parses a shift expression (`<<`, `>>`).
 *
 * Grammar:
 * ```
 * shift = add ("<<" add | ">>" add)*
 * ```
 *
 * This function parses a sequence of addition expressions separated by `<<` or `>>`.
 * Each shift operator creates an `ND_SHL` or `ND_SHR` node.
 *
 * Example:
 * ```
 * a << b >> c
 * ```
 * Parses as:
 * ```
 * ((a << b) >> c)
 * ```
 *
 * @param rest  A pointer to store the next token after parsing.
 * @param tok   The current token being processed.
 * @return A pointer to the parsed AST node representing the shift expression.
 */
static Node *shift(Token **rest, Token *tok)
{
	Node *node = add(&tok, tok);

	for (;;) {
		Token *start = tok;

		if (equal(tok, "<<")) {
			node = new_binary(ND_SHL, node, add(&tok, tok->next), start);
			continue;
		}

		if (equal(tok, ">>")) {
			node = new_binary(ND_SHR, node, add(&tok, tok->next), start);
			continue;
		}

		*rest = tok;
		return node;
	}
}

/**
 * @brief Creates a new addition node, handling pointer arithmetic.
 *
 * In C, the `+` operator is overloaded for pointer arithmetic. If `p` is a pointer,
 * `p + n` is equivalent to `p + sizeof(*p) * n`, so that `p + n` correctly advances
 * by `n` elements rather than `n` bytes. This function handles the necessary scaling.
 *
 * Example:
 * ```
 * int *p;
 * p + 3;
 * ```
 * Converts to:
 * ```
 * p + (3 * sizeof(int))
 * ```
 *
 * @param lhs The left-hand side of the `+` operation.
 * @param rhs The right-hand side of the `+` operation.
 * @param tok The token representing the `+` operator.
 * @return A pointer to the parsed AST node representing the addition.
 */
static Node *new_add(Node *lhs, Node *rhs, Token *tok)
{
	add_type(lhs);
	add_type(rhs);

	// num + num
	if (is_numeric(lhs->ty) && is_numeric(rhs->ty))
		return new_binary(ND_ADD, lhs, rhs, tok);

	if (lhs->ty->base && rhs->ty->base)
		error_tok(tok, "invalid operands");

	// Canonicalize `num + ptr` to `ptr + num`.
	if (!lhs->ty->base && rhs->ty->base) {
		Node *tmp = lhs;
		lhs = rhs;
		rhs = tmp;
	}

	// VLA + num
	if (lhs->ty->base->kind == TY_VLA) {
		rhs = new_binary(ND_MUL, rhs, new_var_node(lhs->ty->base->vla_size, tok), tok);
		return new_binary(ND_ADD, lhs, rhs, tok);
	}

	// ptr + num
	rhs = new_binary(ND_MUL, rhs, new_long(lhs->ty->base->size, tok), tok);
	return new_binary(ND_ADD, lhs, rhs, tok);
}

/**
 * @brief Creates a new subtraction node, handling pointer arithmetic.
 *
 * Like addition, the `-` operator in C is overloaded for pointer arithmetic.
 * If `p` is a pointer and `n` is an integer, `p - n` subtracts `n * sizeof(*p)`.
 * If both operands are pointers, `p - q` computes the number of elements between `p` and `q`.
 *
 * Example:
 * ```
 * int *p, *q;
 * p - q;
 * ```
 * Converts to:
 * ```
 * (p - q) / sizeof(int)
 * ```
 *
 * @param lhs The left-hand side of the `-` operation.
 * @param rhs The right-hand side of the `-` operation.
 * @param tok The token representing the `-` operator.
 * @return A pointer to the parsed AST node representing the subtraction.
 */
static Node *new_sub(Node *lhs, Node *rhs, Token *tok)
{
	add_type(lhs);
	add_type(rhs);

	// num - num
	if (is_numeric(lhs->ty) && is_numeric(rhs->ty))
		return new_binary(ND_SUB, lhs, rhs, tok);

	// VLA - num
	if (lhs->ty->base->kind == TY_VLA) {
		rhs = new_binary(ND_MUL, rhs, new_var_node(lhs->ty->base->vla_size, tok), tok);
		add_type(rhs);
		Node *node = new_binary(ND_SUB, lhs, rhs, tok);
		node->ty = lhs->ty;
		return node;
	}

	// ptr - num
	if (lhs->ty->base && is_integer(rhs->ty)) {
		rhs = new_binary(ND_MUL, rhs, new_long(lhs->ty->base->size, tok), tok);
		add_type(rhs);
		Node *node = new_binary(ND_SUB, lhs, rhs, tok);
		node->ty = lhs->ty;
		return node;
	}

	// ptr - ptr, which returns how many elements are between the two.
	if (lhs->ty->base && rhs->ty->base) {
		Node *node = new_binary(ND_SUB, lhs, rhs, tok);
		node->ty = ty_long;
		return new_binary(ND_DIV, node, new_num(lhs->ty->base->size, tok), tok);
	}

	error_tok(tok, "invalid operands");
}

/**
 * @brief Parses an addition or subtraction expression (`+`, `-`).
 *
 * Grammar:
 * ```
 * add = mul ("+" mul | "-" mul)*
 * ```
 *
 * This function parses a sequence of multiplication expressions separated by `+` or `-`.
 * Addition (`+`) and subtraction (`-`) are handled, including pointer arithmetic.
 *
 * Example:
 * ```
 * a + b - c
 * ```
 * Parses as:
 * ```
 * ((a + b) - c)
 * ```
 *
 * @param rest  A pointer to store the next token after parsing.
 * @param tok   The current token being processed.
 * @return A pointer to the parsed AST node representing the addition or subtraction.
 */
static Node *add(Token **rest, Token *tok)
{
	Node *node = mul(&tok, tok);

	for (;;) {
		Token *start = tok;

		if (equal(tok, "+")) {
			node = new_add(node, mul(&tok, tok->next), start);
			continue;
		}

		if (equal(tok, "-")) {
			node = new_sub(node, mul(&tok, tok->next), start);
			continue;
		}

		*rest = tok;
		return node;
	}
}

/**
 * @brief Parses a multiplication, division, or modulo expression (`*`, `/`, `%`).
 *
 * Grammar:
 * ```
 * mul = cast ("*" cast | "/" cast | "%" cast)*
 * ```
 *
 * This function parses a sequence of cast expressions separated by `*`, `/`, or `%`.
 * Multiplication (`*`), division (`/`), and modulo (`%`) are handled.
 *
 * Example:
 * ```
 * a * b / c % d
 * ```
 * Parses as:
 * ```
 * (((a * b) / c) % d)
 * ```
 *
 * @param rest  A pointer to store the next token after parsing.
 * @param tok   The current token being processed.
 * @return A pointer to the parsed AST node representing the multiplication, division, or modulo.
 */
static Node *mul(Token **rest, Token *tok)
{
	Node *node = cast(&tok, tok);

	for (;;) {
		Token *start = tok;

		if (equal(tok, "*")) {
			node = new_binary(ND_MUL, node, cast(&tok, tok->next), start);
			continue;
		}

		if (equal(tok, "/")) {
			node = new_binary(ND_DIV, node, cast(&tok, tok->next), start);
			continue;
		}

		if (equal(tok, "%")) {
			node = new_binary(ND_MOD, node, cast(&tok, tok->next), start);
			continue;
		}

		*rest = tok;
		return node;
	}
}

/**
 * @brief Parses a type cast or a compound literal.
 *
 * Grammar:
 * ```
 * cast = "(" type-name ")" cast | unary
 * ```
 *
 * This function handles parsing of type casts (e.g., `(int) a`) and compound literals.
 * It checks for the presence of a type name enclosed in parentheses and proceeds to parse
 * the cast or unary operation accordingly.
 *
 * Example:
 * ```
 * (int) a
 * ```
 * Parses as:
 * ```
 * type cast
 * ```
 *
 * @param rest A pointer to store the next token after parsing.
 * @param tok  The current token being processed.
 * @return A pointer to the parsed AST node representing the cast.
 */
static Node *cast(Token **rest, Token *tok)
{
	if (equal(tok, "(") && is_typename(tok->next)) {
		Token *start = tok;
		Type *ty = typename(&tok, tok->next);
		tok = skip(tok, ")");

		// compound literal
		if (equal(tok, "{"))
			return unary(rest, start);

		// type cast
		Node *node = new_cast(cast(rest, tok), ty);
		node->tok = start;
		return node;
	}

	return unary(rest, tok);
}

/**
 * @brief Parses a unary expression (`+`, `-`, `&`, `*`, `!`, `~`, `++`, `--`, and labels).
 *
 * Grammar:
 * ```
 * unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
 *        | ("++" | "--") unary
 *        | "&&" ident
 *        | postfix
 * ```
 *
 * This function handles the unary operators in C, including:
 * - Unary arithmetic operators (`+`, `-`)
 * - Dereference (`*`) and address (`&`) operators
 * - Logical NOT (`!`) and bitwise NOT (`~`)
 * - Increment (`++`) and decrement (`--`) operators
 * - Labels as values (GNU extensions)
 *
 * Example:
 * ```
 * +a, -a, *a, &a, !a, ~a, ++a, --a, &&label
 * ```
 * Parses as:
 * ```
 * (a), (-a), (*a), (&a), (!a), (~a), (++a), (--a), (&&label)
 * ```
 *
 * @param rest A pointer to store the next token after parsing.
 * @param tok  The current token being processed.
 * @return A pointer to the parsed AST node representing the unary operation.
 */
static Node *unary(Token **rest, Token *tok)
{
	if (equal(tok, "+"))
		return cast(rest, tok->next);

	if (equal(tok, "-"))
		return new_unary(ND_NEG, cast(rest, tok->next), tok);

	if (equal(tok, "&")) {
		Node *lhs = cast(rest, tok->next);
		add_type(lhs);
		if (lhs->kind == ND_MEMBER && lhs->member->is_bitfield)
			error_tok(tok, "cannot take address of bitfield");
		return new_unary(ND_ADDR, lhs, tok);
	}

	if (equal(tok, "*")) {
		Node *node = cast(rest, tok->next);
		add_type(node);
		if (node->ty->kind == TY_FUNC)
			return node;
		return new_unary(ND_DEREF, node, tok);
	}

	if (equal(tok, "!"))
		return new_unary(ND_NOT, cast(rest, tok->next), tok);

	if (equal(tok, "~"))
		return new_unary(ND_BITNOT, cast(rest, tok->next), tok);

	// Read ++i as i+=1
	if (equal(tok, "++"))
		return to_assign(new_add(unary(rest, tok->next), new_num(1, tok), tok));

	// Read --i as i-=1
	if (equal(tok, "--"))
		return to_assign(new_sub(unary(rest, tok->next), new_num(1, tok), tok));

	// [GNU] labels-as-values
	if (equal(tok, "&&")) {
		Node *node = new_node(ND_LABEL_VAL, tok);
		node->label = get_ident(tok->next);
		node->goto_next = gotos;
		gotos = node;
		*rest = tok->next->next;
		return node;
	}

	return postfix(rest, tok);
}

/**
 * @brief Parses the members of a struct or union.
 *
 * Grammar:
 * ```
 * struct-members = (declspec declarator (","  declarator)* ";")*
 * ```
 *
 * This function parses the struct or union members. Each member can be a regular member
 * or an anonymous struct/union. Bitfields are also supported.
 *
 * Example:
 * ```
 * struct S {
 *   int a;
 *   int b: 4;
 * };
 * ```
 * Parses as:
 * ```
 * struct members:
 * - int a;
 * - int b: 4;
 * ```
 *
 * @param rest A pointer to store the next token after parsing.
 * @param tok  The current token being processed.
 * @param ty   The type of the struct/union.
 */
static void struct_members(Token **rest, Token *tok, Type *ty)
{
	Member head = {};
	Member *cur = &head;
	int idx = 0;

	while (!equal(tok, "}")) {
		VarAttr attr = {};
		Type *basety = declspec(&tok, tok, &attr);
		bool first = true;

		// Anonymous struct member
		if ((basety->kind == TY_STRUCT || basety->kind == TY_UNION) &&
		    consume(&tok, tok, ";")) {
			Member *mem = calloc(1, sizeof(Member));
			mem->ty = basety;
			mem->idx = idx++;
			mem->align = attr.align ? attr.align : mem->ty->align;
			cur = cur->next = mem;
			continue;
		}

		// Regular struct members
		while (!consume(&tok, tok, ";")) {
			if (!first)
				tok = skip(tok, ",");
			first = false;

			Member *mem = calloc(1, sizeof(Member));
			mem->ty = declarator(&tok, tok, basety);
			mem->name = mem->ty->name;
			mem->idx = idx++;
			mem->align = attr.align ? attr.align : mem->ty->align;

			if (consume(&tok, tok, ":")) {
				mem->is_bitfield = true;
				mem->bit_width = const_expr(&tok, tok);
			}

			cur = cur->next = mem;
		}
	}

	// If the last element is an array of incomplete type, it's
	// called a "flexible array member". It should behave as if
	// it were a zero-sized array.
	if (cur != &head && cur->ty->kind == TY_ARRAY && cur->ty->array_len < 0) {
		cur->ty = array_of(cur->ty->base, 0);
		ty->is_flexible = true;
	}

	*rest = tok->next;
	ty->members = head.next;
}

/**
 * @brief Parses a list of attributes (e.g., `__attribute__((packed))`).
 *
 * Grammar:
 * ```
 * attribute = ("__attribute__" "(" "(" "packed" ")" ")")*
 * ```
 *
 * This function handles the parsing of attributes, specifically the `packed` and `aligned`
 * attributes. It sets the corresponding flags or values in the provided type.
 *
 * Example:
 * ```
 * __attribute__((packed)) struct S { ... }
 * ```
 * Parses as:
 * ```
 * packed attribute set in struct S
 * ```
 *
 * @param tok The current token being processed.
 * @param ty  The type to which the attributes should be applied.
 * @return The next token after parsing the attributes.
 */
static Token *attribute_list(Token *tok, Type *ty)
{
	while (consume(&tok, tok, "__attribute__")) {
		tok = skip(tok, "(");
		tok = skip(tok, "(");

		bool first = true;

		while (!consume(&tok, tok, ")")) {
			if (!first)
				tok = skip(tok, ",");
			first = false;

			if (consume(&tok, tok, "packed")) {
				ty->is_packed = true;
				continue;
			}

			if (consume(&tok, tok, "aligned")) {
				tok = skip(tok, "(");
				ty->align = const_expr(&tok, tok);
				tok = skip(tok, ")");
				continue;
			}

			error_tok(tok, "unknown attribute");
		}

		tok = skip(tok, ")");
	}

	return tok;
}

/**
 * @brief Parses the declaration of a struct or union, optionally with attributes and members.
 *
 * Grammar:
 * ```
 * struct-union-decl = attribute? ident? ("{" struct-members)?
 * ```
 *
 * This function handles the parsing of a struct or union declaration. It processes optional
 * attributes, the tag name (if any), and the list of struct members.
 * It also handles struct redefinitions and tag lookups.
 *
 * Example:
 * ```
 * struct S { int a; }   // parses as a struct definition with tag "S"
 * ```
 * Parses as:
 * ```
 * struct S with members: int a;
 * ```
 *
 * @param rest A pointer to store the next token after parsing.
 * @param tok  The current token being processed.
 * @return A pointer to the parsed type representing the struct or union.
 */
static Type *struct_union_decl(Token **rest, Token *tok)
{
	Type *ty = struct_type();

	tok = attribute_list(tok, ty);

	// Read a tag.
	Token *tag = NULL;
	if (tok->kind == TK_IDENT) {
		tag = tok;
		tok = tok->next;
	}

	if (tag && !equal(tok, "{")) {
		*rest = tok;

		Type *ty2 = find_tag(tag);
		if (ty2)
			return ty2;

		ty->size = -1;
		push_tag_scope(tag, ty);
		return ty;
	}

	tok = skip(tok, "{");

	// Construct a struct object.
	struct_members(&tok, tok, ty);
	*rest = attribute_list(tok, ty);

	if (tag) {
		// If this is a redefinition, overwrite a previous type.
		// Otherwise, register the struct type.
		Type *ty2 = hashmap_get2(&scope->tags, tag->loc, tag->len);
		if (ty2) {
			*ty2 = *ty;
			return ty2;
		}

		push_tag_scope(tag, ty);
	}

	return ty;
}

/**
 * @brief Parses a struct declaration and assigns offsets to its members.
 *
 * Grammar:
 * ```
 * struct-decl = struct-union-decl
 * ```
 *
 * This function processes a struct declaration, parses its members, assigns offsets
 * to each member, and calculates the size of the struct. It takes into account any
 * bitfields, packing, and alignment requirements.
 *
 * Example:
 * ```
 * struct S { int a; char b; };
 * ```
 * Parses as:
 * ```
 * struct S with members: int a, char b.
 * ```
 *
 * @param rest A pointer to store the next token after parsing.
 * @param tok  The current token being processed.
 * @return A pointer to the parsed struct type.
 */
static Type *struct_decl(Token **rest, Token *tok)
{
	Type *ty = struct_union_decl(rest, tok);

	ty->kind = TY_STRUCT;

	if (ty->size < 0)
		return ty;

	// Assign offsets within the struct to members.
	int bits = 0;

	for (Member *mem = ty->members; mem; mem = mem->next) {
		if (mem->is_bitfield && mem->bit_width == 0) {
			// Zero-width anonymous bitfield has a special meaning.
			// It affects only alignment.
			bits = align_to(bits, mem->ty->size * 8);
		} else if (mem->is_bitfield) {
			int sz = mem->ty->size;
			if (bits / (sz * 8) != (bits + mem->bit_width - 1) / (sz * 8))
				bits = align_to(bits, sz * 8);

			mem->offset = align_down(bits / 8, sz);
			mem->bit_offset = bits % (sz * 8);
			bits += mem->bit_width;
		} else {
			if (!ty->is_packed)
				bits = align_to(bits, mem->align * 8);
			mem->offset = bits / 8;
			bits += mem->ty->size * 8;
		}

		if (!ty->is_packed && ty->align < mem->align)
			ty->align = mem->align;
	}

	ty->size = align_to(bits, ty->align * 8) / 8;
	return ty;
}

/**
 * @brief Parses a union declaration.
 *
 * Grammar:
 * ```
 * union-decl = struct-union-decl
 * ```
 *
 * This function processes a union declaration, parses its members, and calculates
 * the union's size and alignment. Unlike a struct, members of a union share the
 * same memory location, so we only need to track the largest member's size.
 *
 * Example:
 * ```
 * union U { int a; char b; };
 * ```
 * Parses as:
 * ```
 * union U with members: int a, char b.
 * ```
 *
 * @param rest A pointer to store the next token after parsing.
 * @param tok  The current token being processed.
 * @return A pointer to the parsed union type.
 */
static Type *union_decl(Token **rest, Token *tok)
{
	Type *ty = struct_union_decl(rest, tok);

	ty->kind = TY_UNION;

	if (ty->size < 0)
		return ty;

	// If union, we don't have to assign offsets because they
	// are already initialized to zero. We need to compute the
	// alignment and the size though.
	for (Member *mem = ty->members; mem; mem = mem->next) {
		if (ty->align < mem->align)
			ty->align = mem->align;
		if (ty->size < mem->ty->size)
			ty->size = mem->ty->size;
	}
	ty->size = align_to(ty->size, ty->align);
	return ty;
}

/**
 * @brief Finds a member of a struct by its name.
 *
 * This function searches through a struct's members and returns the member
 * that matches the provided name.
 *
 * @param ty  The type of the struct.
 * @param tok The token containing the member's name.
 * @return A pointer to the matching member, or NULL if not found.
 */
static Member *get_struct_member(Type *ty, Token *tok)
{
	for (Member *mem = ty->members; mem; mem = mem->next) {
		// Anonymous struct member
		if ((mem->ty->kind == TY_STRUCT || mem->ty->kind == TY_UNION) &&
		    !mem->name) {
			if (get_struct_member(mem->ty, tok))
				return mem;
			continue;
		}

		// Regular struct member
		if (mem->name->len == tok->len &&
		    !strncmp(mem->name->loc, tok->loc, tok->len))
			return mem;
	}
	return NULL;
}

/**
 * @brief Creates a node representing access to a struct member.
 *
 * This function handles member access in a struct, including anonymous structs.
 * It creates a node representing the struct member, such as `foo.bar`, where
 * `foo` is a struct and `bar` is a member name.
 *
 * Example:
 * ```
 * struct S { struct { int a; }; int b; } x;
 * x.a; // Access to the member "a" of the anonymous struct.
 * ```
 *
 * @param node The node representing the struct.
 * @param tok  The token containing the member name.
 * @return A node representing the struct member access.
 */
static Node *struct_ref(Node *node, Token *tok)
{
	add_type(node);
	if (node->ty->kind != TY_STRUCT && node->ty->kind != TY_UNION)
		error_tok(node->tok, "not a struct nor a union");

	Type *ty = node->ty;

	for (;;) {
		Member *mem = get_struct_member(ty, tok);
		if (!mem)
			error_tok(tok, "no such member");
		node = new_unary(ND_MEMBER, node, tok);
		node->member = mem;
		if (mem->name)
			break;
		ty = mem->ty;
	}
	return node;
}

/**
 * @brief Converts a post-increment or post-decrement operation to an expression.
 *
 * The function converts `A++` to `(typeof A)((A += 1) - 1)`.
 * This expression evaluates the current value of `A` and then increments
 * or decrements it.
 *
 * Example:
 * ```
 * A++ becomes (typeof A)((A += 1) - 1)
 * ```
 *
 * @param node The node representing the variable `A`.
 * @param tok  The token representing the `++` or `--` operator.
 * @param addend The value to increment or decrement by (1 for `++`, -1 for `--`).
 * @return A new node representing the increment or decrement operation.
 */
static Node *new_inc_dec(Node *node, Token *tok, int addend)
{
	add_type(node);
	return new_cast(new_add(to_assign(new_add(node, new_num(addend, tok), tok)),
				new_num(-addend, tok), tok),
			node->ty);
}

/**
 * @brief Parses a postfix expression.
 *
 * The function handles expressions such as array indexing (`x[y]`),
 * function calls (`f(x)`), member access (`x.y` or `x->y`), and post-increment or post-decrement (`x++`, `x--`).
 *
 * It supports compound literals, function calls, array indexing, member access,
 * and increment/decrement operators in postfix notation.
 *
 * Grammar:
 * ```
 * postfix = "(" type-name ")" "{" initializer-list "}"
 *         = ident "(" func-args ")" postfix-tail*
 *         | primary postfix-tail*
 * ```
 *
 * Example:
 * ```
 * foo();    // Function call
 * x[5];     // Array indexing
 * obj.foo;  // Member access
 * ptr->bar; // Member access through pointer
 * ```
 *
 * @param rest A pointer to store the next token after parsing.
 * @param tok  The current token being processed.
 * @return A node representing the parsed postfix expression.
 */
static Node *postfix(Token **rest, Token *tok)
{
	if (equal(tok, "(") && is_typename(tok->next)) {
		// Compound literal
		Token *start = tok;
		Type *ty = typename(&tok, tok->next);
		tok = skip(tok, ")");

		if (scope->next == NULL) {
			Obj *var = new_anon_gvar(ty);
			gvar_initializer(rest, tok, var);
			return new_var_node(var, start);
		}

		Obj *var = new_lvar("", ty);
		Node *lhs = lvar_initializer(rest, tok, var);
		Node *rhs = new_var_node(var, tok);
		return new_binary(ND_COMMA, lhs, rhs, start);
	}

	Node *node = primary(&tok, tok);

	for (;;) {
		if (equal(tok, "(")) {
			node = funcall(&tok, tok->next, node);
			continue;
		}

		if (equal(tok, "[")) {
			// x[y] is short for *(x+y)
			Token *start = tok;
			Node *idx = expr(&tok, tok->next);
			tok = skip(tok, "]");
			node = new_unary(ND_DEREF, new_add(node, idx, start), start);
			continue;
		}

		if (equal(tok, ".")) {
			node = struct_ref(node, tok->next);
			tok = tok->next->next;
			continue;
		}

		if (equal(tok, "->")) {
			// x->y is short for (*x).y
			node = new_unary(ND_DEREF, node, tok);
			node = struct_ref(node, tok->next);
			tok = tok->next->next;
			continue;
		}

		if (equal(tok, "++")) {
			node = new_inc_dec(node, tok, 1);
			tok = tok->next;
			continue;
		}

		if (equal(tok, "--")) {
			node = new_inc_dec(node, tok, -1);
			tok = tok->next;
			continue;
		}

		*rest = tok;
		return node;
	}
}

/**
 * @brief Parses a function call expression.
 *
 * The function parses a function call, ensuring that the number and type
 * of arguments match the function's parameter types. It also handles variadic
 * arguments, where argument types are promoted to `double` if omitted.
 *
 * Grammar:
 * ```
 * funcall = (assign ("," assign)*)? ")"
 * ```
 *
 * Example:
 * ```
 * foo(1, 2, 3);
 * ```
 *
 * @param rest A pointer to store the next token after parsing.
 * @param tok  The current token being processed.
 * @param fn   The node representing the function being called.
 * @return A node representing the function call expression.
 */
static Node *funcall(Token **rest, Token *tok, Node *fn)
{
	add_type(fn);

	if (fn->ty->kind != TY_FUNC &&
	    (fn->ty->kind != TY_PTR || fn->ty->base->kind != TY_FUNC))
		error_tok(fn->tok, "not a function");

	Type *ty = (fn->ty->kind == TY_FUNC) ? fn->ty : fn->ty->base;
	Type *param_ty = ty->params;

	Node head = {};
	Node *cur = &head;

	while (!equal(tok, ")")) {
		if (cur != &head)
			tok = skip(tok, ",");

		Node *arg = assign(&tok, tok);
		add_type(arg);

		if (!param_ty && !ty->is_variadic)
			error_tok(tok, "too many arguments");

		if (param_ty) {
			if (param_ty->kind != TY_STRUCT && param_ty->kind != TY_UNION)
				arg = new_cast(arg, param_ty);
			param_ty = param_ty->next;
		} else if (arg->ty->kind == TY_FLOAT) {
			// If parameter type is omitted (e.g. in "..."), float
			// arguments are promoted to double.
			arg = new_cast(arg, ty_double);
		}

		cur = cur->next = arg;
	}

	if (param_ty)
		error_tok(tok, "too few arguments");

	*rest = skip(tok, ")");

	Node *node = new_unary(ND_FUNCALL, fn, tok);
	node->func_ty = ty;
	node->ty = ty->return_ty;
	node->args = head.next;

	// If a function returns a struct, it is caller's responsibility
	// to allocate a space for the return value.
	if (node->ty->kind == TY_STRUCT || node->ty->kind == TY_UNION)
		node->ret_buffer = new_lvar("", node->ty);
	return node;
}

/**
 * @brief Parses a generic selection expression (`_Generic`).
 *
 * This function parses a `_Generic` expression, which is a type-generic
 * expression introduced in C11. It allows selecting different expressions
 * based on the type of a given control expression.
 *
 * Grammar:
 * ```
 * generic-selection = "(" assign "," generic-assoc ("," generic-assoc)* ")"
 *
 * generic-assoc = type-name ":" assign
 *               | "default" ":" assign
 * ```
 *
 * Example:
 * ```
 * _Generic(x, int: f1(x), float: f2(x), default: f3(x))
 * ```
 *
 * @param rest A pointer to store the next token after parsing.
 * @param tok  The current token being processed.
 * @return A node representing the selected expression.
 */
static Node *generic_selection(Token **rest, Token *tok)
{
	Token *start = tok;

	tok = skip(tok, "(");

	Node *ctrl = assign(&tok, tok);
	add_type(ctrl);

	Type *t1 = ctrl->ty;
	if (t1->kind == TY_FUNC)
		t1 = pointer_to(t1);
	else if (t1->kind == TY_ARRAY)
		t1 = pointer_to(t1->base);

	Node *ret = NULL;

	while (!consume(rest, tok, ")")) {
		tok = skip(tok, ",");

		if (equal(tok, "default")) {
			tok = skip(tok->next, ":");
			Node *node = assign(&tok, tok);
			if (!ret)
				ret = node;
			continue;
		}

		Type *t2 = typename(&tok, tok);
		tok = skip(tok, ":");
		Node *node = assign(&tok, tok);
		if (is_compatible(t1, t2))
			ret = node;
	}

	if (!ret)
		error_tok(start, "controlling expression type not compatible with"
			  " any generic association type");
	return ret;
}

/**
 * @brief Parses primary expressions, including literals, identifiers, and various operators.
 *
 * This function handles different types of primary expressions such as
 * parenthesized expressions, sizeof operator, type alignment, and
 * generic selection. It also handles built-in functions and expressions
 * like `__builtin_types_compatible_p`.
 *
 * Grammar:
 * ```
 * primary = "(" "{" stmt+ "}" ")"
 *         | "(" expr ")"
 *         | "sizeof" "(" type-name ")"
 *         | "sizeof" unary
 *         | "_Alignof" "(" type-name ")"
 *         | "_Alignof" unary
 *         | "_Generic" generic-selection
 *         | "__builtin_types_compatible_p" "(" type-name, type-name, ")"
 *         | "__builtin_reg_class" "(" type-name ")"
 *         | ident
 *         | str
 *         | num
 * ```
 *
 * Example:
 * ```
 * sizeof(int)         // Evaluates the size of an int
 * _Alignof(int)       // Evaluates the alignment of an int
 * _Generic(x, ...)    // Selects expression based on type of x
 * ```
 *
 * @param rest A pointer to store the next token after parsing.
 * @param tok  The current token being processed.
 * @return A node representing the primary expression.
 */
static Node *primary(Token **rest, Token *tok)
{
	Token *start = tok;

	if (equal(tok, "(") && equal(tok->next, "{")) {
		// This is a GNU statement expresssion.
		Node *node = new_node(ND_STMT_EXPR, tok);
		node->body = compound_stmt(&tok, tok->next->next)->body;
		*rest = skip(tok, ")");
		return node;
	}

	if (equal(tok, "(")) {
		Node *node = expr(&tok, tok->next);
		*rest = skip(tok, ")");
		return node;
	}

	if (equal(tok, "sizeof") && equal(tok->next, "(") && is_typename(tok->next->next)) {
		Type *ty = typename(&tok, tok->next->next);
		*rest = skip(tok, ")");

		if (ty->kind == TY_VLA) {
			if (ty->vla_size)
				return new_var_node(ty->vla_size, tok);

			Node *lhs = compute_vla_size(ty, tok);
			Node *rhs = new_var_node(ty->vla_size, tok);
			return new_binary(ND_COMMA, lhs, rhs, tok);
		}

		return new_ulong(ty->size, start);
	}

	if (equal(tok, "sizeof")) {
		Node *node = unary(rest, tok->next);
		add_type(node);
		if (node->ty->kind == TY_VLA)
			return new_var_node(node->ty->vla_size, tok);
		return new_ulong(node->ty->size, tok);
	}

	if (equal(tok, "_Alignof") && equal(tok->next, "(") && is_typename(tok->next->next)) {
		Type *ty = typename(&tok, tok->next->next);
		*rest = skip(tok, ")");
		return new_ulong(ty->align, tok);
	}

	if (equal(tok, "_Alignof")) {
		Node *node = unary(rest, tok->next);
		add_type(node);
		return new_ulong(node->ty->align, tok);
	}

	if (equal(tok, "_Generic"))
		return generic_selection(rest, tok->next);

	if (equal(tok, "__builtin_types_compatible_p")) {
		tok = skip(tok->next, "(");
		Type *t1 = typename(&tok, tok);
		tok = skip(tok, ",");
		Type *t2 = typename(&tok, tok);
		*rest = skip(tok, ")");
		return new_num(is_compatible(t1, t2), start);
	}

	if (equal(tok, "__builtin_reg_class")) {
		tok = skip(tok->next, "(");
		Type *ty = typename(&tok, tok);
		*rest = skip(tok, ")");

		if (is_integer(ty) || ty->kind == TY_PTR)
			return new_num(0, start);
		if (is_flonum(ty))
			return new_num(1, start);
		return new_num(2, start);
	}

	if (equal(tok, "__builtin_compare_and_swap")) {
		Node *node = new_node(ND_CAS, tok);
		tok = skip(tok->next, "(");
		node->cas_addr = assign(&tok, tok);
		tok = skip(tok, ",");
		node->cas_old = assign(&tok, tok);
		tok = skip(tok, ",");
		node->cas_new = assign(&tok, tok);
		*rest = skip(tok, ")");
		return node;
	}

	if (equal(tok, "__builtin_atomic_exchange")) {
		Node *node = new_node(ND_EXCH, tok);
		tok = skip(tok->next, "(");
		node->lhs = assign(&tok, tok);
		tok = skip(tok, ",");
		node->rhs = assign(&tok, tok);
		*rest = skip(tok, ")");
		return node;
	}

	if (tok->kind == TK_IDENT) {
		// Variable or enum constant
		VarScope *sc = find_var(tok);
		*rest = tok->next;

		// For "static inline" function
		if (sc && sc->var && sc->var->is_function) {
			if (current_fn)
				strarray_push(&current_fn->refs, sc->var->name);
			else
				sc->var->is_root = true;
		}

		if (sc) {
			if (sc->var)
				return new_var_node(sc->var, tok);
			if (sc->enum_ty)
				return new_num(sc->enum_val, tok);
		}

		if (equal(tok->next, "("))
			error_tok(tok, "implicit declaration of a function");
		error_tok(tok, "undefined variable");
	}

	if (tok->kind == TK_STR) {
		Obj *var = new_string_literal(tok->str, tok->ty);
		*rest = tok->next;
		return new_var_node(var, tok);
	}

	if (tok->kind == TK_NUM) {
		Node *node;
		if (is_flonum(tok->ty)) {
			node = new_node(ND_NUM, tok);
			node->fval = tok->fval;
		} else {
			node = new_num(tok->val, tok);
		}

		node->ty = tok->ty;
		*rest = tok->next;
		return node;
	}

	error_tok(tok, "expected an expression");
}

/**
 * @brief Parses a typedef declaration and adds it to the current scope.
 *
 * This function parses a `typedef` statement and adds the typedef type
 * to the current scope. The declaration may consist of multiple typedefs,
 * separated by commas.
 *
 * Grammar:
 * ```
 * typedef = "typedef" declarator ("," declarator)* ";"
 * ```
 *
 * @param tok    The current token being processed.
 * @param basety The base type to be used for the typedefs.
 * @return The next token after parsing.
 */
static Token *parse_typedef(Token *tok, Type *basety)
{
	bool first = true;

	while (!consume(&tok, tok, ";")) {
		if (!first)
			tok = skip(tok, ",");
		first = false;

		Type *ty = declarator(&tok, tok, basety);
		if (!ty->name)
			error_tok(ty->name_pos, "typedef name omitted");
		push_scope(get_ident(ty->name))->type_def = ty;
	}
	return tok;
}

/**
 * @brief Creates local variables for function parameters.
 *
 * This function creates local variables for each parameter in a function.
 * It checks if the parameter name is provided, and if not, it throws an
 * error. Each parameter is then added to the current scope.
 *
 * @param param The parameter type.
 */
static void create_param_lvars(Type *param)
{
	if (param) {
		create_param_lvars(param->next);
		if (!param->name)
			error_tok(param->name_pos, "parameter name omitted");
		new_lvar(get_ident(param->name), param);
	}
}

/**
 * @brief Resolves goto labels after parsing a function.
 *
 * This function matches `goto` statements with their corresponding labels.
 * Gotos are resolved after the function is fully parsed since they may refer
 * to labels that appear later in the function.
 */
static void resolve_goto_labels(void)
{
	for (Node *x = gotos; x; x = x->goto_next) {
		for (Node *y = labels; y; y = y->goto_next) {
			if (!strcmp(x->label, y->label)) {
				x->unique_label = y->unique_label;
				break;
			}
		}

		if (x->unique_label == NULL)
			error_tok(x->tok->next, "use of undeclared label");
	}

	gotos = labels = NULL;
}

/**
 * @brief Finds a function by its name in the current scope.
 *
 * This function searches for a function with the given name in the current
 * scope and returns the function object if found.
 *
 * @param name The name of the function to search for.
 * @return The function object if found, or NULL if not found.
 */
static Obj *find_func(char *name)
{
	Scope *sc = scope;

	while (sc->next)
		sc = sc->next;

	VarScope *sc2 = hashmap_get(&sc->vars, name);
	if (sc2 && sc2->var && sc2->var->is_function)
		return sc2->var;
	return NULL;
}

/**
 * @brief Marks a function as "live" and recursively marks its referenced functions as live.
 *
 * This function ensures that all functions that are referenced by the given function
 * (either directly or indirectly) are marked as live, meaning that they are required
 * for the program's execution.
 *
 * @param var The function (or variable) to mark as live.
 */
static void mark_live(Obj *var)
{
	if (!var->is_function || var->is_live)
		return;
	var->is_live = true;

	// Recursively mark referenced functions as live
	for (int i = 0; i < var->refs.len; i++) {
		Obj *fn = find_func(var->refs.data[i]);
		if (fn)
			mark_live(fn);
	}
}

/**
 * @brief Parses a function definition or declaration.
 *
 * This function parses a function declaration and definition, handling function
 * parameters, scope, and local variables. It also manages the function's static
 * and inline attributes.
 *
 * Grammar:
 * ```
 * function = declarator (attribute)* compound-stmt
 * ```
 *
 * @param tok    The current token being processed.
 * @param basety The base type for the function.
 * @param attr   Attributes for the function (static, inline, etc.).
 * @return The next token after parsing.
 */
static Token *function(Token *tok, Type *basety, VarAttr *attr)
{
	// Parse the function's return type and declarator (name)
	Type *ty = declarator(&tok, tok, basety);

	if (!ty->name)
		error_tok(ty->name_pos, "function name omitted");
	char *name_str = get_ident(ty->name);

	// Check if the function has already been declared
	Obj *fn = find_func(name_str);
	if (fn) {
		// Handle redeclaration and redefinition cases
		if (!fn->is_function)
			error_tok(tok, "redeclared as a different kind of symbol");
		if (fn->is_definition && equal(tok, "{"))
			error_tok(tok, "redefinition of %s", name_str);
		if (!fn->is_static && attr->is_static)
			error_tok(tok, "static declaration follows a non-static declaration");
		fn->is_definition = fn->is_definition || equal(tok, "{");
	} else {
		// New function definition
		fn = new_gvar(name_str, ty);
		fn->is_function = true;
		fn->is_definition = equal(tok, "{");
		fn->is_static = attr->is_static || (attr->is_inline && !attr->is_extern);
		fn->is_inline = attr->is_inline;
	}

	fn->is_root = !(fn->is_static && fn->is_inline);

	// If it's just a declaration, skip the rest
	if (consume(&tok, tok, ";"))
		return tok;

	// Set up the current function and local variables
	current_fn = fn;
	locals = NULL;
	enter_scope();
	create_param_lvars(ty->params);

	// Handle return type (for structs/unions that require a buffer)
	Type *rty = ty->return_ty;
	if ((rty->kind == TY_STRUCT || rty->kind == TY_UNION) && rty->size > 16)
		new_lvar("", pointer_to(rty));

	fn->params = locals;

	// Handle variadic functions
	if (ty->is_variadic)
		fn->va_area = new_lvar("__va_area__", array_of(ty_char, 136));
	fn->alloca_bottom = new_lvar("__alloca_size__", pointer_to(ty_char));

	// Parse the function body
	tok = skip(tok, "{");

	// Handle predefined string variables (__func__ and __FUNCTION__)
	push_scope("__func__")->var =
		new_string_literal(fn->name, array_of(ty_char, strlen(fn->name) + 1));

	push_scope("__FUNCTION__")->var =
		new_string_literal(fn->name, array_of(ty_char, strlen(fn->name) + 1));

	// Parse the function's compound statement
	fn->body = compound_stmt(&tok, tok);
	fn->locals = locals;
	leave_scope();

	// Resolve goto labels within the function
	resolve_goto_labels();

	return tok;
}

/**
 * @brief Parses a global variable declaration.
 *
 * This function parses global variable declarations and initializes them
 * if necessary. It handles attributes such as static, extern, and TLS.
 *
 * Grammar:
 * ```
 * global-variable = declarator (attribute)* ("=" expr)?
 * ```
 *
 * @param tok    The current token being processed.
 * @param basety The base type for the global variable.
 * @param attr   Attributes for the global variable (static, extern, etc.).
 * @return The next token after parsing.
 */
static Token *global_variable(Token *tok, Type *basety, VarAttr *attr)
{
	bool first = true;

	while (!consume(&tok, tok, ";")) {
		if (!first)
			tok = skip(tok, ",");
		first = false;

		// Parse the variable's type and declarator (name)
		Type *ty = declarator(&tok, tok, basety);
		if (!ty->name)
			error_tok(ty->name_pos, "variable name omitted");

		// Create the global variable
		Obj *var = new_gvar(get_ident(ty->name), ty);
		var->is_definition = !attr->is_extern;
		var->is_static = attr->is_static;
		var->is_tls = attr->is_tls;
		if (attr->align)
			var->align = attr->align;

		// If the variable has an initializer, process it
		if (equal(tok, "="))
			gvar_initializer(&tok, tok->next, var);
		else if (!attr->is_extern && !attr->is_tls)
			var->is_tentative = true;
	}
	return tok;
}

/**
 * @brief Determines if the given token marks the start of a function definition or declaration.
 *
 * This function checks if the token is the beginning of a function declaration or definition.
 * It does so by attempting to parse the token as a declarator and checking if the resulting type
 * is a function type.
 *
 * @param tok The token to check.
 * @return true if the token marks the start of a function definition or declaration, false otherwise.
 */
static bool is_function(Token *tok)
{
	if (equal(tok, ";"))
		return false;

	Type dummy = {};
	Type *ty = declarator(&tok, tok, &dummy);
	return ty->kind == TY_FUNC;
}

/**
 * @brief Scans global variables and removes redundant tentative definitions.
 *
 * A tentative definition is a declaration of a variable without an initializer. This function
 * ensures that tentative definitions are only kept if they do not have a redundant definition
 * elsewhere in the program.
 */
static void scan_globals(void)
{
	Obj head;
	Obj *cur = &head;

	// Iterate through all global variables
	for (Obj *var = globals; var; var = var->next) {
		// If the variable is not tentative, add it to the list
		if (!var->is_tentative) {
			cur = cur->next = var;
			continue;
		}

		// Look for another definition of the same identifier
		Obj *var2 = globals;
		for (; var2; var2 = var2->next)
			if (var != var2 && var2->is_definition && !strcmp(var->name, var2->name))
				break;

		// If there's no other definition, the tentative definition is valid
		if (!var2)
			cur = cur->next = var;
	}

	// Terminate the list of global variables
	cur->next = NULL;
	globals = head.next;
}

/**
 * @brief Declares built-in functions.
 *
 * This function declares built-in functions that are commonly used in many C programs,
 * such as the `alloca` function, which allocates memory dynamically.
 */
static void declare_builtin_functions(void)
{
	Type *ty = func_type(pointer_to(ty_void));

	ty->params = copy_type(ty_int);
	builtin_alloca = new_gvar("alloca", ty);
	builtin_alloca->is_definition = false;
}

/**
 * @brief Parses the program from a sequence of tokens.
 *
 * This function processes the tokens one by one and interprets them as either typedefs,
 * function definitions, or global variable declarations. It also handles marking global variables
 * that are used and removing redundant tentative definitions.
 *
 * Grammar:
 * ```
 * program = (typedef | function-definition | global-variable)*
 * ```
 *
 * @param tok The first token to parse.
 * @return The list of global variables after parsing.
 */
Obj *parse(Token *tok)
{
	declare_builtin_functions();
	globals = NULL;

	// Process each token until the end of file (EOF) is reached
	while (tok->kind != TK_EOF) {
		VarAttr attr = {};
		Type *basety = declspec(&tok, tok, &attr);

		// Handle typedef
		if (attr.is_typedef) {
			tok = parse_typedef(tok, basety);
			continue;
		}

		// Handle function definitions
		if (is_function(tok)) {
			tok = function(tok, basety, &attr);
			continue;
		}

		// Handle global variable declarations
		tok = global_variable(tok, basety, &attr);
	}

	// Mark live global variables
	for (Obj *var = globals; var; var = var->next)
		if (var->is_root)
			mark_live(var);

	// Remove redundant tentative definitions
	scan_globals();
	return globals;
}
/**
 * @brief Determines if the given token marks the start of a function definition or declaration.
 *
 * This function checks if the token is the beginning of a function declaration or definition.
 * It does so by attempting to parse the token as a declarator and checking if the resulting type
 * is a function type.
 *
 * @param tok The token to check.
 * @return true if the token marks the start of a function definition or declaration, false otherwise.
 */
static bool is_function(Token *tok)
{
	if (equal(tok, ";"))
		return false;

	Type dummy = {};
	Type *ty = declarator(&tok, tok, &dummy);
	return ty->kind == TY_FUNC;
}

/**
 * @brief Scans global variables and removes redundant tentative definitions.
 *
 * A tentative definition is a declaration of a variable without an initializer. This function
 * ensures that tentative definitions are only kept if they do not have a redundant definition
 * elsewhere in the program.
 */
static void scan_globals(void)
{
	Obj head;
	Obj *cur = &head;

	// Iterate through all global variables
	for (Obj *var = globals; var; var = var->next) {
		// If the variable is not tentative, add it to the list
		if (!var->is_tentative) {
			cur = cur->next = var;
			continue;
		}

		// Look for another definition of the same identifier
		Obj *var2 = globals;
		for (; var2; var2 = var2->next)
			if (var != var2 && var2->is_definition && !strcmp(var->name, var2->name))
				break;

		// If there's no other definition, the tentative definition is valid
		if (!var2)
			cur = cur->next = var;
	}

	// Terminate the list of global variables
	cur->next = NULL;
	globals = head.next;
}

/**
 * @brief Declares built-in functions.
 *
 * This function declares built-in functions that are commonly used in many C programs,
 * such as the `alloca` function, which allocates memory dynamically.
 */
static void declare_builtin_functions(void)
{
	Type *ty = func_type(pointer_to(ty_void));

	ty->params = copy_type(ty_int);
	builtin_alloca = new_gvar("alloca", ty);
	builtin_alloca->is_definition = false;
}

/**
 * @brief Parses the program from a sequence of tokens.
 *
 * This function processes the tokens one by one and interprets them as either typedefs,
 * function definitions, or global variable declarations. It also handles marking global variables
 * that are used and removing redundant tentative definitions.
 *
 * Grammar:
 * ```
 * program = (typedef | function-definition | global-variable)*
 * ```
 *
 * @param tok The first token to parse.
 * @return The list of global variables after parsing.
 */
Obj *parse(Token *tok)
{
	declare_builtin_functions();
	globals = NULL;

	// Process each token until the end of file (EOF) is reached
	while (tok->kind != TK_EOF) {
		VarAttr attr = {};
		Type *basety = declspec(&tok, tok, &attr);

		// Handle typedef
		if (attr.is_typedef) {
			tok = parse_typedef(tok, basety);
			continue;
		}

		// Handle function definitions
		if (is_function(tok)) {
			tok = function(tok, basety, &attr);
			continue;
		}

		// Handle global variable declarations
		tok = global_variable(tok, basety, &attr);
	}

	// Mark live global variables
	for (Obj *var = globals; var; var = var->next)
		if (var->is_root)
			mark_live(var);

	// Remove redundant tentative definitions
	scan_globals();
	return globals;
}
/**
 * @brief Determines if the given token marks the start of a function definition or declaration.
 *
 * This function checks if the token is the beginning of a function declaration or definition.
 * It does so by attempting to parse the token as a declarator and checking if the resulting type
 * is a function type.
 *
 * @param tok The token to check.
 * @return true if the token marks the start of a function definition or declaration, false otherwise.
 */
static bool is_function(Token *tok)
{
	if (equal(tok, ";"))
		return false;

	Type dummy = {};
	Type *ty = declarator(&tok, tok, &dummy);
	return ty->kind == TY_FUNC;
}

/**
 * @brief Scans global variables and removes redundant tentative definitions.
 *
 * A tentative definition is a declaration of a variable without an initializer. This function
 * ensures that tentative definitions are only kept if they do not have a redundant definition
 * elsewhere in the program.
 */
static void scan_globals(void)
{
	Obj head;
	Obj *cur = &head;

	// Iterate through all global variables
	for (Obj *var = globals; var; var = var->next) {
		// If the variable is not tentative, add it to the list
		if (!var->is_tentative) {
			cur = cur->next = var;
			continue;
		}

		// Look for another definition of the same identifier
		Obj *var2 = globals;
		for (; var2; var2 = var2->next)
			if (var != var2 && var2->is_definition && !strcmp(var->name, var2->name))
				break;

		// If there's no other definition, the tentative definition is valid
		if (!var2)
			cur = cur->next = var;
	}

	// Terminate the list of global variables
	cur->next = NULL;
	globals = head.next;
}

/**
 * @brief Declares built-in functions.
 *
 * This function declares built-in functions that are commonly used in many C programs,
 * such as the `alloca` function, which allocates memory dynamically.
 */
static void declare_builtin_functions(void)
{
	Type *ty = func_type(pointer_to(ty_void));

	ty->params = copy_type(ty_int);
	builtin_alloca = new_gvar("alloca", ty);
	builtin_alloca->is_definition = false;
}

/**
 * @brief Parses the program from a sequence of tokens.
 *
 * This function processes the tokens one by one and interprets them as either typedefs,
 * function definitions, or global variable declarations. It also handles marking global variables
 * that are used and removing redundant tentative definitions.
 *
 * Grammar:
 * ```
 * program = (typedef | function-definition | global-variable)*
 * ```
 *
 * @param tok The first token to parse.
 * @return The list of global variables after parsing.
 */
Obj *parse(Token *tok)
{
	declare_builtin_functions();
	globals = NULL;

	// Process each token until the end of file (EOF) is reached
	while (tok->kind != TK_EOF) {
		VarAttr attr = {};
		Type *basety = declspec(&tok, tok, &attr);

		// Handle typedef
		if (attr.is_typedef) {
			tok = parse_typedef(tok, basety);
			continue;
		}

		// Handle function definitions
		if (is_function(tok)) {
			tok = function(tok, basety, &attr);
			continue;
		}

		// Handle global variable declarations
		tok = global_variable(tok, basety, &attr);
	}

	// Mark live global variables
	for (Obj *var = globals; var; var = var->next)
		if (var->is_root)
			mark_live(var);

	// Remove redundant tentative definitions
	scan_globals();
	return globals;
}
