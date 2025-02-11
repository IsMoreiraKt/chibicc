#include "chibicc.h"

/**
 * @brief Global pointer to the void type.
 *
 * This is a pointer to a `Type` structure representing the void type.
 * It is initialized with a size of 1 byte and an alignment of 1 byte.
 *
 * @see `Type`
 */
Type *ty_void = &(Type){ TY_VOID, 1, 1 };

/**
 * @brief Global pointer to the boolean type.
 *
 * This is a pointer to a `Type` structure representing the boolean type.
 * It is initialized with a size of 1 byte and an alignment of 1 byte.
 *
 * @see `Type`
 */
Type *ty_bool = &(Type){ TY_BOOL, 1, 1 };

/**
 * @brief Global pointer to the char type.
 *
 * This is a pointer to a `Type` structure representing the character type.
 * It is initialized with a size of 1 byte and an alignment of 1 byte.
 *
 * @see `Type`
 */
Type *ty_char = &(Type){ TY_CHAR, 1, 1 };

/**
 * @brief Global pointer to the short type.
 *
 * This is a pointer to a `Type` structure representing the short integer type.
 * It is initialized with a size of 2 bytes and an alignment of 2 bytes.
 *
 * @see `Type`
 */
Type *ty_short = &(Type){ TY_SHORT, 2, 2 };

/**
 * @brief Global pointer to the integer type.
 *
 * This is a pointer to a `Type` structure representing the integer type.
 * It is initialized with a size of 4 bytes and an alignment of 4 bytes.
 *
 * @see `Type`
 */
Type *ty_int = &(Type){ TY_INT, 4, 4 };

/**
 * @brief Global pointer to the long integer type.
 *
 * This is a pointer to a `Type` structure representing the long integer type.
 * It is initialized with a size of 8 bytes and an alignment of 8 bytes.
 *
 * @see `Type`
 */
Type *ty_long = &(Type){ TY_LONG, 8, 8 };

/**
 * @brief Global pointer to the unsigned char type.
 *
 * This is a pointer to a `Type` structure representing the unsigned char type.
 * It is initialized with a size of 1 byte, an alignment of 1 byte, and is unsigned.
 *
 * @see `Type`
 */
Type *ty_uchar = &(Type){ TY_CHAR, 1, 1, true };

/**
 * @brief Global pointer to the unsigned short type.
 *
 * This is a pointer to a `Type` structure representing the unsigned short type.
 * It is initialized with a size of 2 bytes, an alignment of 2 bytes, and is unsigned.
 *
 * @see `Type`
 */
Type *ty_ushort = &(Type){ TY_SHORT, 2, 2, true };

/**
 * @brief Global pointer to the unsigned integer type.
 *
 * This is a pointer to a `Type` structure representing the unsigned integer type.
 * It is initialized with a size of 4 bytes, an alignment of 4 bytes, and is unsigned.
 *
 * @see `Type`
 */
Type *ty_uint = &(Type){ TY_INT, 4, 4, true };

/**
 * @brief Global pointer to the unsigned long type.
 *
 * This is a pointer to a `Type` structure representing the unsigned long type.
 * It is initialized with a size of 8 bytes, an alignment of 8 bytes, and is unsigned.
 *
 * @see `Type`
 */
Type *ty_ulong = &(Type){ TY_LONG, 8, 8, true };

/**
 * @brief Global pointer to the float type.
 *
 * This is a pointer to a `Type` structure representing the float type.
 * It is initialized with a size of 4 bytes and an alignment of 4 bytes.
 *
 * @see `Type`
 */
Type *ty_float = &(Type){ TY_FLOAT, 4, 4 };

/**
 * @brief Global pointer to the double type.
 *
 * This is a pointer to a `Type` structure representing the double type.
 * It is initialized with a size of 8 bytes and an alignment of 8 bytes.
 *
 * @see `Type`
 */
Type *ty_double = &(Type){ TY_DOUBLE, 8, 8 };

/**
 * @brief Global pointer to the long double type.
 *
 * This is a pointer to a `Type` structure representing the long double type.
 * It is initialized with a size of 16 bytes and an alignment of 16 bytes.
 *
 * @see `Type`
 */
Type *ty_ldouble = &(Type){ TY_LDOUBLE, 16, 16 };

/**
 * @brief Allocates and initializes a new type structure.
 *
 * This function allocates memory for a new `Type` structure and initializes it
 * with the provided type kind, size, and alignment values. It is typically used
 * to create custom types or to initialize predefined types like `int`, `char`, etc.
 *
 * @param kind The kind of type to create (e.g., `TY_INT`, `TY_CHAR`).
 * @param size The size (in bytes) of the type.
 * @param align The alignment (in bytes) of the type.
 *
 * @return A pointer to the newly created `Type` structure.
 *
 * @note The allocated `Type` structure should be freed when no longer needed
 *       to avoid memory leaks.
 *
 * @see `Type`, `TypeKind`
 */
static Type *new_type(TypeKind kind, int size, int align)
{
	Type *ty = calloc(1, sizeof(Type));

	ty->kind = kind;
	ty->size = size;
	ty->align = align;
	return ty;
}

/**
 * @brief Checks if the given type is an integer type.
 *
 * This function checks if the provided `Type` structure represents a type
 * that is considered an integer type. The integer types include boolean,
 * character, short, integer, long, and enumeration types.
 *
 * @param ty A pointer to a `Type` structure to check.
 *
 * @return `true` if the type is an integer type (bool, char, short, int, long, enum);
 *         `false` otherwise.
 *
 * @see `Type`, `TypeKind`
 */
bool is_integer(Type *ty)
{
	TypeKind k = ty->kind;

	return k == TY_BOOL || k == TY_CHAR || k == TY_SHORT ||
	       k == TY_INT || k == TY_LONG || k == TY_ENUM;
}

/**
 * @brief Checks if the given type is a floating-point number type.
 *
 * This function checks if the provided `Type` structure represents a type
 * that is a floating-point number. The floating-point types include
 * `float`, `double`, and `long double`.
 *
 * @param ty A pointer to a `Type` structure to check.
 *
 * @return `true` if the type is a floating-point number (`float`, `double`, `long double`);
 *         `false` otherwise.
 *
 * @see `Type`, `TypeKind`
 */
bool is_flonum(Type *ty)
{
	return ty->kind == TY_FLOAT || ty->kind == TY_DOUBLE ||
	       ty->kind == TY_LDOUBLE;
}

/**
 * @brief Checks if the given type is a numeric type (integer or floating-point).
 *
 * This function checks if the provided `Type` structure represents a numeric type.
 * Numeric types include both integer types (e.g., `bool`, `char`, `short`, `int`, `long`, `enum`)
 * and floating-point types (e.g., `float`, `double`, `long double`).
 *
 * @param ty A pointer to a `Type` structure to check.
 *
 * @return `true` if the type is either an integer or a floating-point number;
 *         `false` otherwise.
 *
 * @see `is_integer`, `is_flonum`, `Type`, `TypeKind`
 */
bool is_numeric(Type *ty)
{
	return is_integer(ty) || is_flonum(ty);
}

/**
 * @brief Checks if two types are compatible.
 *
 * This function compares two `Type` structures to determine if they are
 * compatible. Two types are considered compatible if they are of the same kind
 * and have compatible properties (e.g., same unsigned/signed attribute for integers,
 * matching base types for pointers, compatible return types and parameters for functions, etc.).
 *
 * The function also considers type inheritance via the `origin` field, where types may be
 * compatible with their base types.
 *
 * @param t1 A pointer to the first `Type` structure.
 * @param t2 A pointer to the second `Type` structure.
 *
 * @return `true` if the types are compatible, `false` otherwise.
 *
 * @note The function handles types like integers, floating-point numbers, pointers, functions,
 *       and arrays, and checks for compatibility based on their specific attributes.
 *
 * @see `Type`, `TypeKind`, `is_integer`, `is_flonum`, `is_compatible`
 */
bool is_compatible(Type *t1, Type *t2)
{
	if (t1 == t2)
		return true;

	if (t1->origin)
		return is_compatible(t1->origin, t2);

	if (t2->origin)
		return is_compatible(t1, t2->origin);

	if (t1->kind != t2->kind)
		return false;

	switch (t1->kind) {
	case TY_CHAR:
	case TY_SHORT:
	case TY_INT:
	case TY_LONG:
		return t1->is_unsigned == t2->is_unsigned;
	case TY_FLOAT:
	case TY_DOUBLE:
	case TY_LDOUBLE:
		return true;
	case TY_PTR:
		return is_compatible(t1->base, t2->base);
	case TY_FUNC: {
		if (!is_compatible(t1->return_ty, t2->return_ty))
			return false;
		if (t1->is_variadic != t2->is_variadic)
			return false;

		Type *p1 = t1->params;
		Type *p2 = t2->params;
		for (; p1 && p2; p1 = p1->next, p2 = p2->next)
			if (!is_compatible(p1, p2))
				return false;
		return p1 == NULL && p2 == NULL;
	}
	case TY_ARRAY:
		if (!is_compatible(t1->base, t2->base))
			return false;
		return t1->array_len < 0 && t2->array_len < 0 &&
		       t1->array_len == t2->array_len;
	}
	return false;
}

/**
 * @brief Creates a copy of a given type.
 *
 * This function allocates memory for a new `Type` structure and copies the contents
 * of the provided `Type` into the new structure. The `origin` field of the new type
 * is set to point to the original type, preserving the relationship between the types.
 *
 * @param ty A pointer to the `Type` structure to be copied.
 *
 * @return A pointer to the newly created `Type` structure that is a copy of the input type.
 *
 * @note This function performs a shallow copy of the `Type` structure. The `origin` field
 *       is directly copied, meaning that both the original and the new type will reference
 *       the same base type, if applicable.
 *
 * @see `Type`, `origin`
 */
Type *copy_type(Type *ty)
{
	Type *ret = calloc(1, sizeof(Type));

	*ret = *ty;
	ret->origin = ty;
	return ret;
}

/**
 * @brief Creates a pointer type from a base type.
 *
 * This function creates a new pointer type (`TY_PTR`) with the specified base type.
 * The new pointer type has a size and alignment of 8 bytes (typically for 64-bit systems)
 * and is marked as unsigned by default.
 *
 * @param base A pointer to the base `Type` that the pointer type will point to.
 *
 * @return A pointer to the newly created pointer `Type` that points to the specified base type.
 *
 * @note The returned pointer type has a fixed size and alignment of 8 bytes, which is
 *       typical for pointer types on 64-bit systems. The `is_unsigned` field is set to
 *       `true` by default.
 *
 * @see `Type`, `TY_PTR`, `base`, `is_unsigned`
 */
Type *pointer_to(Type *base)
{
	Type *ty = new_type(TY_PTR, 8, 8);

	ty->base = base;
	ty->is_unsigned = true;
	return ty;
}

/**
 * @brief Creates a function type.
 *
 * This function creates a new function type (`TY_FUNC`) with the specified return type.
 * The function type has a size and alignment of 1 byte, which is a workaround for the C
 * specification that disallows using `sizeof` on function types. However, GCC allows
 * this and evaluates the expression to 1.
 *
 * @param return_ty A pointer to the return `Type` of the function.
 *
 * @return A pointer to the newly created function `Type` with the specified return type.
 *
 * @note The returned function type has a fixed size and alignment of 1 byte,
 *       as specified by the C standard, but this is allowed in GCC. The `return_ty`
 *       field holds the return type of the function.
 *
 * @see `Type`, `TY_FUNC`, `return_ty`
 */
Type *func_type(Type *return_ty)
{
	Type *ty = new_type(TY_FUNC, 1, 1);

	ty->return_ty = return_ty;
	return ty;
}

/**
 * @brief Creates an array type.
 *
 * This function creates a new array type (`TY_ARRAY`) based on the given base type
 * and the length of the array. The size of the array is computed as the size of
 * the base type multiplied by the length of the array. The alignment is inherited
 * from the base type.
 *
 * @param base A pointer to the base `Type` of the array elements.
 * @param len The length of the array.
 *
 * @return A pointer to the newly created array `Type` with the specified base type
 *         and length.
 *
 * @note The `array_len` field holds the length of the array. The size of the array
 *       is calculated by multiplying the size of the base type by the length.
 *
 * @see `Type`, `TY_ARRAY`, `base`, `array_len`
 */
Type *array_of(Type *base, int len)
{
	Type *ty = new_type(TY_ARRAY, base->size * len, base->align);

	ty->base = base;
	ty->array_len = len;
	return ty;
}

/**
 * @brief Creates a variable-length array (VLA) type.
 *
 * This function creates a new variable-length array (VLA) type (`TY_VLA`) with a
 * base type and a length that is determined at runtime. The size and alignment are
 * set to 8, as the actual size of the array depends on the runtime value of the length.
 *
 * @param base A pointer to the base `Type` of the VLA elements.
 * @param len A pointer to a `Node` representing the length of the VLA. This length
 *            is evaluated at runtime.
 *
 * @return A pointer to the newly created VLA `Type` with the specified base type
 *         and length.
 *
 * @note The `vla_len` field holds a reference to the length node, which will be
 *       evaluated dynamically during execution. The size and alignment of the VLA
 *       are fixed to 8 bytes.
 *
 * @see `Type`, `TY_VLA`, `base`, `vla_len`
 */
Type *vla_of(Type *base, Node *len)
{
	Type *ty = new_type(TY_VLA, 8, 8);

	ty->base = base;
	ty->vla_len = len;
	return ty;
}

/**
 * @brief Creates a new enum type.
 *
 * This function creates a new type representing an enumeration (`TY_ENUM`).
 * The size and alignment for an enum are both set to 4 bytes, which is typical
 * for most architectures.
 *
 * @return A pointer to the newly created `Type` for the enum.
 *
 * @note The size and alignment of the enum are fixed at 4 bytes. This function
 *       assumes that the underlying representation of the enum is a 32-bit integer.
 *
 * @see `Type`, `TY_ENUM`
 */
Type *enum_type(void)
{
	return new_type(TY_ENUM, 4, 4);
}

/**
 * @brief Creates a new struct type.
 *
 * This function creates a new type representing a structure (`TY_STRUCT`).
 * Initially, the size of the struct is set to 0 and the alignment is set to 1.
 * These values can later be adjusted based on the actual contents of the struct.
 *
 * @return A pointer to the newly created `Type` for the struct.
 *
 * @note The size of the struct is initially set to 0 and the alignment to 1,
 *       and they should be updated when the actual members of the struct are defined.
 *
 * @see `Type`, `TY_STRUCT`
 */
Type *struct_type(void)
{
	return new_type(TY_STRUCT, 0, 1);
}

/**
 * @brief Determines the common type of two types for type compatibility.
 *
 * This function compares two types (`ty1` and `ty2`) and returns a type that can be used
 * for operations between the two types. The common type is determined based on the following rules:
 * - If one of the types is a pointer, the common type is a pointer to the base type.
 * - If one of the types is a function type, the common type is a pointer to the function.
 * - If one of the types is a long double, the common type is `TY_LDOUBLE`.
 * - If one of the types is a double, the common type is `TY_DOUBLE`.
 * - If one of the types is a float, the common type is `TY_FLOAT`.
 * - If the size of one of the types is less than 4 bytes, it is treated as `TY_INT`.
 * - If the sizes of both types are different, the function returns the type with the larger size.
 * - If both types are of the same size, the function returns the unsigned type, if any.
 *
 * @param ty1 The first type to compare.
 * @param ty2 The second type to compare.
 *
 * @return The common type that can be used for operations between `ty1` and `ty2`.
 *
 * @see `Type`, `pointer_to`, `ty_ldouble`, `ty_double`, `ty_float`, `ty_int`
 */
static Type *get_common_type(Type *ty1, Type *ty2)
{
	if (ty1->base)
		return pointer_to(ty1->base);

	if (ty1->kind == TY_FUNC)
		return pointer_to(ty1);
	if (ty2->kind == TY_FUNC)
		return pointer_to(ty2);

	if (ty1->kind == TY_LDOUBLE || ty2->kind == TY_LDOUBLE)
		return ty_ldouble;
	if (ty1->kind == TY_DOUBLE || ty2->kind == TY_DOUBLE)
		return ty_double;
	if (ty1->kind == TY_FLOAT || ty2->kind == TY_FLOAT)
		return ty_float;

	if (ty1->size < 4)
		ty1 = ty_int;
	if (ty2->size < 4)
		ty2 = ty_int;

	if (ty1->size != ty2->size)
		return (ty1->size < ty2->size) ? ty2 : ty1;

	if (ty2->is_unsigned)
		return ty2;
	return ty1;
}

/**
 * @brief Applies the usual arithmetic conversions to the operands.
 *
 * This function performs the usual arithmetic conversions on the left-hand side (`lhs`) and right-hand side (`rhs`) operands
 * of a binary operation. The usual arithmetic conversions are applied to ensure that both operands are of the same type,
 * based on the rules of C for integer and floating-point conversions.
 *
 * The function first determines the common type between the two operand types using `get_common_type`. Then, both the left-hand
 * side and right-hand side operands are cast to this common type using `new_cast`.
 *
 * The conversion ensures type compatibility and prevents type-related errors in expressions.
 *
 * @param lhs A pointer to the left-hand side operand of the binary operation.
 * @param rhs A pointer to the right-hand side operand of the binary operation.
 *
 * @see `get_common_type`, `new_cast`, `Type`
 */
static void usual_arith_conv(Node **lhs, Node **rhs)
{
	Type *ty = get_common_type((*lhs)->ty, (*rhs)->ty);

	*lhs = new_cast(*lhs, ty);
	*rhs = new_cast(*rhs, ty);
}

/**
 * @brief Assigns types to nodes in an abstract syntax tree (AST).
 *
 * This function recursively traverses the AST, starting from the given node and assigns types to each node based on its kind.
 * It ensures that each node has a valid type, either inferred from its operands or explicitly set according to the node's kind.
 * The function also handles type conversion where necessary, including arithmetic conversions, pointer dereferencing,
 * and function calls.
 *
 * The types are assigned based on the node's role in the program, ensuring that the type system is respected throughout the AST.
 * For example, arithmetic operations on integer and floating-point types are appropriately converted, and pointers are handled
 * according to the C specification.
 *
 * @param node The node to which types are being assigned.
 *
 * @note This function performs type assignments for a variety of node types, including arithmetic expressions, assignments,
 *       function calls, conditionals, and more.
 *
 * @see `get_common_type`, `usual_arith_conv`, `new_cast`, `Type`
 */
void add_type(Node *node)
{
	if (!node || node->ty)
		return;

	add_type(node->lhs);
	add_type(node->rhs);
	add_type(node->cond);
	add_type(node->then);
	add_type(node->els);
	add_type(node->init);
	add_type(node->inc);

	for (Node *n = node->body; n; n = n->next)
		add_type(n);
	for (Node *n = node->args; n; n = n->next)
		add_type(n);

	switch (node->kind) {
	case ND_NUM:
		node->ty = ty_int;
		return;
	case ND_ADD:
	case ND_SUB:
	case ND_MUL:
	case ND_DIV:
	case ND_MOD:
	case ND_BITAND:
	case ND_BITOR:
	case ND_BITXOR:
		usual_arith_conv(&node->lhs, &node->rhs);
		node->ty = node->lhs->ty;
		return;
	case ND_NEG: {
		Type *ty = get_common_type(ty_int, node->lhs->ty);
		node->lhs = new_cast(node->lhs, ty);
		node->ty = ty;
		return;
	}
	case ND_ASSIGN:
		if (node->lhs->ty->kind == TY_ARRAY)
			error_tok(node->lhs->tok, "not an lvalue");
		if (node->lhs->ty->kind != TY_STRUCT)
			node->rhs = new_cast(node->rhs, node->lhs->ty);
		node->ty = node->lhs->ty;
		return;
	case ND_EQ:
	case ND_NE:
	case ND_LT:
	case ND_LE:
		usual_arith_conv(&node->lhs, &node->rhs);
		node->ty = ty_int;
		return;
	case ND_FUNCALL:
		node->ty = node->func_ty->return_ty;
		return;
	case ND_NOT:
	case ND_LOGOR:
	case ND_LOGAND:
		node->ty = ty_int;
		return;
	case ND_BITNOT:
	case ND_SHL:
	case ND_SHR:
		node->ty = node->lhs->ty;
		return;
	case ND_VAR:
	case ND_VLA_PTR:
		node->ty = node->var->ty;
		return;
	case ND_COND:
		if (node->then->ty->kind == TY_VOID || node->els->ty->kind == TY_VOID) {
			node->ty = ty_void;
		} else {
			usual_arith_conv(&node->then, &node->els);
			node->ty = node->then->ty;
		}
		return;
	case ND_COMMA:
		node->ty = node->rhs->ty;
		return;
	case ND_MEMBER:
		node->ty = node->member->ty;
		return;
	case ND_ADDR: {
		Type *ty = node->lhs->ty;
		if (ty->kind == TY_ARRAY)
			node->ty = pointer_to(ty->base);
		else
			node->ty = pointer_to(ty);
		return;
	}
	case ND_DEREF:
		if (!node->lhs->ty->base)
			error_tok(node->tok, "invalid pointer dereference");
		if (node->lhs->ty->base->kind == TY_VOID)
			error_tok(node->tok, "dereferencing a void pointer");

		node->ty = node->lhs->ty->base;
		return;
	case ND_STMT_EXPR:
		if (node->body) {
			Node *stmt = node->body;
			while (stmt->next)
				stmt = stmt->next;
			if (stmt->kind == ND_EXPR_STMT) {
				node->ty = stmt->lhs->ty;
				return;
			}
		}
		error_tok(node->tok, "statement expression returning void is not supported");
		return;
	case ND_LABEL_VAL:
		node->ty = pointer_to(ty_void);
		return;
	case ND_CAS:
		add_type(node->cas_addr);
		add_type(node->cas_old);
		add_type(node->cas_new);
		node->ty = ty_bool;

		if (node->cas_addr->ty->kind != TY_PTR)
			error_tok(node->cas_addr->tok, "pointer expected");
		if (node->cas_old->ty->kind != TY_PTR)
			error_tok(node->cas_old->tok, "pointer expected");
		return;
	case ND_EXCH:
		if (node->lhs->ty->kind != TY_PTR)
			error_tok(node->cas_addr->tok, "pointer expected");
		node->ty = node->lhs->ty->base;
		return;
	}
}
