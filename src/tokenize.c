#include "chibicc.h"

/**
 * @brief A file structure representing a file in the compiler.
 */
static File *current_file;

/**
 * @brief A list of input files being processed by the compiler.
 */
static File **input_files;

/**
 * @brief Flag indicating whether the cursor is at the beginning of a line.
 *
 * This flag is used to track whether the parser is currently processing the beginning of a new line in the input
 * source code. It helps determine certain behaviors like handling line comments or preprocessing directives.
 */
static bool at_bol;

/**
 * @brief Flag indicating if there is space in the buffer or not.
 *
 * This flag is used to indicate whether the input buffer has space available to read more data or whether
 * the buffer is full. It may be used in various parts of the program, such as in the lexer or buffer management.
 */
static bool has_space;

/**
 * @brief Prints an error message and terminates the program.
 *
 * This function prints an error message to the standard error stream (`stderr`) using a format string and additional
 * arguments, similar to `printf`. After printing the message, it terminates the program by calling `exit(1)`, which
 * indicates that an error occurred.
 *
 * The `error` function is typically used to report fatal errors during program execution that prevent further
 * processing. It can handle variable argument lists using the `...` syntax, allowing for flexible error messages.
 *
 * @param fmt The format string for the error message.
 * @param ... Additional arguments to be formatted and included in the error message.
 *
 * @note This function does not return; it exits the program immediately after printing the error message.
 */
void error(char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	exit(1);
}

/**
 * @brief Displays a formatted error message, including the line of code where the error occurred.
 *
 * This function prints the line of code where the error was detected, with an indicator pointing to the error location,
 * followed by the formatted error message. It uses the source file and the line number to provide detailed information
 * to the user, making it easier to locate the problem in the code.
 *
 * @param filename The name of the source file where the error occurred.
 * @param input The full content of the input code (could be the original source code).
 * @param line_no The line number where the error occurred.
 * @param loc The specific location within the line where the error was detected.
 * @param fmt The format string used for the error message.
 * @param ap The argument list passed to the error formatting function.
 */
static void verror_at(char *filename, char *input, int line_no,
		      char *loc, char *fmt, va_list ap)
{
	// Find a line containing `loc`.
	char *line = loc;

	// Find the beginning of the line where the error occurred.
	while (input < line && line[-1] != '\n')
		line--;

	// Find the end of the line.
	char *end = loc;
	while (*end && *end != '\n')
		end++;

	// Print the line of code where the error was detected.
	int indent = fprintf(stderr, "%s:%d: ", filename, line_no);
	fprintf(stderr, "%.*s\n", (int)(end - line), line);

	// Display the error message.
	int pos = display_width(line, loc - line) + indent;

	// Print spaces to align the indicator "^".
	fprintf(stderr, "%*s", pos, ""); // print pos spaces.
	fprintf(stderr, "^ ");
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
}

/**
 * @brief Prints an error message along with the line number and location of the error.
 *
 * This function calculates the line number where the error occurred based on the location in the file,
 * then calls `verror_at` to display the error with the line of code, an indicator, and the error message.
 * It then terminates the program with an exit status of 1.
 *
 * @param loc The location within the file where the error occurred.
 * @param fmt The format string for the error message.
 * @param ... The variable arguments required by the format string.
 */
void error_at(char *loc, char *fmt, ...)
{
	// Calculate the line number based on the location of the error.
	int line_no = 1;

	for (char *p = current_file->contents; p < loc; p++)
		if (*p == '\n')
			line_no++;

	// Start processing the variable arguments and call verror_at to display the error.
	va_list ap;
	va_start(ap, fmt);
	verror_at(current_file->name, current_file->contents, line_no, loc, fmt, ap);
	exit(1);
}

/**
 * @brief Prints an error message related to a specific token and terminates the program.
 *
 * This function is used to report errors that occur at a specific token in the source code. It takes the
 * token's location, line number, and associated file, then uses `verror_at` to display the error with
 * the line of code, an indicator, and the error message. The program is then terminated with an exit status of 1.
 *
 * @param tok The token at which the error occurred.
 * @param fmt The format string for the error message.
 * @param ... The variable arguments required by the format string.
 */
void error_tok(Token *tok, char *fmt, ...)
{
	// Start processing the variable arguments and call verror_at to display the error.
	va_list ap;

	va_start(ap, fmt);
	verror_at(tok->file->name, tok->file->contents, tok->line_no, tok->loc, fmt, ap);
	exit(1);
}

/**
 * @brief Prints a warning message related to a specific token.
 *
 * This function is used to issue a warning related to a specific token in the source code. It takes the
 * token's location, line number, and associated file, then uses `verror_at` to display the warning with
 * the line of code, an indicator, and the warning message. Unlike errors, the program is not terminated.
 *
 * @param tok The token at which the warning occurred.
 * @param fmt The format string for the warning message.
 * @param ... The variable arguments required by the format string.
 */
void warn_tok(Token *tok, char *fmt, ...)
{
	// Start processing the variable arguments and call verror_at to display the warning.
	va_list ap;

	va_start(ap, fmt);
	verror_at(tok->file->name, tok->file->contents, tok->line_no, tok->loc, fmt, ap);
	va_end(ap);
}

/**
 * @brief Compares a token's string with a given string.
 *
 * This function checks if the string represented by the token is equal to the given string `op`.
 * It compares the characters in the token with `op` and ensures that the entire token string matches
 * the provided string exactly, including the null terminator.
 *
 * @param tok The token whose string is to be compared.
 * @param op The string to compare against the token's string.
 *
 * @return True if the token's string matches the given string `op`, false otherwise.
 */
bool equal(Token *tok, char *op)
{
	return memcmp(tok->loc, op, tok->len) == 0 && op[tok->len] == '\0';
}

/**
 * @brief Skips the current token if it matches the expected string.
 *
 * This function checks if the current token matches the expected string `op`.
 * If the token matches, it returns the next token in the sequence.
 * If the token does not match, it reports an error with a message indicating
 * the expected string, using the `error_tok` function.
 *
 * @param tok The current token to be checked and skipped.
 * @param op The expected string to compare with the current token's value.
 *
 * @return The next token in the sequence if the current token matches the expected string.
 *
 * @throws error_tok If the current token does not match the expected string.
 */
Token *skip(Token *tok, char *op)
{
	if (!equal(tok, op))
		error_tok(tok, "expected '%s'", op);
	return tok->next;
}

/**
 * @brief Consumes the current token if it matches the expected string.
 *
 * This function checks if the current token matches the given string `str`.
 * If the token matches, it "consumes" the token by updating `rest` to point
 * to the next token and returns `true`. If the token does not match,
 * it leaves `rest` pointing to the current token and returns `false`.
 *
 * @param rest A pointer to a pointer to the current token, which will be updated
 *             to point to the next token if the current token matches the expected string.
 * @param tok The current token to be checked and potentially consumed.
 * @param str The string to compare with the current token's value.
 *
 * @return `true` if the current token matches the expected string,
 *         otherwise `false`.
 */
bool consume(Token **rest, Token *tok, char *str)
{
	if (equal(tok, str)) {
		*rest = tok->next;
		return true;
	}
	*rest = tok;
	return false;
}

/**
 * @brief Creates a new token.
 *
 * This function allocates memory for a new token and initializes its fields
 * based on the provided information. The token represents a specific piece
 * of source code, with a kind, location, length, and associated file information.
 *
 * @param kind The kind of the token, indicating its type (e.g., keyword, identifier, operator).
 * @param start A pointer to the beginning of the token in the source code.
 * @param end A pointer to the end of the token in the source code.
 *
 * @return A pointer to the newly created token.
 */
static Token *new_token(TokenKind kind, char *start, char *end)
{
	Token *tok = calloc(1, sizeof(Token));

	tok->kind = kind;
	tok->loc = start;
	tok->len = end - start;
	tok->file = current_file;
	tok->filename = current_file->display_name;
	tok->at_bol = at_bol;
	tok->has_space = has_space;

	at_bol = has_space = false;
	return tok;
}

/**
 * @brief Checks if a string starts with a given prefix.
 *
 * This function compares the beginning of a string with a specified prefix and
 * returns true if the string starts with the given prefix.
 *
 * @param p The string to be checked.
 * @param q The prefix to be matched against the start of the string.
 *
 * @return `true` if the string `p` starts with the prefix `q`, `false` otherwise.
 */
static bool startswith(char *p, char *q)
{
	return strncmp(p, q, strlen(q)) == 0;
}

/**
 * @brief Reads an identifier from the input string.
 *
 * This function reads a sequence of characters that form a valid identifier
 * according to the language's rules. It starts by checking if the first character
 * is valid for an identifier, then continues to read additional characters until
 * it encounters an invalid one.
 *
 * @param start A pointer to the start of the string to be processed.
 *
 * @return The length of the identifier (in bytes) if a valid identifier is found.
 *         Returns `0` if the first character is not a valid identifier start.
 */
static int read_ident(char *start)
{
	char *p = start;
	uint32_t c = decode_utf8(&p, p);

	// Check if the first character is valid for an identifier
	if (!is_ident1(c))
		return 0;

	// Continuously read the next characters if they are valid for an identifier
	for (;;) {
		char *q;
		c = decode_utf8(&q, p);
		if (!is_ident2(c)) // If we encounter an invalid character, stop
			return p - start;
		p = q;
	}
}

/**
 * @brief Converts a hexadecimal character to its integer value.
 *
 * This function takes a single character representing a hexadecimal digit
 * (either lowercase or uppercase) and returns its corresponding integer value.
 *
 * The function accepts characters '0' to '9' and 'a' to 'f' or 'A' to 'F'.
 * For digits '0' to '9', the function returns the value 0 to 9. For characters
 * 'a' to 'f' or 'A' to 'F', the function returns the value 10 to 15 respectively.
 *
 * @param c The hexadecimal character to convert. It can be one of '0' to '9',
 *          'a' to 'f', or 'A' to 'F'.
 *
 * @return The integer value corresponding to the hexadecimal character.
 *         If the character is not a valid hexadecimal digit, the function
 *         returns the value based on the case conversion logic (e.g., 'a' -> 10).
 */
static int from_hex(char c)
{
	if ('0' <= c && c <= '9')
		return c - '0';
	if ('a' <= c && c <= 'f')
		return c - 'a' + 10;
	return c - 'A' + 10;
}

/**
 * @brief Reads a punctuation or keyword from a given string.
 *
 * This function checks the input string `p` for a match against a predefined list
 * of punctuation operators and keywords commonly used in C and similar languages.
 * It returns the length of the matched operator or keyword.
 *
 * The function first checks for multi-character operators and keywords (e.g., "==", "++", "<<=", etc.).
 * If no match is found, it checks whether the character is a single punctuation mark.
 * If it is, the function returns a length of 1.
 *
 * @param p The input string to check for a keyword or punctuation.
 *
 * @return The length of the matched operator or keyword:
 *         - A positive integer corresponding to the length of the matched operator/keyword.
 *         - 0 if no match is found and the character is not a punctuation mark.
 */
static int read_punct(char *p)
{
	static char *kw[] = {
		"<<=", ">>=", "...", "==", "!=", "<=", ">=", "->", "+=",
		"-=",  "*=",  "/=",  "++", "--", "%=", "&=", "|=", "^=","&&",
		"||",  "<<",  ">>",  "##",
	};

	for (int i = 0; i < sizeof(kw) / sizeof(*kw); i++)
		if (startswith(p, kw[i]))
			return strlen(kw[i]);

	return ispunct(*p) ? 1 : 0;
}

/**
 * @brief Checks if a given token is a keyword.
 *
 * This function determines if the given token `tok` is a keyword in the C language.
 * It uses a hash map to store and quickly look up the list of predefined keywords.
 *
 * The function first initializes the hash map (if it has not been initialized already),
 * and populates it with a list of C keywords. Then it checks if the token's location
 * and length match any of the stored keywords. If a match is found, the function returns `true`;
 * otherwise, it returns `false`.
 *
 * @param tok The token to check.
 *
 * @return `true` if the token is a keyword, `false` otherwise.
 */
static bool is_keyword(Token *tok)
{
	static HashMap map;

	if (map.capacity == 0) {
		static char *kw[] = {
			"return",	 "if",		 "else",	  "for",      "while",	  "int",      "sizeof", "char",
			"struct",	 "union",	 "short",	  "long",     "void",	  "typedef",  "_Bool",
			"enum",		 "static",	 "goto",	  "break",    "continue", "switch",   "case",
			"default",	 "extern",	 "_Alignof",	  "_Alignas", "do",	  "signed",
			"unsigned",	 "const",	 "volatile",	  "auto",     "register", "restrict",
			"__restrict",	 "__restrict__", "_Noreturn",	  "float",    "double",
			"typeof",	 "asm",		 "_Thread_local", "__thread", "_Atomic",
			"__attribute__",
		};

		for (int i = 0; i < sizeof(kw) / sizeof(*kw); i++)
			hashmap_put(&map, kw[i], (void *)1);
	}

	return hashmap_get2(&map, tok->loc, tok->len);
}

/**
 * @brief Reads an escaped character sequence.
 *
 * This function parses a character escape sequence from the given string and returns
 * the corresponding character value. It handles octal, hexadecimal, and standard escape
 * sequences such as `\n`, `\t`, and others.
 *
 * The function works by reading the character at the provided position and determining
 * if it's part of an octal or hexadecimal escape sequence. If it's neither, the function
 * checks for common escape sequences (such as `\n` or `\t`). It returns the corresponding
 * character value for the escape sequence and updates the position pointer (`new_pos`) to
 * the next character after the escape sequence.
 *
 * @param new_pos A pointer to the position variable, which will be updated to point to
 * the next character after the escape sequence.
 * @param p The current position in the input string to start reading the escape sequence.
 *
 * @return The character corresponding to the escape sequence.
 *
 * @note The function supports octal escape sequences (e.g., `\123`), hexadecimal sequences
 * (e.g., `\x7F`), and standard escape sequences (e.g., `\n` for newline).
 */
static int read_escaped_char(char **new_pos, char *p)
{
	if ('0' <= *p && *p <= '7') {
		// Read an octal number.
		int c = *p++ - '0';
		if ('0' <= *p && *p <= '7') {
			c = (c << 3) + (*p++ - '0');
			if ('0' <= *p && *p <= '7')
				c = (c << 3) + (*p++ - '0');
		}
		*new_pos = p;
		return c;
	}

	if (*p == 'x') {
		// Read a hexadecimal number.
		p++;
		if (!isxdigit(*p))
			error_at(p, "invalid hex escape sequence");

		int c = 0;
		for (; isxdigit(*p); p++)
			c = (c << 4) + from_hex(*p);
		*new_pos = p;
		return c;
	}

	*new_pos = p + 1;

	// Escape sequences are defined using themselves here. E.g.
	// '\n' is implemented using '\n'. This tautological definition
	// works because the compiler that compiles our compiler knows
	// what '\n' actually is. In other words, we "inherit" the ASCII
	// code of '\n' from the compiler that compiles our compiler,
	// so we don't have to teach the actual code here.
	//
	// This fact has huge implications not only for the correctness
	// of the compiler but also for the security of the generated code.
	// For more info, read "Reflections on Trusting Trust" by Ken Thompson.
	// https://github.com/rui314/chibicc/wiki/thompson1984.pdf
	switch (*p) {
	case 'a': return '\a';
	case 'b': return '\b';
	case 't': return '\t';
	case 'n': return '\n';
	case 'v': return '\v';
	case 'f': return '\f';
	case 'r': return '\r';
	// [GNU] \e for the ASCII escape character is a GNU C extension.
	case 'e': return 27;
	default: return *p;
	}
}

/**
 * @brief Finds the end of a string literal.
 *
 * This function scans a string literal starting from the given position and returns
 * the position of the closing quotation mark (`"`) that terminates the string literal.
 * It also checks for escaped characters within the string and ensures the string is properly
 * closed. If the string literal is not properly terminated (e.g., missing closing `"`),
 * an error is raised.
 *
 * The function handles escape sequences (e.g., `\"`, `\\`, etc.) inside the string, ensuring
 * that escaped quotes do not prematurely terminate the string.
 *
 * @param p The current position in the input string, which should be the opening quotation mark (`"`).
 *
 * @return A pointer to the character after the closing quotation mark (`"`), marking the end of the string literal.
 *
 * @throws error_at If the string literal is unclosed (e.g., missing closing quotation mark).
 */
static char *string_literal_end(char *p)
{
	char *start = p;

	for (; *p != '"'; p++) {
		if (*p == '\n' || *p == '\0')
			error_at(start, "unclosed string literal");
		if (*p == '\\')
			p++;
	}
	return p;
}

/**
 * @brief Reads a string literal from the input and returns a token representing the string.
 *
 * This function processes a string literal, starting from the opening quotation mark (`"`),
 * and returns a `Token` that contains the string literal's value. The string literal is stored
 * in a dynamically allocated buffer, and escape sequences within the string (e.g., `\n`, `\\`)
 * are properly processed and converted.
 *
 * The function uses the `string_literal_end` function to find the closing quotation mark (`"`)
 * of the string literal and processes the characters between the quotes, handling escape sequences
 * as needed.
 *
 * @param start The starting position of the string literal, which should be the opening quotation mark (`"`).
 * @param quote A pointer to the opening quotation mark (`"`), marking the beginning of the string literal.
 *
 * @return A `Token` representing the string literal, containing:
 *         - The type `TK_STR` to indicate it is a string literal token.
 *         - A pointer to the string value (`buf`).
 *         - The type `ty_char` and length of the string for type information.
 *
 * @throws error_at If the string literal is unclosed (e.g., missing closing quotation mark).
 */
static Token *read_string_literal(char *start, char *quote)
{
	char *end = string_literal_end(quote + 1);
	char *buf = calloc(1, end - quote);
	int len = 0;

	for (char *p = quote + 1; p < end;) {
		if (*p == '\\')
			buf[len++] = read_escaped_char(&p, p + 1);
		else
			buf[len++] = *p++;
	}

	Token *tok = new_token(TK_STR, start, end + 1);
	tok->ty = array_of(ty_char, len + 1);
	tok->str = buf;
	return tok;
}

/**
 * @brief Reads a UTF-16 encoded string literal from the input and returns a token representing the string.
 *
 * This function processes a UTF-16 string literal, starting from the opening quotation mark (`"`), and returns
 * a `Token` that contains the string literal's value in UTF-16 encoding. The function handles escape sequences
 * within the string (e.g., `\n`, `\\`) and properly encodes characters as UTF-16. It supports code points beyond
 * the Basic Multilingual Plane (BMP) by encoding them as surrogate pairs.
 *
 * The function uses the `string_literal_end` function to find the closing quotation mark (`"`) and processes
 * the characters between the quotes. Each character is decoded from UTF-8 and converted into UTF-16, handling
 * surrogate pairs for code points beyond the BMP.
 *
 * @param start The starting position of the string literal, which should be the opening quotation mark (`"`).
 * @param quote A pointer to the opening quotation mark (`"`), marking the beginning of the string literal.
 *
 * @return A `Token` representing the UTF-16 string literal, containing:
 *         - The type `TK_STR` to indicate it is a string literal token.
 *         - A pointer to the UTF-16 encoded string value (`buf`).
 *         - The type `ty_ushort` for UTF-16 characters and the length of the string for type information.
 *
 * @throws error_at If the string literal is unclosed (e.g., missing closing quotation mark).
 */
static Token *read_utf16_string_literal(char *start, char *quote)
{
	char *end = string_literal_end(quote + 1);
	uint16_t *buf = calloc(2, end - start);
	int len = 0;

	for (char *p = quote + 1; p < end;) {
		if (*p == '\\') {
			buf[len++] = read_escaped_char(&p, p + 1);
			continue;
		}

		uint32_t c = decode_utf8(&p, p);
		if (c < 0x10000) {
			// Encode a code point in 2 bytes.
			buf[len++] = c;
		} else {
			// Encode a code point in 4 bytes.
			c -= 0x10000;
			buf[len++] = 0xd800 + ((c >> 10) & 0x3ff);
			buf[len++] = 0xdc00 + (c & 0x3ff);
		}
	}

	Token *tok = new_token(TK_STR, start, end + 1);
	tok->ty = array_of(ty_ushort, len + 1);
	tok->str = (char *)buf;
	return tok;
}

/**
 * @brief Reads a UTF-32 encoded string literal from the input and returns a token representing the string.
 *
 * This function processes a UTF-32 string literal, starting from the opening quotation mark (`"`), and returns
 * a `Token` that contains the string literal's value in UTF-32 encoding. The function handles escape sequences
 * within the string (e.g., `\n`, `\\`) and properly decodes characters from UTF-8 to UTF-32.
 *
 * The function uses the `string_literal_end` function to find the closing quotation mark (`"`) and processes
 * the characters between the quotes. Each character is decoded from UTF-8 and converted into UTF-32.
 *
 * @param start The starting position of the string literal, which should be the opening quotation mark (`"`).
 * @param quote A pointer to the opening quotation mark (`"`), marking the beginning of the string literal.
 * @param ty The type of the elements in the resulting array (usually `ty_uint32` for UTF-32 characters).
 *
 * @return A `Token` representing the UTF-32 string literal, containing:
 *         - The type `TK_STR` to indicate it is a string literal token.
 *         - A pointer to the UTF-32 encoded string value (`buf`).
 *         - The type `ty` for UTF-32 characters and the length of the string for type information.
 *
 * @throws error_at If the string literal is unclosed (e.g., missing closing quotation mark).
 */
static Token *read_utf32_string_literal(char *start, char *quote, Type *ty)
{
	char *end = string_literal_end(quote + 1);
	uint32_t *buf = calloc(4, end - quote);
	int len = 0;

	for (char *p = quote + 1; p < end;) {
		if (*p == '\\')
			buf[len++] = read_escaped_char(&p, p + 1);
		else
			buf[len++] = decode_utf8(&p, p);
	}

	Token *tok = new_token(TK_STR, start, end + 1);
	tok->ty = array_of(ty, len + 1);
	tok->str = (char *)buf;
	return tok;
}

/**
 * @brief Reads a character literal from the input and returns a token representing the character.
 *
 * This function processes a character literal, starting from the opening single quotation mark (`'`), and returns
 * a `Token` that contains the character's value. The function handles escape sequences (e.g., `\'`, `\\`, `\n`)
 * and decodes characters from UTF-8 to a single code point.
 *
 * The function checks for an unclosed character literal (i.e., missing the closing `'`), processes any escape sequences,
 * and stores the character in the token's value.
 *
 * @param start The starting position of the character literal, which should be the opening single quotation mark (`'`).
 * @param quote A pointer to the opening single quotation mark (`'`), marking the beginning of the character literal.
 * @param ty The type of the character (usually `ty_int`).
 *
 * @return A `Token` representing the character literal, containing:
 *         - The type `TK_NUM` to indicate it is a numeric token.
 *         - The decoded character value (`c`) stored in the token's `val`.
 *         - The type `ty` for the character.
 *
 * @throws error_at If the character literal is unclosed (i.e., missing the closing `'`).
 */
static Token *read_char_literal(char *start, char *quote, Type *ty)
{
	char *p = quote + 1;

	if (*p == '\0')
		error_at(start, "unclosed char literal");

	int c;
	if (*p == '\\')
		c = read_escaped_char(&p, p + 1);
	else
		c = decode_utf8(&p, p);

	char *end = strchr(p, '\'');
	if (!end)
		error_at(p, "unclosed char literal");

	Token *tok = new_token(TK_NUM, start, end + 1);
	tok->val = c;
	tok->ty = ty;
	return tok;
}

/**
 * @brief Converts a preprocessor integer token to a numeric token with an inferred type.
 *
 * This function takes a token representing a preprocessor integer literal (e.g., `0x1F`, `0b101`, `123`, etc.)
 * and converts it to a numeric token (`TK_NUM`). The function also infers the correct type based on the integer
 * value and any suffixes (such as `U`, `L`, or `LL` for unsigned or long integers).
 *
 * The supported integer bases are:
 * - Hexadecimal (`0x` or `0X`)
 * - Binary (`0b` or `0B`)
 * - Octal (starting with `0`)
 * - Decimal (no prefix)
 *
 * The function also handles the following suffixes:
 * - `U` or `u`: Unsigned
 * - `L` or `l`: Long
 * - `LL` or `ll`: Long long
 * - `LLU`, `ULL`, `llu`, `ull`, etc.: Long long unsigned
 *
 * The function updates the token to represent the correct numeric value and type.
 *
 * @param tok The token representing the preprocessor integer literal.
 *
 * @return `true` if the conversion is successful, otherwise `false`.
 *
 * @note The function modifies the token to include the inferred type (`ty_int`, `ty_long`, `ty_ulong`, etc.) and
 *       the parsed numeric value (`val`).
 */
static bool convert_pp_int(Token *tok)
{
	char *p = tok->loc;

	// Read a binary, octal, decimal or hexadecimal number.
	int base = 10;

	if (!strncasecmp(p, "0x", 2) && isxdigit(p[2])) {
		p += 2;
		base = 16;
	} else if (!strncasecmp(p, "0b", 2) && (p[2] == '0' || p[2] == '1')) {
		p += 2;
		base = 2;
	} else if (*p == '0') {
		base = 8;
	}

	int64_t val = strtoul(p, &p, base);

	// Read U, L or LL suffixes.
	bool l = false;
	bool u = false;

	if (startswith(p, "LLU") || startswith(p, "LLu") ||
	    startswith(p, "llU") || startswith(p, "llu") ||
	    startswith(p, "ULL") || startswith(p, "Ull") ||
	    startswith(p, "uLL") || startswith(p, "ull")) {
		p += 3;
		l = u = true;
	} else if (!strncasecmp(p, "lu", 2) || !strncasecmp(p, "ul", 2)) {
		p += 2;
		l = u = true;
	} else if (startswith(p, "LL") || startswith(p, "ll")) {
		p += 2;
		l = true;
	} else if (*p == 'L' || *p == 'l') {
		p++;
		l = true;
	} else if (*p == 'U' || *p == 'u') {
		p++;
		u = true;
	}

	if (p != tok->loc + tok->len)
		return false;

	// Infer a type.
	Type *ty;
	if (base == 10) {
		if (l && u)
			ty = ty_ulong;
		else if (l)
			ty = ty_long;
		else if (u)
			ty = (val >> 32) ? ty_ulong : ty_uint;
		else
			ty = (val >> 31) ? ty_long : ty_int;
	} else {
		if (l && u)
			ty = ty_ulong;
		else if (l)
			ty = (val >> 63) ? ty_ulong : ty_long;
		else if (u)
			ty = (val >> 32) ? ty_ulong : ty_uint;
		else if (val >> 63)
			ty = ty_ulong;
		else if (val >> 32)
			ty = ty_long;
		else if (val >> 31)
			ty = ty_uint;
		else
			ty = ty_int;
	}

	tok->kind = TK_NUM;
	tok->val = val;
	tok->ty = ty;
	return true;
}

/**
 * @brief Converts a preprocessor number token to either an integer or floating-point constant.
 *
 * This function attempts to convert a given preprocessor number token into either an integer constant
 * or a floating-point constant. The function first tries to parse the token as an integer constant using
 * `convert_pp_int()`. If that fails, it parses the token as a floating-point constant. It then updates the
 * token with the appropriate value and type (`ty_float`, `ty_double`, or `ty_ldouble`).
 *
 * The supported floating-point constants may have the following suffixes:
 * - `f` or `F`: Float type
 * - `l` or `L`: Long double type
 * - No suffix: Double type (by default)
 *
 * The function checks that the token's entire length is consumed when parsing the numeric constant.
 * If not, an error is raised.
 *
 * @param tok The token representing the numeric constant to be converted.
 *
 * @note This function modifies the token to represent either an integer (`TK_NUM`) or a floating-point
 *       constant (`fval` for floating-point values). It also sets the token's type (`ty_float`, `ty_double`,
 *       or `ty_ldouble`), based on the constant type.
 */
static void convert_pp_number(Token *tok)
{
	// Try to parse as an integer constant.
	if (convert_pp_int(tok))
		return;

	// If it's not an integer, it must be a floating point constant.
	char *end;
	long double val = strtold(tok->loc, &end);

	Type *ty;
	if (*end == 'f' || *end == 'F') {
		ty = ty_float;
		end++;
	} else if (*end == 'l' || *end == 'L') {
		ty = ty_ldouble;
		end++;
	} else {
		ty = ty_double;
	}

	if (tok->loc + tok->len != end)
		error_tok(tok, "invalid numeric constant");

	tok->kind = TK_NUM;
	tok->fval = val;
	tok->ty = ty;
}

/**
 * @brief Converts preprocessor tokens to appropriate types.
 *
 * This function processes a linked list of preprocessor tokens and converts each token based on its kind.
 * It checks if each token is a keyword and updates its kind to `TK_KEYWORD`. If a token is a preprocessor
 * number (`TK_PP_NUM`), it calls `convert_pp_number` to convert the token to an integer or floating-point
 * constant, updating the token's type and value accordingly.
 *
 * The function iterates through all tokens in the list until it encounters the end-of-file (EOF) token (`TK_EOF`).
 *
 * @param tok The first token in the linked list of tokens to be processed.
 *
 * @note This function updates each token's kind and, if applicable, its type and value based on the token's
 *       content. The tokens are processed in a linked list, and the iteration stops when the EOF token is encountered.
 */
void convert_pp_tokens(Token *tok)
{
	for (Token *t = tok; t->kind != TK_EOF; t = t->next) {
		if (is_keyword(t))
			t->kind = TK_KEYWORD;
		else if (t->kind == TK_PP_NUM)
			convert_pp_number(t);
	}
}

/**
 * @brief Adds line numbers to each token.
 *
 * This function processes a linked list of tokens and assigns a `line_no` to each token based on its location
 * in the source code file. The line number is determined by counting the number of newline characters (`\n`)
 * in the file contents before each token's position.
 *
 * The function iterates through the tokens and the file content, comparing the token's location (`loc`) with
 * the current position in the file to determine the correct line number.
 *
 * @param tok The first token in the linked list to be processed.
 *
 * @note The `line_no` field in each token is updated as the function iterates over the tokens. The line number
 *       corresponds to the position of each token in the file contents, based on the occurrence of newline characters.
 */
static void add_line_numbers(Token *tok)
{
	char *p = current_file->contents;
	int n = 1;

	do {
		if (p == tok->loc) {
			tok->line_no = n;
			tok = tok->next;
		}
		if (*p == '\n')
			n++;
	} while (*p++);
}

/**
 * @brief Tokenizes a string literal and converts it to the appropriate token type.
 *
 * This function takes a token representing the start of a string literal and converts it into the corresponding
 * token, either a UTF-16 or UTF-32 encoded string literal token, depending on the size of the base type (`basety`).
 *
 * The function checks the size of the base type (`basety`), and based on that, either calls `read_utf16_string_literal`
 * or `read_utf32_string_literal` to process the string literal and create a new token. The new token is inserted
 * in place of the current token in the linked list.
 *
 * @param tok The token representing the start of the string literal.
 * @param basety The base type that determines the encoding format of the string literal (UTF-16 or UTF-32).
 *
 * @return Token* The new token representing the string literal in the appropriate encoding format (UTF-16 or UTF-32).
 *
 * @note The function modifies the token list by replacing the original token with the newly created token.
 *       The new token is linked to the next token in the list.
 */
Token *tokenize_string_literal(Token *tok, Type *basety)
{
	Token *t;

	if (basety->size == 2)
		t = read_utf16_string_literal(tok->loc, tok->loc);
	else
		t = read_utf32_string_literal(tok->loc, tok->loc, basety);
	t->next = tok->next;
	return t;
}

/**
 * @brief Tokenizes the contents of a file into a linked list of tokens.
 *
 * This function takes a `File` structure, representing the source code, and tokenizes it. It processes the source code
 * character by character, identifying different types of tokens such as comments, string literals, numeric literals,
 * identifiers, punctuators, and more. The tokens are linked in a list and returned to the caller.
 *
 * It handles various types of literals including:
 * - Numeric literals (decimal, hexadecimal, binary, and floating-point)
 * - String literals (UTF-8, UTF-16, UTF-32, wide strings, and character literals)
 * - Identifiers (variables, function names, etc.)
 * - Keywords (reserved words like `return`, `if`, etc.)
 * - Punctuators (symbols like `+`, `-`, `*`, `&&`, etc.)
 *
 * @param file The `File` structure representing the source code to be tokenized.
 *
 * @return Token* A pointer to the first token in the linked list of tokens. The list terminates with a `TK_EOF` token.
 *
 * @note The function processes the input source code and generates a list of tokens. The list includes tokens for all
 *       recognized constructs, including keywords, identifiers, literals, and punctuators. Any invalid token will result
 *       in an error and stop the tokenization process.
 */
Token *tokenize(File *file)
{
	current_file = file;

	char *p = file->contents;
	Token head = {};
	Token *cur = &head;

	at_bol = true;
	has_space = false;

	while (*p) {
		// Skip line comments.
		if (startswith(p, "//")) {
			p += 2;
			while (*p != '\n')
				p++;
			has_space = true;
			continue;
		}

		// Skip block comments.
		if (startswith(p, "/*")) {
			char *q = strstr(p + 2, "*/");
			if (!q)
				error_at(p, "unclosed block comment");
			p = q + 2;
			has_space = true;
			continue;
		}

		// Skip newline.
		if (*p == '\n') {
			p++;
			at_bol = true;
			has_space = false;
			continue;
		}

		// Skip whitespace characters.
		if (isspace(*p)) {
			p++;
			has_space = true;
			continue;
		}

		// Numeric literal
		if (isdigit(*p) || (*p == '.' && isdigit(p[1]))) {
			char *q = p++;
			for (;;) {
				if (p[0] && p[1] && strchr("eEpP", p[0]) && strchr("+-", p[1]))
					p += 2;
				else if (isalnum(*p) || *p == '.')
					p++;
				else
					break;
			}
			cur = cur->next = new_token(TK_PP_NUM, q, p);
			continue;
		}

		// String literal
		if (*p == '"') {
			cur = cur->next = read_string_literal(p, p);
			p += cur->len;
			continue;
		}

		// UTF-8 string literal
		if (startswith(p, "u8\"")) {
			cur = cur->next = read_string_literal(p, p + 2);
			p += cur->len;
			continue;
		}

		// UTF-16 string literal
		if (startswith(p, "u\"")) {
			cur = cur->next = read_utf16_string_literal(p, p + 1);
			p += cur->len;
			continue;
		}

		// Wide string literal
		if (startswith(p, "L\"")) {
			cur = cur->next = read_utf32_string_literal(p, p + 1, ty_int);
			p += cur->len;
			continue;
		}

		// UTF-32 string literal
		if (startswith(p, "U\"")) {
			cur = cur->next = read_utf32_string_literal(p, p + 1, ty_uint);
			p += cur->len;
			continue;
		}

		// Character literal
		if (*p == '\'') {
			cur = cur->next = read_char_literal(p, p, ty_int);
			cur->val = (char)cur->val;
			p += cur->len;
			continue;
		}

		// UTF-16 character literal
		if (startswith(p, "u'")) {
			cur = cur->next = read_char_literal(p, p + 1, ty_ushort);
			cur->val &= 0xffff;
			p += cur->len;
			continue;
		}

		// Wide character literal
		if (startswith(p, "L'")) {
			cur = cur->next = read_char_literal(p, p + 1, ty_int);
			p += cur->len;
			continue;
		}

		// UTF-32 character literal
		if (startswith(p, "U'")) {
			cur = cur->next = read_char_literal(p, p + 1, ty_uint);
			p += cur->len;
			continue;
		}

		// Identifier or keyword
		int ident_len = read_ident(p);
		if (ident_len) {
			cur = cur->next = new_token(TK_IDENT, p, p + ident_len);
			p += cur->len;
			continue;
		}

		// Punctuators
		int punct_len = read_punct(p);
		if (punct_len) {
			cur = cur->next = new_token(TK_PUNCT, p, p + punct_len);
			p += cur->len;
			continue;
		}

		error_at(p, "invalid token");
	}

	cur = cur->next = new_token(TK_EOF, p, p);
	add_line_numbers(head.next);
	return head.next;
}

/**
 * @brief Reads the contents of a file into a dynamically allocated buffer.
 *
 * This function opens the specified file for reading. If the file path is `"-"`, it reads from the standard input (`stdin`).
 * It reads the entire file content into a dynamically allocated buffer and ensures that the last line is properly terminated
 * with a newline character.
 *
 * @param path The path to the file to read. If the path is `"-"`, the function will read from standard input.
 *
 * @return char* A pointer to a dynamically allocated buffer containing the entire file content, including a newline at
 *         the end if necessary. If the file cannot be opened, the function returns `NULL`.
 *
 * @note The caller is responsible for freeing the returned buffer when done with it.
 */
static char *read_file(char *path)
{
	FILE *fp;

	if (strcmp(path, "-") == 0) {
		// By convention, read from stdin if a given filename is "-".
		fp = stdin;
	} else {
		fp = fopen(path, "r");
		if (!fp)
			return NULL;
	}

	char *buf;
	size_t buflen;
	FILE *out = open_memstream(&buf, &buflen);

	// Read the entire file.
	for (;;) {
		char buf2[4096];
		int n = fread(buf2, 1, sizeof(buf2), fp);
		if (n == 0)
			break;
		fwrite(buf2, 1, n, out);
	}

	if (fp != stdin)
		fclose(fp);

	// Make sure that the last line is properly terminated with '\n'.
	fflush(out);
	if (buflen == 0 || buf[buflen - 1] != '\n')
		fputc('\n', out);
	fputc('\0', out);
	fclose(out);
	return buf;
}

/**
 * @brief Retrieves the list of input files.
 *
 * This function returns the global list of input files that have been previously loaded or set.
 * It provides access to the list of files that are being processed by the program.
 *
 * @return File** A pointer to the array of `File` pointers representing the input files.
 *
 * @note The returned list is expected to be handled by the caller, and the caller should be aware of its structure
 *       (typically an array or linked list of `File` objects).
 */
File **get_input_files(void)
{
	return input_files;
}

/**
 * @brief Creates a new `File` object.
 *
 * This function allocates memory for a new `File` structure, initializes its fields with the provided
 * parameters, and returns a pointer to the newly created file object. The `name` field is used both
 * as the name of the file and its display name, while `contents` represents the content of the file.
 *
 * @param name The name of the file (typically the file path).
 * @param file_no A unique identifier (or number) for the file.
 * @param contents A string representing the content of the file.
 *
 * @return File* A pointer to the newly created `File` structure.
 *
 * @note The caller is responsible for ensuring that the `name` and `contents` pointers remain valid for the
 *       lifetime of the `File` object.
 */
File *new_file(char *name, int file_no, char *contents)
{
	File *file = calloc(1, sizeof(File));

	file->name = name;
	file->display_name = name;
	file->file_no = file_no;
	file->contents = contents;
	return file;
}

/**
 * @brief Converts all newline sequences in a string to a canonical form (`\n`).
 *
 * This function scans through the provided string and converts all occurrences of
 * `\r\n` (Windows-style line endings) or `\r` (Mac-style line endings) to `\n`
 * (Unix-style line endings). The transformation is done in-place, modifying the
 * input string.
 *
 * @param p A pointer to the string that contains the line endings to be canonicalized.
 *
 * @note The function operates in-place, meaning the original string is modified.
 *       It processes the string one character at a time, so it's efficient for large inputs.
 *
 * @warning The input string must be null-terminated and mutable (i.e., not a string literal).
 */
static void canonicalize_newline(char *p)
{
	int i = 0, j = 0;

	while (p[i]) {
		if (p[i] == '\r' && p[i + 1] == '\n') {
			i += 2;
			p[j++] = '\n';
		} else if (p[i] == '\r') {
			i++;
			p[j++] = '\n';
		} else {
			p[j++] = p[i++];
		}
	}

	p[j] = '\0';
}

/**
 * @brief Removes backslash-newline (`\n`) sequences and adjusts newlines accordingly.
 *
 * This function processes the provided string, removing any backslash-newline (`\n`)
 * sequences (used to indicate a line continuation in source code). The function ensures
 * that the logical line number remains consistent by keeping the same number of newline
 * characters as there were before the removals.
 *
 * The function modifies the input string in place and preserves the original number of
 * logical newlines, even if some physical newlines are removed.
 *
 * @param p A pointer to the string that contains the backslash-newline sequences to be removed.
 *
 * @note The function processes the string in-place. If the string contains `\` followed by
 *       a newline (`\n`), it removes these characters and combines the adjacent lines into one.
 *
 * @warning The input string must be null-terminated and mutable (i.e., not a string literal).
 */
static void remove_backslash_newline(char *p)
{
	int i = 0, j = 0;

	// We want to keep the number of newline characters so that
	// the logical line number matches the physical one.
	// This counter maintains the number of newlines we have removed.
	int n = 0;

	while (p[i]) {
		if (p[i] == '\\' && p[i + 1] == '\n') {
			i += 2;
			n++;
		} else if (p[i] == '\n') {
			p[j++] = p[i++];
			for (; n > 0; n--)
				p[j++] = '\n';
		} else {
			p[j++] = p[i++];
		}
	}

	for (; n > 0; n--)
		p[j++] = '\n';
	p[j] = '\0';
}

/**
 * @brief Reads a Unicode character from a hexadecimal representation.
 *
 * This function reads a sequence of hexadecimal digits from the given input string
 * and converts it into a 32-bit Unicode character. It is used for processing
 * universal character names (UCNs) in Unicode escape sequences (e.g., `\u1234` or `\U0001F600`).
 *
 * The function expects a string of hexadecimal characters, and it converts them into
 * a corresponding Unicode code point. If the input contains invalid characters or
 * does not have enough digits, it returns 0.
 *
 * @param p A pointer to the string containing the hexadecimal digits.
 * @param len The number of hexadecimal digits to read.
 *
 * @return A 32-bit integer representing the Unicode character. If the input is invalid,
 *         it returns 0.
 *
 * @note The function assumes the input string contains valid hexadecimal digits.
 *       If the input string contains non-hexadecimal characters, the function
 *       returns 0 and doesn't perform the conversion.
 */
static uint32_t read_universal_char(char *p, int len)
{
	uint32_t c = 0;

	for (int i = 0; i < len; i++) {
		if (!isxdigit(p[i]))
			return 0;
		c = (c << 4) | from_hex(p[i]);
	}
	return c;
}

/**
 * @brief Converts universal character names (UCNs) in a string to UTF-8 encoding.
 *
 * This function processes a string and replaces any Unicode escape sequences, such as
 * `\uXXXX` or `\UXXXXXXXX`, with the corresponding UTF-8 encoded characters.
 * It supports both 4-digit (`\u`) and 8-digit (`\U`) Unicode escape sequences.
 *
 * The function scans through the input string character by character. If it encounters a
 * Unicode escape sequence, it attempts to decode it using `read_universal_char`. If the
 * escape sequence is valid, it converts the Unicode code point to UTF-8 and inserts it
 * into the output string. Invalid escape sequences are left unchanged.
 *
 * @param p A pointer to the input string which may contain Unicode escape sequences.
 *          The function modifies this string in place, converting the escape sequences.
 *
 * @note The function assumes that the input string is properly null-terminated.
 *       The escape sequences must be in the format `\uXXXX` or `\UXXXXXXXX`, where `X` is a
 *       hexadecimal digit.
 */
static void convert_universal_chars(char *p)
{
	char *q = p;

	while (*p) {
		if (startswith(p, "\\u")) {
			uint32_t c = read_universal_char(p + 2, 4);
			if (c) {
				p += 6;
				q += encode_utf8(q, c);
			} else {
				*q++ = *p++;
			}
		} else if (startswith(p, "\\U")) {
			uint32_t c = read_universal_char(p + 2, 8);
			if (c) {
				p += 10;
				q += encode_utf8(q, c);
			} else {
				*q++ = *p++;
			}
		} else if (p[0] == '\\') {
			*q++ = *p++;
			*q++ = *p++;
		} else {
			*q++ = *p++;
		}
	}

	*q = '\0';
}

/**
 * @brief Tokenizes the contents of a file, handling BOM, newline conversion, and universal character sequences.
 *
 * This function reads a file from the specified path, processes its contents, and tokenizes the text.
 * It handles various preprocessing steps, such as:
 * - Skipping the BOM (Byte Order Mark) if present (common in UTF-8 encoded files).
 * - Normalizing line endings using `canonicalize_newline`.
 * - Removing backslash-newline continuations with `remove_backslash_newline`.
 * - Converting universal character names (e.g., `\uXXXX` or `\UXXXXXXXX`) to their corresponding UTF-8 encoding with `convert_universal_chars`.
 *
 * After processing the contents, the function creates a `File` structure to represent the file and stores it in the global `input_files` array.
 * It then calls the `tokenize` function to generate tokens from the processed content.
 *
 * @param path A string representing the path to the file to be tokenized.
 *
 * @return A pointer to the first token of the tokenized file, or NULL if the file cannot be read.
 *
 * @note The function modifies the input string by processing the contents in place. The file is read entirely into memory, so large files
 *       could consume a significant amount of memory. The `input_files` array stores the list of input files processed.
 */
Token *tokenize_file(char *path)
{
	char *p = read_file(path);

	if (!p)
		return NULL;

	// UTF-8 texts may start with a 3-byte "BOM" marker sequence.
	// If exists, just skip them because they are useless bytes.
	// (It is actually not recommended to add BOM markers to UTF-8
	// texts, but it's not uncommon particularly on Windows.)
	if (!memcmp(p, "\xef\xbb\xbf", 3))
		p += 3;

	canonicalize_newline(p);
	remove_backslash_newline(p);
	convert_universal_chars(p);

	// Save the filename for assembler .file directive.
	static int file_no;
	File *file = new_file(path, file_no + 1, p);

	// Save the filename for assembler .file directive.
	input_files = realloc(input_files, sizeof(char *) * (file_no + 2));
	input_files[file_no] = file;
	input_files[file_no + 1] = NULL;
	file_no++;

	return tokenize(file);
}
