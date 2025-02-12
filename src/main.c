#include "chibicc.h"

/**
 * @brief Enum representing different types of files handled by the compiler.
 */
typedef enum {
	FILE_NONE,      ///< No file type specified.
	FILE_C,         ///< C source file.
	FILE_ASM,       ///< Assembly source file.
	FILE_OBJ,       ///< Object file.
	FILE_AR,        ///< Archive file.
	FILE_DSO,       ///< Shared object file (dynamic library).
} FileType;

/**
 * @brief List of include paths for the compiler.
 */
StringArray include_paths;

/**
 * @brief Flag for the `-fcommon` option.
 *
 * If true, global variables without explicit `extern` will be treated as common symbols.
 * Default is `true`, following traditional C behavior.
 */
bool opt_fcommon = true;

/**
 * @brief Flag for the `-fPIC` option.
 *
 * If true, generates position-independent code for shared libraries.
 */
bool opt_fpic;

/**
 * @brief Specifies the input file type when using the `-x` option.
 */
static FileType opt_x;

/**
 * @brief List of files to include via the `-include` option.
 */
static StringArray opt_include;

/**
 * @brief Flag for the `-E` option.
 *
 * If true, stops after preprocessing without compiling.
 */
static bool opt_E;

/**
 * @brief Flag for the `-M` option.
 *
 * If true, generates dependencies for makefiles.
 */
static bool opt_M;

/**
 * @brief Flag for the `-MD` option.
 *
 * If true, like `-M` but does not suppress compilation.
 */
static bool opt_MD;

/**
 * @brief Flag for the `-MMD` option.
 *
 * Like `-MD` but ignores system header dependencies.
 */
static bool opt_MMD;

/**
 * @brief Flag for the `-MP` option.
 *
 * If true, generates phony targets for dependencies.
 */
static bool opt_MP;

/**
 * @brief Flag for the `-S` option.
 *
 * If true, stops after generating assembly.
 */
static bool opt_S;

/**
 * @brief Flag for the `-c` option.
 *
 * If true, stops after generating object files without linking.
 */
static bool opt_c;

/**
 * @brief Flag for the `-cc1` option.
 *
 * If true, runs in internal mode for compiler driver communication.
 */
static bool opt_cc1;

/**
 * @brief Flag for the `###` option.
 *
 * If true, prints the commands without executing them.
 */
static bool opt_hash_hash_hash;

/**
 * @brief Flag for the `-static` option.
 *
 * If true, links binaries statically.
 */
static bool opt_static;

/**
 * @brief Flag for the `-shared` option.
 *
 * If true, generates a shared object file.
 */
static bool opt_shared;

/**
 * @brief Specifies the dependency output file when using `-MF`.
 */
static char *opt_MF;

/**
 * @brief Specifies the dependency target name when using `-MT`.
 */
static char *opt_MT;

/**
 * @brief Specifies the output file name when using `-o`.
 */
static char *opt_o;

/**
 * @brief Extra arguments to pass to the linker.
 */
static StringArray ld_extra_args;

/**
 * @brief Standard include paths for system headers.
 */
static StringArray std_include_paths;

/**
 * @brief The base file name of the main input file.
 */
char *base_file;

/**
 * @brief The output file path.
 */
static char *output_file;

/**
 * @brief List of input files provided to the compiler.
 */
static StringArray input_paths;

/**
 * @brief List of temporary files created during compilation.
 */
static StringArray tmpfiles;

/**
 * @brief Prints the usage information and exits.
 *
 * @param status Exit status code.
 */
static void usage(int status)
{
	fprintf(stderr, "chibicc [ -o <path> ] <file>\n");
	exit(status);
}

/**
 * @brief Determines whether a given argument expects a parameter.
 *
 * Some command-line options require an additional argument, such as `-o` for output file
 * or `-I` for include paths. This function checks if an argument is one of those.
 *
 * @param arg The command-line argument to check.
 * @return `true` if the argument requires a parameter, `false` otherwise.
 */
static bool take_arg(char *arg)
{
	// List of options that require an argument.
	char *x[] = {
		"-o",           ///< Output file.
		"-I",           ///< Include path.
		"-idirafter",   ///< Secondary include path.
		"-include",     ///< File to include before compilation.
		"-x",           ///< Specify language type.
		"-MF",          ///< Dependency output file.
		"-MT",          ///< Dependency target name.
		"-Xlinker",     ///< Pass option to the linker.
	};

	// Check if the argument is in the list.
	for (int i = 0; i < sizeof(x) / sizeof(*x); i++)
		if (!strcmp(arg, x[i]))
			return true;

	return false;
}

/**
 * @brief Adds default include paths for the compiler.
 *
 * This function sets up include paths for the preprocessor, including both
 * chibicc-specific paths and standard system include directories.
 *
 * @param argv0 The first command-line argument (`argv[0]`), used to determine
 *              the compiler's installation directory.
 */
static void add_default_include_paths(char *argv0)
{
	// Expect chibicc-specific include files to be in ./include relative to the binary path.
	strarray_push(&include_paths, format("%s/include", dirname(strdup(argv0))));

	// Add standard system include paths.
	strarray_push(&include_paths, "/usr/local/include");
	strarray_push(&include_paths, "/usr/include/x86_64-linux-gnu");
	strarray_push(&include_paths, "/usr/include");

	// Copy the standard include paths to be used for `-MMD` (dependency tracking).
	for (int i = 0; i < include_paths.len; i++)
		strarray_push(&std_include_paths, include_paths.data[i]);
}

/**
 * @brief Defines a macro from a command-line argument.
 *
 * This function processes `-D` options, defining macros for the preprocessor.
 * If the argument is in the form `NAME=VALUE`, it defines `NAME` as `VALUE`.
 * Otherwise, it defines `NAME` as `1`.
 *
 * @param str The macro definition string (e.g., `"FOO=42"` or `"DEBUG"`).
 */
static void define(char *str)
{
	char *eq = strchr(str, '=');

	if (eq)
		define_macro(strndup(str, eq - str), eq + 1);   ///< Define `NAME=VALUE`
	else
		define_macro(str, "1");                         ///< Define `NAME=1` if no value is given.
}

/**
 * @brief Parses the argument of the `-x` option and returns the corresponding file type.
 *
 * The `-x` option specifies the language of the following input files.
 * This function converts the argument into an appropriate `FileType` value.
 *
 * @param s The language specifier (e.g., `"c"`, `"assembler"`, `"none"`).
 * @return The corresponding `FileType` enum value.
 * @throws This function terminates the program with an error if an invalid argument is given.
 */
static FileType parse_opt_x(char *s)
{
	if (!strcmp(s, "c"))
		return FILE_C;
	if (!strcmp(s, "assembler"))
		return FILE_ASM;
	if (!strcmp(s, "none"))
		return FILE_NONE;

	error("<command line>: unknown argument for -x: %s", s);
}

/**
 * @brief Escapes a string for use in a Makefile.
 *
 * This function escapes special characters like `$`, `#`, and spaces so that
 * the string can be safely used in a Makefile rule.
 *
 * @param s The input string.
 * @return A newly allocated escaped string, which must be freed by the caller.
 */
static char *quote_makefile(char *s)
{
	// Allocate enough space to handle worst-case escaping (double the length).
	char *buf = calloc(1, strlen(s) * 2 + 1);

	for (int i = 0, j = 0; s[i]; i++) {
		switch (s[i]) {
		case '$':
			buf[j++] = '$'; ///< Make `$` into `$$` (Makefile variable escaping).
			buf[j++] = '$';
			break;
		case '#':
			buf[j++] = '\\'; ///< Escape `#` to prevent comment interpretation.
			buf[j++] = '#';
			break;
		case ' ':
		case '\t':
			// Escape spaces and tabs, ensuring correct backslash handling.
			for (int k = i - 1; k >= 0 && s[k] == '\\'; k--)
				buf[j++] = '\\';
			buf[j++] = '\\';
			buf[j++] = s[i];
			break;
		default:
			buf[j++] = s[i];
			break;
		}
	}
	return buf;
}

/**
 * @brief Parses command-line arguments and sets appropriate options.
 *
 * This function processes command-line arguments, enabling features,
 * setting output files, configuring compilation flags, and handling
 * unknown options.
 *
 * @param argc Argument count.
 * @param argv Argument vector.
 */
static void parse_args(int argc, char **argv)
{
	// Ensure options that require an argument have one.
	for (int i = 1; i < argc; i++)
		if (take_arg(argv[i]) && !argv[++i])
			usage(1);

	StringArray idirafter = {}; ///< Stores `-idirafter` include paths.

	for (int i = 1; i < argc; i++) {
		// Debug flag: Print command-line arguments and exit.
		if (!strcmp(argv[i], "-###")) {
			opt_hash_hash_hash = true;
			continue;
		}

		// Internal flag: Run in cc1 mode.
		if (!strcmp(argv[i], "-cc1")) {
			opt_cc1 = true;
			continue;
		}

		// Display usage information.
		if (!strcmp(argv[i], "--help"))
			usage(0);

		// Output file specification.
		if (!strcmp(argv[i], "-o")) {
			opt_o = argv[++i];
			continue;
		}
		if (!strncmp(argv[i], "-o", 2)) {
			opt_o = argv[i] + 2;
			continue;
		}

		// Compilation stages.
		if (!strcmp(argv[i], "-S")) {
			opt_S = true; continue;
		}
		if (!strcmp(argv[i], "-c")) {
			opt_c = true; continue;
		}
		if (!strcmp(argv[i], "-E")) {
			opt_E = true; continue;
		}

		// Common symbols handling.
		if (!strcmp(argv[i], "-fcommon")) {
			opt_fcommon = true; continue;
		}
		if (!strcmp(argv[i], "-fno-common")) {
			opt_fcommon = false; continue;
		}

		// Include paths.
		if (!strncmp(argv[i], "-I", 2)) {
			strarray_push(&include_paths, argv[i] + 2);
			continue;
		}

		// Macro definitions.
		if (!strcmp(argv[i], "-D")) {
			define(argv[++i]); continue;
		}
		if (!strncmp(argv[i], "-D", 2)) {
			define(argv[i] + 2); continue;
		}

		// Macro undefinitions.
		if (!strcmp(argv[i], "-U")) {
			undef_macro(argv[++i]); continue;
		}
		if (!strncmp(argv[i], "-U", 2)) {
			undef_macro(argv[i] + 2); continue;
		}

		// Forced includes.
		if (!strcmp(argv[i], "-include")) {
			strarray_push(&opt_include, argv[++i]);
			continue;
		}

		// Language mode.
		if (!strcmp(argv[i], "-x")) {
			opt_x = parse_opt_x(argv[++i]);
			continue;
		}
		if (!strncmp(argv[i], "-x", 2)) {
			opt_x = parse_opt_x(argv[i] + 2);
			continue;
		}

		// Linker options and libraries.
		if (!strncmp(argv[i], "-l", 2) || !strncmp(argv[i], "-Wl,", 4)) {
			strarray_push(&input_paths, argv[i]);
			continue;
		}

		if (!strcmp(argv[i], "-Xlinker")) {
			strarray_push(&ld_extra_args, argv[++i]);
			continue;
		}

		if (!strcmp(argv[i], "-s")) {
			strarray_push(&ld_extra_args, "-s");
			continue;
		}

		// Dependency generation.
		if (!strcmp(argv[i], "-M")) {
			opt_M = true; continue;
		}
		if (!strcmp(argv[i], "-MF")) {
			opt_MF = argv[++i]; continue;
		}
		if (!strcmp(argv[i], "-MP")) {
			opt_MP = true; continue;
		}
		if (!strcmp(argv[i], "-MD")) {
			opt_MD = true; continue;
		}
		if (!strcmp(argv[i], "-MMD")) {
			opt_MD = opt_MMD = true; continue;
		}

		// Target specification for dependency generation.
		if (!strcmp(argv[i], "-MT")) {
			opt_MT = opt_MT ? format("%s %s", opt_MT, argv[++i]) : argv[++i];
			continue;
		}
		if (!strcmp(argv[i], "-MQ")) {
			opt_MT = opt_MT ? format("%s %s", opt_MT, quote_makefile(argv[++i])) : quote_makefile(argv[++i]);
			continue;
		}

		// Position-independent code.
		if (!strcmp(argv[i], "-fpic") || !strcmp(argv[i], "-fPIC")) {
			opt_fpic = true;
			continue;
		}

		// Internal flags for cc1 mode.
		if (!strcmp(argv[i], "-cc1-input")) {
			base_file = argv[++i]; continue;
		}
		if (!strcmp(argv[i], "-cc1-output")) {
			output_file = argv[++i]; continue;
		}

		// Additional include paths.
		if (!strcmp(argv[i], "-idirafter")) {
			strarray_push(&idirafter, argv[++i]);
			continue;
		}

		// Linking options.
		if (!strcmp(argv[i], "-static")) {
			opt_static = true;
			strarray_push(&ld_extra_args, "-static");
			continue;
		}

		if (!strcmp(argv[i], "-shared")) {
			opt_shared = true;
			strarray_push(&ld_extra_args, "-shared");
			continue;
		}

		// Library search paths.
		if (!strcmp(argv[i], "-L")) {
			strarray_push(&ld_extra_args, "-L");
			strarray_push(&ld_extra_args, argv[++i]);
			continue;
		}
		if (!strncmp(argv[i], "-L", 2)) {
			strarray_push(&ld_extra_args, "-L");
			strarray_push(&ld_extra_args, argv[i] + 2);
			continue;
		}

		// Run internal hashmap test and exit.
		if (!strcmp(argv[i], "-hashmap-test")) {
			hashmap_test();
			exit(0);
		}

		// Ignored options.
		if (!strncmp(argv[i], "-O", 2) ||
		    !strncmp(argv[i], "-W", 2) ||
		    !strncmp(argv[i], "-g", 2) ||
		    !strncmp(argv[i], "-std=", 5) ||
		    !strcmp(argv[i], "-ffreestanding") ||
		    !strcmp(argv[i], "-fno-builtin") ||
		    !strcmp(argv[i], "-fno-omit-frame-pointer") ||
		    !strcmp(argv[i], "-fno-stack-protector") ||
		    !strcmp(argv[i], "-fno-strict-aliasing") ||
		    !strcmp(argv[i], "-m64") ||
		    !strcmp(argv[i], "-mno-red-zone") ||
		    !strcmp(argv[i], "-w"))
			continue;

		// Unknown option handling.
		if (argv[i][0] == '-' && argv[i][1] != '\0')
			error("unknown argument: %s", argv[i]);

		// Assume it's an input file.
		strarray_push(&input_paths, argv[i]);
	}

	// Append `-idirafter` paths to the include path list.
	for (int i = 0; i < idirafter.len; i++)
		strarray_push(&include_paths, idirafter.data[i]);

	// Ensure at least one input file is provided.
	if (input_paths.len == 0)
		error("no input files");

	// `-E` forces the input to be treated as C source.
	if (opt_E)
		opt_x = FILE_C;
}

/**
 * @brief Opens a file for writing.
 *
 * If the given path is `NULL` or `"-"`, it returns `stdout`. Otherwise, it attempts to open the file for writing.
 * If the file cannot be opened, the program terminates with an error.
 *
 * @param path The path of the file to open.
 * @return A `FILE*` to the opened file, or `stdout` if the path is `NULL` or `"-"`.
 */
static FILE *open_file(char *path)
{
	if (!path || strcmp(path, "-") == 0)
		return stdout;

	FILE *out = fopen(path, "w");
	if (!out)
		error("cannot open output file: %s: %s", path, strerror(errno));
	return out;
}

/**
 * @brief Checks if a string ends with a given suffix.
 *
 * @param p The string to check.
 * @param q The suffix to look for.
 * @return `true` if `p` ends with `q`, `false` otherwise.
 */
static bool endswith(char *p, char *q)
{
	int len1 = strlen(p);
	int len2 = strlen(q);

	return (len1 >= len2) && !strcmp(p + len1 - len2, q);
}

/**
 * @brief Replaces the file extension of a given filename.
 *
 * Extracts the base name of `tmpl`, removes its extension (if any), and appends the new extension `extn`.
 *
 * @param tmpl The input filename.
 * @param extn The new extension to append.
 * @return A newly allocated string with the new filename.
 */
static char *replace_extn(char *tmpl, char *extn)
{
	char *filename = basename(strdup(tmpl));
	char *dot = strrchr(filename, '.');

	if (dot)
		*dot = '\0';
	return format("%s%s", filename, extn);
}

/**
 * @brief Cleans up temporary files.
 *
 * Iterates through the list of temporary files and removes each one.
 */
static void cleanup(void)
{
	for (int i = 0; i < tmpfiles.len; i++)
		unlink(tmpfiles.data[i]);
}

/**
 * @brief Creates a temporary file.
 *
 * Generates a unique temporary file in `/tmp/`, registers it in the `tmpfiles` list, and returns its path.
 *
 * @return The path of the created temporary file.
 */
static char *create_tmpfile(void)
{
	char *path = strdup("/tmp/chibicc-XXXXXX");
	int fd = mkstemp(path);

	if (fd == -1)
		error("mkstemp failed: %s", strerror(errno));
	close(fd);

	strarray_push(&tmpfiles, path);
	return path;
}

/**
 * @brief Runs a subprocess with the given arguments.
 *
 * If the `-###` option is enabled, the command is printed before execution.
 * The function forks a new process and executes the given command using `execvp()`.
 * If the execution fails, an error message is printed.
 *
 * @param argv Null-terminated array of arguments (first argument is the command).
 */
static void run_subprocess(char **argv)
{
	// If -### is given, dump the subprocess's command line.
	if (opt_hash_hash_hash) {
		fprintf(stderr, "%s", argv[0]);
		for (int i = 1; argv[i]; i++)
			fprintf(stderr, " %s", argv[i]);
		fprintf(stderr, "\n");
	}

	if (fork() == 0) {
		// Child process. Run a new command.
		execvp(argv[0], argv);
		fprintf(stderr, "exec failed: %s: %s\n", argv[0], strerror(errno));
		_exit(1);
	}

	// Wait for the child process to finish.
	int status;
	while (wait(&status) > 0);
	if (status != 0)
		exit(1);
}

/**
 * @brief Runs the `cc1` subprocess with the given arguments.
 *
 * This function constructs a new argument list by appending `-cc1`, the input file (if provided),
 * and the output file (if provided). It then executes `run_subprocess()` to run the `cc1` process.
 *
 * @param argc The number of arguments in `argv`.
 * @param argv The argument list.
 * @param input The input file (can be `NULL`).
 * @param output The output file (can be `NULL`).
 */
static void run_cc1(int argc, char **argv, char *input, char *output)
{
	char **args = calloc(argc + 10, sizeof(char *));

	memcpy(args, argv, argc * sizeof(char *));
	args[argc++] = "-cc1";

	if (input) {
		args[argc++] = "-cc1-input";
		args[argc++] = input;
	}

	if (output) {
		args[argc++] = "-cc1-output";
		args[argc++] = output;
	}

	run_subprocess(args);
}

/**
 * @brief Prints a list of tokens to `stdout`.
 *
 * This function is used when the `-E` option is enabled. It prints the tokenized output of the input file,
 * preserving spacing and newlines.
 *
 * @param tok The first token in the list.
 */
static void print_tokens(Token *tok)
{
	FILE *out = open_file(opt_o ? opt_o : "-");

	int line = 1;

	for (; tok->kind != TK_EOF; tok = tok->next) {
		if (line > 1 && tok->at_bol)
			fprintf(out, "\n");
		if (tok->has_space && !tok->at_bol)
			fprintf(out, " ");
		fprintf(out, "%.*s", tok->len, tok->loc);
		line++;
	}
	fprintf(out, "\n");
}

/**
 * @brief Checks if a given path belongs to a standard include directory.
 *
 * This function compares the given `path` with known standard include paths and determines
 * whether it is part of a system include directory.
 *
 * @param path The path to check.
 * @return `true` if `path` is in a standard include directory, `false` otherwise.
 */
static bool in_std_include_path(char *path)
{
	for (int i = 0; i < std_include_paths.len; i++) {
		char *dir = std_include_paths.data[i];
		int len = strlen(dir);
		if (strncmp(dir, path, len) == 0 && path[len] == '/')
			return true;
	}
	return false;
}

/**
 * @brief Prints the list of dependencies for `make` when `-M` options are used.
 *
 * If `-M` options are provided, the compiler generates a list of input files in a format that `make` can read.
 * This helps automate dependency management. The dependency list is written to `stdout` or a specified file.
 */
static void print_dependencies(void)
{
	char *path;

	if (opt_MF)
		path = opt_MF;
	else if (opt_MD)
		path = replace_extn(opt_o ? opt_o : base_file, ".d");
	else if (opt_o)
		path = opt_o;
	else
		path = "-";

	FILE *out = open_file(path);
	if (opt_MT)
		fprintf(out, "%s:", opt_MT);
	else
		fprintf(out, "%s:", quote_makefile(replace_extn(base_file, ".o")));

	File **files = get_input_files();

	for (int i = 0; files[i]; i++) {
		if (opt_MMD && in_std_include_path(files[i]->name))
			continue;
		fprintf(out, " \\\n  %s", files[i]->name);
	}

	fprintf(out, "\n\n");

	if (opt_MP) {
		for (int i = 1; files[i]; i++) {
			if (opt_MMD && in_std_include_path(files[i]->name))
				continue;
			fprintf(out, "%s:\n\n", quote_makefile(files[i]->name));
		}
	}
}

/**
 * @brief Tokenizes a file and exits with an error if it fails.
 *
 * Calls `tokenize_file()` to tokenize the given file. If tokenization fails,
 * the function prints an error message and terminates the program.
 *
 * @param path The path of the file to tokenize.
 * @return The tokenized representation of the file.
 */
static Token *must_tokenize_file(char *path)
{
	Token *tok = tokenize_file(path);

	if (!tok)
		error("%s: %s", path, strerror(errno));
	return tok;
}

/**
 * @brief Appends two token lists.
 *
 * If `tok1` is `NULL` or represents the end of a token stream, `tok2` is returned.
 * Otherwise, `tok2` is appended to the end of `tok1`.
 *
 * @param tok1 The first token list.
 * @param tok2 The second token list.
 * @return The combined token list.
 */
static Token *append_tokens(Token *tok1, Token *tok2)
{
	if (!tok1 || tok1->kind == TK_EOF)
		return tok2;

	Token *t = tok1;
	while (t->next->kind != TK_EOF)
		t = t->next;
	t->next = tok2;
	return tok1;
}

/**
 * @brief The main function for the `cc1` phase of the compiler.
 *
 * This function handles preprocessing, tokenization, parsing, and code generation.
 * It also processes options like `-include`, `-M`, `-MD`, and `-E`:
 * - `-include`: Includes additional files before tokenizing the main input.
 * - `-M` / `-MD`: Generates dependency information for `make`.
 * - `-E`: Outputs preprocessed C code.
 */
static void cc1(void)
{
	Token *tok = NULL;

	// Process -include option
	for (int i = 0; i < opt_include.len; i++) {
		char *incl = opt_include.data[i];

		char *path;
		if (file_exists(incl)) {
			path = incl;
		} else {
			path = search_include_paths(incl);
			if (!path)
				error("-include: %s: %s", incl, strerror(errno));
		}

		Token *tok2 = must_tokenize_file(path);
		tok = append_tokens(tok, tok2);
	}

	// Tokenize and parse.
	Token *tok2 = must_tokenize_file(base_file);
	tok = append_tokens(tok, tok2);
	tok = preprocess(tok);

	// If -M or -MD are given, print file dependencies.
	if (opt_M || opt_MD) {
		print_dependencies();
		if (opt_M)
			return;
	}

	// If -E is given, print out preprocessed C code as a result.
	if (opt_E) {
		print_tokens(tok);
		return;
	}

	Obj *prog = parse(tok);

	// Open a temporary output buffer.
	char *buf;
	size_t buflen;
	FILE *output_buf = open_memstream(&buf, &buflen);

	// Traverse the AST to emit assembly.
	codegen(prog, output_buf);
	fclose(output_buf);

	// Write the assembly text to a file.
	FILE *out = open_file(output_file);
	fwrite(buf, buflen, 1, out);
	fclose(out);
}

/**
 * @brief Assembles an object file from an assembly file.
 *
 * This function calls the assembler (`as`) to compile an assembly file (`input`)
 * into an object file (`output`).
 *
 * @param input The input assembly file.
 * @param output The output object file.
 */
static void assemble(char *input, char *output)
{
	char *cmd[] = { "as", "-c", input, "-o", output, NULL };

	run_subprocess(cmd);
}

/**
 * @brief Finds a file matching a given pattern.
 *
 * Uses `glob()` to search for files matching `pattern` and returns the last match found.
 * The returned string is dynamically allocated and must be freed by the caller.
 *
 * @param pattern The glob pattern to search for.
 * @return The path of the last matching file, or `NULL` if no match is found.
 */
static char *find_file(char *pattern)
{
	char *path = NULL;
	glob_t buf = {};

	glob(pattern, 0, NULL, &buf);
	if (buf.gl_pathc > 0)
		path = strdup(buf.gl_pathv[buf.gl_pathc - 1]);
	globfree(&buf);
	return path;
}

/**
 * @brief Checks if a given file exists.
 *
 * Uses `stat()` to determine whether the specified file exists.
 *
 * @param path The file path to check.
 * @return `true` if the file exists, `false` otherwise.
 */
bool file_exists(char *path)
{
	struct stat st;

	return !stat(path, &st);
}

/**
 * @brief Finds the standard system library path.
 *
 * Checks for common library locations used in different Linux distributions.
 * If no valid library path is found, the function terminates the program with an error.
 *
 * @return The path to the system libraries.
 */
static char *find_libpath(void)
{
	if (file_exists("/usr/lib/x86_64-linux-gnu/crti.o"))
		return "/usr/lib/x86_64-linux-gnu";
	if (file_exists("/usr/lib64/crti.o"))
		return "/usr/lib64";
	error("library path is not found");
}

/**
 * @brief Finds the GCC-specific library path.
 *
 * Searches for `crtbegin.o` in various known GCC installation paths,
 * including those for Debian, Gentoo, and Fedora-based systems.
 * If no path is found, the function terminates the program with an error.
 *
 * @return The path to the GCC libraries.
 */
static char *find_gcc_libpath(void)
{
	char *paths[] = {
		"/usr/lib/gcc/x86_64-linux-gnu/*/crtbegin.o",
		"/usr/lib/gcc/x86_64-pc-linux-gnu/*/crtbegin.o",        // For Gentoo
		"/usr/lib/gcc/x86_64-redhat-linux/*/crtbegin.o",        // For Fedora
	};

	for (int i = 0; i < sizeof(paths) / sizeof(*paths); i++) {
		char *path = find_file(paths[i]);
		if (path)
			return dirname(path);
	}

	error("gcc library path is not found");
}

/**
 * @brief Runs the linker to produce the final executable or shared library.
 *
 * Constructs the linker command by specifying input object files, necessary startup files,
 * library search paths, and additional options. Calls `ld` to link the final binary.
 *
 * @param inputs A list of input object files.
 * @param output The output file name.
 */
static void run_linker(StringArray *inputs, char *output)
{
	StringArray arr = {};

	strarray_push(&arr, "ld");
	strarray_push(&arr, "-o");
	strarray_push(&arr, output);
	strarray_push(&arr, "-m");
	strarray_push(&arr, "elf_x86_64");

	char *libpath = find_libpath();
	char *gcc_libpath = find_gcc_libpath();

	// Add startup files
	if (opt_shared) {
		strarray_push(&arr, format("%s/crti.o", libpath));
		strarray_push(&arr, format("%s/crtbeginS.o", gcc_libpath));
	} else {
		strarray_push(&arr, format("%s/crt1.o", libpath));
		strarray_push(&arr, format("%s/crti.o", libpath));
		strarray_push(&arr, format("%s/crtbegin.o", gcc_libpath));
	}

	// Add library search paths
	strarray_push(&arr, format("-L%s", gcc_libpath));
	strarray_push(&arr, "-L/usr/lib/x86_64-linux-gnu");
	strarray_push(&arr, "-L/usr/lib64");
	strarray_push(&arr, "-L/lib64");
	strarray_push(&arr, "-L/usr/lib/x86_64-linux-gnu");
	strarray_push(&arr, "-L/usr/lib/x86_64-pc-linux-gnu");
	strarray_push(&arr, "-L/usr/lib/x86_64-redhat-linux");
	strarray_push(&arr, "-L/usr/lib");
	strarray_push(&arr, "-L/lib");

	// Configure dynamic linking
	if (!opt_static) {
		strarray_push(&arr, "-dynamic-linker");
		strarray_push(&arr, "/lib64/ld-linux-x86-64.so.2");
	}

	// Add extra linker arguments
	for (int i = 0; i < ld_extra_args.len; i++)
		strarray_push(&arr, ld_extra_args.data[i]);

	// Add input files
	for (int i = 0; i < inputs->len; i++)
		strarray_push(&arr, inputs->data[i]);

	// Add standard libraries
	if (opt_static) {
		strarray_push(&arr, "--start-group");
		strarray_push(&arr, "-lgcc");
		strarray_push(&arr, "-lgcc_eh");
		strarray_push(&arr, "-lc");
		strarray_push(&arr, "--end-group");
	} else {
		strarray_push(&arr, "-lc");
		strarray_push(&arr, "-lgcc");
		strarray_push(&arr, "--as-needed");
		strarray_push(&arr, "-lgcc_s");
		strarray_push(&arr, "--no-as-needed");
	}

	// Add finalization files
	if (opt_shared)
		strarray_push(&arr, format("%s/crtendS.o", gcc_libpath));
	else
		strarray_push(&arr, format("%s/crtend.o", gcc_libpath));

	strarray_push(&arr, format("%s/crtn.o", libpath));
	strarray_push(&arr, NULL);

	run_subprocess(arr.data);
}

/**
 * @brief Determines the type of a given file based on its extension.
 *
 * If the `-x` option is specified, it overrides automatic detection.
 * Otherwise, the function checks the file extension and returns the corresponding file type.
 * If the extension is unknown, the program terminates with an error.
 *
 * @param filename The name of the file to analyze.
 * @return The detected file type.
 */
static FileType get_file_type(char *filename)
{
	if (opt_x != FILE_NONE)
		return opt_x;

	if (endswith(filename, ".a"))
		return FILE_AR;
	if (endswith(filename, ".so"))
		return FILE_DSO;
	if (endswith(filename, ".o"))
		return FILE_OBJ;
	if (endswith(filename, ".c"))
		return FILE_C;
	if (endswith(filename, ".s"))
		return FILE_ASM;

	error("<command line>: unknown file extension: %s", filename);
}

/**
 * @brief Compiler driver entry point.
 *
 * Parses command-line arguments, processes input files based on their type, and
 * invokes the appropriate compilation, assembly, or linking steps.
 *
 * @param argc The number of command-line arguments.
 * @param argv The array of command-line arguments.
 * @return 0 on successful execution, nonzero on error.
 */
int main(int argc, char **argv)
{
	atexit(cleanup);
	init_macros();
	parse_args(argc, argv);

	// If -cc1 is specified, enter the cc1 processing mode.
	if (opt_cc1) {
		add_default_include_paths(argv[0]);
		cc1();
		return 0;
	}

	// Validate -o usage with multiple input files.
	if (input_paths.len > 1 && opt_o && (opt_c || opt_S || opt_E))
		error("cannot specify '-o' with '-c,' '-S' or '-E' with multiple files");

	StringArray ld_args = {};

	// Process input files
	for (int i = 0; i < input_paths.len; i++) {
		char *input = input_paths.data[i];

		// Linker options starting with -l
		if (!strncmp(input, "-l", 2)) {
			strarray_push(&ld_args, input);
			continue;
		}

		// Pass -Wl options directly to the linker
		if (!strncmp(input, "-Wl,", 4)) {
			char *s = strdup(input + 4);
			char *arg = strtok(s, ",");
			while (arg) {
				strarray_push(&ld_args, arg);
				arg = strtok(NULL, ",");
			}
			continue;
		}

		// Determine output file name
		char *output;
		if (opt_o)
			output = opt_o;
		else if (opt_S)
			output = replace_extn(input, ".s");
		else
			output = replace_extn(input, ".o");

		FileType type = get_file_type(input);

		// Handle object files (.o), archives (.a), and shared libraries (.so)
		if (type == FILE_OBJ || type == FILE_AR || type == FILE_DSO) {
			strarray_push(&ld_args, input);
			continue;
		}

		// Handle assembly files (.s)
		if (type == FILE_ASM) {
			if (!opt_S)
				assemble(input, output);
			continue;
		}

		assert(type == FILE_C);

		// Preprocessing only (-E or -M)
		if (opt_E || opt_M) {
			run_cc1(argc, argv, input, NULL);
			continue;
		}

		// Compile only (-S)
		if (opt_S) {
			run_cc1(argc, argv, input, output);
			continue;
		}

		// Compile and assemble (-c)
		if (opt_c) {
			char *tmp = create_tmpfile();
			run_cc1(argc, argv, input, tmp);
			assemble(tmp, output);
			continue;
		}

		// Compile, assemble, and link
		char *tmp1 = create_tmpfile();
		char *tmp2 = create_tmpfile();
		run_cc1(argc, argv, input, tmp1);
		assemble(tmp1, tmp2);
		strarray_push(&ld_args, tmp2);
		continue;
	}

	// Perform linking if necessary
	if (ld_args.len > 0)
		run_linker(&ld_args, opt_o ? opt_o : "a.out");

	return 0;
}
