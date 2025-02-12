#ifndef __STDARG_H
#define __STDARG_H

/**
 * @struct __va_elem
 * @brief Structure to store argument list state.
 *
 * This structure holds the state information necessary for accessing
 * the arguments passed to a variadic function. It contains offsets for
 * general-purpose (gp) and floating-point (fp) registers, and areas
 * to handle overflow arguments.
 */
typedef struct {
	unsigned int	gp_offset;              /**< Offset for general-purpose registers. */
	unsigned int	fp_offset;              /**< Offset for floating-point registers. */
	void *		overflow_arg_area;      /**< Area for overflow arguments. */
	void *		reg_save_area;          /**< Area for saved register values. */
} __va_elem;

/**
 * @typedef va_list
 * @brief Type for variadic argument list.
 *
 * A `va_list` is an array that holds the state of the argument list
 * for a variadic function. It is used with macros like `va_start` and
 * `va_arg` to access function arguments.
 */
typedef __va_elem va_list[1];

/**
 * @macro va_start
 * @brief Initializes a va_list to start iterating over arguments.
 *
 * This macro initializes the `va_list` (`ap`) so that it points to the
 * first argument of the variadic function. The macro is called with the
 * `ap` variable and the last fixed argument of the function (`last`).
 *
 * @param ap A `va_list` variable to hold the state.
 * @param last The last fixed argument in the function, before variadic arguments.
 */
#define va_start(ap, last) \
	do { *(ap) = *(__va_elem *)__va_area__; } while (0)

/**
 * @macro va_end
 * @brief Cleans up a va_list after use.
 *
 * This macro is used to clean up a `va_list` variable after accessing
 * the arguments. It is typically called at the end of a variadic function.
 *
 * @param ap A `va_list` variable to be cleaned up.
 */
#define va_end(ap)

/**
 * @function __va_arg_mem
 * @brief Handles argument retrieval for general cases.
 *
 * This function retrieves an argument from the overflow area (if the
 * argument is too large to fit in registers). It also handles memory
 * alignment by adjusting the pointer to the next argument.
 *
 * @param ap The `va_list` structure holding the argument list state.
 * @param sz The size of the argument to be retrieved.
 * @param align The alignment required for the argument.
 * @return A pointer to the requested argument.
 */
static void *__va_arg_mem(__va_elem *ap, int sz, int align)
{
	void *p = ap->overflow_arg_area;

	// Align to the next memory boundary if necessary.
	if (align > 8)
		p = (p + 15) / 16 * 16;

	ap->overflow_arg_area = ((unsigned long)p + sz + 7) / 8 * 8;
	return p;
}

/**
 * @function __va_arg_gp
 * @brief Retrieves an argument from general-purpose registers.
 *
 * This function retrieves an argument from the general-purpose register area
 * of the `va_list`. If the number of arguments exceeds the available registers,
 * it falls back to using the memory area.
 *
 * @param ap The `va_list` structure holding the argument list state.
 * @param sz The size of the argument to be retrieved.
 * @param align The alignment required for the argument.
 * @return A pointer to the requested argument.
 */
static void *__va_arg_gp(__va_elem *ap, int sz, int align)
{
	if (ap->gp_offset >= 48)
		return __va_arg_mem(ap, sz, align);

	void *r = ap->reg_save_area + ap->gp_offset;
	ap->gp_offset += 8;
	return r;
}

/**
 * @function __va_arg_fp
 * @brief Retrieves an argument from floating-point registers.
 *
 * This function retrieves an argument from the floating-point register area
 * of the `va_list`. If the number of arguments exceeds the available registers,
 * it falls back to using the memory area.
 *
 * @param ap The `va_list` structure holding the argument list state.
 * @param sz The size of the argument to be retrieved.
 * @param align The alignment required for the argument.
 * @return A pointer to the requested argument.
 */
static void *__va_arg_fp(__va_elem *ap, int sz, int align)
{
	if (ap->fp_offset >= 112)
		return __va_arg_mem(ap, sz, align);

	void *r = ap->reg_save_area + ap->fp_offset;
	ap->fp_offset += 8;
	return r;
}

/**
 * @macro va_arg
 * @brief Retrieves the next argument in a variadic function.
 *
 * This macro retrieves the next argument from the `va_list` `ap`.
 * The type of the argument is provided by `ty`. The macro determines
 * whether the argument is located in general-purpose or floating-point
 * registers, or in the overflow area, and fetches it accordingly.
 *
 * @param ap The `va_list` variable holding the state of the argument list.
 * @param ty The type of the argument to retrieve.
 * @return The next argument of type `ty`.
 */
#define va_arg(ap, ty)                                                  \
	({                                                                    \
		int klass = __builtin_reg_class(ty);                               \
		*(ty *)(klass == 0 ? __va_arg_gp(ap, sizeof(ty), _Alignof(ty)) :    \
			klass == 1 ? __va_arg_fp(ap, sizeof(ty), _Alignof(ty)) :    \
			__va_arg_mem(ap, sizeof(ty), _Alignof(ty)));                \
	})

/**
 * @macro va_copy
 * @brief Copies a va_list to another va_list.
 *
 * This macro copies the state of one `va_list` to another, allowing
 * multiple iterations over the arguments in a variadic function.
 *
 * @param dest The destination `va_list` to copy the state to.
 * @param src The source `va_list` from which to copy the state.
 */
#define va_copy(dest, src) ((dest)[0] = (src)[0])

/**
 * @def __GNUC_VA_LIST
 * @brief Defines the GCC-specific va_list type.
 *
 * This macro defines the type `__gnuc_va_list` as an alias for `va_list`,
 * as used in GCC implementations.
 */
#define __GNUC_VA_LIST 1
typedef va_list __gnuc_va_list;

#endif
