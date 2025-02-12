#ifndef __STDBOOL_H
#define __STDBOOL_H

/**
 * @typedef bool
 * @brief Boolean type.
 *
 * The `bool` type is defined as an alias for `_Bool`, which is a built-in
 * type in C99 and later. This provides a standard way of working with
 * Boolean values in C programs.
 */
#define bool _Bool

/**
 * @def true
 * @brief Boolean true constant.
 *
 * The `true` constant is defined as `1`, representing a true value in Boolean logic.
 * It is used to indicate the logical true condition in comparisons and conditions.
 */
#define true 1

/**
 * @def false
 * @brief Boolean false constant.
 *
 * The `false` constant is defined as `0`, representing a false value in Boolean logic.
 * It is used to indicate the logical false condition in comparisons and conditions.
 */
#define false 0

/**
 * @def __bool_true_false_are_defined
 * @brief Ensures the standard Boolean definitions are available.
 *
 * This macro is defined as `1` to indicate that the `bool`, `true`, and `false`
 * definitions are available in the program. It helps to ensure compatibility with
 * C libraries that check for the presence of the Boolean type and constants.
 */
#define __bool_true_false_are_defined 1

#endif
