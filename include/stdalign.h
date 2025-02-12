#ifndef __STDALIGN_H
#define __STDALIGN_H

/**
 * @macro alignas
 * @brief Specifies the alignment of a variable.
 *
 * This macro is used to specify the alignment requirement of a variable.
 * It is equivalent to the `_Alignas` keyword and ensures that the variable
 * is aligned to the specified boundary.
 *
 * Example usage:
 *
 * ```c
 * alignas(16) int arr[4];  // arr is aligned to a 16-byte boundary
 * ```
 */
#define alignas _Alignas

/**
 * @macro alignof
 * @brief Returns the alignment of a type.
 *
 * This macro returns the alignment (in bytes) of the specified type or variable.
 * It is equivalent to the `_Alignof` keyword in C11 and helps in determining
 * the memory alignment of types.
 *
 * Example usage:
 *
 * ```c
 * size_t align = alignof(int);  // returns the alignment of an int type
 * ```
 */
#define alignof _Alignof

/**
 * @def __alignas_is_defined
 * @brief Defines if `alignas` is available.
 *
 * This macro is defined as `1` to indicate that the `alignas` keyword (or macro)
 * is available in the current environment.
 */
#define __alignas_is_defined 1

/**
 * @def __alignof_is_defined
 * @brief Defines if `alignof` is available.
 *
 * This macro is defined as `1` to indicate that the `alignof` keyword (or macro)
 * is available in the current environment.
 */
#define __alignof_is_defined 1

#endif
