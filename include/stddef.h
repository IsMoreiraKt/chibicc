#ifndef __STDDEF_H
#define __STDDEF_H

/**
 * @brief Null pointer constant.
 *
 * This macro defines `NULL`, which is used to represent a null pointer in C.
 * It is typically used when a pointer is intended to not point to any valid memory location.
 * `NULL` is equivalent to `((void *)0)`.
 */
#define NULL ((void *)0)

/**
 * @typedef size_t
 * @brief Type for sizes of objects.
 *
 * `size_t` is used to represent the size of objects in memory. It is returned by functions
 * like `sizeof()` and is used in memory allocation functions such as `malloc()`.
 */
typedef unsigned long size_t;

/**
 * @typedef ptrdiff_t
 * @brief Type for pointer difference.
 *
 * `ptrdiff_t` is used to represent the difference between two pointers, typically the result
 * of subtracting one pointer from another. It is signed because pointer arithmetic can
 * result in negative values.
 */
typedef long ptrdiff_t;

/**
 * @typedef wchar_t
 * @brief Type for wide characters.
 *
 * `wchar_t` is used to represent wide characters, typically used in multi-byte or Unicode
 * character sets. This type can hold a larger set of characters compared to the standard `char`.
 */
typedef unsigned int wchar_t;

/**
 * @typedef max_align_t
 * @brief Type for maximum alignment requirement.
 *
 * `max_align_t` is used to represent the type with the largest alignment requirement.
 * This is often used to ensure that memory is allocated with proper alignment in structures.
 */
typedef long max_align_t;

/**
 * @brief Macro to get the offset of a member within a structure.
 *
 * This macro calculates the offset (in bytes) of a member within a structure. It is commonly
 * used in low-level programming when working with memory layouts of structures.
 *
 * @param type The type of the structure.
 * @param member The member of the structure.
 * @return The offset of the member from the start of the structure.
 */
#define offsetof(type, member) ((size_t)&(((type *)0)->member))

#endif
