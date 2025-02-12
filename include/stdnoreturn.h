#ifndef __STDNORETURN_H
#define __STDNORETURN_H

/**
 * @brief Marks functions that do not return.
 *
 * This macro is a convenience definition to mark functions that will not return
 * control to the caller. It is equivalent to `_Noreturn` in C11, providing
 * portability across different compilers and environments.
 *
 * @see _Noreturn
 */
#define noreturn _Noreturn

#endif
