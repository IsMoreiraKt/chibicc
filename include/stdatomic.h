#ifndef __STDATOMIC_H
#define __STDATOMIC_H

/**
 * @def ATOMIC_BOOL_LOCK_FREE
 * @brief Indicates that atomic operations on `_Bool` are lock-free.
 *
 * This macro indicates that atomic operations on the `_Bool` type are lock-free.
 */
#define ATOMIC_BOOL_LOCK_FREE 1

/**
 * @def ATOMIC_CHAR_LOCK_FREE
 * @brief Indicates that atomic operations on `char` are lock-free.
 *
 * This macro indicates that atomic operations on the `char` type are lock-free.
 */
#define ATOMIC_CHAR_LOCK_FREE 1

/**
 * @def ATOMIC_CHAR16_T_LOCK_FREE
 * @brief Indicates that atomic operations on `char16_t` are lock-free.
 *
 * This macro indicates that atomic operations on the `char16_t` type are lock-free.
 */
#define ATOMIC_CHAR16_T_LOCK_FREE 1

/**
 * @def ATOMIC_CHAR32_T_LOCK_FREE
 * @brief Indicates that atomic operations on `char32_t` are lock-free.
 *
 * This macro indicates that atomic operations on the `char32_t` type are lock-free.
 */
#define ATOMIC_CHAR32_T_LOCK_FREE 1

/**
 * @def ATOMIC_WCHAR_T_LOCK_FREE
 * @brief Indicates that atomic operations on `wchar_t` are lock-free.
 *
 * This macro indicates that atomic operations on the `wchar_t` type are lock-free.
 */
#define ATOMIC_WCHAR_T_LOCK_FREE 1

/**
 * @def ATOMIC_SHORT_LOCK_FREE
 * @brief Indicates that atomic operations on `short` are lock-free.
 *
 * This macro indicates that atomic operations on the `short` type are lock-free.
 */
#define ATOMIC_SHORT_LOCK_FREE 1

/**
 * @def ATOMIC_INT_LOCK_FREE
 * @brief Indicates that atomic operations on `int` are lock-free.
 *
 * This macro indicates that atomic operations on the `int` type are lock-free.
 */
#define ATOMIC_INT_LOCK_FREE 1

/**
 * @def ATOMIC_LONG_LOCK_FREE
 * @brief Indicates that atomic operations on `long` are lock-free.
 *
 * This macro indicates that atomic operations on the `long` type are lock-free.
 */
#define ATOMIC_LONG_LOCK_FREE 1

/**
 * @def ATOMIC_LLONG_LOCK_FREE
 * @brief Indicates that atomic operations on `long long` are lock-free.
 *
 * This macro indicates that atomic operations on the `long long` type are lock-free.
 */
#define ATOMIC_LLONG_LOCK_FREE 1

/**
 * @def ATOMIC_POINTER_LOCK_FREE
 * @brief Indicates that atomic operations on pointers are lock-free.
 *
 * This macro indicates that atomic operations on pointers are lock-free.
 */
#define ATOMIC_POINTER_LOCK_FREE 1

/**
 * @enum memory_order
 * @brief Memory orderings for atomic operations.
 *
 * Defines the various memory ordering semantics available for atomic operations.
 * These are used to ensure correct ordering of memory operations in concurrent
 * environments.
 */
typedef enum {
	memory_order_relaxed,   /**< No synchronization constraints. */
	memory_order_consume,   /**< Consume ordering (deprecated, treated as acquire). */
	memory_order_acquire,   /**< Acquire memory ordering. */
	memory_order_release,   /**< Release memory ordering. */
	memory_order_acq_rel,   /**< Acquire-release memory ordering. */
	memory_order_seq_cst,   /**< Sequentially consistent memory ordering. */
} memory_order;

/**
 * @def ATOMIC_FLAG_INIT(x)
 * @brief Initializes an atomic flag.
 *
 * Initializes an atomic flag to a given value. The value `x` should be the
 * initial state of the atomic flag (typically `0` or `1`).
 */
#define ATOMIC_FLAG_INIT(x) (x)

/**
 * @macro atomic_init
 * @brief Initializes an atomic variable.
 *
 * This macro initializes an atomic variable at the provided address `addr`
 * with the specified value `val`.
 *
 * @param addr Address of the atomic variable.
 * @param val Value to initialize the atomic variable with.
 */
#define atomic_init(addr, val) (*(addr) = (val))

/**
 * @macro kill_dependency
 * @brief Ensures that memory dependency is handled correctly.
 *
 * This macro ensures that memory dependencies are properly handled, especially
 * in the context of compiler optimizations.
 *
 * @param x Expression to return unchanged.
 */
#define kill_dependency(x) (x)

/**
 * @macro atomic_thread_fence
 * @brief Issues a memory fence to enforce ordering of atomic operations.
 *
 * This macro inserts a memory fence (barrier) according to the specified memory
 * order, ensuring that atomic operations before the fence are completed before
 * those that come after it.
 *
 * @param order The memory order to be used.
 */
#define atomic_thread_fence(order)

/**
 * @macro atomic_signal_fence
 * @brief Issues a signal fence to enforce ordering of atomic operations.
 *
 * This macro inserts a signal fence according to the specified memory order,
 * ensuring that signal-related atomic operations are ordered as per the order.
 *
 * @param order The memory order to be used.
 */
#define atomic_signal_fence(order)

/**
 * @macro atomic_is_lock_free
 * @brief Determines if an atomic operation is lock-free.
 *
 * This macro checks whether atomic operations on a given type are lock-free.
 *
 * @param x Type of the atomic operation.
 * @return `1` if lock-free, otherwise `0`.
 */
#define atomic_is_lock_free(x) 1

/**
 * @macro atomic_load
 * @brief Atomically loads the value of a variable.
 *
 * This macro atomically loads the value from the memory location pointed to by `addr`.
 *
 * @param addr Address of the atomic variable to load.
 * @return The value stored in the atomic variable.
 */
#define atomic_load(addr) (*(addr))

/**
 * @macro atomic_store
 * @brief Atomically stores a value in a variable.
 *
 * This macro atomically stores the value `val` into the memory location pointed
 * to by `addr`.
 *
 * @param addr Address of the atomic variable to store to.
 * @param val Value to store in the atomic variable.
 */
#define atomic_store(addr, val) (*(addr) = (val))

/**
 * @macro atomic_load_explicit
 * @brief Atomically loads the value of a variable with explicit memory ordering.
 *
 * This macro atomically loads the value from the memory location pointed to by `addr`,
 * while respecting the specified memory order `order`.
 *
 * @param addr Address of the atomic variable to load.
 * @param order Memory order to use.
 * @return The value stored in the atomic variable.
 */
#define atomic_load_explicit(addr, order) (*(addr))

/**
 * @macro atomic_store_explicit
 * @brief Atomically stores a value in a variable with explicit memory ordering.
 *
 * This macro atomically stores the value `val` into the memory location pointed
 * to by `addr`, while respecting the specified memory order `order`.
 *
 * @param addr Address of the atomic variable to store to.
 * @param val Value to store in the atomic variable.
 * @param order Memory order to use.
 */
#define atomic_store_explicit(addr, val, order) (*(addr) = (val))

/**
 * @macro atomic_fetch_add
 * @brief Atomically adds a value to a variable.
 *
 * This macro atomically adds `val` to the value at `obj`.
 *
 * @param obj Address of the atomic variable.
 * @param val Value to add.
 * @return The previous value of the atomic variable.
 */
#define atomic_fetch_add(obj, val) (*(obj) += (val))

/**
 * @macro atomic_fetch_sub
 * @brief Atomically subtracts a value from a variable.
 *
 * This macro atomically subtracts `val` from the value at `obj`.
 *
 * @param obj Address of the atomic variable.
 * @param val Value to subtract.
 * @return The previous value of the atomic variable.
 */
#define atomic_fetch_sub(obj, val) (*(obj) -= (val))

/**
 * @macro atomic_fetch_or
 * @brief Atomically performs a bitwise OR on a variable.
 *
 * This macro atomically performs a bitwise OR of `val` with the value at `obj`.
 *
 * @param obj Address of the atomic variable.
 * @param val Value to OR with.
 * @return The previous value of the atomic variable.
 */
#define atomic_fetch_or(obj, val) (*(obj) |= (val))

/**
 * @macro atomic_fetch_xor
 * @brief Atomically performs a bitwise XOR on a variable.
 *
 * This macro atomically performs a bitwise XOR of `val` with the value at `obj`.
 *
 * @param obj Address of the atomic variable.
 * @param val Value to XOR with.
 * @return The previous value of the atomic variable.
 */
#define atomic_fetch_xor(obj, val) (*(obj) ^= (val))

/**
 * @macro atomic_fetch_and
 * @brief Atomically performs a bitwise AND on a variable.
 *
 * This macro atomically performs a bitwise AND of `val` with the value at `obj`.
 *
 * @param obj Address of the atomic variable.
 * @param val Value to AND with.
 * @return The previous value of the atomic variable.
 */
#define atomic_fetch_and(obj, val) (*(obj) &= (val))

/**
 * @macro atomic_compare_exchange_weak
 * @brief Atomically compares and exchanges values with weak memory ordering.
 *
 * This macro atomically compares the value at `p` with `old` and if they are equal,
 * stores the value `new` at `p`.
 *
 * @param p Pointer to the atomic variable.
 * @param old Old value to compare.
 * @param new New value to store.
 * @return `true` if the exchange was successful, `false` otherwise.
 */
#define atomic_compare_exchange_weak(p, old, new) \
	__builtin_compare_and_swap((p), (old), (new))

/**
 * @macro atomic_compare_exchange_strong
 * @brief Atomically compares and exchanges values with strong memory ordering.
 *
 * This macro atomically compares the value at `p` with `old` and if they are equal,
 * stores the value `new` at `p`.
 *
 * @param p Pointer to the atomic variable.
 * @param old Old value to compare.
 * @param new New value to store.
 * @return `true` if the exchange was successful, `false` otherwise.
 */
#define atomic_compare_exchange_strong(p, old, new) \
	__builtin_compare_and_swap((p), (old), (new))

/**
 * @macro atomic_exchange
 * @brief Atomically exchanges values between variables.
 *
 * This macro atomically swaps the value of `obj` with `val`.
 *
 * @param obj Address of the atomic variable.
 * @param val Value to exchange with.
 * @return The previous value of the atomic variable.
 */
#define atomic_exchange(obj, val) __builtin_atomic_exchange((obj), (val))

/**
 * @macro atomic_exchange_explicit
 * @brief Atomically exchanges values with explicit memory ordering.
 *
 * This macro atomically swaps the value of `obj` with `val`, while respecting
 * the specified memory order `order`.
 *
 * @param obj Address of the atomic variable.
 * @param val Value to exchange with.
 * @param order Memory order to use.
 * @return The previous value of the atomic variable.
 */
#define atomic_exchange_explicit(obj, val, order) __builtin_atomic_exchange((obj), (val))

/**
 * @macro atomic_flag_test_and_set
 * @brief Atomically tests and sets a flag.
 *
 * This macro atomically sets the atomic flag `obj` to `1`, and returns the previous value.
 *
 * @param obj Address of the atomic flag.
 * @return The previous value of the atomic flag.
 */
#define atomic_flag_test_and_set(obj) atomic_exchange((obj), 1)

/**
 * @macro atomic_flag_test_and_set_explicit
 * @brief Atomically tests and sets a flag with explicit memory ordering.
 *
 * This macro atomically sets the atomic flag `obj` to `1`, while respecting the
 * specified memory order `order`. It returns the previous value of the atomic flag.
 *
 * @param obj Address of the atomic flag.
 * @param order Memory order to use.
 * @return The previous value of the atomic flag.
 */
#define atomic_flag_test_and_set_explicit(obj, order) atomic_exchange((obj), 1)

/**
 * @macro atomic_flag_clear
 * @brief Atomically clears a flag.
 *
 * This macro atomically clears the atomic flag `obj`, setting it to `0`.
 *
 * @param obj Address of the atomic flag.
 */
#define atomic_flag_clear(obj) (*(obj) = 0)

/**
 * @macro atomic_flag_clear_explicit
 * @brief Atomically clears a flag with explicit memory ordering.
 *
 * This macro atomically clears the atomic flag `obj`, setting it to `0`,
 * while respecting the specified memory order `order`.
 *
 * @param obj Address of the atomic flag.
 * @param order Memory order to use.
 */
#define atomic_flag_clear_explicit(obj, order) (*(obj) = 0)

/**
 * @typedef atomic_flag
 * @brief Atomic flag type.
 *
 * Represents an atomic flag that can be tested and set atomically. Used for
 * implementing lock-free flags in multithreaded programs.
 */
typedef _Atomic _Bool atomic_flag;

/**
 * @typedef atomic_bool
 * @brief Atomic boolean type.
 *
 * Represents an atomic boolean variable that can be modified and accessed atomically.
 */
typedef _Atomic _Bool atomic_bool;

/**
 * @typedef atomic_char
 * @brief Atomic char type.
 *
 * Represents an atomic `char` variable.
 */
typedef _Atomic char atomic_char;

/**
 * @typedef atomic_schar
 * @brief Atomic signed char type.
 *
 * Represents an atomic `signed char` variable.
 */
typedef _Atomic signed char atomic_schar;

/**
 * @typedef atomic_uchar
 * @brief Atomic unsigned char type.
 *
 * Represents an atomic `unsigned char` variable.
 */
typedef _Atomic unsigned char atomic_uchar;

/**
 * @typedef atomic_short
 * @brief Atomic short type.
 *
 * Represents an atomic `short` variable.
 */
typedef _Atomic short atomic_short;

/**
 * @typedef atomic_ushort
 * @brief Atomic unsigned short type.
 *
 * Represents an atomic `unsigned short` variable.
 */
typedef _Atomic unsigned short atomic_ushort;

/**
 * @typedef atomic_int
 * @brief Atomic integer type.
 *
 * Represents an atomic `int` variable.
 */
typedef _Atomic int atomic_int;

/**
 * @typedef atomic_uint
 * @brief Atomic unsigned integer type.
 *
 * Represents an atomic `unsigned int` variable.
 */
typedef _Atomic unsigned int atomic_uint;

/**
 * @typedef atomic_long
 * @brief Atomic long type.
 *
 * Represents an atomic `long` variable.
 */
typedef _Atomic long atomic_long;

/**
 * @typedef atomic_ulong
 * @brief Atomic unsigned long type.
 *
 * Represents an atomic `unsigned long` variable.
 */
typedef _Atomic unsigned long atomic_ulong;

/**
 * @typedef atomic_llong
 * @brief Atomic long long type.
 *
 * Represents an atomic `long long` variable.
 */
typedef _Atomic long long atomic_llong;

/**
 * @typedef atomic_ullong
 * @brief Atomic unsigned long long type.
 *
 * Represents an atomic `unsigned long long` variable.
 */
typedef _Atomic unsigned long long atomic_ullong;

/**
 * @typedef atomic_char16_t
 * @brief Atomic 16-bit character type.
 *
 * Represents an atomic `char16_t` variable.
 */
typedef _Atomic unsigned short atomic_char16_t;

/**
 * @typedef atomic_char32_t
 * @brief Atomic 32-bit character type.
 *
 * Represents an atomic `char32_t` variable.
 */
typedef _Atomic unsigned atomic_char32_t;

/**
 * @typedef atomic_wchar_t
 * @brief Atomic wide character type.
 *
 * Represents an atomic `wchar_t` variable.
 */
typedef _Atomic unsigned atomic_wchar_t;

/**
 * @typedef atomic_int_least8_t
 * @brief Atomic 8-bit signed integer type.
 *
 * Represents an atomic `int_least8_t` variable.
 */
typedef _Atomic signed char atomic_int_least8_t;

/**
 * @typedef atomic_uint_least8_t
 * @brief Atomic 8-bit unsigned integer type.
 *
 * Represents an atomic `uint_least8_t` variable.
 */
typedef _Atomic unsigned char atomic_uint_least8_t;

/**
 * @typedef atomic_int_least16_t
 * @brief Atomic 16-bit signed integer type.
 *
 * Represents an atomic `int_least16_t` variable.
 */
typedef _Atomic short atomic_int_least16_t;

/**
 * @typedef atomic_uint_least16_t
 * @brief Atomic 16-bit unsigned integer type.
 *
 * Represents an atomic `uint_least16_t` variable.
 */
typedef _Atomic unsigned short atomic_uint_least16_t;

/**
 * @typedef atomic_int_least32_t
 * @brief Atomic 32-bit signed integer type.
 *
 * Represents an atomic `int_least32_t` variable.
 */
typedef _Atomic int atomic_int_least32_t;

/**
 * @typedef atomic_uint_least32_t
 * @brief Atomic 32-bit unsigned integer type.
 *
 * Represents an atomic `uint_least32_t` variable.
 */
typedef _Atomic unsigned int atomic_uint_least32_t;

/**
 * @typedef atomic_int_least64_t
 * @brief Atomic 64-bit signed integer type.
 *
 * Represents an atomic `int_least64_t` variable.
 */
typedef _Atomic long atomic_int_least64_t;

/**
 * @typedef atomic_uint_least64_t
 * @brief Atomic 64-bit unsigned integer type.
 *
 * Represents an atomic `uint_least64_t` variable.
 */
typedef _Atomic unsigned long atomic_uint_least64_t;

/**
 * @typedef atomic_int_fast8_t
 * @brief Atomic fast 8-bit signed integer type.
 *
 * Represents an atomic `int_fast8_t` variable.
 */
typedef _Atomic signed char atomic_int_fast8_t;

/**
 * @typedef atomic_uint_fast8_t
 * @brief Atomic fast 8-bit unsigned integer type.
 *
 * Represents an atomic `uint_fast8_t` variable.
 */
typedef _Atomic unsigned char atomic_uint_fast8_t;

/**
 * @typedef atomic_int_fast16_t
 * @brief Atomic fast 16-bit signed integer type.
 *
 * Represents an atomic `int_fast16_t` variable.
 */
typedef _Atomic short atomic_int_fast16_t;

/**
 * @typedef atomic_uint_fast16_t
 * @brief Atomic fast 16-bit unsigned integer type.
 *
 * Represents an atomic `uint_fast16_t` variable.
 */
typedef _Atomic unsigned short atomic_uint_fast16_t;

/**
 * @typedef atomic_int_fast32_t
 * @brief Atomic fast 32-bit signed integer type.
 *
 * Represents an atomic `int_fast32_t` variable.
 */
typedef _Atomic int atomic_int_fast32_t;

/**
 * @typedef atomic_uint_fast32_t
 * @brief Atomic fast 32-bit unsigned integer type.
 *
 * Represents an atomic `uint_fast32_t` variable.
 */
typedef _Atomic unsigned int atomic_uint_fast32_t;

/**
 * @typedef atomic_int_fast64_t
 * @brief Atomic fast 64-bit signed integer type.
 *
 * Represents an atomic `int_fast64_t` variable.
 */
typedef _Atomic long atomic_int_fast64_t;

/**
 * @typedef atomic_uint_fast64_t
 * @brief Atomic fast 64-bit unsigned integer type.
 *
 * Represents an atomic `uint_fast64_t` variable.
 */
typedef _Atomic unsigned long atomic_uint_fast64_t;

/**
 * @typedef atomic_intptr_t
 * @brief Atomic pointer type.
 *
 * Represents an atomic pointer variable.
 */
typedef _Atomic long atomic_intptr_t;

/**
 * @typedef atomic_uintptr_t
 * @brief Atomic unsigned pointer type.
 *
 * Represents an atomic unsigned pointer variable.
 */
typedef _Atomic unsigned long atomic_uintptr_t;

/**
 * @typedef atomic_size_t
 * @brief Atomic size type.
 *
 * Represents an atomic `size_t` variable.
 */
typedef _Atomic unsigned long atomic_size_t;

/**
 * @typedef atomic_ptrdiff_t
 * @brief Atomic pointer difference type.
 *
 * Represents an atomic `ptrdiff_t` variable.
 */
typedef _Atomic long atomic_ptrdiff_t;

/**
 * @typedef atomic_intmax_t
 * @brief Atomic integer max type.
 *
 * Represents an atomic `intmax_t` variable.
 */
typedef _Atomic long atomic_intmax_t;

/**
 * @typedef atomic_uintmax_t
 * @brief Atomic unsigned integer max type.
 *
 * Represents an atomic `uintmax_t` variable.
 */
typedef _Atomic unsigned long atomic_uintmax_t;

#endif
