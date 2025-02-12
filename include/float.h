#ifndef __STDFLOAT_H
#define __STDFLOAT_H

/**
 * @def DECIMAL_DIG
 * @brief Maximum number of decimal digits that can be reliably represented.
 *
 * This macro defines the number of decimal digits that can be safely used
 * to represent a floating-point number without any loss of precision.
 */
#define DECIMAL_DIG 21

/**
 * @def FLT_EVAL_METHOD
 * @brief Floating-point evaluation method.
 *
 * This macro specifies the floating-point evaluation method:
 * - 0: Evaluate expressions as if computed in the type `float`.
 * - 1: Evaluate expressions as if computed in the type `double`.
 * - 2: Evaluate expressions as if computed in the type `long double`.
 */
#define FLT_EVAL_METHOD 0       // C11 5.2.4.2.2p9

/**
 * @def FLT_RADIX
 * @brief Radix (base) of floating-point representation.
 *
 * This macro defines the base of the floating-point number system. For most systems,
 * this will be 2, meaning that floating-point numbers are represented in binary.
 */
#define FLT_RADIX 2

/**
 * @def FLT_ROUNDS
 * @brief Rounding method used by floating-point operations.
 *
 * This macro defines how rounding should be done:
 * - 0: Toward zero.
 * - 1: To nearest, ties to even.
 * - 2: Toward +∞.
 * - 3: Toward −∞.
 */
#define FLT_ROUNDS 1            // C11 5.2.4.2.2p8: to nearest

// Constants for float type (single precision)

/**
 * @def FLT_DIG
 * @brief Number of decimal digits of precision for float.
 */
#define FLT_DIG 6

/**
 * @def FLT_EPSILON
 * @brief Smallest number that, when added to 1.0, produces a result different from 1.0.
 */
#define FLT_EPSILON 0x1p-23

/**
 * @def FLT_MANT_DIG
 * @brief Number of digits in the mantissa of a float.
 */
#define FLT_MANT_DIG 24

/**
 * @def FLT_MAX
 * @brief Maximum finite value of a float.
 */
#define FLT_MAX 0x1.fffffep+127

/**
 * @def FLT_MAX_10_EXP
 * @brief Maximum exponent for a float in base 10.
 */
#define FLT_MAX_10_EXP 38

/**
 * @def FLT_MAX_EXP
 * @brief Maximum exponent for a float.
 */
#define FLT_MAX_EXP 128

/**
 * @def FLT_MIN
 * @brief Minimum positive normalized value of a float.
 */
#define FLT_MIN 0x1p-126

/**
 * @def FLT_MIN_10_EXP
 * @brief Minimum exponent for a float in base 10.
 */
#define FLT_MIN_10_EXP -37

/**
 * @def FLT_MIN_EXP
 * @brief Minimum exponent for a float.
 */
#define FLT_MIN_EXP -125

/**
 * @def FLT_TRUE_MIN
 * @brief Minimum positive subnormal value of a float.
 */
#define FLT_TRUE_MIN 0x1p-149

// Constants for double type (double precision)

/**
 * @def DBL_DIG
 * @brief Number of decimal digits of precision for double.
 */
#define DBL_DIG 15

/**
 * @def DBL_EPSILON
 * @brief Smallest number that, when added to 1.0, produces a result different from 1.0.
 */
#define DBL_EPSILON 0x1p-52

/**
 * @def DBL_MANT_DIG
 * @brief Number of digits in the mantissa of a double.
 */
#define DBL_MANT_DIG 53

/**
 * @def DBL_MAX
 * @brief Maximum finite value of a double.
 */
#define DBL_MAX 0x1.fffffffffffffp+1023

/**
 * @def DBL_MAX_10_EXP
 * @brief Maximum exponent for a double in base 10.
 */
#define DBL_MAX_10_EXP 308

/**
 * @def DBL_MAX_EXP
 * @brief Maximum exponent for a double.
 */
#define DBL_MAX_EXP 1024

/**
 * @def DBL_MIN
 * @brief Minimum positive normalized value of a double.
 */
#define DBL_MIN 0x1p-1022

/**
 * @def DBL_MIN_10_EXP
 * @brief Minimum exponent for a double in base 10.
 */
#define DBL_MIN_10_EXP -307

/**
 * @def DBL_MIN_EXP
 * @brief Minimum exponent for a double.
 */
#define DBL_MIN_EXP -1021

/**
 * @def DBL_TRUE_MIN
 * @brief Minimum positive subnormal value of a double.
 */
#define DBL_TRUE_MIN 0x0.0000000000001p-1022

// Constants for long double type (extended precision)

/**
 * @def LDBL_DIG
 * @brief Number of decimal digits of precision for long double.
 */
#define LDBL_DIG 15

/**
 * @def LDBL_EPSILON
 * @brief Smallest number that, when added to 1.0, produces a result different from 1.0.
 */
#define LDBL_EPSILON 0x1p-52

/**
 * @def LDBL_MANT_DIG
 * @brief Number of digits in the mantissa of a long double.
 */
#define LDBL_MANT_DIG 53

/**
 * @def LDBL_MAX
 * @brief Maximum finite value of a long double.
 */
#define LDBL_MAX 0x1.fffffffffffffp+1023

/**
 * @def LDBL_MAX_10_EXP
 * @brief Maximum exponent for a long double in base 10.
 */
#define LDBL_MAX_10_EXP 308

/**
 * @def LDBL_MAX_EXP
 * @brief Maximum exponent for a long double.
 */
#define LDBL_MAX_EXP 1024

/**
 * @def LDBL_MIN
 * @brief Minimum positive normalized value of a long double.
 */
#define LDBL_MIN 0x1p-1022

/**
 * @def LDBL_MIN_10_EXP
 * @brief Minimum exponent for a long double in base 10.
 */
#define LDBL_MIN_10_EXP -307

/**
 * @def LDBL_MIN_EXP
 * @brief Minimum exponent for a long double.
 */
#define LDBL_MIN_EXP -1021

/**
 * @def LDBL_TRUE_MIN
 * @brief Minimum positive subnormal value of a long double.
 */
#define LDBL_TRUE_MIN 0x0.0000000000001p-1022

#endif
