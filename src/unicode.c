#include "chibicc.h"

/**
 * @brief Encodes a Unicode code point into a UTF-8 sequence.
 *
 * This function takes a Unicode code point (32-bit integer) and encodes it into one or more bytes
 * in the UTF-8 encoding format. The function supports encoding for all valid Unicode code points
 * (from U+0000 to U+10FFFF).
 *
 * UTF-8 is a variable-length encoding, where code points in different ranges are encoded with a
 * different number of bytes:
 * - 1 byte for code points U+0000 to U+007F
 * - 2 bytes for code points U+0080 to U+07FF
 * - 3 bytes for code points U+0800 to U+FFFF
 * - 4 bytes for code points U+10000 to U+10FFFF
 *
 * The function modifies the provided buffer to hold the encoded UTF-8 bytes and returns the number
 * of bytes used to encode the code point.
 *
 * @param buf A pointer to a buffer where the UTF-8 encoded bytes will be stored. The buffer must
 *            be large enough to hold the result (up to 4 bytes).
 * @param c The Unicode code point to be encoded, represented as a 32-bit unsigned integer.
 *          It should be a valid code point (0x0000 <= c <= 0x10FFFF).
 *
 * @return The number of bytes used to encode the given Unicode code point in UTF-8. The return
 *         value will be between 1 and 4, depending on the size of the code point.
 *
 * @note This function does not check for invalid code points (e.g., those greater than 0x10FFFF or
 *       that are surrogate pairs). It is assumed that the input is a valid Unicode code point.
 *
 * @see https://en.wikipedia.org/wiki/UTF-8
 */
int encode_utf8(char *buf, uint32_t c)
{
	if (c <= 0x7F) {
		buf[0] = c;
		return 1;
	}

	if (c <= 0x7FF) {
		buf[0] = 0b11000000 | (c >> 6);
		buf[1] = 0b10000000 | (c & 0b00111111);
		return 2;
	}

	if (c <= 0xFFFF) {
		buf[0] = 0b11100000 | (c >> 12);
		buf[1] = 0b10000000 | ((c >> 6) & 0b00111111);
		buf[2] = 0b10000000 | (c & 0b00111111);
		return 3;
	}

	buf[0] = 0b11110000 | (c >> 18);
	buf[1] = 0b10000000 | ((c >> 12) & 0b00111111);
	buf[2] = 0b10000000 | ((c >> 6) & 0b00111111);
	buf[3] = 0b10000000 | (c & 0b00111111);
	return 4;
}

/**
 * @brief Decodes a UTF-8 sequence into a Unicode code point.
 *
 * This function takes a pointer to a UTF-8 encoded byte sequence and decodes it into the
 * corresponding Unicode code point (32-bit integer). The function handles the variable-length
 * nature of UTF-8 encoding and returns the decoded code point.
 *
 * UTF-8 encoding can use between 1 and 4 bytes to represent a Unicode code point:
 * - 1 byte for code points U+0000 to U+007F
 * - 2 bytes for code points U+0080 to U+07FF
 * - 3 bytes for code points U+0800 to U+FFFF
 * - 4 bytes for code points U+10000 to U+10FFFF
 *
 * The function verifies that the UTF-8 sequence is valid, and if it is, it decodes the sequence
 * and returns the corresponding Unicode code point. If the sequence is invalid, an error is raised.
 *
 * @param new_pos A pointer to a variable that will be updated with the position of the next byte
 *                after the decoded UTF-8 sequence. This allows for sequential decoding of a string
 *                of UTF-8 characters.
 * @param p A pointer to the start of the UTF-8 sequence to be decoded. This is the input byte sequence.
 *
 * @return The Unicode code point (as a 32-bit unsigned integer) decoded from the UTF-8 sequence.
 *
 * @throws error_at If the UTF-8 sequence is invalid, this function will invoke the `error_at`
 *                  function, reporting the error along with the position of the invalid sequence.
 *
 * @note This function assumes that the input pointer `p` points to a valid UTF-8 sequence,
 *       and that `new_pos` is a valid pointer. The function will throw an error if the sequence
 *       is invalid or if it doesn't match one of the expected byte lengths for UTF-8.
 *
 * @see https://en.wikipedia.org/wiki/UTF-8
 */
uint32_t decode_utf8(char **new_pos, char *p)
{
	if ((unsigned char)*p < 128) {
		*new_pos = p + 1;
		return *p;
	}

	char *start = p;
	int len;
	uint32_t c;

	if ((unsigned char)*p >= 0b11110000) {
		len = 4;
		c = *p & 0b111;
	} else if ((unsigned char)*p >= 0b11100000) {
		len = 3;
		c = *p & 0b1111;
	} else if ((unsigned char)*p >= 0b11000000) {
		len = 2;
		c = *p & 0b11111;
	} else {
		error_at(start, "invalid UTF-8 sequence");
	}

	for (int i = 1; i < len; i++) {
		if ((unsigned char)p[i] >> 6 != 0b10)
			error_at(start, "invalid UTF-8 sequence");
		c = (c << 6) | (p[i] & 0b111111);
	}

	*new_pos = p + len;
	return c;
}

/**
 * @brief Checks if a given value is within any of the specified ranges.
 *
 * This function checks whether a given value (typically a Unicode code point) falls within any of
 * the inclusive ranges defined in the `range` array. The `range` array should contain pairs of values,
 * where each pair represents a start and an end of a range. The function returns `true` if the value
 * `c` is within any of the specified ranges, and `false` otherwise.
 *
 * The range array is expected to be terminated with a value of `-1` to indicate the end of the list
 * of ranges. Each range is represented by two consecutive values in the array:
 * - `range[i]` is the start of the range.
 * - `range[i + 1]` is the end of the range.
 *
 * The function will return `true` if the given value `c` lies inclusively between any of the start
 * and end values of the ranges.
 *
 * @param range A pointer to an array of `uint32_t` values representing pairs of range boundaries.
 *              The array should end with `-1` to mark the end of the ranges.
 * @param c The value to be checked. This is typically a code point or another value to compare
 *          against the specified ranges.
 *
 * @return `true` if the value `c` lies within any of the specified ranges; `false` otherwise.
 *
 * @note The `range` array must be terminated with `-1`, and each range must consist of two consecutive
 *       elements: the start and the end of the range.
 *
 * @see A common use case for this function is checking if a Unicode code point lies within a set of
 *      valid or allowed ranges, such as for character classification or validation.
 */
static bool in_range(uint32_t *range, uint32_t c)
{
	for (int i = 0; range[i] != -1; i += 2)
		if (range[i] <= c && c <= range[i + 1])
			return true;
	return false;
}

/**
 * @brief Checks if a given Unicode code point is a valid first character for an identifier.
 *
 * This function checks if the given Unicode code point `c` is allowed as the first character
 * in an identifier, according to a set of valid Unicode ranges. The ranges include characters
 * from various character sets, including Latin, Greek, and others, that are considered valid
 * for the first character in an identifier, as defined by Unicode and programming language
 * identifier conventions.
 *
 * The function utilizes a predefined list of valid ranges, represented by pairs of start and
 * end values in the `range` array. If the code point `c` falls within any of these ranges,
 * it is considered a valid starting character for an identifier, and the function returns `true`;
 * otherwise, it returns `false`.
 *
 * The valid ranges include:
 * - ASCII letters (`A-Z`, `a-z`), underscore (`_`), and dollar sign (`$`).
 * - Various characters from extended Unicode blocks, including letters from non-Latin alphabets
 *   and symbols used in modern programming languages.
 * - Special Unicode characters like non-breaking space, and others used in identifier formation
 *   in certain languages or scripts.
 *
 * @param c The Unicode code point to be checked. This value is typically a character to be
 *          checked as a possible first character of an identifier.
 *
 * @return `true` if the code point `c` is within one of the valid ranges for the first
 *         character of an identifier; `false` otherwise.
 *
 * @note The function uses a predefined static array of valid ranges for identifier characters.
 *       The ranges are terminated with `-1` to mark the end of the list.
 *
 * @see The list of valid Unicode ranges is based on characters commonly used for identifiers
 *      in various programming languages, including Unicode letters and symbols.
 */
bool is_ident1(uint32_t c)
{
	static uint32_t range[] = {
		'_',	 '_',	  'a',	   'z',	    'A',     'Z',     '$',     '$',
		0x00A8,	 0x00A8,  0x00AA,  0x00AA,  0x00AD,  0x00AD,  0x00AF,  0x00AF,
		0x00B2,	 0x00B5,  0x00B7,  0x00BA,  0x00BC,  0x00BE,  0x00C0,  0x00D6,
		0x00D8,	 0x00F6,  0x00F8,  0x00FF,  0x0100,  0x02FF,  0x0370,  0x167F,
		0x1681,	 0x180D,  0x180F,  0x1DBF,  0x1E00,  0x1FFF,  0x200B,  0x200D,
		0x202A,	 0x202E,  0x203F,  0x2040,  0x2054,  0x2054,  0x2060,  0x206F,
		0x2070,	 0x20CF,  0x2100,  0x218F,  0x2460,  0x24FF,  0x2776,  0x2793,
		0x2C00,	 0x2DFF,  0x2E80,  0x2FFF,  0x3004,  0x3007,  0x3021,  0x302F,
		0x3031,	 0x303F,  0x3040,  0xD7FF,  0xF900,  0xFD3D,  0xFD40,  0xFDCF,
		0xFDF0,	 0xFE1F,  0xFE30,  0xFE44,  0xFE47,  0xFFFD,
		0x10000, 0x1FFFD, 0x20000, 0x2FFFD, 0x30000, 0x3FFFD, 0x40000, 0x4FFFD,
		0x50000, 0x5FFFD, 0x60000, 0x6FFFD, 0x70000, 0x7FFFD, 0x80000, 0x8FFFD,
		0x90000, 0x9FFFD, 0xA0000, 0xAFFFD, 0xB0000, 0xBFFFD, 0xC0000, 0xCFFFD,
		0xD0000, 0xDFFFD, 0xE0000, 0xEFFFD, -1,
	};

	return in_range(range, c);
}

/**
 * @brief Checks if a given Unicode code point is a valid character for an identifier (not the first character).
 *
 * This function checks if the given Unicode code point `c` is allowed as a valid character in an
 * identifier, excluding the first character. It combines two checks:
 * - First, it checks if the code point is a valid first character using the `is_ident1` function.
 * - Then, it checks if the code point falls within a set of additional valid ranges for non-first
 *   identifier characters, such as digits, combining characters, and various symbols used in identifiers.
 *
 * The valid ranges for non-first characters in an identifier include:
 * - Digits (`0-9`) and the dollar sign (`$`).
 * - Combining characters (`0x0300` to `0x036F`).
 * - Various other characters from Unicode blocks used in identifier formation, such as:
 *   - `0x1DC0` to `0x1DFF` (Combining Diacritical Marks Supplement),
 *   - `0x20D0` to `0x20FF` (Combining Marks for Symbols),
 *   - `0xFE20` to `0xFE2F` (Combining Half Marks).
 *
 * The function returns `true` if the code point `c` is valid either as the first character of an
 * identifier (checked by `is_ident1`) or as a valid subsequent character in the identifier.
 *
 * @param c The Unicode code point to be checked. This value is typically a character to be checked
 *          as a possible part of an identifier.
 *
 * @return `true` if the code point `c` is valid as a non-first character of an identifier,
 *         or if it is a valid first character (via `is_ident1`); `false` otherwise.
 *
 * @note The function combines checks from both `is_ident1` (for the first character) and an additional
 *       set of valid ranges for subsequent characters in an identifier. The valid ranges are terminated
 *       with `-1` to mark the end of the list.
 *
 * @see The function `is_ident1` is used to validate the first character of an identifier. The valid
 *      ranges for subsequent characters are defined by a separate list of Unicode code point ranges.
 */
bool is_ident2(uint32_t c)
{
	static uint32_t range[] = {
		'0',	'9',	'$', '$', 0x0300, 0x036F, 0x1DC0, 0x1DFF, 0x20D0, 0x20FF,
		0xFE20, 0xFE2F, -1,
	};

	return is_ident1(c) || in_range(range, c);
}

/**
 * @brief Determines the visual width of a Unicode character.
 *
 * This function computes the display width of a Unicode character `c` based on its code point.
 * The width is calculated according to the general classifications of Unicode characters:
 * - Characters that are classified as "non-printable" or "zero-width" (such as control characters,
 *   diacritical marks, etc.) are considered to have a width of 0.
 * - Characters that are typically displayed in one column (like letters, digits, and common punctuation)
 *   are considered to have a width of 1.
 * - Characters that typically require more space, such as certain characters in East Asian scripts,
 *   or special symbols, are considered to have a width of 2.
 *
 * The function works by checking the Unicode code point `c` against two sets of ranges:
 * - `range1`: Defines code points for characters that should have a width of 0 (e.g., control characters).
 * - `range2`: Defines code points for characters that should have a width of 2 (e.g., full-width characters).
 * - If a character doesn't belong to either set, it is assumed to have a width of 1.
 *
 * @param c The Unicode code point of the character whose width is to be determined.
 *
 * @return The width of the character as an integer:
 *         - `0` for zero-width characters,
 *         - `1` for regular characters that take up a single column,
 *         - `2` for characters that are wider (e.g., full-width characters).
 *
 * @note The function uses two static ranges (`range1` and `range2`) to classify characters into different
 *       width categories. These ranges are stored in arrays of Unicode code points.
 *
 * @see `in_range` is used to check if the character falls within the specified ranges.
 */
static int char_width(uint32_t c)
{
	static uint32_t range1[] = {
		0x0000,	 0x001F,  0x007f,  0x00a0,  0x0300,  0x036F,  0x0483,  0x0486,
		0x0488,	 0x0489,  0x0591,  0x05BD,  0x05BF,  0x05BF,  0x05C1,  0x05C2,
		0x05C4,	 0x05C5,  0x05C7,  0x05C7,  0x0600,  0x0603,  0x0610,  0x0615,
		0x064B,	 0x065E,  0x0670,  0x0670,  0x06D6,  0x06E4,  0x06E7,  0x06E8,
		0x06EA,	 0x06ED,  0x070F,  0x070F,  0x0711,  0x0711,  0x0730,  0x074A,
		0x07A6,	 0x07B0,  0x07EB,  0x07F3,  0x0901,  0x0902,  0x093C,  0x093C,
		0x0941,	 0x0948,  0x094D,  0x094D,  0x0951,  0x0954,  0x0962,  0x0963,
		0x0981,	 0x0981,  0x09BC,  0x09BC,  0x09C1,  0x09C4,  0x09CD,  0x09CD,
		0x09E2,	 0x09E3,  0x0A01,  0x0A02,  0x0A3C,  0x0A3C,  0x0A41,  0x0A42,
		0x0A47,	 0x0A48,  0x0A4B,  0x0A4D,  0x0A70,  0x0A71,  0x0A81,  0x0A82,
		0x0ABC,	 0x0ABC,  0x0AC1,  0x0AC5,  0x0AC7,  0x0AC8,  0x0ACD,  0x0ACD,
		0x0AE2,	 0x0AE3,  0x0B01,  0x0B01,  0x0B3C,  0x0B3C,  0x0B3F,  0x0B3F,
		0x0B41,	 0x0B43,  0x0B4D,  0x0B4D,  0x0B56,  0x0B56,  0x0B82,  0x0B82,
		0x0BC0,	 0x0BC0,  0x0BCD,  0x0BCD,  0x0C3E,  0x0C40,  0x0C46,  0x0C48,
		0x0C4A,	 0x0C4D,  0x0C55,  0x0C56,  0x0CBC,  0x0CBC,  0x0CBF,  0x0CBF,
		0x0CC6,	 0x0CC6,  0x0CCC,  0x0CCD,  0x0CE2,  0x0CE3,  0x0D41,  0x0D43,
		0x0D4D,	 0x0D4D,  0x0DCA,  0x0DCA,  0x0DD2,  0x0DD4,  0x0DD6,  0x0DD6,
		0x0E31,	 0x0E31,  0x0E34,  0x0E3A,  0x0E47,  0x0E4E,  0x0EB1,  0x0EB1,
		0x0EB4,	 0x0EB9,  0x0EBB,  0x0EBC,  0x0EC8,  0x0ECD,  0x0F18,  0x0F19,
		0x0F35,	 0x0F35,  0x0F37,  0x0F37,  0x0F39,  0x0F39,  0x0F71,  0x0F7E,
		0x0F80,	 0x0F84,  0x0F86,  0x0F87,  0x0F90,  0x0F97,  0x0F99,  0x0FBC,
		0x0FC6,	 0x0FC6,  0x102D,  0x1030,  0x1032,  0x1032,  0x1036,  0x1037,
		0x1039,	 0x1039,  0x1058,  0x1059,  0x1160,  0x11FF,  0x135F,  0x135F,
		0x1712,	 0x1714,  0x1732,  0x1734,  0x1752,  0x1753,  0x1772,  0x1773,
		0x17B4,	 0x17B5,  0x17B7,  0x17BD,  0x17C6,  0x17C6,  0x17C9,  0x17D3,
		0x17DD,	 0x17DD,  0x180B,  0x180D,  0x18A9,  0x18A9,  0x1920,  0x1922,
		0x1927,	 0x1928,  0x1932,  0x1932,  0x1939,  0x193B,  0x1A17,  0x1A18,
		0x1B00,	 0x1B03,  0x1B34,  0x1B34,  0x1B36,  0x1B3A,  0x1B3C,  0x1B3C,
		0x1B42,	 0x1B42,  0x1B6B,  0x1B73,  0x1DC0,  0x1DCA,  0x1DFE,  0x1DFF,
		0x200B,	 0x200F,  0x202A,  0x202E,  0x2060,  0x2063,  0x206A,  0x206F,
		0x20D0,	 0x20EF,  0x302A,  0x302F,  0x3099,  0x309A,  0xA806,  0xA806,
		0xA80B,	 0xA80B,  0xA825,  0xA826,  0xFB1E,  0xFB1E,  0xFE00,  0xFE0F,
		0xFE20,	 0xFE23,  0xFEFF,  0xFEFF,  0xFFF9,  0xFFFB,  0x10A01, 0x10A03,
		0x10A05, 0x10A06, 0x10A0C, 0x10A0F, 0x10A38, 0x10A3A, 0x10A3F, 0x10A3F,
		0x1D167, 0x1D169, 0x1D173, 0x1D182, 0x1D185, 0x1D18B, 0x1D1AA, 0x1D1AD,
		0x1D242, 0x1D244, 0xE0001, 0xE0001, 0xE0020, 0xE007F, 0xE0100, 0xE01EF,
		-1,
	};

	if (in_range(range1, c))
		return 0;

	static uint32_t range2[] = {
		0x1100,	 0x115F,  0x2329,  0x2329,  0x232A, 0x232A, 0x2E80,  0x303E,
		0x3040,	 0xA4CF,  0xAC00,  0xD7A3,  0xF900, 0xFAFF, 0xFE10,  0xFE19,
		0xFE30,	 0xFE6F,  0xFF00,  0xFF60,  0xFFE0, 0xFFE6, 0x1F000, 0x1F644,
		0x20000, 0x2FFFD, 0x30000, 0x3FFFD, -1,
	};

	if (in_range(range2, c))
		return 2;
	return 1;
}

/**
 * @brief Calculates the display width of a UTF-8 encoded string.
 *
 * This function calculates the total display width of a string encoded in UTF-8. It takes into account
 * the width of each character based on its Unicode code point. The width of each character is determined
 * by the `char_width` function, which assigns a width of:
 * - `0` for zero-width characters (e.g., control characters),
 * - `1` for regular characters (e.g., letters, digits, punctuation),
 * - `2` for characters that typically take up more space (e.g., full-width characters).
 *
 * The function iterates through the UTF-8 encoded string, decoding each character one by one,
 * and adding the corresponding width to the total width.
 *
 * @param p A pointer to the UTF-8 encoded string whose display width is to be calculated.
 * @param len The length of the UTF-8 encoded string in bytes.
 *
 * @return The total display width of the string as an integer. This is the sum of the widths of all
 *         characters in the string.
 *
 * @note The function assumes that the string is valid UTF-8 and that `decode_utf8` is used to properly
 *       decode each character.
 *
 * @see `char_width` is used to determine the width of each individual character based on its Unicode code point.
 */
int display_width(char *p, int len)
{
	char *start = p;
	int w = 0;

	while (p - start < len) {
		uint32_t c = decode_utf8(&p, p);
		w += char_width(c);
	}

	return w;
}
