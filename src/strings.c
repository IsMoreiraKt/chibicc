#include "chibicc.h"

/**
 * @brief Adds a string to a dynamic array of strings.
 *
 * This function adds the given string to the end of the dynamic array of strings `StringArray`.
 * If the array has not been allocated yet, it will be initially allocated with a capacity for 8 elements.
 * If the capacity is reached, the array will be reallocated to double its capacity.
 *
 * @param arr A pointer to the dynamic array of strings to be modified.
 * @param s The string to be added to the array.
 */
void strarray_push(StringArray *arr, char *s)
{
	if (!arr->data) {
		arr->data = calloc(8, sizeof(char *));
		arr->capacity = 8;
	}

	if (arr->capacity == arr->len) {
		arr->data = realloc(arr->data, sizeof(char *) * arr->capacity * 2);
		arr->capacity *= 2;
		for (int i = arr->len; i < arr->capacity; i++)
			arr->data[i] = NULL;
	}

	arr->data[arr->len++] = s;
}

/**
 * @brief Formats a string with variable arguments.
 *
 * This function creates a new formatted string, similar to the `printf` function,
 * but returns the result as a dynamically allocated string.
 * It uses the provided format string and associated variable arguments.
 *
 * @param fmt The format string, which may contain specifiers like "%d", "%s", etc.
 * @param ... The variable arguments to be used in the formatting.
 *
 * @return A new string with the formatted content.
 *
 * @note The memory for the returned string should be freed by the user when no longer needed.
 */
char *format(char *fmt, ...)
{
	char *buf;
	size_t buflen;
	FILE *out = open_memstream(&buf, &buflen);

	va_list ap;

	va_start(ap, fmt);
	vfprintf(out, fmt, ap);
	va_end(ap);
	fclose(out);
	return buf;
}
