#include "chibicc.h"

// Initial hash bucket size
#define INIT_SIZE 16

// Rehash if the usage exceeds 70%.
#define HIGH_WATERMARK 70

// Keep the usage below 50% after rehashing.
#define LOW_WATERMARK 50

// Represents a deleted hash entry
#define TOMBSTONE ((void *)-1)

/**
 * @brief FNV-1a hash function.
 *
 * This function computes a hash for a string `s` of a given length.
 * It uses the FNV-1a algorithm, which is a simple and fast hash function.
 *
 * @param s The string to hash.
 * @param len The length of the string.
 * @return The computed hash value.
 */
static uint64_t fnv_hash(char *s, int len)
{
	uint64_t hash = 0xcbf29ce484222325;

	for (int i = 0; i < len; i++) {
		hash *= 0x100000001b3;
		hash ^= (unsigned char)s[i];
	}
	return hash;
}

/**
 * @brief Rehash the hashmap to increase its capacity and reduce usage.
 *
 * If the number of keys exceeds a threshold, this function will resize the hashmap
 * to ensure better performance and keep the usage under the LOW_WATERMARK threshold.
 *
 * @param map A pointer to the hashmap to rehash.
 */
static void rehash(HashMap *map)
{
	int nkeys = 0;

	for (int i = 0; i < map->capacity; i++)
		if (map->buckets[i].key && map->buckets[i].key != TOMBSTONE)
			nkeys++;

	int cap = map->capacity;
	while ((nkeys * 100) / cap >= LOW_WATERMARK)
		cap = cap * 2;
	assert(cap > 0);

	// Create a new hashmap and copy all key-values
	HashMap map2 = {};
	map2.buckets = calloc(cap, sizeof(HashEntry));
	map2.capacity = cap;

	for (int i = 0; i < map->capacity; i++) {
		HashEntry *ent = &map->buckets[i];
		if (ent->key && ent->key != TOMBSTONE)
			hashmap_put2(&map2, ent->key, ent->keylen, ent->val);
	}

	assert(map2.used == nkeys);
	*map = map2;
}

/**
 * @brief Checks if a hash entry matches a given key.
 *
 * Compares a hash entry's key with a provided key and checks if they are equal.
 *
 * @param ent The hash entry to compare.
 * @param key The key to check against.
 * @param keylen The length of the key.
 * @return True if the entry's key matches the provided key.
 */
static bool match(HashEntry *ent, char *key, int keylen)
{
	return ent->key && ent->key != TOMBSTONE &&
	       ent->keylen == keylen && memcmp(ent->key, key, keylen) == 0;
}

/**
 * @brief Retrieves a hash entry from the hashmap.
 *
 * Searches for a given key in the hashmap and returns the corresponding entry.
 *
 * @param map The hashmap to search in.
 * @param key The key to search for.
 * @param keylen The length of the key.
 * @return A pointer to the hash entry if found, NULL otherwise.
 */
static HashEntry *get_entry(HashMap *map, char *key, int keylen)
{
	if (!map->buckets)
		return NULL;

	uint64_t hash = fnv_hash(key, keylen);

	for (int i = 0; i < map->capacity; i++) {
		HashEntry *ent = &map->buckets[(hash + i) % map->capacity];
		if (match(ent, key, keylen))
			return ent;
		if (ent->key == NULL)
			return NULL;
	}
	unreachable();
}

/**
 * @brief Retrieves an existing entry or inserts a new one in the hashmap.
 *
 * This function checks if the given key already exists in the hashmap.
 * If it does, it returns the existing entry. If not, it inserts a new entry.
 * If the hashmap exceeds the HIGH_WATERMARK, a rehash will be triggered.
 *
 * @param map The hashmap to search or insert into.
 * @param key The key to insert or search for.
 * @param keylen The length of the key.
 * @return A pointer to the hash entry (either existing or newly inserted).
 */
static HashEntry *get_or_insert_entry(HashMap *map, char *key, int keylen)
{
	if (!map->buckets) {
		map->buckets = calloc(INIT_SIZE, sizeof(HashEntry));
		map->capacity = INIT_SIZE;
	} else if ((map->used * 100) / map->capacity >= HIGH_WATERMARK) {
		rehash(map);
	}

	uint64_t hash = fnv_hash(key, keylen);

	for (int i = 0; i < map->capacity; i++) {
		HashEntry *ent = &map->buckets[(hash + i) % map->capacity];

		if (match(ent, key, keylen))
			return ent;

		if (ent->key == TOMBSTONE) {
			ent->key = key;
			ent->keylen = keylen;
			return ent;
		}

		if (ent->key == NULL) {
			ent->key = key;
			ent->keylen = keylen;
			map->used++;
			return ent;
		}
	}
	unreachable();
}

/**
 * @brief Retrieves a value associated with a key in the hashmap.
 *
 * This function uses the key's length and the hashmap's hash function to find the key-value pair.
 *
 * @param map The hashmap to search.
 * @param key The key to search for.
 * @return The value associated with the key, or NULL if not found.
 */
void *hashmap_get(HashMap *map, char *key)
{
	return hashmap_get2(map, key, strlen(key));
}

/**
 * @brief Retrieves a value associated with a key in the hashmap, given the key's length.
 *
 * @param map The hashmap to search.
 * @param key The key to search for.
 * @param keylen The length of the key.
 * @return The value associated with the key, or NULL if not found.
 */
void *hashmap_get2(HashMap *map, char *key, int keylen)
{
	HashEntry *ent = get_entry(map, key, keylen);

	return ent ? ent->val : NULL;
}

/**
 * @brief Inserts a key-value pair into the hashmap.
 *
 * This function inserts a new key-value pair or updates the value if the key already exists.
 *
 * @param map The hashmap to insert the pair into.
 * @param key The key to insert or update.
 * @param val The value to associate with the key.
 */
void hashmap_put(HashMap *map, char *key, void *val)
{
	hashmap_put2(map, key, strlen(key), val);
}

/**
 * @brief Inserts a key-value pair into the hashmap, given the key's length.
 *
 * @param map The hashmap to insert the pair into.
 * @param key The key to insert or update.
 * @param keylen The length of the key.
 * @param val The value to associate with the key.
 */
void hashmap_put2(HashMap *map, char *key, int keylen, void *val)
{
	HashEntry *ent = get_or_insert_entry(map, key, keylen);

	ent->val = val;
}

/**
 * @brief Deletes a key-value pair from the hashmap.
 *
 * This function removes the key-value pair from the hashmap and marks the key as a tombstone.
 *
 * @param map The hashmap to delete the pair from.
 * @param key The key to delete.
 */
void hashmap_delete(HashMap *map, char *key)
{
	hashmap_delete2(map, key, strlen(key));
}

/**
 * @brief Deletes a key-value pair from the hashmap, given the key's length.
 *
 * @param map The hashmap to delete the pair from.
 * @param key The key to delete.
 * @param keylen The length of the key.
 */
void hashmap_delete2(HashMap *map, char *key, int keylen)
{
	HashEntry *ent = get_entry(map, key, keylen);

	if (ent)
		ent->key = TOMBSTONE;
}

/**
 * @brief Tests the hashmap functionality with a series of operations.
 *
 * This function performs a series of inserts, deletions, and lookups to ensure the hashmap works correctly.
 */
void hashmap_test(void)
{
	HashMap *map = calloc(1, sizeof(HashMap));

	// Insert keys and values into the hashmap
	for (int i = 0; i < 5000; i++)
		hashmap_put(map, format("key %d", i), (void *)(size_t)i);
	for (int i = 1000; i < 2000; i++)
		hashmap_delete(map, format("key %d", i));
	for (int i = 1500; i < 1600; i++)
		hashmap_put(map, format("key %d", i), (void *)(size_t)i);
	for (int i = 6000; i < 7000; i++)
		hashmap_put(map, format("key %d", i), (void *)(size_t)i);

	// Verify the values in the hashmap
	for (int i = 0; i < 1000; i++)
		assert((size_t)hashmap_get(map, format("key %d", i)) == i);
	for (int i = 1000; i < 1500; i++)
		assert(hashmap_get(map, "no such key") == NULL);
	for (int i = 1500; i < 1600; i++)
		assert((size_t)hashmap_get(map, format("key %d", i)) == i);
	for (int i = 1600; i < 2000; i++)
		assert(hashmap_get(map, "no such key") == NULL);
	for (int i = 2000; i < 5000; i++)
		assert((size_t)hashmap_get(map, format("key %d", i)) == i);
	for (int i = 5000; i < 6000; i++)
		assert(hashmap_get(map, "no such key") == NULL);
	for (int i = 6000; i < 7000; i++)
		hashmap_put(map, format("key %d", i), (void *)(size_t)i);

	assert(hashmap_get(map, "no such key") == NULL);
	printf("OK\n");
}
