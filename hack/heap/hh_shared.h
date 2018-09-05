#ifndef HH_SHARED_H
#define HH_SHARED_H

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>

/*****************************************************************************/
/* Initialization & connection. */
/*****************************************************************************/
/* Initializes the shared heap. */
/* Must be called by the master BEFORE forking the workers! */
CAMLprim value hh_shared_init( value config_val, value shm_dir_val);
/* Must be called after the program is done initializing. We keep the original
 * size of the heap to estimate how often we should garbage collect.
 */
void hh_call_after_init(void);
value hh_check_heap_overflow(void);
/* Must be called by every worker before any operation is performed. */
value hh_connect(value connector, value is_master);

/*****************************************************************************/
/* Heap diagnostics. */
/*****************************************************************************/
CAMLprim value hh_heap_size(void);
CAMLprim value hh_log_level(void);
CAMLprim value hh_hash_used_slots(void);
CAMLprim value hh_hash_slots(void);

/* Provides a counter which increases over the lifetime of the program
 * including all forks. Uses a global until hh_shared_init is called.
 * Safe to use in the early init stages of the program, as long as you fork
 * after hh_shared_init. Wraps around at the maximum value of an ocaml int.
 */
CAMLprim value hh_counter_next(void);

/*****************************************************************************/
/* Worker management. */
/*****************************************************************************/
CAMLprim value hh_stop_workers(void);
CAMLprim value hh_resume_workers(void);
CAMLprim value hh_check_should_exit(void);
CAMLprim value hh_set_can_worker_stop(value val);

/*****************************************************************************/
/* Global storage. */
/*****************************************************************************/
void hh_shared_store(value data);
CAMLprim value hh_shared_load(void);
void hh_shared_clear(void);


/*****************************************************************************/
/* Garbage collection. */
/*****************************************************************************/
CAMLprim value hh_should_collect(value aggressive_val);
CAMLprim value hh_collect(value aggressive_val);

/*****************************************************************************/
/* Deserialization. */
/*****************************************************************************/
/* Deserializes the value pointed by src. */
CAMLprim value hh_deserialize(char *src);
/* Returns the value associated to a given key, and deserialize it. */
/* The key MUST be present. */
CAMLprim value hh_get_and_deserialize(value key);

/*****************************************************************************/
/* Dependency table operations. */
/*****************************************************************************/
void hh_add_dep(value ocaml_dep);
CAMLprim value hh_dep_used_slots(void);
CAMLprim value hh_dep_slots(void);
CAMLprim value hh_get_dep(value ocaml_key);

/*****************************************************************************/
/* Hashtable operations. */
/*****************************************************************************/
/* Returns the size of the value associated to a given key.
 * The key MUST be present.
 */
CAMLprim value hh_get_size(value key);
/* Adds a key/value pair to the hashtable. Returns the number of bytes
 * allocated in the heap, or a negative number if no memory was allocated. */
value hh_add(value key, value data);
/* Returns true if the key is presen in the hashtable. */
value hh_mem(value key);
/* Returns one of {1, -1, -2}.
 *  1 -- key exists and is associated with non-zero data
 * -1 -- key is not present in the hash table at all
 * -2 -- key is present in the hash table but associated with zero-valued data.
 *         This means that the data has been explicitly deleted.
 */
CAMLprim value hh_mem_status(value key);
/* The following operations are only to be performed by the master. */
/* Moves the data associated to key1 to key2.
 * key1 must be present. key2 must be free.
 */
void hh_move(value key1, value key2);
/* Removes a key from the hash table. */
void hh_remove(value key);

/*****************************************************************************/
/* Saved State with SQLite */
/*****************************************************************************/
/* Safe to call outside of sql. */
void hh_cleanup_sqlite(void);
/* Safe to call outside of sql. */
void hh_hashtable_cleanup_sqlite(void);

/* Dependency table. */
CAMLprim value hh_save_dep_table_sqlite(
        value out_filename,
        value build_revision
);
CAMLprim value hh_load_dep_table_sqlite(
        value in_filename,
        value ignore_hh_version
);
CAMLprim value hh_get_dep_sqlite(value ocaml_key);

/* Hash table. */
CAMLprim value hh_save_table_sqlite(value out_filename);
CAMLprim value hh_save_table_keys_sqlite(value out_filename, value keys);
CAMLprim value hh_load_table_sqlite(value in_filename, value verify);
CAMLprim value hh_get_sqlite(value ocaml_key);

/* File information. */
CAMLprim value hh_save_file_info_init(value ml_path);
CAMLprim value hh_save_file_info_free(value ml_unit);
CAMLprim value hh_save_file_info_sqlite(
        value ml_hash,
        value ml_name,
        value ml_kind,
        value ml_filespec
);

#endif
