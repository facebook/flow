/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#ifndef HH_SHARED_H
#define HH_SHARED_H

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>

/*****************************************************************************/
/* Initialization & connection. */
/*****************************************************************************/
/* Initializes the shared heap. */
/* Must be called by the master BEFORE forking the workers! */
CAMLprim value hh_shared_init(
    value config_val,
    value shm_dir_val,
    value num_workers_val
);
value hh_check_heap_overflow(void);
/* Must be called by every worker before any operation is performed. */
value hh_connect(value connector, value worker_id_val);

/*****************************************************************************/
/* Heap diagnostics. */
/*****************************************************************************/
CAMLprim value hh_used_heap_size(void);
CAMLprim value hh_wasted_heap_size(void);
CAMLprim value hh_log_level(void);
CAMLprim value hh_sample_rate(void);
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
CAMLprim value hh_collect(void);

/*****************************************************************************/
/* Deserialization. */
/*****************************************************************************/
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

#endif
