/**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */

/*****************************************************************************/
/* File Implementing the shared memory system for Hack.
 *
 * THIS CODE ONLY WORKS WITH HACK, IT MAY LOOK LIKE A GENERIC ATOMIC
 * HASHTABLE FOR OCAML: IT IS NOT!
 * BUT ... YOU WERE GOING TO SAY BUT? BUT ...
 * THERE IS NO BUT! DONNY YOU'RE OUT OF YOUR ELEMENT!
 *
 * The lock-free data structures implemented here only work because of how
 * the Hack phases are synchronized.
 *
 * There are 3 kinds of storage implemented in this file.
 * I) The global storage. Used by the master to efficiently transfer a blob
 *    of data to the workers. This is used to share an environment in
 *    read-only mode with all the workers.
 *    The master stores, the workers read.
 *    Only concurrent reads allowed. No concurrent write/read and write/write.
 *    There are a few different OCaml modules that act as interfaces to this
 *    global storage. They all use the same area of memory, so only one can be
 *    active at any one time. The first word indicates the size of the global
 *    storage currently in use; callers are responsible for setting it to zero
 *    once they are done.
 *
 * II) The dependency table. It's a hashtable that contains all the
 *    dependencies between Hack objects. It is filled concurrently by
 *    the workers. The dependency table is made of 2 hashtables, one that
 *    is used to quickly answer if a dependency exists. The other one
 *    to retrieve the list of dependencies associated with an object.
 *    Only the hashes of the objects are stored, so this uses relatively
 *    little memory. No dynamic allocation is required.
 *
 * III) The hashtable that maps string keys to string values. (The strings
 *    are really serialized / marshalled representations of OCaml structures.)
 *    Key observation of the table is that data with the same key are
 *    considered equivalent, and so you can arbitrarily get any copy of it;
 *    furthermore if data is missing it can be recomputed, so incorrectly
 *    saying data is missing when it is being written is only a potential perf
 *    loss. Note that "equivalent" doesn't necessarily mean "identical", e.g.,
 *    two alpha-converted types are "equivalent" though not literally byte-
 *    identical. (That said, I'm pretty sure the Hack typechecker actually does
 *    always write identical data, but the hashtable doesn't need quite that
 *    strong of an invariant.)
 *
 *    The operations implemented, and their limitations:
 *
 *    -) Concurrent writes: SUPPORTED
 *       One will win and the other will get dropped on the floor. There is no
 *       way to tell which happened. Only promise is that after a write, the
 *       one thread which did the write will see data in the table (though it
 *       may be slightly different data than what was written, see above about
 *       equivalent data).
 *
 *    -) Concurrent reads: SUPPORTED
 *       If interleaved with a concurrent write, the read will arbitrarily
 *       say that there is no data at that slot or return the entire new data
 *       written by the concurrent writer.
 *
 *    -) Concurrent removes: NOT SUPPORTED
 *       Only the master can remove, and can only do so if there are no other
 *       concurrent operations (reads or writes).
 *
 *    Since the values are variably sized and can get quite large, they are
 *    stored separately from the hashes in a garbage-collected heap.
 *
 * Both II and III resolve hash collisions via linear probing.
 */
/*****************************************************************************/

/* define CAML_NAME_SPACE to ensure all the caml imports are prefixed with
 * 'caml_' */
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>
#include <caml/intext.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#endif

#include <lz4.h>

#ifndef NO_SQLITE3
#include <sqlite3.h>
#endif

// Ideally these would live in a handle.h file but our internal build system
// can't support that at the moment. These are shared with handle_stubs.c
#ifdef _WIN32
#define Val_handle(fd) (win_alloc_handle(fd))
#else
#define Handle_val(fd) (Long_val(fd))
#define Val_handle(fd) (Val_long(fd))
#endif

#if !defined _CUSTOM_ASSERT_FUNCTIONS_
#define _CUSTOM_ASSERT_FUNCTIONS_
/**
 * Concatenate the __LINE__ and __FILE__ strings in a macro.
 */
#define S1(x) #x
#define S2(x) S1(x)
#define LOCATION __FILE__ " : " S2(__LINE__)
#define assert(f) (f ? 0 : raise_assertion_failure(LOCATION))
#endif

#define HASHTBL_WRITE_IN_PROGRESS ((char*)1)

/****************************************************************************
 * Quoting the linux manpage: memfd_create() creates an anonymous file
 * and returns a file descriptor that refers to it. The file behaves
 * like a regular file, and so can be modified, truncated,
 * memory-mapped, and so on. However, unlike a regular file, it lives
 * in RAM and has a volatile backing storage. Once all references to
 * the file are dropped, it is automatically released. Anonymous
 * memory is used for all backing pages of the file. Therefore, files
 * created by memfd_create() have the same semantics as other
 * anonymous memory allocations such as those allocated using mmap(2)
 * with the MAP_ANONYMOUS flag. The memfd_create() system call first
 * appeared in Linux 3.17.
 ****************************************************************************/
#if !defined __APPLE__ && !defined _WIN32
  // Linux version for the architecture must support syscall memfd_create
  #ifndef SYS_memfd_create
    #if defined(__x86_64__)
      #define SYS_memfd_create 319
    #elif defined(__powerpc64__)
      #define SYS_memfd_create 360
    #elif defined(__aarch64__)
      #define SYS_memfd_create 385
    #else
      #error "hh_shared.c requires a architecture that supports memfd_create"
    #endif
  #endif

  #define MEMFD_CREATE 1
  #include <asm/unistd.h>

  /* Originally this function would call uname(), parse the linux
   * kernel release version and make a decision based on whether
   * the kernel was >= 3.17 or not. However, syscall will return -1
   * with an strerr(errno) of "Function not implemented" if the
   * kernel is < 3.17, and that's good enough.
   */
  static int memfd_create(const char *name, unsigned int flags) {
    return syscall(SYS_memfd_create, name, flags);
  }
#endif

// The following 'typedef' won't be required anymore
// when dropping support for OCaml < 4.03
#ifdef __MINGW64__
typedef unsigned __int32 uint32_t;
typedef unsigned __int64 uint64_t;
#endif

#ifdef _WIN32
static int win32_getpagesize(void) {
  SYSTEM_INFO siSysInfo;
  GetSystemInfo(&siSysInfo);
  return siSysInfo.dwPageSize;
}
#define getpagesize win32_getpagesize
#endif

/*****************************************************************************/
/* Config settings (essentially constants, so they don't need to live in shared
 * memory), initialized in hh_shared_init */
/*****************************************************************************/

/* Convention: .*_b = Size in bytes. */

static size_t global_size_b;
static size_t heap_size;

/* Used for the dependency hashtable */
static uint64_t dep_size;
static size_t dep_size_b;
static size_t bindings_size_b;

/* Used for the shared hashtable */
static uint64_t hashtbl_size;
static size_t hashtbl_size_b;

typedef enum {
  KIND_STRING = 1,
  KIND_SERIALIZED = !KIND_STRING
} storage_kind;

typedef struct {
  // Size of data in the heap
  uint32_t size : 31;
  storage_kind kind : 1;
  // Size of the data stored in the heap after decompression.
  // If the data was not compressed this will be 0
  uint32_t uncompressed_size;
} hh_header_t;

/* Size of where we allocate shared objects. */
#define Get_buf_size(x) (((hh_header_t*)(x))[-1].size + sizeof(hh_header_t))
#define Get_buf(x)      (x - sizeof(hh_header_t))

/* Too lazy to use getconf */
#define CACHE_LINE_SIZE (1 << 6)
#define CACHE_MASK      (~(CACHE_LINE_SIZE - 1))
#define ALIGNED(x)      ((x + CACHE_LINE_SIZE - 1) & CACHE_MASK)

/* Fix the location of our shared memory so we can save and restore the
 * hashtable easily */
#ifdef _WIN32
/* We have to set differently our shared memory location on Windows. */
#define SHARED_MEM_INIT ((char *) 0x48047e00000ll)
#else
#define SHARED_MEM_INIT ((char *)0x500000000000ll)
#endif

/* As a sanity check when loading from a file */
static const uint64_t MAGIC_CONSTANT = 0xfacefacefaceb000ull;

/* The VCS identifier (typically a git hash) of the build */
extern const char* const BuildInfo_kRevision;

/*****************************************************************************/
/* Types */
/*****************************************************************************/

/* Cells of the Hashtable */
typedef struct {
  uint64_t hash;
  char* addr;
} helt_t;

/*****************************************************************************/
/* Globals */
/*****************************************************************************/

/* Total size of allocated shared memory */
static size_t shared_mem_size;

/* Beginning of shared memory */
static char* shared_mem = NULL;

/* ENCODING: The first element is the size stored in bytes, the rest is
 * the data. The size is set to zero when the storage is empty.
 */
static value* global_storage;

/* A pair of a 31-bit unsigned number and a tag bit. */
typedef struct {
  uint32_t num : 31;
  uint32_t tag : 1;
} tagged_uint_t;

/* A deptbl_entry_t is one slot in the deptbl hash table.
 *
 * deptbl maps a 31-bit integer key to a linked list of 31-bit integer values.
 * The key corresponds to a node in a graph and the values correspond to all
 * nodes to which that node has an edge. List order does not matter, and there
 * are no duplicates. Edges are only added, never removed.
 *
 * This data structure, while conceptually simple, is implemented in a
 * complicated way because we store it in shared memory and update it from
 * multiple processes without using any mutexes.  In particular, both the
 * traditional hash table entries and the storage for the linked lists to
 * which they point are stored in the same shared memory array. A tag bit
 * distinguishes the two cases so that hash lookups never accidentally match
 * linked list nodes.
 *
 * Each slot s in deptbl is in one of three states:
 *
 * if s.raw == 0:
 *   empty (the initial state).
 * elif s.key.tag == TAG_KEY:
 *   A traditional hash table entry, where s.key.num is the key
 *   used for hashing/equality and s.next is a "pointer" to a linked
 *   list of all the values for that key, as described below.
 * else (s.key.tag == TAG_VAL):
 *   A node in a linked list of values. s.key.num contains one value and
 *   s.next "points" to the rest of the list, as described below.
 *   Such a slot is NOT matchable by any hash lookup due to the tag bit.
 *
 * To save space, a "next" entry can be one of two things:
 *
 * if next.tag == TAG_NEXT:
 *   next.num is the deptbl slot number of the next node in the linked list
 *   (i.e. just a troditional linked list "next" pointer, shared-memory style).
 * else (next.tag == TAG_VAL):
 *   next.num actually holds the final value in the linked list, rather
 *   than a "pointer" to another entry or some kind of "NULL" sentinel.
 *   This space optimization provides the useful property that each edge
 *   in the graph takes up exactly one slot in deptbl.
 *
 * For example, a mapping from key K to one value V takes one slot S in deptbl:
 *
 *     S = { .key = { K, TAG_KEY }, .val = { V, TAG_VAL } }
 *
 * Mapping K to two values V1 and V2 takes two slots, S1 and S2:
 *
 *     S1 = { .key = { K,  TAG_KEY }, .val = { &S2, TAG_NEXT } }
 *     S2 = { .key = { V1, TAG_VAL }, .val = { V2,  TAG_VAL } }
 *
 * Mapping K to three values V1, V2 and V3 takes three slots:
 *
 *     S1 = { .key = { K,  TAG_KEY }, .val = { &S2, TAG_NEXT } }
 *     S2 = { .key = { V1, TAG_VAL }, .val = { &S3, TAG_NEXT } }
 *     S3 = { .key = { V2, TAG_VAL }, .val = { V3,  TAG_VAL } }
 *
 * ...and so on.
 *
 * You can see that the final node in a linked list always contains
 * two values.
 *
 * As an important invariant, we need to ensure that a non-empty hash table
 * slot can never legally be encoded as all zero bits, because that would look
 * just like an empty slot. How could this happen? Because TAG_VAL == 0,
 * an all-zero slot would look like this:
 *
 *    { .key = { 0, TAG_VAL }, .val = { 0, TAG_VAL } }
 *
 * But fortunately that is impossible -- this entry would correspond to
 * having the same value (0) in the list twice, which is forbidden. Since
 * one of the two values must be nonzero, the entire "raw" uint64_t must
 * be nonzero, and thus distinct from "empty".
 */

enum {
  /* Valid for both the deptbl_entry_t 'key' and 'next' fields. */
  TAG_VAL = 0,

  /* Only valid for the deptbl_entry_t 'key' field (so != TAG_VAL). */
  TAG_KEY = !TAG_VAL,

  /* Only valid for the deptbl_entry_t 'next' field (so != TAG_VAL). */
  TAG_NEXT = !TAG_VAL
};

typedef union {
  struct {
    /* Tag bit is either TAG_KEY or TAG_VAL. */
    tagged_uint_t key;

    /* Tag bit is either TAG_VAL or TAG_NEXT. */
    tagged_uint_t next;
  } s;

  /* Raw 64 bits of this slot. Useful for atomic operations. */
  uint64_t raw;
} deptbl_entry_t;

static deptbl_entry_t* deptbl;
static uint64_t* dcounter;


/* ENCODING:
 * The highest 2 bits are unused.
 * The next 31 bits encode the key the lower 31 bits the value.
 */
static uint64_t* deptbl_bindings;

/* The hashtable containing the shared values. */
static helt_t* hashtbl;
static uint64_t* hcounter;   // the number of slots taken in the table

/* A counter increasing globally across all forks. */
static uintptr_t* counter;

/* Logging level for shared memory statistics
 * 0 = nothing
 * 1 = log totals, averages, min, max bytes marshalled and unmarshalled
 */
static size_t* log_level;

/* This should only be used before forking */
static uintptr_t early_counter = 1;

/* The top of the heap */
static char** heap;

/* Useful to add assertions */
static pid_t* master_pid;
static pid_t my_pid;

static char *db_filename = NULL;

#ifndef NO_SQLITE3
// SQLite DB pointer
static sqlite3 *db = NULL;
#endif

/* Where the heap started (bottom) */
static char* heap_init;
/* Where the heap will end (top) */
static char* heap_max;

/* The size of the heap after initialization of the server */
/* This should only be used by the master */
static size_t heap_init_size = 0;

/* Size of the heap since the last garbage collect*/
static size_t latest_heap_size = 0;

static size_t used_heap_size(void) {
  return *heap - heap_init;
}

void raise_assertion_failure(char * msg) {
  caml_raise_with_string(*caml_named_value("c_assertion_failure"), msg);
}

/* Expose so we can display diagnostics */
CAMLprim value hh_heap_size(void) {
  CAMLparam0();
  CAMLreturn(Val_long(used_heap_size()));
}

CAMLprim value hh_log_level(void) {
  CAMLparam0();
  CAMLreturn(Val_long(*log_level));
}

CAMLprim value hh_hash_used_slots(void) {
  CAMLparam0();
  uint64_t filled_slots = 0;
  uint64_t nonempty_slots = 0;
  uintptr_t i = 0;
  for (i = 0; i < hashtbl_size; ++i) {
    if (hashtbl[i].addr != NULL) {
      filled_slots++;
    }
    if (hashtbl[i].hash != 0) {
      nonempty_slots++;
    }
  }
  assert(nonempty_slots == *hcounter);
  value connector = caml_alloc_tuple(2);
  Field(connector, 0) = Val_long(filled_slots);
  Field(connector, 1) = Val_long(nonempty_slots);

  CAMLreturn(connector);
}

CAMLprim value hh_hash_slots(void) {
  CAMLparam0();
  CAMLreturn(Val_long(hashtbl_size));
}

#ifdef _WIN32

struct timeval log_duration(const char *prefix, struct timeval start_t) {
   return start_t; // TODO
}

#else

struct timeval log_duration(const char *prefix, struct timeval start_t) {
  struct timeval end_t;
  gettimeofday(&end_t, NULL);
  time_t secs = end_t.tv_sec - start_t.tv_sec;
  suseconds_t usecs = end_t.tv_usec - start_t.tv_usec;
  double time_taken = secs + ((double)usecs / 1000000);
  fprintf(stderr, "%s took %.2lfs\n", prefix, time_taken);
  return end_t;
}

#endif

#ifdef _WIN32

static HANDLE memfd;

/**************************************************************************
 * We create an anonymous memory file, whose `handle` might be
 * inherited by slave processes.
 *
 * This memory file is tagged "reserved" but not "committed". This
 * means that the memory space will be reserved in the virtual memory
 * table but the pages will not be bound to any physical memory
 * yet. Further calls to 'VirtualAlloc' will "commit" pages, meaning
 * they will be bound to physical memory.
 *
 * This is behavior that should reflect the 'MAP_NORESERVE' flag of
 * 'mmap' on Unix. But, on Unix, the "commit" is implicit.
 *
 * Committing the whole shared heap at once would require the same
 * amount of free space in memory (or in swap file).
 **************************************************************************/
void memfd_init(char *shm_dir, size_t shared_mem_size, uint64_t minimum_avail) {
  memfd = CreateFileMapping(
    INVALID_HANDLE_VALUE,
    NULL,
    PAGE_READWRITE | SEC_RESERVE,
    shared_mem_size >> 32, shared_mem_size & ((1ll << 32) - 1),
    NULL);
  if (memfd == NULL) {
    win32_maperr(GetLastError());
    uerror("CreateFileMapping", Nothing);
  }
  if (!SetHandleInformation(memfd, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT)) {
    win32_maperr(GetLastError());
    uerror("SetHandleInformation", Nothing);
  }
}

#else

static int memfd = -1;

static void raise_failed_anonymous_memfd_init() {
  static value *exn = NULL;
  if (!exn) exn = caml_named_value("failed_anonymous_memfd_init");
  caml_raise_constant(*exn);
}

static void raise_less_than_minimum_available(uint64_t avail) {
  value arg;
  static value *exn = NULL;
  if (!exn) exn = caml_named_value("less_than_minimum_available");
  arg = Val_long(avail);
  caml_raise_with_arg(*exn, arg);
}

#include <sys/statvfs.h>
void assert_avail_exceeds_minimum(char *shm_dir, uint64_t minimum_avail) {
  struct statvfs stats;
  uint64_t avail;
  if (statvfs(shm_dir, &stats)) {
    uerror("statvfs", caml_copy_string(shm_dir));
  }
  avail = stats.f_bsize * stats.f_bavail;
  if (avail < minimum_avail) {
    raise_less_than_minimum_available(avail);
  }
}

/**************************************************************************
 * The memdfd_init function creates a anonymous memory file that might
 * be inherited by `Daemon.spawned` processus (contrary to a simple
 * anonymous mmap).
 *
 * The preferred mechanism is memfd_create(2) (see the upper
 * description).  Then we try shm_open(2) (on Apple OS X). As a safe fallback,
 * we use `mkstemp/unlink`.
 *
 * mkstemp is preferred over shm_open on Linux as it allows to
 * choose another directory that `/dev/shm` on system where this
 * partition is too small (e.g. the Travis containers).
 *
 * The resulting file descriptor should be mmaped with the memfd_map
 * function (see below).
 ****************************************************************************/
void memfd_init(char *shm_dir, size_t shared_mem_size, uint64_t minimum_avail) {
  if (shm_dir == NULL) {
    // This means that we should try to use the anonymous-y system calls
#if defined(MEMFD_CREATE)
    memfd = memfd_create("fb_heap", 0);
#endif
#if defined(__APPLE__)
    if (memfd < 0) {
      char memname[255];
      snprintf(memname, sizeof(memname), "/fb_heap.%d", getpid());
      // the ftruncate below will fail with errno EINVAL if you try to
      // ftruncate the same sharedmem fd more than once. We're seeing this in
      // some tests, which might imply that two flow processes with the same
      // pid are starting up. This shm_unlink should prevent that from
      // happening. Here's a stackoverflow about it
      // http://stackoverflow.com/questions/25502229/ftruncate-not-working-on-posix-shared-memory-in-mac-os-x
      shm_unlink(memname);
      memfd = shm_open(memname, O_CREAT | O_RDWR, 0666);
      if (memfd < 0) {
          uerror("shm_open", Nothing);
      }

      // shm_open sets FD_CLOEXEC automatically. This is undesirable, because
      // we want this fd to be open for other processes, so that they can
      // reconnect to the shared memory.
      int fcntl_flags = fcntl(memfd, F_GETFD);
      if (fcntl_flags == -1) {
        printf("Error with fcntl(memfd): %s\n", strerror(errno));
        uerror("fcntl", Nothing);
      }
      // Unset close-on-exec
      fcntl(memfd, F_SETFD, fcntl_flags & ~FD_CLOEXEC);
    }
#endif
    if (memfd < 0) {
      raise_failed_anonymous_memfd_init();
    }
  } else {
    assert_avail_exceeds_minimum(shm_dir, minimum_avail);
    if (memfd < 0) {
      char template[1024];
      if (!snprintf(template, 1024, "%s/fb_heap-XXXXXX", shm_dir)) {
        uerror("snprintf", Nothing);
      };
      memfd = mkstemp(template);
      if (memfd < 0) {
        uerror("mkstemp", caml_copy_string(template));
      }
      unlink(template);
    }
  }
  if(ftruncate(memfd, shared_mem_size) == -1) {
    uerror("ftruncate", Nothing);
  }
}

#endif


/*****************************************************************************/
/* Given a pointer to the shared memory address space, initializes all
 * the globals that live in shared memory.
 */
/*****************************************************************************/

#ifdef _WIN32

static char *memfd_map(size_t shared_mem_size) {
  char *mem;
  mem = MapViewOfFileEx(
    memfd,
    FILE_MAP_ALL_ACCESS,
    0, 0, 0,
    (char *)SHARED_MEM_INIT);
  if (mem != SHARED_MEM_INIT) {
    win32_maperr(GetLastError());
    uerror("MapViewOfFileEx", Nothing);
  }
  return mem;
}

#else

static char *memfd_map(size_t shared_mem_size) {
  char *mem;
  /* MAP_NORESERVE is because we want a lot more virtual memory than what
   * we are actually going to use.
   */
  int flags = MAP_SHARED | MAP_NORESERVE | MAP_FIXED;
  int prot  = PROT_READ  | PROT_WRITE;
  mem =
    (char*)mmap((void *)SHARED_MEM_INIT, shared_mem_size, prot,
                flags, memfd, 0);
  if(mem == MAP_FAILED) {
    printf("Error initializing: %s\n", strerror(errno));
    exit(2);
  }
  return mem;
}

#endif

/****************************************************************************
 * The function memfd_reserve force allocation of (mem -> mem+sz) in
 * the shared heap. This is mandatory on Windows. This is optional on
 * Linux but it allows to have explicit "Out of memory" error
 * messages. Otherwise, the kernel might terminate the process with
 * `SIGBUS`.
 ****************************************************************************/


static void raise_out_of_shared_memory()
{
  static value *exn = NULL;
  if (!exn) exn = caml_named_value("out_of_shared_memory");
  caml_raise_constant(*exn);
}

#ifdef _WIN32

/* Reserves memory. This is required on Windows */
static void win_reserve(char * mem, size_t sz) {
  if (!VirtualAlloc(mem, sz, MEM_COMMIT, PAGE_READWRITE)) {
    win32_maperr(GetLastError());
    raise_out_of_shared_memory();
  }
}

/* On Linux, memfd_reserve is only used to reserve memory that is mmap'd to the
 * memfd file. Memory outside of that mmap does not need to be reserved, so we
 * don't call memfd_reserve on things like the temporary mmap used by
 * hh_collect. Instead, they use win_reserve() */
static void memfd_reserve(char * mem, size_t sz) {
  win_reserve(mem, sz);
}

#elif defined(__APPLE__)

/* So OSX lacks fallocate, but in general you can do
 * fcntl(fd, F_PREALLOCATE, &store)
 * however it doesn't seem to work for a shm_open fd, so this function is
 * currently a no-op. This means that our OOM handling for OSX is a little
 * weaker than the other OS's */
static void memfd_reserve(char * mem, size_t sz) {
  (void)mem;
  (void)sz;
}

#else

static void memfd_reserve(char *mem, size_t sz) {
  off_t offset = (off_t)(mem - shared_mem);
  if(posix_fallocate(memfd, offset, sz)) {
    raise_out_of_shared_memory();
  }
}

#endif

static void define_globals(char * shared_mem_init) {
  size_t page_size = getpagesize();
  char *mem = shared_mem_init;

  // Beginning of the shared memory
  shared_mem = mem;

  #ifdef MADV_DONTDUMP
    // We are unlikely to get much useful information out of the shared heap in
    // a core file. Moreover, it can be HUGE, and the extensive work done dumping
    // it once for each CPU can mean that the user will reboot their machine
    // before the much more useful stack gets dumped!
    madvise(shared_mem, shared_mem_size, MADV_DONTDUMP);
  #endif

  /* BEGINNING OF THE SMALL OBJECTS PAGE
   * We keep all the small objects in this page.
   * They are on different cache lines because we modify them atomically.
   */

  /* The pointer to the top of the heap.
   * We will atomically increment *heap every time we want to allocate.
   */
  heap = (char**)mem;

  // The number of elements in the hashtable
  assert(CACHE_LINE_SIZE >= sizeof(uint64_t));
  hcounter = (uint64_t*)(mem + CACHE_LINE_SIZE);

  // The number of elements in the deptable
  assert(CACHE_LINE_SIZE >= sizeof(uint64_t));
  dcounter = (uint64_t*)(mem + 2*CACHE_LINE_SIZE);

  assert (CACHE_LINE_SIZE >= sizeof(uintptr_t));
  counter = (uintptr_t*)(mem + 3*CACHE_LINE_SIZE);

  assert (CACHE_LINE_SIZE >= sizeof(pid_t));
  master_pid = (pid_t*)(mem + 4*CACHE_LINE_SIZE);

  assert (CACHE_LINE_SIZE >= sizeof(size_t));
  log_level = (size_t*)(mem + 5*CACHE_LINE_SIZE);

  mem += page_size;
  // Just checking that the page is large enough.
  assert(page_size > 6*CACHE_LINE_SIZE + (int)sizeof(int));

  /* File name we get in hh_load_dep_table_sqlite needs to be smaller than
   * page_size - it should be since page_size is quite big for a string
   */
  db_filename = (char*)mem;
  memset(mem, 0, page_size);
  mem += page_size;
  /* END OF THE SMALL OBJECTS PAGE */

  /* Global storage initialization */
  global_storage = (value*)mem;
  mem += global_size_b;

  /* Dependencies */
  deptbl = (deptbl_entry_t*)mem;
  mem += dep_size_b;

  deptbl_bindings = (uint64_t*)mem;
  mem += bindings_size_b;

  /* Hashtable */
  hashtbl = (helt_t*)mem;
  mem += hashtbl_size_b;

  /* Heap */
  heap_init = mem;
  heap_max = heap_init + heap_size;

#ifdef _WIN32
  /* Reserve all memory space except the "huge" `global_size_b`. This is
   * required for Windows but we don't do this for Linux since it lets us run
   * more processes in parallel without running out of memory immediately
   * (though we do risk it later on) */
  memfd_reserve((char *)global_storage, sizeof(global_storage[0]));
  memfd_reserve((char *)heap, heap_init - (char *)heap);
#endif

}

/* The total size of the shared memory.  Most of it is going to remain
 * virtual. */
static size_t get_shared_mem_size() {
  size_t page_size = getpagesize();
  return (global_size_b + dep_size_b + bindings_size_b + hashtbl_size_b +
          heap_size + 2 * page_size);
}

static void init_shared_globals(size_t config_log_level) {
  // Initial size is zero for global storage is zero
  global_storage[0] = 0;
  // Initialize the number of element in the table
  *hcounter = 0;
  *dcounter = 0;
  *counter = early_counter + 1;
  *log_level = config_log_level;
  // Initialize top heap pointers
  *heap = heap_init;
}

static void set_sizes(
  uint64_t config_global_size,
  uint64_t config_heap_size,
  uint64_t config_dep_table_pow,
  uint64_t config_hash_table_pow) {

  global_size_b = config_global_size;
  heap_size = config_heap_size;

  dep_size        = 1ul << config_dep_table_pow;
  dep_size_b      = dep_size * sizeof(deptbl[0]);
  bindings_size_b = dep_size * sizeof(deptbl_bindings[0]);
  hashtbl_size    = 1ul << config_hash_table_pow;
  hashtbl_size_b  = hashtbl_size * sizeof(hashtbl[0]);

  shared_mem_size = get_shared_mem_size();
}

/*****************************************************************************/
/* Must be called by the master BEFORE forking the workers! */
/*****************************************************************************/

CAMLprim value hh_shared_init(
  value config_val,
  value shm_dir_val
) {
  CAMLparam2(config_val, shm_dir_val);
  CAMLlocal5(
    connector,
    config_global_size_val,
    config_heap_size_val,
    config_dep_table_pow_val,
    config_hash_table_pow_val
  );

  config_global_size_val = Field(config_val, 0);
  config_heap_size_val = Field(config_val, 1);
  config_dep_table_pow_val = Field(config_val, 2);
  config_hash_table_pow_val = Field(config_val, 3);

  set_sizes(
    Long_val(config_global_size_val),
    Long_val(config_heap_size_val),
    Long_val(config_dep_table_pow_val),
    Long_val(config_hash_table_pow_val)
  );

  // None -> NULL
  // Some str -> String_val(str)
  char *shm_dir = NULL;
  if (shm_dir_val != Val_int(0)) {
    shm_dir = String_val(Field(shm_dir_val, 0));
  }

  memfd_init(
    shm_dir,
    shared_mem_size,
    Long_val(Field(config_val, 5))
  );
  char *shared_mem_init = memfd_map(shared_mem_size);
  define_globals(shared_mem_init);

  // Keeping the pids around to make asserts.
#ifdef _WIN32
  *master_pid = 0;
  my_pid = *master_pid;
#else
  *master_pid = getpid();
  my_pid = *master_pid;
#endif

  init_shared_globals(Long_val(Field(config_val, 6)));
  // Checking that we did the maths correctly.
  assert(*heap + heap_size == shared_mem + shared_mem_size);

#ifndef _WIN32
  // Uninstall ocaml's segfault handler. It's supposed to throw an exception on
  // stack overflow, but we don't actually handle that exception, so what
  // happens in practice is we terminate at toplevel with an unhandled exception
  // and a useless ocaml backtrace. A core dump is actually more useful. Sigh.
  struct sigaction sigact;
  sigact.sa_handler = SIG_DFL;
  sigemptyset(&sigact.sa_mask);
  sigact.sa_flags = 0;
  sigaction(SIGSEGV, &sigact, NULL);
#endif

  connector = caml_alloc_tuple(5);
  Field(connector, 0) = Val_handle(memfd);
  Field(connector, 1) = config_global_size_val;
  Field(connector, 2) = config_heap_size_val;
  Field(connector, 3) = config_dep_table_pow_val;
  Field(connector, 4) = config_hash_table_pow_val;

  CAMLreturn(connector);
}

/* Must be called by every worker before any operation is performed */
value hh_connect(value connector, value is_master) {
  CAMLparam2(connector, is_master);
  memfd = Handle_val(Field(connector, 0));
  set_sizes(
    Long_val(Field(connector, 1)),
    Long_val(Field(connector, 2)),
    Long_val(Field(connector, 3)),
    Long_val(Field(connector, 4))
  );
#ifdef _WIN32
  my_pid = 1; // Trick
#else
  my_pid = getpid();
#endif
  char *shared_mem_init = memfd_map(shared_mem_size);
  define_globals(shared_mem_init);

  if (Bool_val(is_master)) {
    *master_pid = my_pid;
  }

  CAMLreturn(Val_unit);
}

/*****************************************************************************/
/* Counter
 *
 * Provides a counter intended to be increasing over the lifetime of the program
 * including all forks. Uses a global variable until hh_shared_init is called,
 * so it's safe to use in the early init stages of the program (as long as you
 * fork after hh_shared_init of course). Wraps around at the maximum value of an
 * ocaml int, which is something like 30 or 62 bits on 32 and 64-bit
 * architectures respectively.
 */
/*****************************************************************************/

CAMLprim value hh_counter_next(void) {
  CAMLparam0();
  CAMLlocal1(result);

  uintptr_t v;
  if (counter) {
    v = __sync_fetch_and_add(counter, 1);
  } else {
    v = ++early_counter;
  }

  result = Val_long(v % Max_long); // Wrap around.
  CAMLreturn(result);
}

/*****************************************************************************/
/* There are a bunch of operations that only the designated master thread is
 * allowed to do. This assert will fail if the current process is not the master
 * process
 */
/*****************************************************************************/
void assert_master() {
  assert(my_pid == *master_pid);
}

/*****************************************************************************/

/*****************************************************************************/
/* Global storage */
/*****************************************************************************/

void hh_shared_store(value data) {
  CAMLparam1(data);
  size_t size = caml_string_length(data);

  assert_master();                               // only the master can store
  assert(global_storage[0] == 0);                // Is it clear?
  assert(size < global_size_b - sizeof(value));  // Do we have enough space?

  global_storage[0] = size;
  memfd_reserve((char *)&global_storage[1], size);
  memcpy(&global_storage[1], &Field(data, 0), size);

  CAMLreturn0;
}

/*****************************************************************************/
/* We are allocating ocaml values. The OCaml GC must know about them.
 * caml_alloc_string might trigger the GC, when that happens, the GC needs
 * to scan the stack to find the OCaml roots. The macros CAMLparam0 and
 * CAMLlocal1 register the roots.
 */
/*****************************************************************************/

CAMLprim value hh_shared_load(void) {
  CAMLparam0();
  CAMLlocal1(result);

  size_t size = global_storage[0];
  assert(size != 0);
  result = caml_alloc_string(size);
  memcpy(&Field(result, 0), &global_storage[1], size);

  CAMLreturn(result);
}

void hh_shared_clear(void) {
  assert_master();
  global_storage[0] = 0;
}

/*****************************************************************************/
/* Dependencies */
/*****************************************************************************/

static void raise_dep_table_full() {
  static value *exn = NULL;
  if (!exn) exn = caml_named_value("dep_table_full");
  caml_raise_constant(*exn);
}

/*****************************************************************************/
/* Hashes an integer such that the low bits are a good starting hash slot. */
/*****************************************************************************/
static uint64_t hash_uint64(uint64_t n) {
  // Multiplying produces a well-mixed value in the high bits of the result.
  // The bswap moves those "good" high bits into the low bits, to serve as the
  // initial hash table slot number.
  const uint64_t golden_ratio = 0x9e3779b97f4a7c15ull;
  return __builtin_bswap64(n * golden_ratio);
}


/*****************************************************************************/
/* This code is very perf sensitive, please check the performance before
 * modifying.
 * The table contains key/value bindings encoded in a word.
 * The higher bits represent the key, the lower ones the value.
 * Each key/value binding is unique.
 * Concretely, if you try to add a key/value pair that is already in the table
 * the data structure is left unmodified.
 *
 * Returns 1 if the dep did not previously exist, else 0.
 */
/*****************************************************************************/
static int add_binding(uint64_t value) {
  volatile uint64_t* const table = deptbl_bindings;

  size_t slot = (size_t)hash_uint64(value) & (dep_size - 1);

  while(1) {
    /* It considerably speeds things up to do a normal load before trying using
     * an atomic operation.
     */
    uint64_t slot_val = table[slot];

    // The binding exists, done!
    if(slot_val == value)
      return 0;

    if (*dcounter >= dep_size) {
      raise_dep_table_full();
    }

    // The slot is free, let's try to take it.
    if(slot_val == 0) {
      // See comments in hh_add about its similar construction here.
      if(__sync_bool_compare_and_swap(&table[slot], 0, value)) {
        uint64_t size = __sync_fetch_and_add(dcounter, 1);
        // Sanity check
        assert(size <= dep_size);
        return 1;
      }

      if(table[slot] == value) {
        return 0;
      }
    }

    slot = (slot + 1) & (dep_size - 1);
  }
}

/*****************************************************************************/
/* Allocates a linked list node in deptbl holding the given value, and returns
 * the slot number where it was stored. The caller is responsible for filling
 * in its "next" field, which starts out in an invalid state.
 */
/*****************************************************************************/
static uint32_t alloc_deptbl_node(uint32_t key, uint32_t val) {
  volatile deptbl_entry_t* const table = deptbl;

  // We can allocate this node in any free slot in deptbl, because
  // linked list nodes are only findable from another slot which
  // explicitly specifies its index in the 'next' field. The caller will
  // initialize such a field using the slot number this function returns.
  //
  // Since we know the pair (key, val) is unique, we hash them together to
  // pick a good "random" starting point to scan for a free slot. But
  // we could start anywhere.
  uint64_t start_hint = hash_uint64(((uint64_t)key << 31) | val);

  // Linked list node to create. Its "next" field will get set by the caller.
  const deptbl_entry_t list_node = { { { val, TAG_VAL }, { ~0, TAG_NEXT } } };

  uint32_t slot;
  for (slot = (uint32_t)start_hint; ; ++slot) {
    slot &= dep_size - 1;

    if (table[slot].raw == 0 &&
        __sync_bool_compare_and_swap(&table[slot].raw, 0, list_node.raw)) {
      return slot;
    }
  }
}

/*****************************************************************************/
/* Prepends 'val' to the linked list of values associated with 'key'.
 * Assumes 'val' is not already in that list, a property guaranteed by the
 * deptbl_bindings pre-check.
 */
/*****************************************************************************/
static void prepend_to_deptbl_list(uint32_t key, uint32_t val) {
  volatile deptbl_entry_t* const table = deptbl;

  size_t slot;
  for (slot = (size_t)hash_uint64(key); ; ++slot) {
    slot &= dep_size - 1;

    deptbl_entry_t slotval = table[slot];

    if (slotval.raw == 0) {
      // Slot is empty. Try to create a new linked list head here.

      deptbl_entry_t head = { { { key, TAG_KEY }, { val, TAG_VAL } } };
      slotval.raw = __sync_val_compare_and_swap(&table[slot].raw, 0, head.raw);

      if (slotval.raw == 0) {
        // The CAS succeeded, we are done.
        break;
      }

      // slotval now holds whatever some racing writer put there.
    }

    if (slotval.s.key.num == key && slotval.s.key.tag == TAG_KEY) {
      // A list for this key already exists. Prepend to it by chaining
      // our new linked list node to whatever the head already points to
      // then making the head point to our node.
      //
      // The head can of course change if someone else prepends first, in
      // which case we'll retry. This is just the classic "atomic push onto
      // linked list stock" algarithm.

      // Allocate a linked list node to prepend to the list.
      uint32_t list_slot = alloc_deptbl_node(key, val);

      // The new head is going to point to our node as the first entry.
      deptbl_entry_t head = { { { key, TAG_KEY }, { list_slot, TAG_NEXT } } };

      while (1) {
        // Update our new linked list node, which no one can see yet, to
        // point to the current list head.
        table[list_slot].s.next = slotval.s.next;

        // Try to atomically set the new list head to be our node.
        uint64_t old = slotval.raw;
        slotval.raw =
          __sync_val_compare_and_swap(&table[slot].raw, old, head.raw);
        if (slotval.raw == old) {
          // The CAS succeeded, we are done.
          break;
        }
      }

      break;
    }
  }
}


/* Record an edge from key -> val. Does nothing if one already exists. */
static void add_dep(uint32_t key, uint32_t val) {
  // Both key and val must be 31-bit integers, since we use tag bits.
  assert(key < 0x80000000 && val < 0x80000000);

  if (add_binding(((uint64_t)key << 31) | val)) {
    prepend_to_deptbl_list(key, val);
  }
}

void hh_add_dep(value ocaml_dep) {
  CAMLparam1(ocaml_dep);
  uint64_t dep = Long_val(ocaml_dep);
  add_dep((uint32_t)(dep >> 31), (uint32_t)(dep & 0x7FFFFFFF));
  CAMLreturn0;
}

CAMLprim value hh_dep_used_slots(void) {
  CAMLparam0();
  uint64_t count = 0;
  uintptr_t slot = 0;
  for (slot = 0; slot < dep_size; ++slot) {
    if (deptbl[slot].raw != 0) {
      count++;
    }
  }
  CAMLreturn(Val_long(count));
}

CAMLprim value hh_dep_slots(void) {
  CAMLparam0();
  CAMLreturn(Val_long(dep_size));
}

/* Given a key, returns the list of values bound to it. */
CAMLprim value hh_get_dep(value ocaml_key) {
  CAMLparam1(ocaml_key);
  CAMLlocal2(result, cell);

  volatile deptbl_entry_t* const table = deptbl;

  // The caller is required to pass a 32-bit node ID.
  const uint64_t key64 = Long_val(ocaml_key);
  const uint32_t key = (uint32_t)key64;
  assert((key & 0x7FFFFFFF) == key64);

  result = Val_int(0); // The empty list

  size_t slot;
  for (slot = (size_t)hash_uint64(key); ; ++slot) {
    slot &= dep_size - 1;

    deptbl_entry_t slotval = table[slot];

    if (slotval.raw == 0) {
      // There are no entries associated with this key.
      break;
    }

    if (slotval.s.key.num == key && slotval.s.key.tag == TAG_KEY) {
      // We found the list for 'key', so walk it.

      while (slotval.s.next.tag == TAG_NEXT) {
        assert(slotval.s.next.num < dep_size);
        slotval = table[slotval.s.next.num];

        cell = caml_alloc_tuple(2);
        Field(cell, 0) = Val_long(slotval.s.key.num);
        Field(cell, 1) = result;
        result = cell;
      }

      // The tail of the list is special, "next" is really a value.
      cell = caml_alloc_tuple(2);
      Field(cell, 0) = Val_long(slotval.s.next.num);
      Field(cell, 1) = result;
      result = cell;

      // We are done!
      break;
    }
  }

  CAMLreturn(result);
}

/*****************************************************************************/
/* Garbage collector */
/*****************************************************************************/

/** Wrappers around mmap/munmap */

#ifdef _WIN32

static char *temp_memory_map() {
  char *tmp_heap;
  tmp_heap = VirtualAlloc(NULL, heap_size, MEM_RESERVE, PAGE_READWRITE);
  if (!tmp_heap) {
    win32_maperr(GetLastError());
    uerror("VirtualAlloc2", Nothing);
  }
  return tmp_heap;
}

static void temp_memory_unmap(char * tmp_heap) {
  if(!VirtualFree(tmp_heap, 0, MEM_RELEASE)) {
    win32_maperr(GetLastError());
    uerror("VirtualFree", Nothing);
  }
}

#else

static char *temp_memory_map() {
  char *tmp_heap;
  int flags       = MAP_PRIVATE | MAP_ANON | MAP_NORESERVE;
  int prot        = PROT_READ | PROT_WRITE;
  tmp_heap = (char*)mmap(NULL, heap_size, prot, flags, 0, 0);
  if(tmp_heap == MAP_FAILED) {
    printf("Error while collecting: %s\n", strerror(errno));
    exit(2);
  }
  return tmp_heap;
}

static void temp_memory_unmap(char * tmp_heap) {
  if(munmap(tmp_heap, heap_size) == -1) {
    printf("Error while collecting: %s\n", strerror(errno));
    exit(2);
  }
}

#endif

/*****************************************************************************/
/* Must be called after the hack server is done initializing.
 * We keep the original size of the heap to estimate how often we should
 * garbage collect.
 */
/*****************************************************************************/
void hh_call_after_init() {
  CAMLparam0();
  heap_init_size = used_heap_size();
  latest_heap_size = heap_init_size;
  if(2 * heap_init_size >= heap_size) {
    caml_failwith("Heap init size is too close to max heap size; "
      "GC will never get triggered!");
  }
  CAMLreturn0;
}

value hh_check_heap_overflow() {
  if (*heap >= shared_mem + shared_mem_size) {
    return Val_bool(1);
  }
  return Val_bool(0);
}

/*****************************************************************************/
/* We compact the heap when it gets twice as large as its initial size.
 * Step one, copy the live values in a new heap.
 * Step two, memcopy the values back into the shared heap.
 * We could probably use something smarter, but this is fast enough.
 *
 * The collector should only be called by the master.
 */
/*****************************************************************************/
void hh_collect(value aggressive_val) {
  int aggressive  = Bool_val(aggressive_val);
  char* tmp_heap, *dest;
  size_t mem_size = 0;

  float space_overhead = aggressive ? 1.2 : 2.0;
  size_t used = used_heap_size();
  if(used < (size_t)(space_overhead * latest_heap_size)) {
    // We have not grown past twice the size since we last gc'd

    /* We maintain the invariant that the latest_heap_size
      is at most the current heap size, to make sure the GC
      always collects */
    if (used < latest_heap_size) {
      latest_heap_size = used;
    }
    return;
  }

  tmp_heap = temp_memory_map();
  dest = tmp_heap;
  assert_master(); // Comes from the master

  // Walking the table
  size_t i;
  for(i = 0; i < hashtbl_size; i++) {
    if(hashtbl[i].addr != NULL) { // Found a non empty slot
      // No workers should be writing at the moment. If a worker died in the
      // middle of a write, that is also very bad
      assert(hashtbl[i].addr != HASHTBL_WRITE_IN_PROGRESS);

      size_t bl_size      = Get_buf_size(hashtbl[i].addr);
      size_t aligned_size = ALIGNED(bl_size);
      char* addr          = Get_buf(hashtbl[i].addr);

#ifdef _WIN32
      win_reserve(dest, bl_size);
#endif
      memcpy(dest, addr, bl_size);
      // This is where the data ends up after the copy
      hashtbl[i].addr = heap_init + mem_size + sizeof(hh_header_t);
      dest     += aligned_size;
      mem_size += aligned_size;
    }
  }

  // Copying the result back into shared memory
  memcpy(heap_init, tmp_heap, mem_size);
  *heap = heap_init + mem_size;

  temp_memory_unmap(tmp_heap);
  latest_heap_size = used_heap_size();
}

static void raise_heap_full() {
  static value *exn = NULL;
  if (!exn) exn = caml_named_value("heap_full");
  caml_raise_constant(*exn);
}

/*****************************************************************************/
/* Allocates in the shared heap.
 * The chunks are cache aligned.
 * The word before the chunk address contains the size of the chunk in bytes.
 * The function returns a pointer to the data (the size can be accessed by
 * looking at the address: chunk - sizeof(size_t)).
 */
/*****************************************************************************/

static char* hh_alloc(hh_header_t header) {
  size_t slot_size  = ALIGNED(header.size + sizeof(hh_header_t));
  char* chunk       = __sync_fetch_and_add(heap, (char*)slot_size);
  if (chunk + slot_size > heap_max) {
    raise_heap_full();
  }
  memfd_reserve(chunk, slot_size);
  *((hh_header_t*)chunk) = header;
  return (chunk + sizeof(hh_header_t));
}

/*****************************************************************************/
/* Allocates an ocaml value in the shared heap.
 * Any ocaml value is valid, except closures. It returns the address of
 * the allocated chunk.
 */
/*****************************************************************************/
static char* hh_store_ocaml(value data, /*out*/size_t *alloc_size) {
  char* value;
  size_t size;
  storage_kind kind;

  // If the data is an Ocaml string it is more efficient to copy its contents
  // directly in our heap instead of serializing it.
  if (Is_block(data) && Tag_val(data) == String_tag) {
    value = String_val(data);
    size = caml_string_length(data);
    kind = KIND_STRING;
  } else {
    intnat serialized_size;
    // We are responsible for freeing the memory allocated by this function
    // After copying value into our object heap we need to make sure to free
    // value
    caml_output_value_to_malloc(
      data, Val_int(0)/*flags*/, &value, &serialized_size);

    assert(serialized_size >= 0);
    size = (size_t) serialized_size;
    kind = KIND_SERIALIZED;
  }

  // We limit the size of elements we will allocate to our heap to ~2GB
  assert(size < 0x80000000);
  hh_header_t header = { size, kind, 0 };

  size_t max_compression_size = LZ4_compressBound(header.size);
  char* compressed_data = malloc(max_compression_size);
  size_t compressed_size = LZ4_compress_default(
    value,
    compressed_data,
    header.size,
    max_compression_size);

  if (compressed_size != 0 && compressed_size < header.size) {
    header.uncompressed_size = header.size;
    header.size = compressed_size;
  }

  char* addr = hh_alloc(header);
  memcpy(addr,
         header.uncompressed_size ? compressed_data : value,
         header.size);

  free(compressed_data);
  // We temporarily allocate memory using malloc to serialize the Ocaml object.
  // When we have finished copying the serialized data into our heap we need
  // to free the memory we allocated to avoid a leak.
  if (header.kind == KIND_SERIALIZED) free(value);

  *alloc_size = header.size;
  return addr;
}

/*****************************************************************************/
/* Given an OCaml string, returns the 8 first bytes in an unsigned long.
 * The key is generated using MD5, but we only use the first 8 bytes because
 * it allows us to use atomic operations.
 */
/*****************************************************************************/
static uint64_t get_hash(value key) {
  return *((uint64_t*)String_val(key));
}

/*****************************************************************************/
/* Writes the data in one of the slots of the hashtable. There might be
 * concurrent writers, when that happens, the first writer wins.
 *
 * Returns the number of bytes allocated in the shared heap. If the slot
 * was already written to, a negative value is returned to indicate no new
 * memory was allocated.
 */
/*****************************************************************************/
static value write_at(unsigned int slot, value data) {
  // Try to write in a value to indicate that the data is being written.
  if(hashtbl[slot].addr == NULL &&
     __sync_bool_compare_and_swap(
       &(hashtbl[slot].addr),
       NULL,
       HASHTBL_WRITE_IN_PROGRESS
     )
  ) {
    size_t alloc_size;
    hashtbl[slot].addr = hh_store_ocaml(data, &alloc_size);
    return Val_long(alloc_size);
  }
  return Min_long;
}

static void raise_hash_table_full() {
  static value *exn = NULL;
  if (!exn) exn = caml_named_value("hash_table_full");
  caml_raise_constant(*exn);
}

/*****************************************************************************/
/* Adds a key value to the hashtable. This code is perf sensitive, please
 * check the perf before modifying.
 *
 * Returns the number of bytes allocated into the shared heap, or a negative
 * number if nothing no new memory was allocated.
 */
/*****************************************************************************/
value hh_add(value key, value data) {
  CAMLparam2(key, data);
  uint64_t hash = get_hash(key);
  unsigned int slot = hash & (hashtbl_size - 1);
  unsigned int init_slot = slot;
  while(1) {
    uint64_t slot_hash = hashtbl[slot].hash;

    if(slot_hash == hash) {
      CAMLreturn(write_at(slot, data));
    }

    if (*hcounter >= hashtbl_size) {
      // We're never going to find a spot
      raise_hash_table_full();
    }

    if(slot_hash == 0) {
      // We think we might have a free slot, try to atomically grab it.
      if(__sync_bool_compare_and_swap(&(hashtbl[slot].hash), 0, hash)) {
        uint64_t size = __sync_fetch_and_add(hcounter, 1);
        // Sanity check
        assert(size < hashtbl_size);
        CAMLreturn(write_at(slot, data));
      }

      // Grabbing it failed -- why? If someone else is trying to insert
      // the data we were about to, try to insert it ourselves too.
      // Otherwise, keep going.
      // Note that this read relies on the __sync call above preventing the
      // compiler from caching the value read out of memory. (And of course
      // isn't safe on any arch that requires memory barriers.)
      if(hashtbl[slot].hash == hash) {
        // Some other thread already grabbed this slot to write this
        // key, but they might not have written the address (or even
        // the sigil value) yet. We can't return from hh_add until we
        // know that hh_mem would succeed, which is to say that addr is
        // no longer null. To make sure hh_mem will work, we try
        // writing the value ourselves; either we insert it ourselves or
        // we know the address is now non-NULL.
        CAMLreturn(write_at(slot, data));
      }
    }

    slot = (slot + 1) & (hashtbl_size - 1);
    if (slot == init_slot) {
      // We're never going to find a spot
      raise_hash_table_full();
    }
  }
}

/*****************************************************************************/
/* Finds the slot corresponding to the key in a hash table. The returned slot
 * is either free or points to the key.
 */
/*****************************************************************************/
static unsigned int find_slot(value key) {
  uint64_t hash = get_hash(key);
  unsigned int slot = hash & (hashtbl_size - 1);
  unsigned int init_slot = slot;
  while(1) {
    if(hashtbl[slot].hash == hash) {
      return slot;
    }
    if(hashtbl[slot].hash == 0) {
      return slot;
    }
    slot = (slot + 1) & (hashtbl_size - 1);

    if (slot == init_slot) {
      raise_hash_table_full();
    }
  }
}

/*****************************************************************************/
/* Returns true if the key is present. We need to check both the hash and
 * the address of the data. This is due to the fact that we remove by setting
 * the address slot to NULL (we never remove a hash from the table, outside
 * of garbage collection).
 */
/*****************************************************************************/
value hh_mem(value key) {
  CAMLparam1(key);
  unsigned int slot = find_slot(key);
  if(hashtbl[slot].hash == get_hash(key) &&
     hashtbl[slot].addr != NULL) {
    // The data is currently in the process of being written, wait until it
    // actually is ready to be used before returning.
    while (hashtbl[slot].addr == (char*)1) {
#if defined(__aarch64__) || defined(__powerpc64__)
      asm volatile("yield" : : : "memory");
#else
      asm volatile("pause" : : : "memory");
#endif
    }
    CAMLreturn(Val_bool(1));
  }
  CAMLreturn(Val_bool(0));
}

/*****************************************************************************/
/* Returns the value associated to a given key, and deserialize it. */
/* The key MUST be present. */
/*****************************************************************************/
CAMLprim value hh_get_and_deserialize(value key) {
  CAMLparam1(key);
  CAMLlocal1(result);

  unsigned int slot = find_slot(key);
  assert(hashtbl[slot].hash == get_hash(key));
  hh_header_t header =
    *(hh_header_t*)(hashtbl[slot].addr - sizeof(hh_header_t));

  char* data = hashtbl[slot].addr;
  size_t size = header.size;
  if (header.uncompressed_size) {
    data = malloc(header.uncompressed_size);
    size_t uncompressed_size = LZ4_decompress_safe(
      hashtbl[slot].addr,
      data,
      header.size,
      header.uncompressed_size);
    assert(uncompressed_size == header.uncompressed_size);
    size = uncompressed_size;
  }

  if (header.kind == KIND_STRING) {
    result = caml_alloc_string(size);
    memcpy(String_val(result), data, size);
  } else {
    result = caml_input_value_from_block(data, size);
  }

  if (header.uncompressed_size) {
    free(data);
  }

  CAMLreturn(result);
}

/*****************************************************************************/
/* Returns the size of the value associated to a given key. */
/* The key MUST be present. */
/*****************************************************************************/
CAMLprim value hh_get_size(value key) {
  CAMLparam1(key);

  unsigned int slot = find_slot(key);
  assert(hashtbl[slot].hash == get_hash(key));
  hh_header_t header =
    *(hh_header_t*)(hashtbl[slot].addr - sizeof(hh_header_t));

  CAMLreturn(Long_val(header.size));
}

/*****************************************************************************/
/* Moves the data associated to key1 to key2.
 * key1 must be present.
 * key2 must be free.
 * Only the master can perform this operation.
 */
/*****************************************************************************/
void hh_move(value key1, value key2) {
  unsigned int slot1 = find_slot(key1);
  unsigned int slot2 = find_slot(key2);

  assert_master();
  assert(hashtbl[slot1].hash == get_hash(key1));
  assert(hashtbl[slot2].addr == NULL);
  // We are taking up a previously empty slot. Let's increment the counter.
  if (hashtbl[slot2].hash == 0) {
    __sync_fetch_and_add(hcounter, 1);
  }
  hashtbl[slot2].hash = get_hash(key2);
  hashtbl[slot2].addr = hashtbl[slot1].addr;
  hashtbl[slot1].addr = NULL;
}

/*****************************************************************************/
/* Removes a key from the hash table.
 * Only the master can perform this operation.
 */
/*****************************************************************************/
void hh_remove(value key) {
  unsigned int slot = find_slot(key);

  assert_master();
  assert(hashtbl[slot].hash == get_hash(key));
  hashtbl[slot].addr = NULL;
}

/*****************************************************************************/
/* Saved State */
/*****************************************************************************/

static void fwrite_no_fail(
  const void* ptr, size_t size, size_t nmemb, FILE* fp
) {
  size_t nmemb_written = fwrite(ptr, size, nmemb, fp);
  assert(nmemb_written == nmemb);
}

static void fread_all(void* start, size_t size, FILE* fp) {
  size_t nmemb_read = fread(start, 1, size, fp);
  assert(nmemb_read == size);
}

static void fclose_no_fail(FILE* fp) {
  int status = fclose(fp);
  assert(status == 0);
}

static void fwrite_header(FILE* fp) {
  fwrite_no_fail(&MAGIC_CONSTANT, sizeof MAGIC_CONSTANT, 1, fp);

  size_t revlen = strlen(BuildInfo_kRevision);
  fwrite_no_fail(&revlen, sizeof revlen, 1, fp);
  fwrite_no_fail(BuildInfo_kRevision, sizeof(char), revlen, fp);
}

static void fread_header(FILE* fp) {
  // Verify magic number.
  uint64_t magic = 0;
  fread_all(&magic, sizeof magic, fp);
  assert(magic == MAGIC_CONSTANT);

  // Verify the build revision.
  size_t revlen = 0;
  fread_all(&revlen, sizeof revlen, fp);
  assert(revlen == strlen(BuildInfo_kRevision));

  size_t i;
  for (i = 0; i < revlen; ++i) {
    assert(getc(fp) == BuildInfo_kRevision[i]);
  }
}

/* Writes the dependency graph to a file. The file format looks like:
 *
 * - MAGIC_CONSTANT: 8 bytes
 * - Length of build revision string: size_t
 * - build revision string: [number of bytes given by previous field]
 * - A sequence of (key1, [val1, val2, ...], (key2, [val3, ...])) until EOF:
 *   - Each key is 4 bytes, with tag bit set to TAG_KEY.
 *   - Each val is 4 bytes, with tag bit set to TAG_VAL.
 */
CAMLprim value hh_save_dep_table(value out_filename) {
  CAMLparam1(out_filename);
  struct timeval tv;
  struct timeval tv2;
  gettimeofday(&tv, NULL);

  FILE* fp = fopen(String_val(out_filename), "wb");
  if (fp == NULL) {
    unix_error(errno, "fopen", out_filename);
  }

  fwrite_header(fp);

  size_t slot;
  for (slot = 0; slot < dep_size; ++slot) {
    deptbl_entry_t slotval = deptbl[slot];

    if (slotval.raw != 0 && slotval.s.key.tag == TAG_KEY) {
      // This is the head of a linked list.

      // First write out the key (already tagged with TAG_KEY).
      fwrite_no_fail(&slotval.s.key, sizeof(slotval.s.key), 1, fp);

      // Then write out each value (already tagged with TAG_VAL).
      while (slotval.s.next.tag == TAG_NEXT) {
        assert(slotval.s.next.num < dep_size);
        slotval = deptbl[slotval.s.next.num];
        fwrite_no_fail(&slotval.s.key, sizeof(slotval.s.key), 1, fp);
      }

      // The final "next" in the list is always a value, not a next pointer.
      fwrite_no_fail(&slotval.s.next, sizeof(slotval.s.next), 1, fp);
    }
  }

  fclose_no_fail(fp);

  tv2 = log_duration("Writing dependency file without sqlite", tv);
  int secs = tv2.tv_sec - tv.tv_sec;
  // Reporting only seconds, ignore milli seconds
  CAMLreturn(Val_long(secs));
}

/* Reads a dependency graph from a file. See hh_save_dep_table for a
 * description of the file format.
 */
CAMLprim value hh_load_dep_table(value in_filename) {
  CAMLparam1(in_filename);
  struct timeval tv;
  struct timeval tv2;
  gettimeofday(&tv, NULL);

  FILE* fp = fopen(String_val(in_filename), "rb");

  if (fp == NULL) {
    unix_error(errno, "fopen", in_filename);
  }

  fread_header(fp);

  // Read a sequence of key1, [val1, val2,...], key2, [val3, val4, ...], ...
  // where a tag bit distinguishes the keys from the values, to create
  // edges (key1, val1), (key1, val2), ..., (key2, val3), (key2, val4), ...
  uint32_t key = 0;
  tagged_uint_t n;
  while (fread(&n, sizeof(n), 1, fp) == 1) {
    if (n.tag == TAG_KEY) {
      // Switching to a new key.
      key = n.num;
    } else {
      // Record this key/value pair.
      add_dep(key, n.num);
    }
  }

  // We should have hit EOF and not some other kind of error.
  assert(feof(fp));

  fclose_no_fail(fp);

  tv2 = log_duration("Reading dependency file", tv);
  int secs = tv2.tv_sec - tv.tv_sec;
  // Reporting only seconds, ignore milli seconds
  CAMLreturn(Val_long(secs));
}

/*****************************************************************************/
/* Saved State with SQLite */
/*****************************************************************************/

#ifndef NO_SQLITE3

// Expects the database to be open
static void create_sqlite_header(sqlite3 *db) {
  // Create Header
  const char *sql = "CREATE TABLE HEADER(" \
               "MAGIC_CONSTANT INTEGER PRIMARY KEY NOT NULL," \
               "BUILDINFO TEXT NOT NULL);";

  assert(sqlite3_exec(db, sql, NULL, 0, NULL) == SQLITE_OK);

  // Insert magic constant and build info
  sqlite3_stmt *insert_stmt = NULL;
  sql = "INSERT INTO HEADER (MAGIC_CONSTANT, BUILDINFO) VALUES (?,?)";
  assert(sqlite3_prepare_v2(db, sql, -1, &insert_stmt, NULL) == SQLITE_OK);
  assert(sqlite3_bind_int64(insert_stmt, 1, MAGIC_CONSTANT) == SQLITE_OK);
  assert(sqlite3_bind_text(insert_stmt, 2,
        BuildInfo_kRevision, -1,
        SQLITE_TRANSIENT)
      == SQLITE_OK);
  assert(sqlite3_step(insert_stmt) == SQLITE_DONE);
  assert(sqlite3_finalize(insert_stmt) == SQLITE_OK);
}

// Expects the database to be open
static void verify_sqlite_header(sqlite3 *db) {
  sqlite3_stmt *select_stmt = NULL;
  const char *sql = "SELECT * FROM HEADER;";
  assert(sqlite3_prepare_v2(db, sql, -1, &select_stmt, NULL) == SQLITE_OK);

  if (sqlite3_step(select_stmt) == SQLITE_ROW) {
      // Columns are 0 indexed
      assert(sqlite3_column_int64(select_stmt, 0) == MAGIC_CONSTANT);
      assert(strcmp((char *)sqlite3_column_text(select_stmt, 1),
                    BuildInfo_kRevision) == 0);
  }
  assert(sqlite3_finalize(select_stmt) == SQLITE_OK);
}

size_t deptbl_entry_count_for_slot(size_t slot) {
  assert(slot < dep_size);

  size_t count = 0;
  deptbl_entry_t slotval = deptbl[slot];

  if (slotval.raw != 0 && slotval.s.key.tag == TAG_KEY) {
    while (slotval.s.next.tag == TAG_NEXT) {
      assert(slotval.s.next.num < dep_size);
      slotval = deptbl[slotval.s.next.num];
      count++;
    }

    // The final "next" in the list is always a value, not a next pointer.
    count++;
  }

  return count;
}

/*
 * Assumption: When we save the dependency table, we do a fresh load
 * aka there is saved state
 */
CAMLprim value hh_save_dep_table_sqlite(value out_filename) {
  CAMLparam1(out_filename);

  // This can only happen in the master
  assert_master();

  struct timeval tv;
  struct timeval tv2;
  gettimeofday(&tv, NULL);

  sqlite3 *db_out;
  assert(sqlite3_open(String_val(out_filename), &db_out) == SQLITE_OK);

  // Create header for verification while we read from the db
  create_sqlite_header(db_out);

  // Create Dep able
  const char *sql = "CREATE TABLE DEPTABLE(" \
               "KEY_VERTEX INT PRIMARY KEY NOT NULL," \
               "VALUE_VERTEX BLOB NOT NULL);";

  assert(sqlite3_exec(db_out, sql, NULL, 0, NULL)
      == SQLITE_OK);
  // Hand-off the data to the OS for writing and continue,
  // don't wait for it to complete
  assert(sqlite3_exec(db_out, "PRAGMA synchronous = OFF", NULL, 0, NULL)
      == SQLITE_OK);
  // Store the rollback journal in memory
  assert(sqlite3_exec(db_out, "PRAGMA journal_mode = MEMORY", NULL, 0, NULL)
      == SQLITE_OK);
  // Use one transaction for all the insertions
  assert(sqlite3_exec(db_out, "BEGIN TRANSACTION", NULL, 0, NULL)
      == SQLITE_OK);

  // Create entries on the table
  size_t slot;
  size_t count;
  size_t prev_count = 0;
  uint32_t *values = NULL;
  size_t iter;
  sqlite3_stmt *insert_stmt = NULL;
  sql = "INSERT INTO DEPTABLE (KEY_VERTEX, VALUE_VERTEX) VALUES (?,?)";
  assert(sqlite3_prepare_v2(db_out, sql, -1, &insert_stmt, NULL) == SQLITE_OK);
  for (slot = 0; slot < dep_size; ++slot) {
    count = deptbl_entry_count_for_slot(slot);
    if (count == 0) {
      continue;
    }
    if (count > prev_count) {
      // No need to allocate new space if can just re use the old one
      values = realloc(values, count * sizeof(uint32_t));
      prev_count = count;
    }
    assert(values != NULL);
    iter = 0;

    deptbl_entry_t slotval = deptbl[slot];

    if (slotval.raw != 0 && slotval.s.key.tag == TAG_KEY) {
      // This is the head of a linked list aka KEY VERTEX
      assert(sqlite3_bind_int(insert_stmt, 1, slotval.s.key.num) == SQLITE_OK);

      // Then combine each value to VALUE VERTEX
      while (slotval.s.next.tag == TAG_NEXT) {
        assert(slotval.s.next.num < dep_size);
        slotval = deptbl[slotval.s.next.num];
        values[iter] = slotval.s.key.num;
        iter++;
      }

      // The final "next" in the list is always a value, not a next pointer.
      values[iter] = slotval.s.next.num;
      iter++;
      assert(sqlite3_bind_blob(insert_stmt, 2, values,
                               iter * sizeof(uint32_t), SQLITE_TRANSIENT)
          == SQLITE_OK);
      assert(sqlite3_step(insert_stmt) == SQLITE_DONE);
      assert(sqlite3_clear_bindings(insert_stmt) == SQLITE_OK);
      assert(sqlite3_reset(insert_stmt) == SQLITE_OK);
    }
  }

  if (values != NULL) {
    free(values);
  }

  assert(sqlite3_finalize(insert_stmt) == SQLITE_OK);
  assert(sqlite3_exec(db_out, "END TRANSACTION", NULL, 0, NULL) == SQLITE_OK);

  assert(sqlite3_close(db_out) == SQLITE_OK);
  tv2 = log_duration("Writing dependency file with sqlite", tv);
  int secs = tv2.tv_sec - tv.tv_sec;
  // Reporting only seconds, ignore milli seconds
  CAMLreturn(Val_long(secs));
}

CAMLprim value hh_load_dep_table_sqlite(value in_filename) {
  CAMLparam1(in_filename);
  struct timeval tv;
  struct timeval tv2;
  gettimeofday(&tv, NULL);

  // This can only happen in the master
  assert_master();

  const char *filename = String_val(in_filename);
  size_t filename_len = strlen(filename);

  /* Since we save the filename on the heap, and have allocated only
   * getpagesize() space
   */
  assert(filename_len < getpagesize());

  memcpy(db_filename, filename, filename_len);
  db_filename[filename_len] = '\0';

  assert(sqlite3_open(filename, &db) == SQLITE_OK);

  // Verify the header
  verify_sqlite_header(db);

  tv2 = log_duration("Reading the dependency file with sqlite", tv);
  int secs = tv2.tv_sec - tv.tv_sec;
  // Reporting only seconds, ignore milli seconds
  CAMLreturn(Val_long(secs));
}

/* Given a key, returns the list of values bound to it from the sql db. */
CAMLprim value hh_get_dep_sqlite(value ocaml_key) {
  CAMLparam1(ocaml_key);
  CAMLlocal2(result, cell);

  // The caller is required to pass a 32-bit node ID.
  const uint64_t key64 = Long_val(ocaml_key);
  const uint32_t key = (uint32_t)key64;
  assert((key & 0x7FFFFFFF) == key64);

  result = Val_int(0); // The empty list

  // Make sure db connection is made
  // If not the master process, then try to connect
  // Otherwise return empty list
  if(db == NULL) {
    if (my_pid != *master_pid && db_filename != NULL) {
      assert(sqlite3_open(db_filename, &db) == SQLITE_OK);
    } else {
      CAMLreturn(result);
    }
  }

  uint32_t *values;
  size_t size, count, i;

  sqlite3_stmt *select_stmt = NULL;
  const char *sql = "SELECT VALUE_VERTEX FROM DEPTABLE WHERE KEY_VERTEX=?;";
  assert(sqlite3_prepare_v2(db, sql, -1, &select_stmt, NULL) == SQLITE_OK);
  assert(sqlite3_bind_int(select_stmt, 1, key) == SQLITE_OK);

  if (sqlite3_step(select_stmt) == SQLITE_ROW) {
    // Means we found it in the table
    // Columns are 0 indexed
    values = (uint32_t *) sqlite3_column_blob(select_stmt, 0);
    size = (size_t) sqlite3_column_bytes(select_stmt, 0);
    // Make sure we don't have malformed output
    assert(size % sizeof(uint32_t) == 0);
    count = size / sizeof(uint32_t);

    for (i = 0; i < count; i++) {
      cell = caml_alloc_tuple(2);
      Field(cell, 0) = Val_long(values[i]);
      Field(cell, 1) = result;
      result = cell;
    }
  }
  assert(sqlite3_finalize(select_stmt) == SQLITE_OK);
  CAMLreturn(result);
}

#else
CAMLprim value hh_save_dep_table_sqlite(value out_filename) {
  CAMLparam0();
  CAMLreturn(Val_long(0));
}

CAMLprim value hh_load_dep_table_sqlite(value in_filename) {
  CAMLparam0();
  CAMLreturn(Val_long(0));
}

CAMLprim value hh_get_dep_sqlite(value ocaml_key) {
  // Empty list
  CAMLparam0();
  CAMLreturn(Val_int(0));
}
#endif
