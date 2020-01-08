/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "hh_shared.h"

/*****************************************************************************/
/* File Implementing the shared memory system for Hack.
 *
 * THIS CODE ONLY WORKS WITH HACK, IT MAY LOOK LIKE A GENERIC ATOMIC
 * HASHTABLE FOR OCAML: IT IS NOT!
 * BUT ... YOU WERE GOING TO SAY BUT? BUT ...
 * THERE IS NO BUT! DONNY YOU'RE OUT OF YOUR ELEMENT!
 *
 * The lock-free data structures implemented here only work because of how the
 * Hack phases are synchronized.
 *
 * The hashtable maps string keys to string values. (The strings are really
 * serialized / marshalled representations of OCaml structures.) Key observation
 * of the table is that data with the same key are considered equivalent, and so
 * you can arbitrarily get any copy of it; furthermore if data is missing it can
 * be recomputed, so incorrectly saying data is missing when it is being written
 * is only a potential perf loss. Note that "equivalent" doesn't necessarily
 * mean "identical", e.g., two alpha-converted types are "equivalent" though not
 * literally byte- identical. (That said, I'm pretty sure the Hack typechecker
 * actually does always write identical data, but the hashtable doesn't need
 * quite that strong of an invariant.)
 *
 *    The operations implemented, and their limitations:
 *
 *    -) Concurrent writes: SUPPORTED One will win and the other will get
 *    dropped on the floor. There is no way to tell which happened. Only promise
 *    is that after a write, the one thread which did the write will see data in
 *    the table (though it may be slightly different data than what was written,
 *    see above about equivalent data).
 *
 *    -) Concurrent reads: SUPPORTED If interleaved with a concurrent write, the
 *    read will arbitrarily say that there is no data at that slot or return the
 *    entire new data written by the concurrent writer.
 *
 *    -) Concurrent removes: NOT SUPPORTED Only the master can remove, and can
 *    only do so if there are no other concurrent operations (reads or writes).
 *
 *    Since the values are variably sized and can get quite large, they are
 *    stored separately from the hashes in a garbage-collected heap.
 *
 * Hash collisions are resolved via linear probing.
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
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <unistd.h>
#endif

#include <inttypes.h>
#include <lz4.h>
#include <sys/time.h>
#include <time.h>

#include "hh_assert.h"

// Ideally these would live in a handle.h file but our internal build system
// can't support that at the moment. These are shared with handle_stubs.c
#ifdef _WIN32
#define Val_handle(fd) (win_alloc_handle(fd))
#else
#define Handle_val(fd) (Long_val(fd))
#define Val_handle(fd) (Val_long(fd))
#endif

#define HASHTBL_WRITE_IN_PROGRESS (0xfffffffffffffffeull)

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
#ifdef __linux__
  // Linux version for the architecture must support syscall memfd_create
  #ifndef SYS_memfd_create
    #if defined(__x86_64__)
      #define SYS_memfd_create 319
    #elif defined(__powerpc64__)
      #define SYS_memfd_create 360
    #elif defined(__aarch64__)
      #define SYS_memfd_create 385
    #else
      #error "hh_shared.c requires an architecture that supports memfd_create"
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

#ifndef MAP_NORESERVE
  // This flag was unimplemented in FreeBSD and then later removed
  #define MAP_NORESERVE 0
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

static size_t heap_size;

/* Used for the shared hashtable */
static uint64_t hashtbl_size;
static size_t hashtbl_size_b;

/* Used for worker-local data */
static size_t locals_size_b;

typedef enum {
  KIND_STRING = 1,
  KIND_SERIALIZED = !KIND_STRING
} storage_kind;

/* Too lazy to use getconf */
#define CACHE_LINE_SIZE (1 << 6)

#define __ALIGN_MASK(x,mask)    (((x)+(mask))&~(mask))
#define ALIGN(x,a)              __ALIGN_MASK(x,(typeof(x))(a)-1)
#define CACHE_ALIGN(x)          ALIGN(x,CACHE_LINE_SIZE)

/*****************************************************************************/
/* Types */
/*****************************************************************************/

/* Per-worker data which can be quickly updated non-atomically. Will be placed
 * in cache-aligned array in the first few pages of shared memory, indexed by
 * worker id. */
typedef struct {
  uint64_t counter;
} local_t;

// Every heap entry starts with a 64-bit header with the following layout:
//
//  6                                3 3  3                                0 0
//  3                                3 2  1                                1 0
// +----------------------------------+-+-----------------------------------+-+
// |11111111 11111111 11111111 1111111|0| 11111111 11111111 11111111 1111111|1|
// +----------------------------------+-+-----------------------------------+-+
// |                                  | |                                   |
// |                                  | |                                   * 0 tag
// |                                  | |
// |                                  | * 31-1 uncompressed size (0 if uncompressed)
// |                                  |
// |                                  * 32 kind (0 = serialized, 1 = string)
// |
// * 63-33 size of heap entry
//
// The tag bit is always 1 and is used to differentiate headers from addresses
// during garbage collection (see hh_collect).
typedef uint64_t hh_header_t;

// Locations in the heap are encoded as byte offsets from the beginning of
// the hash table, shifted left by 1 with the least-significant bit always set
// to 0 to distinguish addresses from headers during garbage collection.
//
// Note that the offsets do not start at the beginning of the heap, but the
// start of the hash table. This has two important implications:
//
// 1. The offset 0 will always point to the hash of the first hash table entry,
// which is never a meaningful offset. Because of this, we can take the
// address 0 to be the "null" address.
//
// 2. During garbage collection, it is necessary to point from the heap to the
// hash table itself, since we temporarily store heap headers in the addr field
// of helt_t. By starting the offsets at the beginning of the hash table, we can
// represent offsets into the hash table itself.
typedef uint64_t addr_t;

#define NULL_ADDR 0
#define Offset_of_addr(addr) ((addr) >> 1)
#define Addr_of_offset(offset) ((offset) << 1)
#define Addr_of_ptr(entry) (Addr_of_offset((char *)(entry) - (char *)hashtbl))
#define Ptr_of_offset(offset) ((char *)hashtbl + (offset))
#define Ptr_of_addr(addr) (Ptr_of_offset(Offset_of_addr(addr)))
#define Entry_of_addr(addr) ((heap_entry_t *)Ptr_of_addr(addr))
#define Entry_of_offset(offset) ((heap_entry_t *)Ptr_of_offset(offset))
#define Header_of_addr(addr) ((hh_header_t *)Ptr_of_addr(addr))

#define Entry_size(x) ((x) >> 33)
#define Entry_kind(x) (((x) >> 32) & 1)
#define Entry_uncompressed_size(x) (((x) >> 1) & 0x7FFFFFFF)
#define Heap_entry_total_size(header) sizeof(heap_entry_t) + Entry_size(header)

/* Shared memory structures. hh_shared.h typedefs this to heap_entry_t. */
typedef struct {
  hh_header_t header;
  char data[];
} heap_entry_t;

/* Cells of the Hashtable */
typedef struct {
  uint64_t hash;
  addr_t addr;
} helt_t;

/*****************************************************************************/
/* Globals */
/*****************************************************************************/

/* Total size of allocated shared memory */
static size_t shared_mem_size = 0;

/* Beginning of shared memory */
static char* shared_mem = NULL;

/* The hashtable containing the shared values. */
static helt_t* hashtbl = NULL;
/* The number of nonempty slots in the hashtable. A nonempty slot has a
 * non-zero hash. We never clear hashes so this monotonically increases */
static uint64_t* hcounter = NULL;
/* The number of nonempty filled slots in the hashtable. A nonempty filled slot
 * has a non-zero hash AND a non-null addr. It increments when we write data
 * into a slot with addr==NULL and decrements when we clear data from a slot */
static uint64_t* hcounter_filled = NULL;

/* A counter increasing globally across all forks. */
static uintptr_t* counter = NULL;

/* Each process reserves a range of values at a time from the shared counter.
 * Should be a power of two for more efficient modulo calculation. */
#define COUNTER_RANGE 2048

/* Logging level for shared memory statistics
 * 0 = nothing
 * 1 = log totals, averages, min, max bytes marshalled and unmarshalled
 */
static size_t* log_level = NULL;

static size_t* workers_should_exit = NULL;

/* Worker-local storage is cache line aligned. */
static char* locals;
#define LOCAL(id) ((local_t *)(locals + id * CACHE_ALIGN(sizeof(local_t))))

/* This should only be used before forking */
static uintptr_t early_counter = 0;

/* The top of the heap, offset from hashtbl pointer */
static size_t* heap = NULL;

/* Useful to add assertions */
static pid_t* master_pid = NULL;
static pid_t my_pid = 0;

static size_t num_workers;

/* This is a process-local value. The master process is 0, workers are numbered
 * starting at 1. This is an offset into the worker local values in the heap. */
static size_t worker_id;

static size_t worker_can_exit = 1;

/* Where the heap started (bottom), offset from hashtbl pointer */
static size_t heap_init = 0;
/* Where the heap will end (top), offset from hashtbl pointer */
static size_t heap_max = 0;

static size_t* wasted_heap_size = NULL;

static size_t used_heap_size(void) {
  return *heap - heap_init;
}

static long removed_count = 0;

/* Expose so we can display diagnostics */
CAMLprim value hh_used_heap_size(void) {
  return Val_long(used_heap_size());
}

/* Part of the heap not reachable from hashtable entries. Can be reclaimed with
 * hh_collect. */
CAMLprim value hh_wasted_heap_size(void) {
  assert(wasted_heap_size != NULL);
  return Val_long(*wasted_heap_size);
}

CAMLprim value hh_log_level(void) {
  return Val_long(*log_level);
}

CAMLprim value hh_hash_used_slots(void) {
  CAMLparam0();
  CAMLlocal1(connector);

  connector = caml_alloc_tuple(2);
  Store_field(connector, 0, Val_long(*hcounter_filled));
  Store_field(connector, 1, Val_long(*hcounter));

  CAMLreturn(connector);
}

CAMLprim value hh_hash_slots(void) {
  CAMLparam0();
  CAMLreturn(Val_long(hashtbl_size));
}

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

static void raise_failed_anonymous_memfd_init(void) {
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
  char *mem = NULL;
  mem = MapViewOfFile(memfd, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  if (mem == NULL) {
    win32_maperr(GetLastError());
    uerror("MapViewOfFile", Nothing);
  }
  return mem;
}

#else

static char *memfd_map(size_t shared_mem_size) {
  char *mem = NULL;
  /* MAP_NORESERVE is because we want a lot more virtual memory than what
   * we are actually going to use.
   */
  int flags = MAP_SHARED | MAP_NORESERVE;
  int prot  = PROT_READ  | PROT_WRITE;
  mem = (char*)mmap(NULL, shared_mem_size, prot, flags, memfd, 0);
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


static void raise_out_of_shared_memory(void)
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
  int err;
  do {
    err = posix_fallocate(memfd, offset, sz);
  } while (err == EINTR);
  if (err) {
    raise_out_of_shared_memory();
  }
}

#endif

// DON'T WRITE TO THE SHARED MEMORY IN THIS FUNCTION!!!  This function just
// calculates where the memory is and sets local globals. The shared memory
// might not be ready for writing yet! If you want to initialize a bit of
// shared memory, check out init_shared_globals
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
  heap = (size_t*)mem;

  // The number of elements in the hashtable
  assert(CACHE_LINE_SIZE >= sizeof(uint64_t));
  hcounter = (uint64_t*)(mem + CACHE_LINE_SIZE);

  assert (CACHE_LINE_SIZE >= sizeof(uintptr_t));
  counter = (uintptr_t*)(mem + 2*CACHE_LINE_SIZE);

  assert (CACHE_LINE_SIZE >= sizeof(pid_t));
  master_pid = (pid_t*)(mem + 3*CACHE_LINE_SIZE);

  assert (CACHE_LINE_SIZE >= sizeof(size_t));
  log_level = (size_t*)(mem + 4*CACHE_LINE_SIZE);

  assert (CACHE_LINE_SIZE >= sizeof(size_t));
  workers_should_exit = (size_t*)(mem + 5*CACHE_LINE_SIZE);

  assert (CACHE_LINE_SIZE >= sizeof(size_t));
  wasted_heap_size = (size_t*)(mem + 6*CACHE_LINE_SIZE);

  assert (CACHE_LINE_SIZE >= sizeof(uint64_t));
  hcounter_filled = (uint64_t*)(mem + 7*CACHE_LINE_SIZE);

  mem += page_size;
  // Just checking that the page is large enough.
  assert(page_size > 8*CACHE_LINE_SIZE + (int)sizeof(int));

  assert (CACHE_LINE_SIZE >= sizeof(local_t));
  locals = mem;
  mem += locals_size_b;

  /* END OF THE SMALL OBJECTS PAGE */

  /* Hashtable */
  hashtbl = (helt_t*)mem;
  mem += hashtbl_size_b;

  /* Heap */
  heap_init = hashtbl_size_b;
  heap_max = heap_init + heap_size;

#ifdef _WIN32
  /* Reserve all memory space. This is required for Windows but we don't do this
   * for Linux since it lets us run more processes in parallel without running
   * out of memory immediately (though we do risk it later on) */
  memfd_reserve((char *)heap, page_size + locals_size_b + hashtbl_size_b);
#endif

}

/* The total size of the shared memory.  Most of it is going to remain
 * virtual. */
static size_t get_shared_mem_size(void) {
  size_t page_size = getpagesize();
  return (hashtbl_size_b + heap_size + page_size + locals_size_b);
}

static void init_shared_globals(
  size_t config_log_level
) {
  // Initialize the number of element in the table
  *hcounter = 0;
  *hcounter_filled = 0;
  // Ensure the global counter starts on a COUNTER_RANGE boundary
  *counter = ALIGN(early_counter + 1, COUNTER_RANGE);
  *log_level = config_log_level;
  *workers_should_exit = 0;
  *wasted_heap_size = 0;

  for (uint64_t i = 0; i <= num_workers; i++) {
    LOCAL(i)->counter = 0;
  }

  // Initialize top heap pointers
  *heap = heap_init;
}

static void set_sizes(
  uint64_t config_heap_size,
  uint64_t config_hash_table_pow,
  uint64_t config_num_workers) {

  size_t page_size = getpagesize();

  heap_size = config_heap_size;

  hashtbl_size    = 1ul << config_hash_table_pow;
  hashtbl_size_b  = hashtbl_size * sizeof(hashtbl[0]);

  // We will allocate a cache line for the master process and each worker
  // process, then pad that out to the nearest page.
  num_workers = config_num_workers;
  locals_size_b = ALIGN((1 + num_workers) * CACHE_LINE_SIZE, page_size);

  shared_mem_size = get_shared_mem_size();
}

/*****************************************************************************/
/* Must be called by the master BEFORE forking the workers! */
/*****************************************************************************/

CAMLprim value hh_shared_init(
  value config_val,
  value shm_dir_val,
  value num_workers_val
) {
  CAMLparam3(config_val, shm_dir_val, num_workers_val);
  CAMLlocal3(
    connector,
    config_heap_size_val,
    config_hash_table_pow_val
  );

  config_heap_size_val = Field(config_val, 0);
  config_hash_table_pow_val = Field(config_val, 1);

  set_sizes(
    Long_val(config_heap_size_val),
    Long_val(config_hash_table_pow_val),
    Long_val(num_workers_val)
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
    Long_val(Field(config_val, 3))
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

  init_shared_globals(
    Long_val(Field(config_val, 4)));
  // Checking that we did the maths correctly.
  assert(Ptr_of_offset(*heap) + heap_size == shared_mem + shared_mem_size);

#ifndef _WIN32
  // Uninstall ocaml's segfault handler. It's supposed to throw an exception on
  // stack overflow, but we don't actually handle that exception, so what
  // happens in practice is we terminate at toplevel with an unhandled exception
  // and a useless ocaml backtrace. A core dump is actually more useful. Sigh.
  struct sigaction sigact = { 0 };
  sigact.sa_handler = SIG_DFL;
  sigemptyset(&sigact.sa_mask);
  sigact.sa_flags = 0;
  sigaction(SIGSEGV, &sigact, NULL);
#endif

  connector = caml_alloc_tuple(5);
  Store_field(connector, 0, Val_handle(memfd));
  Store_field(connector, 1, config_heap_size_val);
  Store_field(connector, 2, config_hash_table_pow_val);
  Store_field(connector, 3, num_workers_val);

  CAMLreturn(connector);
}

/* Must be called by every worker before any operation is performed */
value hh_connect(value connector, value worker_id_val) {
  CAMLparam2(connector, worker_id_val);
  memfd = Handle_val(Field(connector, 0));
  set_sizes(
    Long_val(Field(connector, 1)),
    Long_val(Field(connector, 2)),
    Long_val(Field(connector, 3))
  );
  worker_id = Long_val(worker_id_val);
#ifdef _WIN32
  my_pid = 1; // Trick
#else
  my_pid = getpid();
#endif
  char *shared_mem_init = memfd_map(shared_mem_size);
  define_globals(shared_mem_init);

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

  uintptr_t v = 0;
  if (counter) {
    v = LOCAL(worker_id)->counter;
    if (v % COUNTER_RANGE == 0) {
      v = __atomic_fetch_add(counter, COUNTER_RANGE, __ATOMIC_RELAXED);
    }
    ++v;
    LOCAL(worker_id)->counter = v;
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
void assert_master(void) {
  assert(my_pid == *master_pid);
}

void assert_not_master(void) {
  assert(my_pid != *master_pid);
}

/*****************************************************************************/

CAMLprim value hh_stop_workers(void) {
  CAMLparam0();
  assert_master();
  *workers_should_exit = 1;
  CAMLreturn(Val_unit);
}

CAMLprim value hh_resume_workers(void) {
  CAMLparam0();
  assert_master();
  *workers_should_exit = 0;
  CAMLreturn(Val_unit);
}

CAMLprim value hh_set_can_worker_stop(value val) {
  CAMLparam1(val);
  worker_can_exit = Bool_val(val);
  CAMLreturn(Val_unit);
}

void check_should_exit(void) {
  assert(workers_should_exit != NULL);
  if(worker_can_exit && *workers_should_exit) {
    static value *exn = NULL;
    if (!exn) exn = caml_named_value("worker_should_exit");
    caml_raise_constant(*exn);
  }
}

CAMLprim value hh_check_should_exit (void) {
  CAMLparam0();
  check_should_exit();
  CAMLreturn(Val_unit);
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

CAMLprim value hh_collect(void) {
  // NOTE: explicitly do NOT call CAMLparam or any of the other functions/macros
  // defined in caml/memory.h .
  // This function takes a boolean and returns unit.
  // Those are both immediates in the OCaml runtime.
  assert_master();

  // Step 1: Walk the hashtbl entries, which are the roots of our marking pass.

  for (size_t i = 0; i < hashtbl_size; i++) {
    // Skip empty slots
    if (hashtbl[i].addr == NULL_ADDR) { continue; }

    // No workers should be writing at the moment. If a worker died in the
    // middle of a write, that is also very bad
    assert(hashtbl[i].addr != HASHTBL_WRITE_IN_PROGRESS);

    // The hashtbl addr will be wrong after we relocate the heap entry, but we
    // don't know where the heap entry will relocate to yet. We need to first
    // move the heap entry, then fix up the hashtbl addr.
    //
    // We accomplish this by storing the heap header in the now useless addr
    // field and storing a pointer to the addr field where the header used to
    // be. Then, after moving the heap entry, we can follow the pointer to
    // restore our original header and update the addr field to our relocated
    // address.

    heap_entry_t *entry = Entry_of_addr(hashtbl[i].addr);
    hashtbl[i].addr = entry->header;
    entry->header = Addr_of_ptr(&hashtbl[i].addr);
  }

  // Step 2: Walk the heap and relocate entries, updating the hashtbl to point
  // to relocated addresses.

  // Offset of free space in the heap where moved values will move to.
  size_t dest = heap_init;

  // Pointer that walks the heap from bottom to top.
  char *src = Ptr_of_offset(heap_init);
  char *heap_ptr = Ptr_of_offset(*heap);

  size_t aligned_size;
  hh_header_t header;
  while (src < heap_ptr) {
    if (*(hh_header_t *)src & 1) {
      // If the lsb is set, this is a header. If it's a header, that means the
      // entry was not marked in the first pass and should be collected. Don't
      // move dest pointer, but advance src pointer to next heap entry.
      header = *(hh_header_t *)src;
      aligned_size = CACHE_ALIGN(Heap_entry_total_size(header));
    } else {
      // If the lsb is 0, this is an addr to the addr field of the hashtable
      // element, which holds the header bytes. This entry is live.
      hh_header_t *hashtbl_addr = Header_of_addr(*(addr_t *)src);
      header = *hashtbl_addr;
      aligned_size = CACHE_ALIGN(Heap_entry_total_size(header));

      // Fix the hashtbl addr field to point to our new location and restore the
      // heap header data temporarily stored in the addr field bits.
      *hashtbl_addr = Addr_of_offset(dest);

      // Restore the heap entry header
      *(hh_header_t *)src = header;

      // Move the entry as far to the left as possible.
      memmove(Ptr_of_offset(dest), src, aligned_size);
      dest += aligned_size;
    }

    src += aligned_size;
  }

  // TODO: Space between dest and *heap is unused, but will almost certainly
  // become used again soon. Currently we will never decommit, which may cause
  // issues when there is memory pressure.
  //
  // If the kernel supports it, we might consider using madvise(MADV_FREE),
  // which allows the kernel to reclaim the memory lazily under pressure, but
  // would not force page faults under healthy operation.

  *heap = dest;
  *wasted_heap_size = 0;

  return Val_unit;
}

static void raise_heap_full(void) {
  static value *exn = NULL;
  if (!exn) exn = caml_named_value("heap_full");
  caml_raise_constant(*exn);
}

/*****************************************************************************/
/* Allocates in the shared heap. The chunks are cache aligned. */
/*****************************************************************************/

static addr_t hh_alloc(hh_header_t header, char *data) {
  // the size of this allocation needs to be kept in sync with wasted_heap_size
  // modification in hh_remove
  size_t slot_size = CACHE_ALIGN(Heap_entry_total_size(header));
  size_t offset = __sync_fetch_and_add(heap, slot_size);
  if (offset + slot_size > heap_max) {
    raise_heap_full();
  }
  heap_entry_t *chunk = Entry_of_offset(offset);
  memfd_reserve((char *)chunk, slot_size);
  chunk->header = header;
  memcpy(&chunk->data, data, Entry_size(header));
  return Addr_of_offset(offset);
}

/*****************************************************************************/
/* Allocates an ocaml value in the shared heap.
 * Any ocaml value is valid, except closures. It returns the address of
 * the allocated chunk.
 */
/*****************************************************************************/
static addr_t hh_store_ocaml(
  value data,
  /*out*/size_t *alloc_size,
  /*out*/size_t *orig_size
) {
  char* value = NULL;
  size_t size = 0;
  size_t uncompressed_size = 0;
  storage_kind kind = 0;

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
  *orig_size = size;

  size_t max_compression_size = LZ4_compressBound(size);
  char* compressed_data = malloc(max_compression_size);
  size_t compressed_size = LZ4_compress_default(
    value,
    compressed_data,
    size,
    max_compression_size);

  if (compressed_size != 0 && compressed_size < size) {
    uncompressed_size = size;
    size = compressed_size;
  }

  *alloc_size = size;

  // Both size and uncompressed_size will certainly fit in 31 bits, as the
  // original size fits per the assert above and we check that the compressed
  // size is less than the original size.
  hh_header_t header
    = size << 33
    | (uint64_t)kind << 32
    | uncompressed_size << 1
    | 1;

  addr_t heap_addr = hh_alloc(
    header,
    uncompressed_size ? compressed_data : value);

  free(compressed_data);
  // We temporarily allocate memory using malloc to serialize the Ocaml object.
  // When we have finished copying the serialized data into our heap we need
  // to free the memory we allocated to avoid a leak.
  if (kind == KIND_SERIALIZED) free(value);

  return heap_addr;
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
  CAMLparam1(data);
  CAMLlocal1(result);
  result = caml_alloc_tuple(2);
  // Try to write in a value to indicate that the data is being written.
  if(
     __sync_bool_compare_and_swap(
       &(hashtbl[slot].addr),
       NULL_ADDR,
       HASHTBL_WRITE_IN_PROGRESS
     )
  ) {
    size_t alloc_size = 0;
    size_t orig_size = 0;
    hashtbl[slot].addr = hh_store_ocaml(data, &alloc_size, &orig_size);
    Store_field(result, 0, Val_long(alloc_size));
    Store_field(result, 1, Val_long(orig_size));
    __sync_fetch_and_add(hcounter_filled, 1);
  } else {
    Store_field(result, 0, Min_long);
    Store_field(result, 1, Min_long);
  }
  CAMLreturn(result);
}

static void raise_hash_table_full(void) {
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
  check_should_exit();
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

/*
hh_mem_inner
 1 -- key exists and is associated with non-zero data
-1 -- key is not present in the hash table at all
-2 -- key is present in the hash table but associated with zero-valued data.
      This means that the data has been explicitly deleted.

Note that the only valid return values are {1,-1,-2}. In order to use the result
of this function in an "if" statement an explicit test must be performed.
*/
int hh_mem_inner(value key) {
  check_should_exit();
  unsigned int slot = find_slot(key);
  _Bool good_hash = hashtbl[slot].hash == get_hash(key);
  _Bool non_null_addr = hashtbl[slot].addr != NULL_ADDR;
  if (good_hash && non_null_addr) {
    // The data is currently in the process of being written, wait until it
    // actually is ready to be used before returning.
    time_t start = 0;
    while (hashtbl[slot].addr == HASHTBL_WRITE_IN_PROGRESS) {
#if defined(__aarch64__) || defined(__powerpc64__)
      asm volatile("yield" : : : "memory");
#else
      asm volatile("pause" : : : "memory");
#endif
      // if the worker writing the data dies, we can get stuck. Timeout check
      // to prevent it.
      time_t now = time(0);
      if (start == 0 || start > now) {
        start = now;
      } else if (now - start > 60) {
        caml_failwith("hh_mem busy-wait loop stuck for 60s");
      }
    }
    return 1;
  }
  else if (good_hash) {
    // if the hash matches and the key is zero
    // then we've removed the key.
    return -2;
  } else {
    // otherwise the key is simply absent
    return -1;
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
  CAMLreturn(Val_bool(hh_mem_inner(key) == 1));
}

CAMLprim value hh_mem_status(value key) {
  CAMLparam1(key);
  int res = hh_mem_inner(key);
  switch (res) {
    case 1:
    case -1:
    case -2:
      CAMLreturn(Val_int(res));
    default:
      caml_failwith("Unreachable case: result must be 1 or -1 or -2");
  }
}

/*****************************************************************************/
/* Deserializes the value pointed to by elt. */
/*****************************************************************************/
CAMLprim value hh_deserialize(addr_t heap_addr) {
  CAMLparam0();
  CAMLlocal1(result);
  heap_entry_t *entry = Entry_of_addr(heap_addr);
  size_t size = Entry_size(entry->header);
  size_t uncompressed_size_exp = Entry_uncompressed_size(entry->header);
  char *src = entry->data;
  char *data = entry->data;
  if (uncompressed_size_exp) {
    data = malloc(uncompressed_size_exp);
    size_t uncompressed_size = LZ4_decompress_safe(
      src,
      data,
      size,
      uncompressed_size_exp);
    assert(uncompressed_size == uncompressed_size_exp);
    size = uncompressed_size;
  }

  if (Entry_kind(entry->header) == KIND_STRING) {
    result = caml_alloc_string(size);
    memcpy(String_val(result), data, size);
  } else {
    result = caml_input_value_from_block(data, size);
  }

  if (data != src) {
    free(data);
  }
  CAMLreturn(result);
}

/*****************************************************************************/
/* Returns the value associated to a given key, and deserialize it. */
/* The key MUST be present. */
/*****************************************************************************/
CAMLprim value hh_get_and_deserialize(value key) {
  CAMLparam1(key);
  check_should_exit();
  CAMLlocal1(result);

  unsigned int slot = find_slot(key);
  assert(hashtbl[slot].hash == get_hash(key));
  result = hh_deserialize(hashtbl[slot].addr);
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
  heap_entry_t *entry = Entry_of_addr(hashtbl[slot].addr);
  CAMLreturn(Val_long(Entry_size(entry->header)));
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
  assert(hashtbl[slot2].addr == NULL_ADDR);
  // We are taking up a previously empty slot. Let's increment the counter.
  // hcounter_filled doesn't change, since slot1 becomes empty and slot2 becomes
  // filled.
  if (hashtbl[slot2].hash == 0) {
    *hcounter += 1;
  }
  hashtbl[slot2].hash = get_hash(key2);
  hashtbl[slot2].addr = hashtbl[slot1].addr;
  hashtbl[slot1].addr = NULL_ADDR;
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
  // see hh_alloc for the source of this size
  heap_entry_t *entry = Entry_of_addr(hashtbl[slot].addr);
  size_t slot_size = CACHE_ALIGN(Heap_entry_total_size(entry->header));
  *wasted_heap_size += slot_size;
  hashtbl[slot].addr = NULL_ADDR;
  removed_count++;
  *hcounter_filled -= 1;
}

CAMLprim value hh_removed_count(value unit) {
    CAMLparam1(unit);
    return Val_long(removed_count);
}
