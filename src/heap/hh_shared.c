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

#include <stdalign.h>
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
 #define MEMFD_CREATE 1

 // glibc only added support for memfd_create in version 2.27.
 #ifndef MFD_CLOEXEC
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

/* Too lazy to use getconf */
#define CACHE_LINE_SIZE (1 << 6)

#define __ALIGN_MASK(x,mask)    (((x)+(mask))&~(mask))
#define ALIGN(x,a)              __ALIGN_MASK(x,(typeof(x))(a)-1)
#define CACHE_ALIGN(x)          ALIGN(x,CACHE_LINE_SIZE)

/* Each process reserves a range of values at a time from the shared counter.
 * Should be a power of two for more efficient modulo calculation. */
#define COUNTER_RANGE 2048

/*****************************************************************************/
/* Types */
/*****************************************************************************/

/* Convention: .*_b = Size in bytes. */

typedef struct {
  /* Layout information, used by workers to create memory mappings. */
  size_t locals_size_b;
  size_t hashtbl_size_b;
  size_t heap_size_b;
  size_t shared_mem_size_b;

  /* Maximum number of hashtable elements */
  size_t hashtbl_slots;

  /* Where the heap started (bottom), offset from hashtbl pointer */
  size_t heap_init;

  /* Where the heap will end (top), offset from hashtbl pointer */
  size_t heap_max;

  /* Logging level for shared memory statistics
   * 0 = nothing
   * 1 = log totals, averages, min, max bytes marshalled and unmarshalled
   * 2+ = log size of deserialized values in OCaml heap
   */
  size_t log_level;

  /* Initially 0; set to 1 to signal that workers should exit */
  size_t workers_should_exit;

  size_t wasted_heap_size;

  /* A counter increasing globally across all forks. */
  alignas(128) uintnat counter;

  /* The number of nonempty slots in the hashtable. A nonempty slot has a
   * non-zero hash. We never clear hashes so this monotonically increases */
  alignas(128) uintnat hcounter;

  /* The number of nonempty filled slots in the hashtable. A nonempty filled slot
   * has a non-zero hash AND a non-null addr. It increments when we write data
   * into a slot with addr==NULL and decrements when we clear data from a slot */
  alignas(128) uintnat hcounter_filled;

  /* The top of the heap, offset from hashtbl pointer */
  alignas(128) size_t heap;
} shmem_info_t;

/* Per-worker data which can be quickly updated non-atomically. Will be placed
 * in cache-aligned array in the first few pages of shared memory, indexed by
 * worker id.
 *
 * The first member of this struct is over-aligned to ensure that each element
 * of the global locals array is on a separate cache line. */
typedef struct {
  alignas(128) uint64_t counter;
} local_t;

// Every heap entry starts with a 64-bit header with the following layout:
//
//  6                                 3  3                                0 0
//  3                                 2  1                                1 0
// +------------------------------------+----------------------------------+-+
// |11111111 11111111 11111111 11111111 |11111111 11111111 11111111 1111111|1|
// +------------------------------------+----------------------------------+-+
// |                                    |                                  |
// |                                    |                                  * 0 tag
// |                                    |
// |                                    * 31-1 uncompressed size (0 if uncompressed)
// * 63-32 size of heap entry
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

#define Entry_size(header) ((header) >> 32)
#define Entry_uncompressed_size(header) (((header) >> 1) & 0x7FFFFFFF)
#define Heap_entry_total_size(header) sizeof(heap_entry_t) + Entry_size(header)

/* Shared memory structures. hh_shared.h typedefs this to heap_entry_t. */
typedef struct {
  hh_header_t header;
  char data[];
} heap_entry_t;

/* The hash table supports lock-free writes by performing a 16-byte CAS,
 * ensuring that the hash and address are written together atomically. */
typedef union {
  __int128_t value;
  struct {
    uint64_t hash;
    addr_t addr;
  };
} helt_t;

/*****************************************************************************/
/* Globals */
/*****************************************************************************/

/* Shared memory metadata */
static shmem_info_t* info = NULL;

/* Beginning of shared memory */
static char* shared_mem = NULL;

/* Worker-local storage is cache line aligned. */
static local_t* locals = NULL;

/* The hashtable containing the shared values. */
static helt_t* hashtbl = NULL;

/* This should only be used before forking */
static uintnat early_counter = 0;

/* This is a process-local value. The master process is 0, workers are numbered
 * starting at 1. This is an offset into the worker local values in the heap. */
static size_t worker_id = 0;

static size_t worker_can_exit = 1;

static size_t used_heap_size(void) {
  return info->heap - info->heap_init;
}

/* Expose so we can display diagnostics */
CAMLprim value hh_used_heap_size(void) {
  return Val_long(used_heap_size());
}

/* Part of the heap not reachable from hashtable entries. Can be reclaimed with
 * hh_collect. */
CAMLprim value hh_wasted_heap_size(void) {
  assert(info != NULL);
  return Val_long(info->wasted_heap_size);
}

CAMLprim value hh_log_level(void) {
  return Val_long(info->log_level);
}

CAMLprim value hh_hash_used_slots(void) {
  CAMLparam0();
  CAMLlocal1(connector);

  connector = caml_alloc_tuple(2);
  Store_field(connector, 0, Val_long(info->hcounter_filled));
  Store_field(connector, 1, Val_long(info->hcounter));

  CAMLreturn(connector);
}

CAMLprim value hh_hash_slots(void) {
  CAMLparam0();
  CAMLreturn(Val_long(info->hashtbl_slots));
}

static void raise_failed_memfd_init(int errcode) {
  static value *exn = NULL;
  if (!exn) exn = caml_named_value("failed_memfd_init");
  caml_raise_with_arg(*exn, unix_error_of_code(errcode));
}

#ifdef _WIN32

static HANDLE memfd;

/**************************************************************************
 * We create an anonymous memory file, whose `handle` might be
 * inherited by subprocesses.
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
void memfd_init(size_t shared_mem_size) {
  memfd = CreateFileMapping(
    INVALID_HANDLE_VALUE,
    NULL,
    PAGE_READWRITE | SEC_RESERVE,
    shared_mem_size >> 32, shared_mem_size & ((1ll << 32) - 1),
    NULL);
  if (memfd == NULL) {
    win32_maperr(GetLastError());
    raise_failed_memfd_init(errno);
  }
  if (!SetHandleInformation(memfd, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT)) {
    win32_maperr(GetLastError());
    raise_failed_memfd_init(errno);
  }
}

#else

static int memfd = -1;

/**************************************************************************
 * The memdfd_init function creates a anonymous memory file that might
 * be inherited by `Daemon.spawned` processus (contrary to a simple
 * anonymous mmap).
 *
 * The preferred mechanism is memfd_create(2) (see the upper
 * description). Then we try shm_open(3).
 *
 * The resulting file descriptor should be mmaped with the memfd_map
 * function (see below).
 ****************************************************************************/
void memfd_init(size_t shared_mem_size) {
#if defined(MEMFD_CREATE)
  memfd = memfd_create("fb_heap", 0);
#endif
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
      raise_failed_memfd_init(errno);
    }

    // shm_open sets FD_CLOEXEC automatically. This is undesirable, because
    // we want this fd to be open for other processes, so that they can
    // reconnect to the shared memory.
    int flags = fcntl(memfd, F_GETFD);
    if (flags == -1) {
      raise_failed_memfd_init(errno);
    }
    // Unset close-on-exec
    if (fcntl(memfd, F_SETFD, flags & ~FD_CLOEXEC) == -1) {
      raise_failed_memfd_init(errno);
    };
  }
  if (ftruncate(memfd, shared_mem_size) == -1) {
    raise_failed_memfd_init(errno);
  }
}

#endif

#ifdef _WIN32

static char *memfd_map(size_t size) {
  char *mem = NULL;
  mem = MapViewOfFile(memfd, FILE_MAP_ALL_ACCESS, 0, 0, size);
  if (mem == NULL) {
    win32_maperr(GetLastError());
    uerror("MapViewOfFile", Nothing);
  }
  return mem;
}

#else

static char *memfd_map(size_t size) {
  char *mem = NULL;
  /* MAP_NORESERVE is because we want a lot more virtual memory than what
   * we are actually going to use.
   */
  int flags = MAP_SHARED | MAP_NORESERVE;
  int prot  = PROT_READ  | PROT_WRITE;
  mem = (char*)mmap(NULL, size, prot, flags, memfd, 0);
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

static void map_info_page(int page_size) {
  // The first page of shared memory contains (1) size information describing
  // the layout of the rest of the shared file; (2) values which are atomically
  // updated by workers, like the heap pointer; and (3) various configuration
  // which is convenient to stick here, like the log level.
  assert(page_size >= sizeof(shmem_info_t));
  info = (shmem_info_t*)memfd_map(page_size);

#ifdef _WIN32
  // Memory must be reserved on Windows
  win_reserve((char *)info, page_size);
#endif
}

static void define_mappings(int page_size) {
  assert(info != NULL);
  size_t locals_size = info->locals_size_b;
  size_t hashtbl_size = info->hashtbl_size_b;
  size_t heap_size = info->heap_size_b;

  shared_mem = memfd_map(info->shared_mem_size_b);

  /* Process-local storage */
  locals = (local_t*)(shared_mem + page_size);

  /* Hashtable */
  hashtbl = (helt_t*)(shared_mem + page_size + locals_size);

#ifdef _WIN32
  // Memory must be reserved on Windows. Heap allocations will be reserved
  // in hh_alloc, so we just reserve the locals and hashtbl memory here.
  win_reserve((char *)locals, locals_size);
  win_reserve((char *)hashtbl, hashtbl_size);
#endif

#ifdef MADV_DONTDUMP
  // We are unlikely to get much useful information out of the shared heap in
  // a core file. Moreover, it can be HUGE, and the extensive work done dumping
  // it once for each CPU can mean that the user will reboot their machine
  // before the much more useful stack gets dumped!
  madvise(hashtbl, hashtbl_size + heap_size, MADV_DONTDUMP);
#endif
}

/*****************************************************************************/
/* Must be called by the master BEFORE forking the workers! */
/*****************************************************************************/

CAMLprim value hh_shared_init(
  value config_val,
  value num_workers_val
) {
  CAMLparam2(config_val, num_workers_val);

  size_t page_size = getpagesize();

  /* Calculate layout information. We need to figure out how big the shared file
   * needs to be in order to create the file. We will also store enough of the
   * layout information in the first page of the shared file so that workers can
   * create mappings for the rest of the shared data. */
  size_t num_workers = Long_val(num_workers_val);
  size_t locals_size_b = CACHE_ALIGN((1 + num_workers) * sizeof(local_t));
  size_t hashtbl_slots = 1ul << Long_val(Field(config_val, 1));
  size_t hashtbl_size_b = CACHE_ALIGN(hashtbl_slots * sizeof(helt_t));
  size_t heap_size_b = Long_val(Field(config_val, 0));

  /* The total size of the shared file must have space for the info page, local
   * data, the hash table, and the heap. */
  size_t shared_mem_size_b =
    page_size + locals_size_b + hashtbl_size_b + heap_size_b;

  memfd_init(shared_mem_size_b);

  /* The info page contains (1) size information describing the layout of the
   * rest of the shared file; (2) values which are atomically updated by
   * workers, like the heap pointer; and (3) various configuration which is
   * conventient to stick here, like the log level. */
  map_info_page(page_size);
  info->locals_size_b = locals_size_b;
  info->hashtbl_size_b = hashtbl_size_b;
  info->heap_size_b = heap_size_b;
  info->shared_mem_size_b = shared_mem_size_b;
  info->hashtbl_slots = hashtbl_slots;
  info->heap_init = hashtbl_size_b;
  info->heap_max = info->heap_init + heap_size_b;
  info->log_level = Long_val(Field(config_val, 2));

  // Ensure the global counter starts on a COUNTER_RANGE boundary
  info->counter = ALIGN(early_counter + 1, COUNTER_RANGE);

  // Initialize top heap pointers
  info->heap = info->heap_init;

  define_mappings(page_size);

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

  CAMLreturn(Val_handle(memfd));
}

/* Must be called by every worker before any operation is performed */
value hh_connect(value handle_val, value worker_id_val) {
  CAMLparam2(handle_val, worker_id_val);
  memfd = Handle_val(handle_val);
  worker_id = Long_val(worker_id_val);

  // Avoid confusion with master process, which is designated 0
  assert(worker_id > 0);

  int page_size = getpagesize();
  map_info_page(page_size);
  define_mappings(page_size);

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
  if (info) {
    v = locals[worker_id].counter;
    if (v % COUNTER_RANGE == 0) {
      v = __atomic_fetch_add(&info->counter, COUNTER_RANGE, __ATOMIC_RELAXED);
    }
    ++v;
    locals[worker_id].counter = v;
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
  assert(worker_id == 0);
}

void assert_not_master(void) {
  assert(worker_id != 0);
}

/*****************************************************************************/

CAMLprim value hh_stop_workers(void) {
  CAMLparam0();
  assert_master();
  info->workers_should_exit = 1;
  CAMLreturn(Val_unit);
}

CAMLprim value hh_resume_workers(void) {
  CAMLparam0();
  assert_master();
  info->workers_should_exit = 0;
  CAMLreturn(Val_unit);
}

CAMLprim value hh_set_can_worker_stop(value val) {
  CAMLparam1(val);
  worker_can_exit = Bool_val(val);
  CAMLreturn(Val_unit);
}

CAMLprim value hh_get_can_worker_stop(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_bool(worker_can_exit));
}

void check_should_exit(void) {
  assert(info != NULL);
  if(worker_can_exit && info->workers_should_exit) {
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
  size_t hashtbl_slots = info->hashtbl_slots;
  for (size_t i = 0; i < hashtbl_slots; i++) {
    // Skip empty slots
    if (hashtbl[i].addr == NULL_ADDR) { continue; }

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
  size_t dest = info->heap_init;

  // Pointer that walks the heap from bottom to top.
  char *src = Ptr_of_offset(info->heap_init);
  char *heap_ptr = Ptr_of_offset(info->heap);

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

  info->heap = dest;
  info->wasted_heap_size = 0;

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
  size_t offset = __sync_fetch_and_add(&info->heap, slot_size);
  if (offset + slot_size > info->heap_max) {
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
  intnat uncompressed_size = 0;

  // Note: we take ownership of the memory allocated by this call
  intnat serialized_size;
  caml_output_value_to_malloc(
    data, Val_int(0)/*flags*/, &value, &serialized_size);

  assert(serialized_size >= 0);
  intnat heap_size = serialized_size;

  // We limit the size of elements we will allocate to our heap to ~2GB
  assert(heap_size < 0x80000000);
  *orig_size = heap_size;

  size_t max_compression_size = LZ4_compressBound(heap_size);
  char* compressed_data = malloc(max_compression_size);
  size_t compressed_size = LZ4_compress_default(
    value,
    compressed_data,
    heap_size,
    max_compression_size);

  if (compressed_size != 0 && compressed_size < heap_size) {
    uncompressed_size = heap_size;
    heap_size = compressed_size;
  }

  *alloc_size = heap_size;

  // Both size and uncompressed_size will certainly fit in 31 bits, as the
  // original size fits per the assert above and we check that the compressed
  // size is less than the original size.
  hh_header_t header
    = heap_size << 32
    | uncompressed_size << 1
    | 1;

  addr_t heap_addr = hh_alloc(
    header,
    uncompressed_size ? compressed_data : value);

  free(compressed_data);
  // We temporarily allocate memory using malloc to serialize the Ocaml object.
  // When we have finished copying the serialized data into our heap we need
  // to free the memory we allocated to avoid a leak.
  free(value);

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
  CAMLlocal1(result);
  check_should_exit();

  // Optimistically write this value to the heap. If the value is already
  // present in the heap, the write will still proceed, but the address will not
  // be registered with the hash table. If the caller expects the value may
  // already be present, the caller should first do a mem check before adding.
  //
  // If concurrently executing threads race to write the same value, then they
  // might each successfully write to the heap, but only one thread will
  // successfully register their write to the hash table.
  size_t alloc_size = 0;
  size_t orig_size = 0;
  helt_t elt;
  elt.hash = get_hash(key);
  elt.addr = hh_store_ocaml(data, &alloc_size, &orig_size);

  // Record sizes written to the heap. Note that we will track these sizes even
  // if the value written above is not recorded in the hash table.
  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_long(alloc_size));
  Store_field(result, 1, Val_long(orig_size));

  size_t hashtbl_slots = info->hashtbl_slots;
  unsigned int slot = elt.hash & (hashtbl_slots - 1);
  unsigned int init_slot = slot;

  while (1) {
    uint64_t slot_hash = hashtbl[slot].hash;

    if (slot_hash == elt.hash) {
      // This value has already been been written to this slot, except that the
      // value may have been deleted. Deleting a slot leaves the hash in place
      // but replaces the addr field with NULL_ADDR. In this case, we can re-use
      // the slot by writing the new address into.
      //
      // Remember that only reads and writes can happen concurrently, so we
      // don't need to worry about concurrent deletes.

      if (hashtbl[slot].addr == NULL_ADDR) {
        // Two threads may be racing to write this value, so try to grab the slot
        // atomically.
        if (__sync_bool_compare_and_swap(&hashtbl[slot].addr, NULL_ADDR, elt.addr)) {
          __sync_fetch_and_add(&info->hcounter_filled, 1);
        }
      }

      break;
    }

    if (info->hcounter >= hashtbl_slots) {
      // We're never going to find a spot
      raise_hash_table_full();
    }

    if (slot_hash == 0) {
      // This slot is free, but two threads may be racing to write to this slot,
      // so try to grab the slot atomically. Note that this is a 16-byte CAS
      // writing both the hash and the address at the same time. Whatever data
      // was in the slot at the time of the CAS will be stored in `old`.
      helt_t old;
      old.value = __sync_val_compare_and_swap(&hashtbl[slot].value, 0, elt.value);

      if (old.hash == 0) {
        // The slot was still empty when we tried to CAS, meaning we
        // successfully grabbed the slot.
        uint64_t size = __sync_fetch_and_add(&info->hcounter, 1);
        __sync_fetch_and_add(&info->hcounter_filled, 1);
        assert(size < hashtbl_slots); // sanity check
        break;
      }

      if (old.hash == elt.hash) {
        // The slot was non-zero, so we failed to grab the slot. However, the
        // thread which won the race wrote the value we were trying to write,
        // meaning out work is done.
        break;
      }
    }

    slot = (slot + 1) & (hashtbl_slots - 1);
    if (slot == init_slot) {
      // We're never going to find a spot
      raise_hash_table_full();
    }
  }

  CAMLreturn(result);
}

/*****************************************************************************/
/* Finds the slot corresponding to the key in a hash table. The returned slot
 * is either free or points to the key.
 */
/*****************************************************************************/
static unsigned int find_slot(value key) {
  size_t hashtbl_slots = info->hashtbl_slots;
  uint64_t hash = get_hash(key);
  unsigned int slot = hash & (hashtbl_slots - 1);
  unsigned int init_slot = slot;
  while(1) {
    if(hashtbl[slot].hash == hash) {
      return slot;
    }
    if(hashtbl[slot].hash == 0) {
      return slot;
    }
    slot = (slot + 1) & (hashtbl_slots - 1);

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
  check_should_exit();
  helt_t elt = hashtbl[find_slot(key)];
  CAMLreturn(Val_bool(elt.hash == get_hash(key) && elt.addr != NULL_ADDR));
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

  result = caml_input_value_from_block(data, size);

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
    info->hcounter += 1;
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
  info->wasted_heap_size += slot_size;
  hashtbl[slot].addr = NULL_ADDR;
  info->hcounter_filled -= 1;
}
