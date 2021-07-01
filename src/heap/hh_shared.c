/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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
#include <caml/bigarray.h>

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

#include <limits.h>
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
#define WORD_SIZE sizeof(value)

#define __ALIGN_MASK(x,mask)    (((x)+(mask))&~(mask))
#define ALIGN(x,a)              __ALIGN_MASK(x,(typeof(x))(a)-1)
#define CACHE_ALIGN(x)          ALIGN(x,CACHE_LINE_SIZE)
#define WORD_ALIGN(x)           ALIGN(x,WORD_SIZE)

/* Each process reserves a range of values at a time from the shared counter.
 * Should be a power of two for more efficient modulo calculation. */
#define COUNTER_RANGE 2048

/*****************************************************************************/
/* Types */
/*****************************************************************************/

/* Convention: bsize = size in bytes, wsize = size in words. */

// Locations in the heap are encoded as byte offsets from the beginning of the
// hash table. Because all data in the heap is word-aligned, these offsets will
// always have 0 in the 3 low bits.
//
// Currently, we rely on the least significant bit being 0 to distinguish these
// addresses from headers. Eventually we may want to rely on the 2 lower bits,
// to represent 4 states in the GC tags instead of 3 (e.g., a gray color state
// for incremental marking.)
//
// Note that the offsets do not start at the beginning of the heap, but the
// start of the hash table. This has two important implications:
//
// 1. The offset 0 will always point to the hash of the first hash table entry,
// which is never a meaningful offset. Because of this, we can take the address
// 0 to be the "null" address.
//
// 2. During garbage collection, it is necessary to point from the heap to the
// hash table itself, since we temporarily store heap headers in the addr field
// of helt_t. By starting the offsets at the beginning of the hash table, we can
// represent offsets into the hash table itself.
typedef uintnat addr_t;

typedef struct {
  /* Layout information, used by workers to create memory mappings. */
  size_t locals_bsize;
  size_t hashtbl_bsize;
  size_t heap_bsize;
  size_t shared_mem_bsize;

  /* Maximum number of hashtable elements */
  size_t hashtbl_slots;

  /* Where the heap started (bottom), offset from hashtbl pointer */
  addr_t heap_init;

  /* Where the heap will end (top), offset from hashtbl pointer */
  addr_t heap_max;

  uintnat gc_phase;

  /* Bytes which are free (color=Blue). This quantity is initially 0 and
   * incremented during the GC sweep phase. The number will increase
   * monotonically until compaction, when all free space is reclaimed. */
  uintnat free_bsize;

  /* Logging level for shared memory statistics
   * 0 = nothing
   * 1 = log totals, averages, min, max bytes marshalled and unmarshalled
   * 2+ = log size of deserialized values in OCaml heap
   */
  size_t log_level;

  /* Initially 0; set to 1 to signal that workers should exit */
  size_t workers_should_exit;

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
  alignas(128) addr_t heap;
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
//  6                               3                             0  0    0 00
//  3                               6                             8  7    2 10
// +-------------------------------+--------------------------------+------+--+
// |11111111 11111111 11111111 1111|1111 11111111 11111111 11111111 |111111|11|
// +-------------------------------+--------------------------------+------+--+
// |                               |                                |      |
// |                               |                                |      * 0-1 GC
// |                               |                                |
// |                               |                                * 2-7 tag
// |                               |
// |                               * 31-1 decompress capacity (in words)
// * 63-32 compressed size (in words)
//
// For GC, to distinguish headers from (word-aligned) pointers, the least bits
// are never 00. The remaining 6 bits of the low word are used to encode a tag,
// describing the type of object.
//
// For serialized objects (tag = 0), the remaining 7 bytes of the header word
// encode two sizes, as outlined above, but other kinds of objects can use these
// bytes differently, as long as it's possible to recover the size of the heap
// object -- i.e., implement Obj_wosize, below.
typedef uintnat hh_header_t;

// The reserved header bits contain a tag used to distinguish between different
// object layouts and find pointers within the object.
typedef uintnat hh_tag_t;

// Keep these in sync with "tag" type definition in sharedMem.ml
#define Serialized_tag 0
#define Addr_tbl_tag 13

static _Bool should_scan(hh_tag_t tag) {
  // By convention, tags below Addr_tbl_tag contain no pointers, whereas
  // Addr_tbl_tag and above contain only pointers. We can exploit this fact to
  // reduce pointer finding to a single branch.
  //
  // In the future, if we add different layouts with a mixture of pointers and
  // other data, scanning for pointers will probably require a jump table.
  return tag >= Addr_tbl_tag;
}

#define NULL_ADDR 0
#define Addr_of_ptr(entry) ((char *)(entry) - (char *)hashtbl)
#define Ptr_of_addr(addr) ((char *)hashtbl + (addr))
#define Entry_of_addr(addr) ((heap_entry_t *)Ptr_of_addr(addr))

#define Deref(addr) (*(uintnat *)(Ptr_of_addr(addr))) /* also an l-value */

// During GC, we read words from the heap which might be an addr or a header,
// and we need to distinguish between them.
#define Is_addr(x) (((x) & 0b11) == 0)

// The low 2 bits of headers are reserved for GC. The white bit pattern
// denotes an unmarked object, black denotes a marked object, and blue denotes a
// free object.
#define Color_white 0b01
#define Color_black 0b11
#define Color_blue  0b10

#define Color_hd(hd) ((hd) & 0b11)

#define Is_white(hd) (Color_hd(hd) == Color_white)
#define Is_black(hd) (Color_hd(hd) == Color_black)
#define Is_blue(hd) (Color_hd(hd) == Color_blue)

#define White_hd(hd) (((hd) & ~0b11) | Color_white)
#define Black_hd(hd) ((hd) | Color_black)
#define Blue_hd(hd) (((hd) & ~0b11) | Color_blue)

// Object headers contain a mark bit, tag, and size information. Objects with
// the serialized tag contain the size information in a slightly different place
// from other objects, so we need to look up the tag to read the size.

#define Obj_tag(hd) (((hd) >> 2) & 0x3F)
#define Obj_wosize_shift(tag) ((tag) == Serialized_tag ? 36 : 8)
#define Obj_wosize_tag(hd, tag) ((hd) >> Obj_wosize_shift(tag))
#define Obj_wosize(hd) (Obj_wosize_tag(hd, Obj_tag(hd)))
#define Obj_whsize(hd) (1 + Obj_wosize(hd))
#define Obj_bhsize(hd) (Bsize_wsize(Obj_whsize(hd)))

// Addrs point to the object header, so field 0 is +1 word. We should consider
// making addrs point to the first field, and accessing the header at a -1 word
// offset instead.
#define Obj_field(addr, i) ((addr) + ((i) + 1) * WORD_SIZE)

// Each heap entry starts with a word-sized header. The header encodes the size
// (in words) of the entry in the heap and the capacity (in words) of the buffer
// needed to decompress the entry.
#define Entry_wsize(header) ((header) >> 36)
#define Entry_decompress_capacity(header) (Bsize_wsize(((header) >> 8) & 0xFFFFFFF))

// The distance (in bytes) from one hh_entry_t* to the next. Entries are laid
// out contiguously in memory.
#define Heap_entry_slot_size(header) \
  (sizeof(heap_entry_t) + Bsize_wsize(Entry_wsize(header)))

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
/* GC */
/*****************************************************************************/

// The possible values of info->gc_phase
#define Phase_idle 0
#define Phase_mark 1
#define Phase_sweep 2

// The mark stack used during heap traversal for the GC's marking pass, which
// visits every heap object once.
//
// When initializing the heap, we allocate stack space which is reused across
// collections. During a marking pass, if we exceed the preallocated space, we
// will grow the stack. If we grow the mark stack during a collection, we will
// free that additional space at the end of the collection.
//
// The max size is explicit to avoid exhausting available memory in the event of
// a programmer error. We should not hit this limit, or come close to it. It
// might become necessary to handle a mark stack overflow without crashing, but
// this is not implemented.
#define MARK_STACK_INIT_SIZE 4096
#define MARK_STACK_MAX_SIZE (1024 * 1024 * 200)

// The current size of the mark stack buffer.
static uintnat mark_stack_size = 0;

// Note: because collection only happens on the master process, the following
// pointers are only initialized in the master process and will remain NULL in
// workers.

// The initial stack space, allocated at startup for the mark stack. This space
// will persist between collections.
static addr_t *mark_stack_init = NULL;

// Base of the current mark stack. This is initially aliased to
// mark_stack_init, but will change if the mark stack is realloced.
static addr_t *mark_stack = NULL;

// Head of the current mark stack, equal to `mark_stack` when the stack is
// empty, adjusted during push/pop.
static addr_t *mark_stack_ptr = NULL;

// End of the current mark stack, equal to `mark_stack + mark_stack_size`, used
// to trigger resize.
static addr_t *mark_stack_end = NULL;

// When we start a GC, we record the heap pointer here. We use this to identify
// allocations performed during marking. These objects are not explicitly
// marked, but are treated as reachable during the current collection pass.
//
// This address should always fall between info->heap_init and info->heap. This
// invariant is set up in hh_shared_init and maintained in hh_collect_slice.
static addr_t gc_end = NULL_ADDR;

// The marking phase treats the shared hash table as GC roots, but these are
// marked incrementally. Because we might modify the hash table between mark
// slices, we insert write barriers in hh_remove and hh_move.
static uintnat roots_ptr = 0;

// Holds the current position of the sweep phase between slices.
static addr_t sweep_ptr = NULL_ADDR;

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

CAMLprim value hh_used_heap_size(value unit) {
  CAMLparam1(unit);
  assert(info != NULL);
  CAMLreturn(Val_long(info->heap - info->heap_init));
}

CAMLprim value hh_new_alloc_size(value unit) {
  CAMLparam1(unit);
  assert(info != NULL);
  CAMLreturn(Val_long(info->heap - gc_end));
}

CAMLprim value hh_free_heap_size(value unit) {
  CAMLparam1(unit);
  assert(info != NULL);
  CAMLreturn(Val_long(info->free_bsize));
}

CAMLprim value hh_gc_phase(value unit) {
  CAMLparam1(unit);
  assert(info != NULL);
  CAMLreturn(Val_long(info->gc_phase));
}

CAMLprim value hh_log_level(value unit) {
  CAMLparam1(unit);
  assert(info != NULL);
  CAMLreturn(Val_long(info->log_level));
}

CAMLprim value hh_hash_stats(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(stats);

  stats = caml_alloc_tuple(3);
  Store_field(stats, 0, Val_long(info->hcounter));
  Store_field(stats, 1, Val_long(info->hcounter_filled));
  Store_field(stats, 2, Val_long(info->hashtbl_slots));

  CAMLreturn(stats);
}

static void raise_failed_memfd_init(int errcode) {
  static const value *exn = NULL;
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
static void memfd_init(size_t shared_mem_size) {
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
static void memfd_init(size_t shared_mem_size) {
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
  static const value *exn = NULL;
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
 * memfd file. */
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

static void map_info_page(int page_bsize) {
  // The first page of shared memory contains (1) size information describing
  // the layout of the rest of the shared file; (2) values which are atomically
  // updated by workers, like the heap pointer; and (3) various configuration
  // which is convenient to stick here, like the log level.
  assert(page_bsize >= sizeof(shmem_info_t));
  info = (shmem_info_t*)memfd_map(page_bsize);

#ifdef _WIN32
  // Memory must be reserved on Windows
  win_reserve((char *)info, page_bsize);
#endif
}

static void define_mappings(int page_bsize) {
  assert(info != NULL);
  size_t locals_bsize = info->locals_bsize;
  size_t hashtbl_bsize = info->hashtbl_bsize;
  size_t heap_bsize = info->heap_bsize;

  shared_mem = memfd_map(info->shared_mem_bsize);

  /* Process-local storage */
  locals = (local_t*)(shared_mem + page_bsize);

  /* Hashtable */
  hashtbl = (helt_t*)(shared_mem + page_bsize + locals_bsize);

#ifdef _WIN32
  // Memory must be reserved on Windows. Heap allocations will be reserved
  // in hh_alloc, so we just reserve the locals and hashtbl memory here.
  win_reserve((char *)locals, locals_bsize);
  win_reserve((char *)hashtbl, hashtbl_bsize);
#endif

#ifdef MADV_DONTDUMP
  // We are unlikely to get much useful information out of the shared heap in
  // a core file. Moreover, it can be HUGE, and the extensive work done dumping
  // it once for each CPU can mean that the user will reboot their machine
  // before the much more useful stack gets dumped!
  madvise(hashtbl, hashtbl_bsize + heap_bsize, MADV_DONTDUMP);
#endif
}

static value alloc_heap_bigarray(void) {
  CAMLparam0();
  CAMLlocal1(heap);
  int heap_flags = CAML_BA_CHAR | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL;
  intnat heap_dim[1] = {info->hashtbl_bsize + info->heap_bsize};
  heap = caml_ba_alloc(heap_flags, 1, hashtbl, heap_dim);
  CAMLreturn(heap);
}

/*****************************************************************************/
/* Must be called by the master BEFORE forking the workers! */
/*****************************************************************************/

CAMLprim value hh_shared_init(
  value config_val,
  value num_workers_val
) {
  CAMLparam2(config_val, num_workers_val);
  CAMLlocal1(result);

  int page_bsize = getpagesize();

  /* Calculate layout information. We need to figure out how big the shared file
   * needs to be in order to create the file. We will also store enough of the
   * layout information in the first page of the shared file so that workers can
   * create mappings for the rest of the shared data. */
  size_t num_workers = Long_val(num_workers_val);
  size_t locals_bsize = CACHE_ALIGN((1 + num_workers) * sizeof(local_t));
  size_t hashtbl_slots = 1ul << Long_val(Field(config_val, 1));
  size_t hashtbl_bsize = CACHE_ALIGN(hashtbl_slots * sizeof(helt_t));
  size_t heap_bsize = Long_val(Field(config_val, 0));

  /* The total size of the shared file must have space for the info page, local
   * data, the hash table, and the heap. */
  size_t shared_mem_bsize =
    page_bsize + locals_bsize + hashtbl_bsize + heap_bsize;

  memfd_init(shared_mem_bsize);

  /* The info page contains (1) size information describing the layout of the
   * rest of the shared file; (2) values which are atomically updated by
   * workers, like the heap pointer; and (3) various configuration which is
   * conventient to stick here, like the log level. */
  map_info_page(page_bsize);
  info->locals_bsize = locals_bsize;
  info->hashtbl_bsize = hashtbl_bsize;
  info->heap_bsize = heap_bsize;
  info->shared_mem_bsize = shared_mem_bsize;
  info->hashtbl_slots = hashtbl_slots;
  info->heap_init = hashtbl_bsize;
  info->heap_max = info->heap_init + heap_bsize;
  info->gc_phase = Phase_idle;
  info->log_level = Long_val(Field(config_val, 2));

  // Ensure the global counter starts on a COUNTER_RANGE boundary
  info->counter = ALIGN(early_counter + 1, COUNTER_RANGE);

  // Initialize top heap pointers
  info->heap = info->heap_init;

  define_mappings(page_bsize);

  mark_stack_size = MARK_STACK_INIT_SIZE;
  mark_stack_init = malloc(MARK_STACK_INIT_SIZE * sizeof(addr_t));
  mark_stack = mark_stack_init;
  mark_stack_ptr = mark_stack;
  mark_stack_end = mark_stack + MARK_STACK_INIT_SIZE;

  // Invariant: info->heap_init <= gc_end <= info->heap
  // See declaration of gc_end
  gc_end = info->heap;

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

  result = caml_alloc_tuple(2);
  Store_field(result, 0, alloc_heap_bigarray());
  Store_field(result, 1, Val_handle(memfd));

  CAMLreturn(result);
}

/* Must be called by every worker before any operation is performed */
value hh_connect(value handle_val, value worker_id_val) {
  CAMLparam2(handle_val, worker_id_val);
  memfd = Handle_val(handle_val);
  worker_id = Long_val(worker_id_val);

  // Avoid confusion with master process, which is designated 0
  assert(worker_id > 0);

  int page_bsize = getpagesize();
  map_info_page(page_bsize);
  define_mappings(page_bsize);

  CAMLreturn(alloc_heap_bigarray());
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

CAMLprim value hh_counter_next(value unit) {
  CAMLparam1(unit);
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
static void assert_master(void) {
  assert(worker_id == 0);
}

static void assert_not_master(void) {
  assert(worker_id != 0);
}

/*****************************************************************************/

CAMLprim value hh_stop_workers(value unit) {
  CAMLparam1(unit);
  assert_master();
  info->workers_should_exit = 1;
  CAMLreturn(Val_unit);
}

CAMLprim value hh_resume_workers(value unit) {
  CAMLparam1(unit);
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

static void check_should_exit(void) {
  assert(info != NULL);
  if(worker_can_exit && info->workers_should_exit) {
    static const value *exn = NULL;
    if (!exn) exn = caml_named_value("worker_should_exit");
    caml_raise_constant(*exn);
  }
}

CAMLprim value hh_check_should_exit(value unit) {
  CAMLparam1(unit);
  check_should_exit();
  CAMLreturn(Val_unit);
}

/*****************************************************************************/
/* GC: Incremental Mark and Sweep
 *
 * Before compacting the heap, we must first find all live values. We can mark
 * all live values in the heap by starting with the root objects in the hash
 * table, then traversing the graph of all reachable objects from those roots.
 *
 * To avoid long pauses, we do this work incrementally. Between commands, we
 * perform "slices" of mark and sweep work. After a slice of work, we return
 * back to the server, which can either handle another request of perform
 * another slice.
 *
 * Because the program can modify the heap between slices of mark and sweep, we
 * need to be careful that all reachable objects are marked. We use a shapshot-
 * at-the-beginning approach, which ensures that all reachable objects at the
 * beginning of GC pass are marked. We also use an "allocate black" strategy,
 * meaning that any new objects allocated during a collection are considered
 * reachable.
 *
 * For snapshot-at-the-beginning, we use a Yuasa style deletion barrier. If a
 * field is modified during collection, the "old" reference is added to the mark
 * stack. The only modifications that happen during a GC pass are hh_move and
 * hh_remove. These are both only called from the main server process, which
 * means we don't need to store the mark stack in shared memory.
 *
 * The "allocate black" strategy is a bit non-standard. Because the shared heap
 * is a bump allocator, we don't actually use the black color for new
 * allocations. Instead, we record the location of the heap pointer at the
 * beginning of a collection. Any addresses below that address need to be
 * marked, while any addresses above that address are assumed to be live.
 */
/*****************************************************************************/

// Trigger the start of a new cycle (idle -> mark)
CAMLprim value hh_start_cycle(value unit) {
  CAMLparam1(unit);
  assert(info->gc_phase == Phase_idle);
  gc_end = info->heap;
  roots_ptr = 0;
  sweep_ptr = info->heap_init;
  info->gc_phase = Phase_mark;
  CAMLreturn(Val_unit);
}

static void mark_stack_overflow() {
  caml_failwith("mark_stack_resize: could not allocate space for mark stack");
}

// Mark stack starts out with some initial capacity, which grows as needed.
static void mark_stack_resize(void) {
  assert(mark_stack_ptr == mark_stack_end);

  uintnat new_size = 2 * mark_stack_size;

  // To avoid exhausting the heap in the event of a programmer error, we fail if
  // the mark stack exceeds some fixed huge size (currently ~1.5 GB).
  if (new_size >= MARK_STACK_MAX_SIZE) mark_stack_overflow();

  // Keep the initial stack, which will be restored by mark_stack_reset.
  // Otherwise realloc, which frees the underlying memory if necessary.
  addr_t *new_stack;
  if (mark_stack == mark_stack_init) {
    new_stack = malloc(new_size * sizeof(addr_t));
    if (new_stack == NULL) mark_stack_overflow();
    memcpy(new_stack, mark_stack_init, MARK_STACK_INIT_SIZE * sizeof(addr_t));
  } else {
    new_stack = realloc(mark_stack, new_size * sizeof(addr_t));
    if (new_stack == NULL) mark_stack_overflow();
  }

  mark_stack = new_stack;
  mark_stack_ptr = new_stack + mark_stack_size;
  mark_stack_size = new_size;
  mark_stack_end = mark_stack + new_size;
}

// Free any additional stack space allocated during marking. The initial stack
// space allocation is re-used across marking passes.
static void mark_stack_reset(void) {
  if (mark_stack != mark_stack_init) {
    free(mark_stack);
    mark_stack_size = MARK_STACK_INIT_SIZE;
    mark_stack = mark_stack_init;
    mark_stack_ptr = mark_stack;
    mark_stack_end = mark_stack + MARK_STACK_INIT_SIZE;
  }
}

// Add a reachable object to the mark stack.
//
// Objects allocated during marking will have an address greater than gc_end
// and are treated as reachable. This is morally an "allocate black" scheme,
// except we allocate white to avoid needing to sweep. Because we allocate
// white and don't sweep these addresses, it's important that they are not
// darkened.
static inline void mark_slice_darken(addr_t addr) {
  if (addr != NULL_ADDR && addr < gc_end) {
    hh_header_t hd = Deref(addr);
    if (Is_white(hd)) {
      Deref(addr) = Black_hd(hd);
      if (mark_stack_ptr == mark_stack_end) {
        mark_stack_resize();
      }
      *mark_stack_ptr++ = addr;
    }
  }
}

// Perform a bounded amount of marking work, incrementally. During the marking
// phase, this function is called repeatedly until marking is complete. Once
// complete, this function will transition to the sweep phase.
CAMLprim value hh_mark_slice(value work_val) {
  CAMLparam1(work_val);
  assert(info->gc_phase == Phase_mark);

  // We are able to partially scan an object for pointers and resume scanning in
  // a subsequent slice. This is useful in the event of large objects which
  // would otherwise cause long pauses if we needed to scan them all at once.
  //
  // If we stop in the middle of an object, we will store the address of that
  // object and the index of the field where we should resume. Otherwise, these
  // values will be NULL_ADDR and 0 respectively.
  static addr_t current_value = NULL_ADDR;
  static uintnat current_index = 0;

  intnat work = Long_val(work_val);
  intnat hashtbl_slots = info->hashtbl_slots;

  addr_t v;
  hh_header_t hd;
  hh_tag_t tag;
  uintnat i, size, start, end;

  // If the previous slice stopped in the middle of scanning an object, the
  // first thing we do in this slice is resume scanning where we left off.
  v = current_value;
  start = current_index;

  // Work through the mark stack, scanning all gray objects for pointers.
  // Because roots are colored gray but not added to the mark stack, also walk
  // the heap to find marked roots.
  while (work > 0) {
    if (v == NULL_ADDR && mark_stack_ptr > mark_stack) {
      v = *--mark_stack_ptr;
    }
    if (v != NULL_ADDR) {
      hd = Deref(v);
      tag = Obj_tag(hd);
      size = Obj_wosize_tag(hd, tag);
      if (should_scan(tag)) {
        // Avoid scanning large objects all at once
        end = start + work;
        if (size < end) {
          end = size;
        }
        for (i = start; i < end; i++) {
          mark_slice_darken(Deref(Obj_field(v, i)));
        }
        if (end < size) {
          // We did not finish scanning this object. We will resume scanning
          // this object in the next slice.
          start = end;
        } else {
          v = NULL_ADDR;
          start = 0;
        }
      } else {
        v = NULL_ADDR;
      }
      work--;
    } else if (roots_ptr < hashtbl_slots) {
      // Visit roots in shared hash table
      mark_slice_darken(hashtbl[roots_ptr++].addr);
      work--;
    } else {
      // Done marking, transition to sweep phase.
      mark_stack_reset();
      info->gc_phase = Phase_sweep;
      break;
    }
  }

  current_value = v;
  current_index = start;

  CAMLreturn(Val_long(work));
}

// Perform a bounded amount of sweeping work, incrementally. During the sweeping
// phase, this function is called repeatedly until sweeping is complete. Once
// complete, this function will transition to the idle phase.
CAMLprim value hh_sweep_slice(value work_val) {
  CAMLparam1(work_val);
  assert(info->gc_phase == Phase_sweep);

  intnat work = Long_val(work_val);

  while (work > 0) {
    if (sweep_ptr < gc_end) {
      uintnat hd = Deref(sweep_ptr);
      uintnat bhsize = Obj_bhsize(hd);
      switch (Color_hd(hd)) {
        case Color_white:
          Deref(sweep_ptr) = Blue_hd(hd);
          info->free_bsize += bhsize;
          break;
        case Color_black:
          Deref(sweep_ptr) = White_hd(hd);
          break;
        case Color_blue:
          break;
      }
      sweep_ptr += bhsize;
      work--;
    } else {
      // Done sweeping
      info->gc_phase = Phase_idle;
      break;
    }
  }

  CAMLreturn(Val_long(work));
}

/*****************************************************************************/
/* GC: Compact
 *
 * We collect the shared heap by compacting: moving live values "to the left"
 * until there is no more free space. We can then continue to bump allocate from
 * the end.
 *
 * The compaction algorithm is a Jonkers collector which performs the compaction
 * "in place" without allocating additional memory to maintain state.
 *
 * The algorithm is published, unfortunately, behind a costly subscription.
 * https://doi.org/10.1016/0020-0190(79)90103-0
 *
 * Happily, an excellent description of the algorithm can be found in a freely
 * accessible paper by Benedikt Meurer, along with an extension for interior
 * pointers which is unused here:
 * https://benediktmeurer.de/files/fast-garbage-compaction-with-interior-pointers.pdf
 *
 * This particular algorithm has some nice properties, namely:
 * - Heap objects can have varying size
 * - We can compact the heap in-place without auxiliary storage
 * - The compacted heap preserves order, keeping related objects close together
 * - Is actually pretty simple, in the sense that it has few moving pieces,
 *   although pointer reversal can take a moment to "click"
 *
 * However, there are also downsides to this choice:
 * - Is fully stop-the-world and non-incremental
 * - Pointer reversal techniques are not cache-friendly, and generally slow
 *   compared to both contemporary and modern techniques
 *
 * Happily, the bursty nature of the type checker means that there are long (in
 * human scale) periods of down time between requests, so a long pause is not a
 * problem as long as it is well timed.
 *
 * For future work, it might be worthwhile to explore more incremental GC
 * strategies, which could spread the work over more small pauses instead.
 */
/*****************************************************************************/

// Threading is the fundamental operation of the GC. Moving objects potentially
// invalidates every address in the heap. Threading makes it possible to update
// those addresses without requiring extra storage.
//
// In a single step, threading replaces a pointer to a value with the value
// itself. Where the value was, we insert a pointer to original pointer. This
// swap is more easily understood when visualized:
//
// P       Q
// +---+   +---+
// | * |-->| X |
// +---+   +---+
//
// becomes
//
// P       Q
// +---+   +---+
// | X |<--| * |
// +---+   +---+
//
// Performing this single step repeatedly has the effect of replacing multiple
// pointers to a given value with a linked list (or "thread") of pointers to the
// value. For example:
//
// P       Q       R
// +---+   +---+   +---+
// | * |   | * |-->| X |
// +---+   +---+   +---+
//   |             ^
//   +-------------+
//
// becomes (in two steps)
//
// P       Q       R
// +---+   +---+   +---+
// | X |<--| * |<--| * |
// +---+   +---+   +---+
static void gc_thread(addr_t p) {
  if (Deref(p) == NULL_ADDR) { return; }
  uintnat q = Deref(p);
  Deref(p) = Deref(q);
  Deref(q) = p;
}

// As we walk the heap, we must be sure to thread every pointer to live data, as
// any live object may be relocated.
static void gc_scan(addr_t addr) {
  hh_header_t hd = Deref(addr);
  hh_tag_t tag = Obj_tag(hd);
  if (should_scan(tag)) {
    for (int i = 0; i < Obj_wosize_tag(hd, tag); i++) {
      gc_thread(Obj_field(addr, i));
    }
  }
}

// With the heap threaded, we can now relocate "R" to a different known address
// by "unthreading," or following the linked list of pointers until we reach the
// original value X. Each word in the thread is replaced with the new address
// and the original value is copied back into place.
static void gc_update(addr_t src, addr_t dst) {
  uintnat p = Deref(src);
  while (Is_addr(p)) {
    uintnat q = Deref(p);
    Deref(p) = dst;
    p = q;
  }
  Deref(src) = p;
}

// Compacting the heap proceeds in three phases:
// 1. Thread the root set
// 2. Walk heap, update forward pointers
// 3. Walk heap, update backward pointers and move objects
CAMLprim value hh_compact(value unit) {
  CAMLparam1(unit);
  assert_master();
  assert(info->gc_phase == Phase_idle);

  intnat hashtbl_slots = info->hashtbl_slots;

  // Step 1: Scan the root set, threading any pointers to the heap. The
  // threading performed during this step will be unthreaded in the next step,
  // updating the hash table to point to the updated locations.
  for (intnat i = 0; i < hashtbl_slots; i++) {
    addr_t hashtbl_addr = Addr_of_ptr(&hashtbl[i].addr);
    gc_thread(hashtbl_addr);
  }

  // Step 2: Scan the heap object-by-object from bottom to top. The dst pointer
  // keeps track of where objects will move to, but we do not move anything
  // during this step.
  //
  // If we encounter an unmarked header, the object is unreachable, so do not
  // update the dst pointer.
  //
  // If we encounter an address, then this object was reachable via "forward"
  // reference, i.e., a pointer stored at a lower address. Because we know where
  // the object will move to (dst), we eagerly update the forward references and
  // copy the original header back.
  //
  // If we encounter a marked header, then the object is reachable only via
  // "backwards" reference. These backwards references will be handled in the
  // next step.
  //
  // NB: Instead of scanning the entire heap, it may be worthwhile to track the
  // min/max live addresses during the marking phase, and only scan that part.
  // Possible that the extra marking work would be more expensive than a linear
  // memory scan, but worth experimenting.
  //
  // NB: Also worth experimenting with explicit prefetching.
  addr_t src = info->heap_init;
  addr_t dst = info->heap_init;
  addr_t heap_ptr = info->heap;
  while (src < heap_ptr) {
    hh_header_t hd = Deref(src);
    intnat size;
    if (Is_blue(hd)) {
      size = Obj_bhsize(hd);
    } else {
      gc_update(src, dst);
      hd = Deref(src);
      size = Obj_bhsize(hd);
      gc_scan(src);
      dst += size;
    }
    src += size;
  }

  // Step 3: Scan the heap object-by-object again, actually moving objects this
  // time around.
  //
  // Unmarked headers still indicate unreachable data and is not moved.
  //
  // If we encounter an address, then the object was reachable via a "backwards"
  // reference from the previous step, and we fix up those references to point
  // to the new location and copy the original header back.
  //
  // Finally we can move the object. We unset the mark bit on the header so that
  // future collections can free the space if the object becomes unreachable.
  src = info->heap_init;
  dst = info->heap_init;
  while (src < heap_ptr) {
    hh_header_t hd = Deref(src);
    intnat size;
    if (Is_blue(hd)) {
      size = Obj_bhsize(hd);
    } else {
      gc_update(src, dst);
      hd = Deref(src);
      size = Obj_bhsize(hd);
      memmove(Ptr_of_addr(dst), Ptr_of_addr(src), size);
      dst += size;
    }
    src += size;
  }

  // TODO: Space between dst and info->heap is unused, but will almost certainly
  // become used again soon. Currently we will never decommit, which may cause
  // issues when there is memory pressure.
  //
  // If the kernel supports it, we might consider using madvise(MADV_FREE),
  // which allows the kernel to reclaim the memory lazily under pressure, but
  // would not force page faults under healthy operation.

  info->heap = dst;

  // Invariant: info->heap_init <= gc_end <= info->heap
  // See declaration of gc_end
  gc_end = dst;

  info->free_bsize = 0;

  CAMLreturn(Val_unit);
}

static void raise_heap_full(void) {
  static const value *exn = NULL;
  if (!exn) exn = caml_named_value("heap_full");
  caml_raise_constant(*exn);
}

/*****************************************************************************/
/* Allocates a slot in the shared heap, given a size (in words). The caller is
 * responsible for initializing the allocated space with valid heap objects. */
/*****************************************************************************/

static addr_t hh_alloc(size_t wsize) {
  size_t slot_size = Bsize_wsize(wsize);
  addr_t addr = __sync_fetch_and_add(&info->heap, slot_size);
  if (addr + slot_size > info->heap_max) {
    raise_heap_full();
  }
  memfd_reserve(Ptr_of_addr(addr), slot_size);
  return addr;
}

CAMLprim value hh_ml_alloc(value wsize) {
  CAMLparam1(wsize);
  addr_t addr = hh_alloc(Long_val(wsize));
  CAMLreturn(Val_long(addr));
}

/*****************************************************************************/
/* Allocates an ocaml value in the shared heap.
 * Any ocaml value is valid, except closures. It returns the address of
 * the allocated chunk.
 */
/*****************************************************************************/
CAMLprim value hh_store_ocaml(value v) {
  CAMLparam1(v);
  CAMLlocal1(result);
  check_should_exit();

  char *serialized, *compressed;
  intnat serialized_size;
  int compress_bound, compressed_size;

  caml_output_value_to_malloc(
    v, Val_int(0)/*flags*/, &serialized, &serialized_size);

  // Compress the serialized data. LZ4's maximum input size is ~2GB. If the
  // input is larger than that, LZ4_compressBound will return 0 and the
  // compression itself will do nothing.
  if (serialized_size > LZ4_MAX_INPUT_SIZE) {
    caml_invalid_argument("hh_store_ocaml: value larger than max input size");
  }

  compress_bound = LZ4_compressBound(serialized_size);
  compressed = malloc(compress_bound);
  compressed_size = LZ4_compress_default(
    serialized,
    compressed,
    serialized_size,
    compress_bound);

  assert(compressed_size > 0);

  // Construct a header to describe the serialized and compressed data:
  //
  // A header is a single word where The low-order byte is reserved, meaning we
  // have 56 bits to store the serialized size and compressed size. Is it
  // enough?
  //
  // In the worst case, we try to compress uncompressible input of
  // LZ4_MAX_INPUT_SIZE, consuming the entire compress bound. That would be
  // 0x7E7E7E8E bytes compressed size.
  //
  // NOTE: The compressed size might actually be bigger than the serialized
  // size, in a worst case scenario where the input is not compressible. This
  // shouldn't happen in practice, but we account for it in the worse case.
  //
  // If we store the size in words instead of bytes, the max size is 0xFCFCFD2
  // words, which fits in 2^28, so we can fit both sizes (in words) in 56 bits.
  //
  // All this is somewhat academic, since we have bigger problems if we're
  // trying to store 2 gig entries.

  // The compressed size is not necessarily word sized. To accommodate this, we
  // use a trick lifted from OCaml's own representation of strings, which also
  // have a header that stores the size in words.
  //
  // In the last byte of the block, we store a value which we can use to recover
  // the exact byte size of the string. If the string is exactly word sized, we
  // add another word to hold the final byte.
  size_t compressed_wsize = (compressed_size + WORD_SIZE) / WORD_SIZE;

  // Similarly, the serialized size might not necessarily be a multiple of the
  // word size. To decompress, we only need to provide a buffer that is large
  // enough, so we round up to the nearest word.
  size_t decompress_capacity = Wsize_bsize(WORD_ALIGN(serialized_size));

  // Just in case the math above doesn't check out
  assert(compressed_size < 0x10000000);
  assert(decompress_capacity < 0x10000000);

  hh_header_t header
    = compressed_wsize << 36
    | decompress_capacity << 8
    | Color_white;

  // Allocate space for the header and compressed data
  heap_entry_t *entry = Entry_of_addr(hh_alloc(1 + compressed_wsize));

  // Write header and data into allocated space.
  entry->header = header;
  memcpy(&entry->data, compressed, compressed_size);

  // Write offset into final byte for byte size calculation
  // See entry_compressed_bsize for how this is used.
  size_t offset_index = Bsize_wsize(compressed_wsize) - 1;
  entry->data[offset_index] = offset_index - compressed_size;

  free(serialized);
  free(compressed);

  result = caml_alloc_tuple(3);
  Store_field(result, 0, Val_long(Addr_of_ptr(entry)));
  Store_field(result, 1, Val_long(compressed_size));
  Store_field(result, 2, Val_long(serialized_size));

  CAMLreturn(result);
}

// The final byte of a compressed heap entry contains an offset, which we can
// use to convert the approximate size in words to the precise size in bytes.
static size_t entry_compressed_bsize(heap_entry_t *entry) {
  size_t compressed_wsize = Entry_wsize(entry->header);
  size_t offset_index = Bsize_wsize(compressed_wsize) - 1;
  return offset_index - entry->data[offset_index];
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
  static const value *exn = NULL;
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
CAMLprim value hh_add(value key, value addr) {
  CAMLparam2(key, addr);
  check_should_exit();

  helt_t elt;
  elt.hash = get_hash(key);
  elt.addr = Long_val(addr);

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

    if (slot_hash == 0) {
      helt_t old;

      // This slot is free, but two threads may be racing to write to this slot,
      // so try to grab the slot atomically. Note that this is a 16-byte CAS
      // writing both the hash and the address at the same time. We expect the
      // slot to contain `0`. If that's the case, `success == true`; otherwise,
      // whatever data was in the slot at the time of the CAS will be stored in
      // `old`.
      old.value = 0;
      _Bool success = __atomic_compare_exchange(
        /* ptr */ &hashtbl[slot].value,
        /* expected */ &old.value,
        /* desired */ &elt.value,
        /* weak */ 0,
        /* success_memorder */ __ATOMIC_SEQ_CST,
        /* failure_memorder */ __ATOMIC_SEQ_CST
      );

      if (success) {
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

  CAMLreturn(Val_unit);
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
CAMLprim value hh_mem(value key) {
  CAMLparam1(key);
  check_should_exit();
  helt_t elt = hashtbl[find_slot(key)];
  CAMLreturn(Val_bool(elt.hash == get_hash(key) && elt.addr != NULL_ADDR));
}

/*****************************************************************************/
/* Deserializes the value at the given address. */
/*****************************************************************************/
CAMLprim value hh_deserialize(value addr_val) {
  CAMLparam1(addr_val);
  CAMLlocal1(result);
  check_should_exit();

  heap_entry_t *entry = Entry_of_addr(Long_val(addr_val));
  size_t compressed_bsize = entry_compressed_bsize(entry);
  size_t decompress_capacity = Entry_decompress_capacity(entry->header);

  char *decompressed = malloc(decompress_capacity);

  size_t serialized_size = LZ4_decompress_safe(
    entry->data,
    decompressed,
    compressed_bsize,
    decompress_capacity);

  result = caml_input_value_from_block(decompressed, serialized_size);
  free(decompressed);

  CAMLreturn(result);
}

/*****************************************************************************/
/* Returns the address associated to a given key. */
/* The key MUST be present. */
/*****************************************************************************/
CAMLprim value hh_get(value key) {
  CAMLparam1(key);
  check_should_exit();

  unsigned int slot = find_slot(key);
  assert(hashtbl[slot].hash == get_hash(key));
  CAMLreturn(Val_long(hashtbl[slot].addr));
}

/*****************************************************************************/
/* Returns the size of the value at the given address. */
/*****************************************************************************/
CAMLprim value hh_get_size(value addr_val) {
  CAMLparam1(addr_val);
  heap_entry_t *entry = Entry_of_addr(Long_val(addr_val));
  size_t compressed_bsize = entry_compressed_bsize(entry);
  CAMLreturn(Val_long(compressed_bsize));
}

/*****************************************************************************/
/* Moves the data associated to key1 to key2.
 * key1 must be present.
 * key2 must be free.
 * Only the master can perform this operation.
 */
/*****************************************************************************/
CAMLprim value hh_move(value key1, value key2) {
  CAMLparam2(key1, key2);
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

  // GC write barrier
  if (info->gc_phase == Phase_mark) {
    mark_slice_darken(hashtbl[slot1].addr);
  }

  hashtbl[slot2].hash = get_hash(key2);
  hashtbl[slot2].addr = hashtbl[slot1].addr;
  hashtbl[slot1].addr = NULL_ADDR;
  CAMLreturn(Val_unit);
}

/*****************************************************************************/
/* Removes a key from the hash table.
 * Only the master can perform this operation.
 */
/*****************************************************************************/
CAMLprim value hh_remove(value key) {
  CAMLparam1(key);
  unsigned int slot = find_slot(key);

  assert_master();
  assert(hashtbl[slot].hash == get_hash(key));
  assert(hashtbl[slot].addr != NULL_ADDR);

  // GC write barrier
  if (info->gc_phase == Phase_mark) {
    mark_slice_darken(hashtbl[slot].addr);
  }

  hashtbl[slot].addr = NULL_ADDR;
  info->hcounter_filled -= 1;
  CAMLreturn(Val_unit);
}

/*****************************************************************************/
/* Blits an OCaml string representation into the shared heap.
 *
 * Note that, like OCaml's heap, the shared heap is word-addressible. Like
 * OCaml's strings, strings in the shared heap are encoded with a header
 * containing the size in words, where the last byte of the last word contains
 * an offset used to calculate the exact bytes size. */
/*****************************************************************************/

CAMLprim value hh_write_string(value addr, value s) {
  memcpy(Ptr_of_addr(Long_val(addr)), String_val(s), Bosize_val(s));
  return Val_unit;
}

/*****************************************************************************/
/* Reads a string in the shared heap into a the OCaml heap.
 *
 * Because we store string data in the shared heap in the same format as OCaml
 * does for it's own heap, we can simply blit the data directly into the OCaml
 * heap, instead of using the designated caml_alloc_string function. */
/*****************************************************************************/
CAMLprim value hh_read_string(value addr, value wsize) {
  CAMLparam2(addr, wsize);
  CAMLlocal1(s);
  s = caml_alloc(Long_val(wsize), String_tag);
  memcpy(String_val(s), Ptr_of_addr(Long_val(addr)), Bsize_wsize(Long_val(wsize)));
  CAMLreturn(s);
}
