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
 *    can is used to quickly answer if a dependency exists. The other one
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

#include <assert.h>

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
#if !defined __APPLE__ && !defined _WIN32
  // We're assuming x86_64 linux here
  #ifndef __x86_64__
    #error "hh_shared.c requires Linux to be x86_64"
  #endif

  #define MEMFD_CREATE 1
  #include <asm/unistd.h>

  /* Originally this function would call uname(), parse the linux
   * kernel release version and make a decision based on whether
   * the kernel was >= 3.17 or not. However, syscall will return -1
   * with an strerr(errno) of "Function not implemented" if the
   * kernel is < 3.17, and that's good enough. Also, I got the value
   * of 319 from here: https://github.com/kernelslacker/trinity/blob/825b51cfe8fcbc7461c9a327411475ae481654c8/include/memfd.h
   */
  static int memfd_create(const char *name, unsigned int flags) {
    return syscall(319, name, flags);
  }
#endif

// The following 'typedef' won't be required anymore
// when dropping support for OCaml < 4.03
#ifdef __MINGW64__
typedef unsigned __int64 uint64_t;
#endif

#ifndef NO_LZ4
#include <lz4.h>
#include <lz4hc.h>
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

static size_t global_size_b;
static size_t heap_size;

// XXX: DEP_POW and HASHTBL_POW are not configurable because we take a ~2% perf
// hit by doing so, likely because the compiler does some constant folding.
// Should revisit this if / when we switch to compiling with an optimization
// level higher than -O0. In lieu of that, let's use a define so we don't use
// absurd amounts of RAM for OSS users.
#ifdef OSS_SMALL_HH_TABLE_POWS
#define DEP_POW         17
#define HASHTBL_POW     18
#else
#define DEP_POW         25
#define HASHTBL_POW     25
#endif

/* Convention: .*_B = Size in bytes. */

/* Used for the dependency hashtable */
#define DEP_SIZE        (1ul << DEP_POW)
#define DEP_SIZE_B      (DEP_SIZE * sizeof(value))

/* Used for the shared hashtable */
#define HASHTBL_SIZE    (1ul << HASHTBL_POW)
#define HASHTBL_SIZE_B  (HASHTBL_SIZE * sizeof(helt_t))

/* Size of where we allocate shared objects. */
#define Get_size(x)     (((size_t*)(x))[-1])
#define Get_buf_size(x) (((size_t*)(x))[-1] + sizeof(size_t))
#define Get_buf(x)      (x - sizeof(size_t))

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
static uint64_t MAGIC_CONSTANT = 0xfacefacefaceb000ll;

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

/* ENCODING:
 * The highest 2 bits are unused.
 * The next 31 bits encode the key the lower 31 bits the value.
 */
static uint64_t* deptbl;
static uint64_t* deptbl_bindings;

/* The hashtable containing the shared values. */
static helt_t* hashtbl;
static int* hcounter;   // the number of slots taken in the table

/* A counter increasing globally across all forks. */
static uintptr_t* counter;
/* This should only be used before forking */
static uintptr_t early_counter = 1;

/* The top of the heap */
static char** heap;

/* Useful to add assertions */
static pid_t* master_pid;
static pid_t my_pid;

/* Where the heap started (bottom) */
static char* heap_init;

/* The size of the heap after initialization of the server */
/* This should only be used by the master */
static size_t heap_init_size = 0;

static size_t used_heap_size(void) {
  return *heap - heap_init;
}

/* Expose so we can display diagnostics */
CAMLprim value hh_heap_size(void) {
  CAMLparam0();
  CAMLreturn(Val_long(used_heap_size()));
}

CAMLprim value hh_hash_used_slots(void) {
  CAMLparam0();
  uint64_t count = 0;
  uintptr_t i = 0;
  for (i = 0; i < HASHTBL_SIZE; ++i) {
    if (hashtbl[i].addr != NULL) {
      count++;
    }
  }
  CAMLreturn(Val_long(count));
}

CAMLprim value hh_hash_slots(void) {
  CAMLparam0();
  CAMLreturn(Val_long(HASHTBL_SIZE));
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
void memfd_init(char *shm_dir, size_t shared_mem_size, long minimum_avail) {
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

static void raise_less_than_minimum_available(long avail) {
  CAMLlocal1(arg);
  static value *exn = NULL;
  if (!exn) exn = caml_named_value("less_than_minimum_available");
  arg = Val_long(avail);
  caml_raise_with_arg(*exn, arg);
}

#include <sys/statvfs.h>
void assert_avail_exceeds_minimum(char *shm_dir, long minimum_avail) {
  struct statvfs stats;
  long avail;
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
void memfd_init(char *shm_dir, size_t shared_mem_size, long minimum_avail) {
  if (shm_dir == NULL) {
    // This means that we should try to use the anonymous-y system calls
#if defined(MEMFD_CREATE)
    memfd = memfd_create("fb_heap", 0);
#endif
#if defined(__APPLE__)
    if (memfd < 0) {
      char memname[255];
      snprintf(memname, sizeof(memname), "/fb_heap.%d", getpid());
      memfd = shm_open(memname, O_CREAT | O_RDWR, 0666);
      if (memfd < 0) {
          uerror("shm_open", Nothing);
      }
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

static void memfd_reserve(char * mem, size_t sz) {
  if (!VirtualAlloc(mem, sz, MEM_COMMIT, PAGE_READWRITE)) {
    win32_maperr(GetLastError());
    raise_out_of_shared_memory();
  }
}

#elif defined(__APPLE__)

static void memfd_reserve(char * mem, size_t sz) {
  (void)mem;
  (void)sz;
  // TODO ??
  /* fstore_t store = { F_ALLOCATECONTIG, F_PEOFPOSMODE, */
  /*                    (uint64_t)(mem - SHARED_MEM_INIT), sz }; */
  /* int ret = fcntl(memfd, F_PREALLOCATE, &store); */
  /* if (ret == -1) { */
  /*   store.fst_flags = F_ALLOCATEALL; */
  /*   ret = fcntl(memfd, F_PREALLOCATE, &store); */
  /*   if (ret == -1) { */
  /*     uerror("preallocate", Nothing); */
  /*   } */
  /* } */
}

#else

static void memfd_reserve(char *mem, size_t sz) {
  if(posix_fallocate(memfd, (uint64_t)(mem - shared_mem), sz)) {
    raise_out_of_shared_memory();
  }
}

#endif


static void define_globals(char * shared_mem_init) {
  size_t page_size = getpagesize();
  char *mem = shared_mem_init;

  // Beginning of the shared memory
  shared_mem = mem;

  /* BEGINNING OF THE SMALL OBJECTS PAGE
   * We keep all the small objects in this page.
   * They are on different cache lines because we modify them atomically.
   */

  /* The pointer to the top of the heap.
   * We will atomically increment *heap every time we want to allocate.
   */
  heap = (char**)mem;

  // The number of elements in the hashtable
  assert(CACHE_LINE_SIZE >= sizeof(int));
  hcounter = (int*)(mem + CACHE_LINE_SIZE);

  assert (CACHE_LINE_SIZE >= sizeof(uintptr_t));
  counter = (uintptr_t*)(mem + 2*CACHE_LINE_SIZE);

  assert (CACHE_LINE_SIZE >= sizeof(pid_t));
  master_pid = (pid_t*)(mem + 3*CACHE_LINE_SIZE);

  mem += page_size;
  // Just checking that the page is large enough.
  assert(page_size > 3*CACHE_LINE_SIZE + (int)sizeof(int));
  /* END OF THE SMALL OBJECTS PAGE */

  /* Global storage initialization */
  global_storage = (value*)mem;
  mem += global_size_b;

  /* Dependencies */
  deptbl = (uint64_t*)mem;
  mem += DEP_SIZE_B;

  deptbl_bindings = (uint64_t*)mem;
  mem += DEP_SIZE_B;

  /* Hashtable */
  hashtbl = (helt_t*)mem;
  mem += HASHTBL_SIZE_B;

  /* Heap */
  heap_init = mem;

#ifdef _WIN32
  /* Reserve all memory space except the "huge" `global_size_b`. This is
   * required for Windows but we don't do this for Linux since it lets us run
   * more processes in parallel without running out of memory immediately
   * (though we do risk it later on) */
  memfd_reserve((char *)global_storage, sizeof(global_storage[0]));
  memfd_reserve((char *)heap, heap_init - (char *)heap);
#endif

}

static void init_shared_globals() {
  // Initial size is zero for global storage is zero
  global_storage[0] = 0;
  // Initialize the number of element in the table
  *hcounter = 0;
  *counter = early_counter + 1;
  // Initialize top heap pointers
  *heap = heap_init;
}


/* The total size of the shared memory.  Most of it is going to remain
 * virtual. */
static size_t get_shared_mem_size() {
  size_t page_size = getpagesize();
  return (global_size_b + 2 * DEP_SIZE_B + HASHTBL_SIZE_B +
          heap_size + page_size);
}

/*****************************************************************************/
/* Must be called by the master BEFORE forking the workers! */
/*****************************************************************************/

CAMLprim value hh_shared_init(
  value global_size_val,
  value heap_size_val,
  value min_avail_val,
  value shm_dir_val
) {

  CAMLparam4(global_size_val, heap_size_val, min_avail_val, shm_dir_val);
  CAMLlocal1(connector);

  global_size_b = Long_val(global_size_val);
  heap_size = Long_val(heap_size_val);

  shared_mem_size = get_shared_mem_size();

  // None -> NULL
  // Some str -> String_val(str)
  char *shm_dir = NULL;
  if (shm_dir_val != Val_int(0)) {
    shm_dir = String_val(Field(shm_dir_val, 0));
  }

  memfd_init(shm_dir, shared_mem_size, Long_val(min_avail_val));
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

#ifdef MADV_DONTDUMP
  // We are unlikely to get much useful information out of the shared heap in
  // a core file. Moreover, it can be HUGE, and the extensive work done dumping
  // it once for each CPU can mean that the user will reboot their machine
  // before the much more useful stack gets dumped!
  madvise(shared_mem, shared_mem_size, MADV_DONTDUMP);
#endif

  init_shared_globals();
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

  connector = caml_alloc_tuple(3);
  Field(connector, 0) = Val_handle(memfd);
  Field(connector, 1) = global_size_val;
  Field(connector, 2) = heap_size_val;

  CAMLreturn(connector);
}

void hh_shared_reset() {
#ifndef _WIN32
  assert(shared_mem);
  early_counter = 1;
  memset(shared_mem, 0, heap_init - shared_mem);
  init_shared_globals();
#endif
}

/* Must be called by every worker before any operation is performed */
value hh_connect(value connector, value is_master) {
  CAMLparam2(connector, is_master);
  memfd = Handle_val(Field(connector, 0));
  global_size_b = Long_val(Field(connector, 1));
  heap_size = Long_val(Field(connector, 2));
#ifdef _WIN32
  my_pid = 1; // Trick
#else
  my_pid = getpid();
#endif
  char *shared_mem_init = memfd_map(get_shared_mem_size());
  define_globals(shared_mem_init);

  if (Bool_val(is_master)) {
    fprintf(stderr, "Reconnecting as master %d", my_pid);
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
 * Each key/value binding is unique, but a key can have multiple value
 * bound to it.
 * Concretely, if you try to add a key/value pair that is already in the table
 * the data structure is left unmodified.
 * If you try to add a key bound to a new value, the binding is added, the
 * old binding is not removed.
 */
/*****************************************************************************/

static int htable_add(uint64_t* table, unsigned long hash, uint64_t value) {
  unsigned long slot = hash & (DEP_SIZE - 1);

  while(1) {
    /* It considerably speeds things up to do a normal load before trying using
     * an atomic operation.
     */
    uint64_t slot_val = table[slot];

    // The binding exists, done!
    if(slot_val == value)
      return 0;

    // The slot is free, let's try to take it.
    if(slot_val == 0) {
      // See comments in hh_add about its similar construction here.
      if(__sync_bool_compare_and_swap(&table[slot], 0, value)) {
        return 1;
      }

      if(table[slot] == value) {
        return 0;
      }
    }

    slot = (slot + 1) & (DEP_SIZE - 1);
  }
}

void hh_add_dep(value ocaml_dep) {
  CAMLparam1(ocaml_dep);
  uint64_t dep = Long_val(ocaml_dep);

  if (!htable_add(deptbl_bindings, hash_uint64(dep), dep)) {
    CAMLreturn0;
  }

  htable_add(deptbl, dep >> 31, dep);

  CAMLreturn0;
}

CAMLprim value hh_dep_used_slots(void) {
  CAMLparam0();
  uint64_t count = 0;
  uintptr_t slot = 0;
  for (slot = 0; slot < DEP_SIZE; ++slot) {
    if (deptbl[slot]) {
      count++;
    }
  }
  CAMLreturn(Val_long(count));
}

CAMLprim value hh_dep_slots(void) {
  CAMLparam0();
  CAMLreturn(Val_long(DEP_SIZE));
}

/* Given a key, returns the list of values bound to it. */
CAMLprim value hh_get_dep(value dep) {
  CAMLparam1(dep);
  CAMLlocal2(result, cell);

  unsigned long hash = Long_val(dep);
  unsigned long slot = hash & (DEP_SIZE - 1);

  result = Val_int(0); // The empty list

  while(1) {
    if(deptbl[slot] == 0) {
      break;
    }
    if(deptbl[slot] >> 31 == hash) {
      cell = caml_alloc_tuple(2);
      Field(cell, 0) = Val_long(deptbl[slot] & ((1ul << 31) - 1));
      Field(cell, 1) = result;
      result = cell;
    }
    slot = (slot + 1) & (DEP_SIZE - 1);
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
  if(used_heap_size() < (size_t)(space_overhead * heap_init_size)) {
    // We have not grown past twice the size of the initial size
    return;
  }

  tmp_heap = temp_memory_map();
  dest = tmp_heap;
  assert_master(); // Comes from the master

  // Walking the table
  size_t i;
  for(i = 0; i < HASHTBL_SIZE; i++) {
    if(hashtbl[i].addr != NULL) { // Found a non empty slot
      size_t bl_size      = Get_buf_size(hashtbl[i].addr);
      size_t aligned_size = ALIGNED(bl_size);
      char* addr          = Get_buf(hashtbl[i].addr);

      memfd_reserve(dest, bl_size);
      memcpy(dest, addr, bl_size);
      // This is where the data ends up after the copy
      hashtbl[i].addr = heap_init + mem_size + sizeof(size_t);
      dest     += aligned_size;
      mem_size += aligned_size;
    }
  }

  // Copying the result back into shared memory
  memcpy(heap_init, tmp_heap, mem_size);
  *heap = heap_init + mem_size;

  temp_memory_unmap(tmp_heap);

}

/*****************************************************************************/
/* Allocates in the shared heap.
 * The chunks are cache aligned.
 * The word before the chunk address contains the size of the chunk in bytes.
 * The function returns a pointer to the data (the size can be accessed by
 * looking at the address: chunk - sizeof(size_t)).
 */
/*****************************************************************************/

static char* hh_alloc(size_t size) {
  size_t slot_size  = ALIGNED(size + sizeof(size_t));
  char* chunk       = __sync_fetch_and_add(heap, (char*)slot_size);
  memfd_reserve(chunk, slot_size);
  *((size_t*)chunk) = size;
  return (chunk + sizeof(size_t));
}

/*****************************************************************************/
/* Allocates an ocaml value in the shared heap.
 * The values can only be ocaml strings. It returns the address of the
 * allocated chunk.
 */
/*****************************************************************************/
static char* hh_store_ocaml(value data) {
  size_t data_size = caml_string_length(data);
  char* addr = hh_alloc(data_size);
  memcpy(addr, String_val(data), data_size);
  return addr;
}

/*****************************************************************************/
/* Given an OCaml string, returns the 8 first bytes in an unsigned long.
 * The key is generated using MD5, but we only use the first 8 bytes because
 * it allows us to use atomic operations.
 */
/*****************************************************************************/
static unsigned long get_hash(value key) {
  return *((unsigned long*)String_val(key));
}

/*****************************************************************************/
/* Writes the data in one of the slots of the hashtable. There might be
 * concurrent writers, when that happens, the first writer wins.
 */
/*****************************************************************************/
static void write_at(unsigned int slot, value data) {
  // Try to write in a value to indicate that the data is being written.
  if(hashtbl[slot].addr == NULL &&
     __sync_bool_compare_and_swap(&(hashtbl[slot].addr), NULL, (char*)1)) {
    hashtbl[slot].addr = hh_store_ocaml(data);
  }
}

/*****************************************************************************/
/* Adds a key value to the hashtable. This code is perf sensitive, please
 * check the perf before modifying.
 */
/*****************************************************************************/
void hh_add(value key, value data) {
  unsigned long hash = get_hash(key);
  unsigned int slot = hash & (HASHTBL_SIZE - 1);

  while(1) {
    unsigned long slot_hash = hashtbl[slot].hash;

    if(slot_hash == hash) {
      write_at(slot, data);
      return;
    }

    if(slot_hash == 0) {
      // We think we might have a free slot, try to atomically grab it.
      if(__sync_bool_compare_and_swap(&(hashtbl[slot].hash), 0, hash)) {
        unsigned long size = __sync_fetch_and_add(hcounter, 1);
        assert(size < HASHTBL_SIZE);
        write_at(slot, data);
        return;
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
        write_at(slot, data);
        return;
      }
    }

    slot = (slot + 1) & (HASHTBL_SIZE - 1);
  }
}

/*****************************************************************************/
/* Finds the slot corresponding to the key in a hash table. The returned slot
 * is either free or points to the key.
 */
/*****************************************************************************/
static unsigned int find_slot(value key) {
  unsigned long hash = get_hash(key);
  unsigned int slot = hash & (HASHTBL_SIZE - 1);

  while(1) {
    if(hashtbl[slot].hash == hash) {
      return slot;
    }
    if(hashtbl[slot].hash == 0) {
      return slot;
    }
    slot = (slot + 1) & (HASHTBL_SIZE - 1);
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
#ifdef __aarch64__
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
/* Returns the value associated to a given key. The key MUST be present. */
/*****************************************************************************/
CAMLprim value hh_get(value key) {
  CAMLparam1(key);
  CAMLlocal1(result);

  unsigned int slot = find_slot(key);
  assert(hashtbl[slot].hash == get_hash(key));
  size_t size = *(size_t*)(hashtbl[slot].addr - sizeof(size_t));
  result = caml_alloc_string(size);
  memcpy(String_val(result), hashtbl[slot].addr, size);

  CAMLreturn(result);
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

#ifdef NO_LZ4
void hh_save_dep_table(value out_filename) {
  CAMLparam1(out_filename);
  caml_failwith("Program not linked with lz4, so saving is not supported!");
  CAMLreturn0;
}

void hh_load_dep_table(value in_filename) {
  CAMLparam1(in_filename);
  caml_failwith("Program not linked with lz4, so loading is not supported!");
  CAMLreturn0;
}
#else
static void fwrite_no_fail(
  const void* ptr, size_t size, size_t nmemb, FILE* fp
) {
  size_t nmemb_written = fwrite(ptr, size, nmemb, fp);
  assert(nmemb_written == nmemb);
}

/* We want to use read() instead of fread() for the large shared memory block
 * because buffering slows things down. This means we cannot use fread() for
 * the other (smaller) values in our file either, because the buffering can
 * move the file position indicator ahead of the values read. */
static void read_all(int fd, void* start, size_t size) {
  size_t total_read = 0;
  do {
    void* ptr = (void*)((uintptr_t)start + total_read);
    ssize_t bytes_read = read(fd, (void*)ptr, size);
    assert(bytes_read != -1 && bytes_read != 0);
    total_read += bytes_read;
  } while (total_read < size);
}

static void fwrite_header(FILE* fp) {
  fwrite_no_fail(&MAGIC_CONSTANT, sizeof MAGIC_CONSTANT, 1, fp);

  size_t revlen = strlen(BuildInfo_kRevision);
  fwrite_no_fail(&revlen, sizeof revlen, 1, fp);
  fwrite_no_fail(BuildInfo_kRevision, sizeof(char), revlen, fp);
}

static void fread_header(FILE* fp) {
  uint64_t magic = 0;
  read_all(fileno(fp), (void*)&magic, sizeof magic);
  assert(magic == MAGIC_CONSTANT);

  size_t revlen = 0;
  read_all(fileno(fp), (void*)&revlen, sizeof revlen);
  char revision[revlen];
  if (revlen > 0) {
    read_all(fileno(fp), (void*)revision, revlen * sizeof(char));
    assert(strncmp(revision, BuildInfo_kRevision, revlen) == 0);
  }
}

void hh_save_dep_table(value out_filename) {
  CAMLparam1(out_filename);
  FILE* fp = fopen(String_val(out_filename), "wb");

  fwrite_header(fp);

  int compressed_size = 0;

  assert(LZ4_MAX_INPUT_SIZE >= DEP_SIZE_B);
  char* compressed = malloc(DEP_SIZE_B);
  assert(compressed != NULL);

  compressed_size = LZ4_compressHC((char*)deptbl, compressed, DEP_SIZE_B);
  assert(compressed_size > 0);

  fwrite_no_fail(&compressed_size, sizeof compressed_size, 1, fp);
  fwrite_no_fail((void*)compressed, 1, compressed_size, fp);
  free(compressed);

  fclose(fp);
  CAMLreturn0;
}

void hh_load_dep_table(value in_filename) {
  CAMLparam1(in_filename);
  struct timeval tv;
  gettimeofday(&tv, NULL);

  FILE* fp = fopen(String_val(in_filename), "rb");

  if (fp == NULL) {
    unix_error(errno, "fopen", in_filename);
  }

  fread_header(fp);

  int compressed_size = 0;
  read_all(fileno(fp), (void*)&compressed_size, sizeof compressed_size);

  char* compressed = malloc(compressed_size * sizeof(char));
  assert(compressed != NULL);
  read_all(fileno(fp), compressed, compressed_size * sizeof(char));

  int actual_compressed_size = LZ4_decompress_fast(
      compressed,
      (char*)deptbl,
      DEP_SIZE_B);
  assert(compressed_size == actual_compressed_size);
  tv = log_duration("Loading file", tv);

  uintptr_t slot = 0;
  for (slot = 0; slot < DEP_SIZE; ++slot) {
    uint64_t dep = deptbl[slot];
    if (dep != 0) {
      htable_add(deptbl_bindings, hash_uint64(dep), dep);
    }
  }

  fclose(fp);

  log_duration("Bindings", tv);
  CAMLreturn0;
}

#endif
