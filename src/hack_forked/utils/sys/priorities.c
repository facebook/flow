/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#undef CAML_NAME_SPACE

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/syscall.h>
#include <sys/types.h>
#include <unistd.h>
#endif

/*****************************************************************************/
/* Sets CPU and IO priorities. */
/*****************************************************************************/

// glibc refused to add ioprio_set, sigh.
// https://sourceware.org/bugzilla/show_bug.cgi?id=4464
#define IOPRIO_CLASS_SHIFT 13
#define IOPRIO_PRIO_VALUE(cl, dat) (((cl) << IOPRIO_CLASS_SHIFT) | (dat))
#define IOPRIO_WHO_PROCESS 1
#define IOPRIO_CLASS_BE 2

value hh_set_priorities(value cpu_prio_val, value io_prio_val) {
  CAMLparam2(cpu_prio_val, io_prio_val);
  int cpu_prio = Long_val(cpu_prio_val);
  int io_prio = Long_val(io_prio_val);

// No need to check the return value, if we failed then whatever.
#ifdef __linux__
  syscall(
      SYS_ioprio_set,
      IOPRIO_WHO_PROCESS,
      getpid(),
      IOPRIO_PRIO_VALUE(IOPRIO_CLASS_BE, io_prio));
#endif

#ifdef _WIN32
  SetPriorityClass(GetCurrentProcess(), BELOW_NORMAL_PRIORITY_CLASS);
// One might also try: PROCESS_MODE_BACKGROUND_BEGIN
#else
  int dummy = nice(cpu_prio);
  (void)dummy; // https://gcc.gnu.org/bugzilla/show_bug.cgi?id=25509
#endif
  CAMLreturn(Val_unit);
}
