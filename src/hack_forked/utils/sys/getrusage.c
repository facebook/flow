/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <caml/alloc.h>
#include <caml/memory.h>
#ifdef _WIN32
#include <caml/fail.h>
#include <windows.h>

value hh_getrusage(void) {
  caml_failwith("getrusage is unimplemented for Windows");
}
#else
#include <sys/resource.h>

value hh_getrusage(void) {
  CAMLparam0();
  CAMLlocal1(result);

  struct rusage ru;
  getrusage(RUSAGE_SELF, &ru);

  result = caml_alloc_tuple(14);

  /* maximum resident set size */
  Store_field(result, 0, Val_long(ru.ru_maxrss));
  /* integral shared memory size */
  Store_field(result, 1, Val_long(ru.ru_ixrss));
  /* integral unshared data size */
  Store_field(result, 2, Val_long(ru.ru_idrss));
  /* integral unshared stack size */
  Store_field(result, 3, Val_long(ru.ru_isrss));
  /* page reclaims (soft page faults) */
  Store_field(result, 4, Val_long(ru.ru_minflt));
  /* page faults (hard page faults) */
  Store_field(result, 5, Val_long(ru.ru_majflt));
  /* swaps */
  Store_field(result, 6, Val_long(ru.ru_nswap));
  /* block input operations */
  Store_field(result, 7, Val_long(ru.ru_inblock));
  /* block output operations */
  Store_field(result, 8, Val_long(ru.ru_oublock));
  /* IPC messages sent */
  Store_field(result, 9, Val_long(ru.ru_msgsnd));
  /* IPC messages received */
  Store_field(result, 10, Val_long(ru.ru_msgrcv));
  /* signals received */
  Store_field(result, 11, Val_long(ru.ru_nsignals));
  /* voluntary context switches */
  Store_field(result, 12, Val_long(ru.ru_nvcsw));
  /* involuntary context switches */
  Store_field(result, 13, Val_long(ru.ru_nivcsw));

  CAMLreturn(result);
}

#endif
