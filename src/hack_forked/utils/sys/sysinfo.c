/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define CAML_NAME_SPACE
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <assert.h>
#ifdef __linux__
#include <sys/sysinfo.h>
#endif

#ifdef _WIN32
#include <windows.h>
#endif

value hh_sysinfo_totalram(void) {
  CAMLparam0();
#ifdef __linux__
  struct sysinfo info;
  int success = sysinfo(&info);
  assert(success == 0 && "sysinfo() failed");
  CAMLreturn(Val_long(info.totalram));
#else
  /* Not implemented */
  CAMLreturn(Val_long(0));
#endif
}

value hh_sysinfo_uptime(void) {
  CAMLparam0();
#ifdef __linux__
  struct sysinfo info;
  int success = sysinfo(&info);
  assert(success == 0 && "sysinfo() failed");
  CAMLreturn(Val_long(info.uptime));
#else
  /* Not implemented */
  CAMLreturn(Val_long(0));
#endif
}

CAMLprim value hh_sysinfo_is_apple_os(void) {
  CAMLparam0();
#ifdef __APPLE__
  return Val_bool(1);
#else
  return Val_bool(0);
#endif
}

/**
 * There are a bunch of functions that you expect to return a pid,
 * like Unix.getpid() and Unix.create_process(). However, on
 * Windows, instead of returning the process ID, they return a
 * process handle.
 *
 * Process handles seem act like pointers to a process. You can have
 * more than one handle that points to a single process (unlike
 * pids, where there is a single pid for a process).
 *
 * This isn't a problem normally, since functons like Unix.waitpid()
 * will take the process handle on Windows. But if you want to print
 * or log the pid, then you need to dereference the handle and get
 * the pid. And that's what this function does.
 */
value pid_of_handle(value handle) {
  CAMLparam1(handle);
#ifdef _WIN32
  CAMLreturn(Val_int(GetProcessId((HANDLE)Long_val(handle))));
#else
  CAMLreturn(handle);
#endif
}

value handle_of_pid_for_termination(value pid) {
  CAMLparam1(pid);
#ifdef _WIN32
  CAMLreturn(Val_int(OpenProcess(PROCESS_TERMINATE, FALSE, Int_val(pid))));
#else
  CAMLreturn(pid);
#endif
}
