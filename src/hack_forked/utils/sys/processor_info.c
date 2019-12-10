/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <stdio.h>

#define CPU_INFO_USER 0
#define CPU_INFO_USER_NICE 1
#define CPU_INFO_SYSTEM 2
#define CPU_INFO_IDLE 3

#define PROCESSOR_INFO_PROC_TOTALS 0
#define PROCESSOR_INFO_PROC_PER_CPU 1

#ifdef _WIN32
// TODO if we every start profiling Windows
// Rather than crash, we'll just return empty info
value hh_processor_info(void) {
  CAMLparam0();
  CAMLlocal3(result, array, info);

  // Empty array of cpu info
  array = caml_alloc_tuple(0);

  // Empty total cpu info
  info = caml_alloc(4, Double_array_tag);
  Store_double_field(info, CPU_INFO_USER, 0);
  Store_double_field(info, CPU_INFO_USER_NICE, 0);
  Store_double_field(info, CPU_INFO_SYSTEM, 0);
  Store_double_field(info, CPU_INFO_IDLE, 0);

  result = caml_alloc_tuple(2);
  Store_field(result, PROCESSOR_INFO_PROC_TOTALS, info);
  Store_field(result, PROCESSOR_INFO_PROC_PER_CPU, array);

  CAMLreturn(result);
}

#elif defined(__APPLE__)

#include <mach/mach_error.h>
#include <mach/mach_host.h>
#include <mach/processor_info.h>
#include <unistd.h>

value hh_processor_info(void) {
  CAMLparam0();
  CAMLlocal3(result, array, info);

  processor_cpu_load_info_t cpu_load_info;
  mach_msg_type_number_t msg_count;
  char buf[256];

  double total_user = 0, total_nice = 0, total_system = 0, total_idle = 0;
  int i;
  long ticks_per_second = sysconf(_SC_CLK_TCK);
  natural_t num_cpus;

  kern_return_t ret = host_processor_info(
    mach_host_self(),
    PROCESSOR_CPU_LOAD_INFO,
    &num_cpus,
    (processor_info_array_t *)&cpu_load_info,
    &msg_count);

  if (ret) {
    snprintf(buf, 256, "host_processor_info error: %s", mach_error_string(ret));
    caml_failwith(buf);
  }

  array = caml_alloc_tuple(num_cpus);

  for (i = 0; i < num_cpus; i++) {
    info = caml_alloc(4, Double_array_tag);
    Store_double_field(
      info,
      CPU_INFO_USER,
      cpu_load_info[i].cpu_ticks[CPU_STATE_USER] / ticks_per_second);
    Store_double_field(
      info,
      CPU_INFO_USER_NICE,
      cpu_load_info[i].cpu_ticks[CPU_STATE_NICE] / ticks_per_second);
    Store_double_field(
      info,
      CPU_INFO_SYSTEM,
      cpu_load_info[i].cpu_ticks[CPU_STATE_SYSTEM] / ticks_per_second);
    Store_double_field(
      info,
      CPU_INFO_IDLE,
      cpu_load_info[i].cpu_ticks[CPU_STATE_IDLE] / ticks_per_second);
    Store_field(array, i, info);

    total_user += cpu_load_info[i].cpu_ticks[CPU_STATE_USER];
    total_nice += cpu_load_info[i].cpu_ticks[CPU_STATE_NICE];
    total_system += cpu_load_info[i].cpu_ticks[CPU_STATE_SYSTEM];
    total_idle += cpu_load_info[i].cpu_ticks[CPU_STATE_IDLE];
  }

  info = caml_alloc(4, Double_array_tag);
  Store_double_field(info, CPU_INFO_USER, total_user / ticks_per_second);
  Store_double_field(info, CPU_INFO_USER_NICE, total_nice / ticks_per_second);
  Store_double_field(info, CPU_INFO_SYSTEM, total_system / ticks_per_second);
  Store_double_field(info, CPU_INFO_IDLE, total_idle / ticks_per_second);

  result = caml_alloc_tuple(2);
  Store_field(result, PROCESSOR_INFO_PROC_TOTALS, info);
  Store_field(result, PROCESSOR_INFO_PROC_PER_CPU, array);

  CAMLreturn(result);
}

#else // Not Windows and not OS X means Linux
#include <unistd.h> // nolint (linter thinks this is a duplicate include)

value scan_line(FILE *fp, long ticks_per_second) {
  CAMLparam0();
  CAMLlocal1(cpu_info);
  double user = 0.0;
  double nice = 0.0;
  double sys = 0.0;
  double idle = 0.0;

  fscanf(fp, "%*s %lf %lf %lf %lf %*[^\n]", &user, &nice, &sys, &idle);

  // Usually, ocaml will box the float type. However, arrays and records that
  // only contain floats get their own special representation which is unboxed
  cpu_info = caml_alloc(4, Double_array_tag);

  // The numbers in /proc/stat represent clock ticks, so convert to seconds.
  Store_double_field(cpu_info, CPU_INFO_USER, user / ticks_per_second);
  Store_double_field(cpu_info, CPU_INFO_USER_NICE, nice / ticks_per_second);
  Store_double_field(cpu_info, CPU_INFO_SYSTEM, sys / ticks_per_second);
  Store_double_field(cpu_info, CPU_INFO_IDLE, idle / ticks_per_second);

  CAMLreturn(cpu_info);
}

value hh_processor_info(void) {
  CAMLparam0();
  CAMLlocal2(result, array);

  FILE *fp = fopen("/proc/stat", "r");
  long num_cpus = sysconf(_SC_NPROCESSORS_ONLN);
  int i;
  long ticks_per_second = sysconf(_SC_CLK_TCK);

  if (fp == NULL) {
    caml_failwith("Failed to open /proc/stat");
  }

  result = caml_alloc_tuple(2);
  if (fp != NULL) {
    Store_field(
      result,
      PROCESSOR_INFO_PROC_TOTALS,
      scan_line(fp, ticks_per_second));

    array = caml_alloc_tuple(num_cpus);
    Store_field(result, PROCESSOR_INFO_PROC_PER_CPU, array);

    // For each CPU, scan in the line
    for (i = 0; i < num_cpus; i++) {
      Store_field(array, i, scan_line(fp, ticks_per_second));
    }
    fclose(fp);
  }

  CAMLreturn(result);
}

#endif
