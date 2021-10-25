/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <stdio.h>
#include <sys/time.h>

static double minor_start_time = 0.0;
static double major_start_time = 0.0;

static double minor_time = 0.0;
static double major_time = 0.0;

void minor_begin() {
  struct timeval time;
  gettimeofday(&time, NULL);
  minor_start_time = time.tv_sec + ((double)time.tv_usec / 1000000);
}

void major_begin() {
  struct timeval time;
  gettimeofday(&time, NULL);
  major_start_time = time.tv_sec + ((double)time.tv_usec / 1000000);
}

void minor_end() {
  struct timeval time;
  gettimeofday(&time, NULL);
  minor_time +=
      time.tv_sec + ((double)time.tv_usec / 1000000) - minor_start_time;
}

/**
 * This seems to be called every time that caml_major_collection_slice is
 * called, which is quite often. I don't 100% understand the major collection,
 * but it seems to have phases and is executed over multiple calls. If I run
 * `bin/flow check .`, this will be called around 579 times totaling 0.0028s
 * (compared to 291 minor collections totaling 0.057s), though the gc stats will
 * claim only a single major collection happened. If I use caml_gc_phase to only
 * fire the callback when it equals 2 (Phase_sweep, the last phase), it is
 * called 3 times for 0.0008s.
 */
void major_end() {
  struct timeval time;
  gettimeofday(&time, NULL);
  major_time +=
      time.tv_sec + ((double)time.tv_usec / 1000000) - major_start_time;
}

void hh_start_gc_profiling() {
  major_time = 0.0;
  minor_time = 0.0;
  caml_minor_gc_begin_hook = minor_begin;
  caml_minor_gc_end_hook = minor_end;
  caml_major_slice_begin_hook = major_begin;
  caml_major_slice_end_hook = major_end;
}

/**
 * I (glevi) originally used to allow more complicated callbacks for the end of
 * GC. However, I was observing weird crashes (including marshal'ing things
 * outside of the heap), so I switched to super simple callbacks which
 * definitely don't allocate anything during GC
 */
value hh_get_gc_time() {
  caml_minor_gc_begin_hook = NULL;
  caml_minor_gc_end_hook = NULL;
  caml_major_slice_begin_hook = NULL;
  caml_major_slice_end_hook = NULL;
  CAMLparam0();
  CAMLlocal1(ret);

  ret = caml_alloc_tuple(2);
  Store_field(ret, 0, caml_copy_double(major_time));
  Store_field(ret, 1, caml_copy_double(minor_time));

  CAMLreturn(ret);
}
