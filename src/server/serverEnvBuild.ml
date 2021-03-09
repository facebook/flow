(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Saves the default GC settings, which are restored by the workers. *)
let default_gc_control = Caml.Gc.get ()

(** Workers can have more relaxed GC configs as they are short-lived processes,
    and this prevents the workers from inheriting GC settings the master needs. *)
let worker_gc_control options =
  let open Caml.Gc in
  let {
    Options.gc_minor_heap_size;
    gc_major_heap_increment;
    gc_space_overhead;
    gc_window_size;
    gc_custom_major_ratio;
    gc_custom_minor_ratio;
    gc_custom_minor_max_size;
  } =
    Options.gc_worker options
  in
  let gc_control = default_gc_control in
  let gc_control =
    match gc_minor_heap_size with
    | Some minor_heap_size -> { gc_control with minor_heap_size }
    | None -> gc_control
  in
  let gc_control =
    match gc_major_heap_increment with
    | Some major_heap_increment -> { gc_control with major_heap_increment }
    | None -> gc_control
  in
  let gc_control =
    match gc_space_overhead with
    | Some space_overhead -> { gc_control with space_overhead }
    | None -> gc_control
  in
  let gc_control =
    match gc_window_size with
    | Some window_size -> { gc_control with window_size }
    | None -> gc_control
  in
  let gc_control =
    match gc_custom_major_ratio with
    | Some custom_major_ratio -> { gc_control with custom_major_ratio }
    | None -> gc_control
  in
  let gc_control =
    match gc_custom_minor_ratio with
    | Some custom_minor_ratio -> { gc_control with custom_minor_ratio }
    | None -> gc_control
  in
  let gc_control =
    match gc_custom_minor_max_size with
    | Some custom_minor_max_size -> { gc_control with custom_minor_max_size }
    | None -> gc_control
  in
  if Options.should_profile options then
    Hh_logger.info
      "Worker GC params: minor_heap_size = %d; major_heap_increment = %d; space_overhead = %d; window_size = %d; custom_major_ratio = %d%%; custom_minor_ratio = %d%%; custom_minor_max_size = %d"
      gc_control.minor_heap_size
      gc_control.major_heap_increment
      gc_control.space_overhead
      gc_control.window_size
      gc_control.custom_major_ratio
      gc_control.custom_minor_ratio
      gc_control.custom_minor_max_size;
  gc_control

let make_genv ~options ~init_id handle =
  let workers =
    let num_workers = Options.max_workers options in
    if num_workers > 0 then
      let gc_control = worker_gc_control options in
      Some (ServerWorker.make ~n:num_workers ~gc_control ~init_id handle)
    else
      None
  in
  { ServerEnv.options; workers }
