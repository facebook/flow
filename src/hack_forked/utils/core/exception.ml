(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  exn: exn;
  backtrace: Printexc.raw_backtrace;
}

(* In ocaml, backtraces (the path that the exception bubbled up after being thrown) are stored as
 * global state and NOT with the exception itself. This means the only safe place to ever read the
 * backtrace is immediately after the exception is caught in the `with` block of a `try...with`.
 *
 * Proper use of this module is something like
 *
 *  try
 *    ...
 *  with exn ->
 *    let e = Exception.wrap exn in (* DO THIS FIRST!!! *)
 *    my_fun e; (* If this code throws internally it will overwrite the global backtrace *)
 *    Exception.reraise e
 *)

let wrap exn =
  let backtrace = Printexc.get_raw_backtrace () in
  { exn; backtrace }

(* The inverse of `wrap`, returns the wrapped `exn`. You might use this to pattern
   match on the raw exception or print it, but should not reraise it since it
   will not include the correct backtrace; use `reraise` or `to_exn` instead. *)
let unwrap { exn; backtrace = _ } = exn

let reraise { exn; backtrace } = Printexc.raise_with_backtrace exn backtrace

(* Converts back to an `exn` with the right backtrace. Generally, avoid this in favor of
   the helpers in this module, like `to_string` and `get_backtrace_string`. *)
let to_exn t = (try reraise t with exn -> exn)

(* Like `wrap`, but for the unusual case where you want to create an `Exception`
   for an un-raised `exn`, capturing its stack trace. If you've caught an exception,
   you should use `wrap` instead, since it already has a stack trace. *)
let wrap_unraised ?(frames = 100) exn =
  let frames =
    if Printexc.backtrace_status () then
      frames
    else
      0
  in
  let backtrace = Printexc.get_callstack frames in
  { exn; backtrace }

let get_ctor_string { exn; backtrace = _ } = Printexc.to_string exn

let get_backtrace_string { exn = _; backtrace } = Printexc.raw_backtrace_to_string backtrace

let to_string t =
  let ctor = get_ctor_string t in
  let bt = get_backtrace_string t in
  if bt = "" then
    ctor
  else
    ctor ^ "\n" ^ bt

let get_current_callstack_string n = Printexc.get_callstack n |> Printexc.raw_backtrace_to_string

(** `internal_get_backtrace_slots_of_callstack x y` returns the top `y`
  backtrace slots after skipping the top `x` slots. Skipping slots is
  useful for hiding our own exception printing functions. *)
let internal_get_backtrace_slots_of_callstack ~skip n =
  (* skip `Printexc.get_callstack` *)
  let skip = skip + 1 in
  let n =
    if max_int - skip < n then
      max_int
    else
      n + skip
  in
  let callstack = Printexc.get_callstack n in
  match Printexc.backtrace_slots callstack with
  | Some callstack_slots ->
    if Array.length callstack_slots > skip then
      Some (Array.sub callstack_slots skip (Array.length callstack_slots - skip))
    else
      None
  | None -> None

(** Appends the current callstack (up to `n` slots, after skipping `skip` frames) to
  the given backtrace. *)
let internal_get_full_backtrace_slots ~skip n backtrace =
  (* skip `internal_get_backtrace_slots_of_callstack` *)
  let callstack_slots = internal_get_backtrace_slots_of_callstack ~skip:(skip + 1) n in
  let backtrace_slots = Printexc.backtrace_slots backtrace in
  match (backtrace_slots, callstack_slots) with
  | (Some backtrace_slots, Some callstack_slots) ->
    Some (Array.append backtrace_slots callstack_slots)
  | (Some backtrace_slots, None) -> Some backtrace_slots
  | (None, Some callstack_slots) -> Some callstack_slots
  | (None, None) -> None

(** Reimplements the private `Printexc.print_exception_backtrace` *)
let print_exception_backtrace outchan backtrace =
  match backtrace with
  | None -> Printf.fprintf outchan "(Program not linked with -g, cannot print stack backtrace)\n"
  | Some a ->
    for i = 0 to Array.length a - 1 do
      match Printexc.Slot.format i a.(i) with
      | None -> ()
      | Some str -> Printf.fprintf outchan "%s\n" str
    done

let print_full_backtrace outchan n { exn = _; backtrace } =
  (* skip `print_full_backtrace` and `internal_get_full_backtrace_slots` *)
  let slots = internal_get_full_backtrace_slots ~skip:2 n backtrace in
  print_exception_backtrace outchan slots

(** Reimplements the private `Printexc.backtrace_to_string` *)
let backtrace_slots_to_string backtrace =
  match backtrace with
  | None -> "(Program not linked with -g, cannot print stack backtrace)\n"
  | Some a ->
    let b = Buffer.create 1024 in
    for i = 0 to Array.length a - 1 do
      match Printexc.Slot.format i a.(i) with
      | None -> ()
      | Some str -> Printf.bprintf b "%s\n" str
    done;
    Buffer.contents b

let get_full_backtrace_string n { exn = _; backtrace } =
  (* skip `get_full_backtrace_string` and `internal_get_full_backtrace_slots` *)
  let slots = internal_get_full_backtrace_slots ~skip:2 n backtrace in
  backtrace_slots_to_string slots

let record_backtrace = Printexc.record_backtrace
