(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* we model abnormal control flows using exceptions during traversal *)

(* control directives encountered during traversal *)
type t =
  | Return
  | Throw
  | Break of string option
  | Continue of string option

exception Exn of t

open Utils
open Utils_js

(* called from traversal. value indicates control flow directive encountered *)
let throw_control_flow_exception abnormal =
  raise (Exn abnormal)

(* if argument is Some abnormal, throw it *)
let check_control_flow_exception = function
  | None -> ()
  | Some abnormal -> throw_control_flow_exception abnormal

(* helper *)
let check_env_depth depth =
  let new_depth = Env_js.env_depth () in
  if new_depth = depth then ()
  else assert_false (spf
    "env depth %d != %d after no control flow catch"
    new_depth depth)

(* run a function, return first control-flow exception or none *)
let catch_control_flow_exception f =
  let depth = Env_js.env_depth () in
  try (
    f ();
    check_env_depth depth;
    None
  ) with
  | Exn abnormal ->
    Env_js.trunc_env depth;
    Some abnormal
  | exn ->
    raise exn

(* like check_control_flow_exception, except break statements
   specifying the given label (or None) are ignored *)
let ignore_break_to_label label f =
  match catch_control_flow_exception f with
    | Some (Break break_label) when break_label = label -> None
    | result -> result

(* like ignore_break_to_label, except continue statements
   on the same label (or None) are also ignored *)
let ignore_break_or_continue_to_label label f =
  match ignore_break_to_label label f with
    | Some (Continue cont_label) when cont_label = label -> None
    | result -> result

(* thread-local state used to detect abnormal control flows *)
module AbSet = Set.Make(struct
  type abnormal = t
  type t = abnormal
  let compare = Pervasives.compare
end)

let abnormals = ref AbSet.empty

(* register a control flow directive *)
let set abnormal =
  abnormals := AbSet.add abnormal !abnormals;
  throw_control_flow_exception abnormal

(* swap in a new presence value for a given control directive,
   and return the current value *)
let swap abnormal newv =
  let oldv = AbSet.mem abnormal !abnormals in
  if oldv = newv then ()
  else abnormals := AbSet.(if newv then add else remove) abnormal !abnormals;
  oldv

let string = function
  | Return -> "return"
  | Throw -> "throw"
  | Break (Some lbl) -> spf "break `%s`" lbl
  | Break None -> "break"
  | Continue (Some lbl) -> spf "continue `%s`" lbl
  | Continue None -> "continue"
