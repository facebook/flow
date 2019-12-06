(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

open Hh_core

(** Callstack is simply a typed way to indicate that a string is a callstack *)
type callstack = Callstack of string

let () = Random.self_init ()

module Map = struct end

let spf = Printf.sprintf

let print_endlinef fmt = Printf.ksprintf print_endline fmt

let prerr_endlinef fmt = Printf.ksprintf prerr_endline fmt

let opt f env = function
  | None -> (env, None)
  | Some x ->
    let (env, x) = f env x in
    (env, Some x)

let opt_fold f env = function
  | None -> env
  | Some x -> f env x

let singleton_if cond x =
  if cond then
    [x]
  else
    []

let smap_inter m1 m2 =
  SMap.fold
    (fun x y acc ->
      if SMap.mem x m2 then
        SMap.add x y acc
      else
        acc)
    m1
    SMap.empty

let imap_inter m1 m2 =
  IMap.fold
    (fun x y acc ->
      if IMap.mem x m2 then
        IMap.add x y acc
      else
        acc)
    m1
    IMap.empty

let smap_inter_list = function
  | [] -> SMap.empty
  | x :: rl -> List.fold_left rl ~f:smap_inter ~init:x

let imap_inter_list = function
  | [] -> IMap.empty
  | x :: rl -> List.fold_left rl ~f:imap_inter ~init:x

let rec wfold_left2 f env l1 l2 =
  match (l1, l2) with
  | ([], _)
  | (_, []) ->
    env
  | (x1 :: rl1, x2 :: rl2) ->
    let env = f env x1 x2 in
    wfold_left2 f env rl1 rl2

let sl l = List.fold_right l ~f:( ^ ) ~init:""

let maybe f env = function
  | None -> ()
  | Some x -> f env x

(* Since OCaml usually runs w/o backtraces enabled, the note makes errors
 * easier to debug. *)
let unsafe_opt_note note = function
  | None -> raise (Invalid_argument note)
  | Some x -> x

let unsafe_opt x = unsafe_opt_note "unsafe_opt got None" x

let inter_list = function
  | [] -> SSet.empty
  | x :: rl -> List.fold_left rl ~f:SSet.inter ~init:x

let rec list_last f1 f2 = function
  | [] -> ()
  | [x] -> f2 x
  | x :: rl ->
    f1 x;
    list_last f1 f2 rl

let is_prefix_dir dir fn =
  let prefix = dir ^ Filename.dir_sep in
  String.length fn > String.length prefix
  && String.sub fn 0 (String.length prefix) = prefix

let try_with_channel
    (oc : out_channel) (f1 : out_channel -> 'a) (f2 : exn -> 'a) : 'a =
  try
    let result = f1 oc in
    close_out oc;
    result
  with e ->
    close_out oc;
    f2 e

let try_with_stack (f : unit -> 'a) : ('a, exn * callstack) result =
  try Ok (f ())
  with exn ->
    let stack = Callstack (Printexc.get_backtrace ()) in
    Error (exn, stack)

let map_of_list list =
  List.fold_left ~f:(fun m (k, v) -> SMap.add k v m) ~init:SMap.empty list

let set_of_list l = List.fold_right l ~f:SSet.add ~init:SSet.empty

(* \A\B\C -> A\B\C *)
let strip_ns s =
  if String.length s == 0 || s.[0] <> '\\' then
    s
  else
    String.sub s 1 (String.length s - 1)

let strip_xhp_ns s =
  if String.length s == 0 || s.[0] <> ':' then
    s
  else
    String.sub s 1 (String.length s - 1)

let strip_both_ns s = s |> strip_ns |> strip_xhp_ns

(* A\B\C -> \A\B\C *)
let add_ns s =
  if String.length s = 0 || s.[0] <> '\\' then
    "\\" ^ s
  else
    s

(* A:B:C -> :A:B:C *)
let add_xhp_ns s =
  if String.length s = 0 || s.[0] <> ':' then
    ":" ^ s
  else
    s

(* \A\B\C -> C *)
let strip_all_ns s =
  try
    let base_name_start = String.rindex s '\\' + 1 in
    String.sub s base_name_start (String.length s - base_name_start)
  with Not_found -> s

(* "\\A\\B\\C" -> ("\\A\\B\\" * "C") *)
let split_ns_from_name (s : string) : string * string =
  try
    let base_name_start = String.rindex s '\\' + 1 in
    let name_part =
      String.sub s base_name_start (String.length s - base_name_start)
    in
    let namespace_part = String.sub s 0 base_name_start in
    (namespace_part, name_part)
  with Not_found -> ("\\", s)

let double_colon = Str.regexp_string "::"

(* Expands a namespace using the namespace map, a list of (string, string) tuples
 * Ensures the beginning backslash is present
 *
 * "Str\\join" -> "\\HH\\Lib\\Str\\join" (when "Str", "HH\\Lib\\Str" is present in map)
 * "HH\\Lib\\Str\\Join" -> "\\HH\\Lib\\Str\\join"
 * "\\HH\\Lib\\Str\\Join" -> "\\HH\\Lib\\Str\\join"
 * "just_plain_func" -> "\\just_plain_func"
 *)
let expand_namespace (ns_map : (string * string) list) (s : string) : string =
  let (raw_ns, name) = split_ns_from_name s in
  (* Might need left backslash *)
  let ns = add_ns raw_ns in
  let matching_alias =
    List.find ns_map (fun (alias, _) ->
        let fixup = add_ns alias ^ "\\" in
        fixup = ns)
  in
  match matching_alias with
  | None -> add_ns s
  | Some (_, expanded) -> add_ns (expanded ^ "\\" ^ name)

(*
 * "A::B" -> Some "A" * "B"
 * "::B" "A::" "Abc" -> None
 *)
let split_class_from_method (s : string) : (string * string) option =
  try
    let i = Str.search_forward double_colon s 0 in
    let len = String.length s in
    let class_part = String.sub s 0 i in
    Printf.printf "Class part is [%s]\n" class_part;
    let meth_part = String.sub s (i + 2) (len - i - 2) in
    Printf.printf "Meth part is [%s]\n" meth_part;
    if class_part = "" || meth_part = "" then
      None
    else
      Some (class_part, meth_part)
  with _ -> None

(*****************************************************************************)
(* Same as List.iter2, except that we only iterate as far as the shortest
 * of both lists.
 *)
(*****************************************************************************)

let rec iter2_shortest f l1 l2 =
  match (l1, l2) with
  | ([], _)
  | (_, []) ->
    ()
  | (x1 :: rl1, x2 :: rl2) ->
    f x1 x2;
    iter2_shortest f rl1 rl2

let fold_fun_list acc fl = List.fold_left fl ~f:( |> ) ~init:acc

let compose f g x = f (g x)

module With_complete_flag = struct
  type 'a t = {
    is_complete: bool;
    value: 'a;
  }
end

let try_finally ~f ~(finally : unit -> unit) =
  let res =
    try f ()
    with e ->
      finally ();
      raise e
  in
  finally ();
  res

let with_context ~enter ~exit ~do_ =
  enter ();
  let result =
    try do_ ()
    with e ->
      let stack = Printexc.get_raw_backtrace () in
      exit ();
      Printexc.raise_with_backtrace e stack
  in
  exit ();
  result

(* We run with exception backtraces turned off for performance reasons. But for
 * some kinds of catastrophic exceptions, which we never recover from (so the
 * performance doesn't matter) we do want the backtrace. "assert false" is one
 * of such conditions.
 *)
let assert_false_log_backtrace msg =
  Printf.eprintf "assert false with backtrace:\n";
  Option.iter msg ~f:(Printf.eprintf "%s\n");
  Printf.eprintf
    "%s"
    (Printexc.raw_backtrace_to_string (Printexc.get_callstack 100));
  assert false

(* Returns the largest element in arr strictly less than `bound` *)
let infimum (arr : 'a array) (bound : 'b) (compare : 'a -> 'b -> int) :
    int option =
  let rec binary_search low high =
    if low = high then
      Some low
    else if low > high then
      None
    else
      let mid = (low + high + 1) / 2 in
      let test = arr.(mid) in
      if compare test bound < 0 then
        binary_search mid high
      else
        binary_search low (mid - 1)
  in
  binary_search 0 (Array.length arr - 1)

let unwrap_snd (a, b_opt) =
  match b_opt with
  | None -> None
  | Some b -> Some (a, b)
