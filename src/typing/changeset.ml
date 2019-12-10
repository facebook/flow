(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Utils = Utils_js

(** A Changeset holds records of var read/writes and refis.
    First part of this module defines the structure and an API
    over it, second part hosts global changeset state and a
    separate API. Second part is embedded here due to dependencies.
  *)

(* operations on vars and refis.
   refine is a read that results in a type update *)
type op =
  | Read
  | Write
  | Refine

let string_of_op = function
  | Read -> "Read"
  | Write -> "Write"
  | Refine -> "Refine"

(* ref to scope entry *)
module EntryRef = struct
  type t = int * string * op

  let compare = Pervasives.compare
end

module EntryRefSet : Set.S with type elt = EntryRef.t = Set.Make (EntryRef)

(* ref to scope refi *)
module RefiRef = struct
  type t = int * Key.t * op

  let compare = Pervasives.compare
end

module RefiRefSet : Set.S with type elt = RefiRef.t = Set.Make (RefiRef)

(* changeset is a set of changed variables by name
   and a set of changed refinements by key *)
type t = EntryRefSet.t * RefiRefSet.t

let empty = (EntryRefSet.empty, RefiRefSet.empty)

let add_var var_ref (vars, refis) = (EntryRefSet.add var_ref vars, refis)

let add_refi refi_ref (vars, refis) = (vars, RefiRefSet.add refi_ref refis)

(* ugh ocaml *)
let iset_of_list list = List.fold_left (fun acc elem -> ISet.add elem acc) ISet.empty list

(* filter changeset to contain only changes for given scopes *)
let include_scopes ids (vars, refis) =
  let mem =
    let ids = iset_of_list ids in
    (fun id -> ISet.mem id ids)
  in
  ( EntryRefSet.filter (fun (scope_id, _, _) -> mem scope_id) vars,
    RefiRefSet.filter (fun (scope_id, _, _) -> mem scope_id) refis )

let include_ops ops (vars, refis) =
  ( EntryRefSet.filter (fun (_, _, op) -> List.mem op ops) vars,
    RefiRefSet.filter (fun (_, _, op) -> List.mem op ops) refis )

let include_reads = include_ops [Read]

let include_writes = include_ops [Write]

let exclude_refines = include_ops [Read; Write]

let iter ?ops f_vars f_refis changeset =
  let (vars, refis) =
    match ops with
    | None -> changeset
    | Some ops -> include_ops ops changeset
  in
  vars |> EntryRefSet.iter f_vars;
  refis |> RefiRefSet.iter f_refis

let iter_reads = iter ~ops:[Read]

let iter_writes = iter ~ops:[Write]

let iter_refines = iter ~ops:[Refine]

let iter_type_updates = iter ~ops:[Write; Refine]

let is_empty (vars, refis) = vars = EntryRefSet.empty && refis = RefiRefSet.empty

let union (vars1, refis1) (vars2, refis2) =
  (EntryRefSet.union vars1 vars2, RefiRefSet.union refis1 refis2)

let inter (vars1, refis1) (vars2, refis2) =
  (EntryRefSet.inter vars1 vars2, RefiRefSet.inter refis1 refis2)

let diff (vars1, refis1) (vars2, refis2) =
  (EntryRefSet.diff vars1 vars2, RefiRefSet.diff refis1 refis2)

let comp x y = union (diff x y) (diff y x)

let string_of_entry_ref (scope_id, name, op) =
  Utils.spf "(%d, %s, %s)" scope_id name (string_of_op op)

let string_of_refi_ref (scope_id, key, op) =
  Utils.spf "(%d, %s, %s)" scope_id (Key.string_of_key key) (string_of_op op)

let to_string =
  let string_of_changed_vars changed_vars =
    Utils.spf
      "{ %s }"
      (let entry_refs =
         EntryRefSet.fold
           (fun entry_ref acc -> string_of_entry_ref entry_ref :: acc)
           changed_vars
           []
       in
       String.concat "; " (List.rev entry_refs))
  in
  let string_of_changed_refis changed_refis =
    Utils.spf
      "{ %s }"
      (let refi_refs =
         RefiRefSet.fold (fun refi_ref acc -> string_of_refi_ref refi_ref :: acc) changed_refis []
       in
       String.concat "; " (List.rev refi_refs))
  in
  fun (changed_vars, changed_refis) ->
    Utils.spf "%s, %s" (string_of_changed_vars changed_vars) (string_of_changed_refis changed_refis)

(*************************************************************)

(** change tracking **)

(** provides an API over a global stack of changesets to track
    read/write ops and refinements as AST traversal duing infer
    drives calls into Env module.
    (Kill globals TODO: move to context.)
  *)

(* due to the current dependency situation, we locate the
   global changeset stack here for now, so it can be accessed
   from both Env and Flow_js. *)
module Global = struct
  type 'a stack = 'a list ref

  let changesets : t stack = ref []

  let is_active () = List.length !changesets > 0

  let init () = changesets := []

  let push () = changesets := (EntryRefSet.empty, RefiRefSet.empty) :: !changesets

  let pop () = changesets := List.tl !changesets

  (* return the current changeset *)
  let peek () = List.hd !changesets

  (* helper: transform current changeset, given
     transform functions for vars and/or refis.
     swap, return prev *)
  let swap f_vars f_refis =
    let (prev_vars, prev_refis) = peek () in
    let apply_opt arg = function
      | None -> arg
      | Some f -> f arg
    in
    let new_vars = apply_opt prev_vars f_vars in
    let new_refis = apply_opt prev_refis f_refis in
    changesets := (new_vars, new_refis) :: List.tl !changesets;
    (prev_vars, prev_refis)

  (* clear changeset, return previous *)
  let clear () = swap (Some (fun _ -> EntryRefSet.empty)) (Some (fun _ -> RefiRefSet.empty))

  (* restore passed changeset, return previous *)
  let restore (vars, refis) = swap (Some (fun _ -> vars)) (Some (fun _ -> refis))

  (* merge changeset with passed one, return previous *)
  let merge (vars, refis) = swap (Some (EntryRefSet.union vars)) (Some (RefiRefSet.union refis))

  (* filter changes targeting the given scope from the current changeset *)
  let filter_scope_changes id =
    swap
      (Some (EntryRefSet.filter (fun (scope_id, _, _) -> scope_id != id)))
      (Some (RefiRefSet.filter (fun (scope_id, _, _) -> scope_id != id)))

  (* record a changed var in current changeset *)
  let change_var entry_ref = ignore (swap (Some (EntryRefSet.add entry_ref)) None)

  (* record a refinement in current changeset *)
  let change_refi refi_ref = ignore (swap None (Some (RefiRefSet.add refi_ref)))
end
