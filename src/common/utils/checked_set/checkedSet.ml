(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Disjoint sets of checked files (files which we not only parse but also merge)
 *
 * Lazy mode:
 *   Focused files - files which the user cares about.
 *   Dependent files - files which directly or transitively depend on focused files
 *   Dependency files - files on which Focused or Dependent files  directly or transitively depend
 *
 * Non-lazy mode:
 *   Focused files - Every checked file
 *   Dependent files - Empty
 *   Dependency files - Empty
 *)

open Utils_js

(* A file foo.js can be focused, a dependent, and a dependency all at the same time. However, in
 * CheckedSet.t, we just keep track of its most important role. A focused file is more important
 * than a dependent file, which is more important than a dependency file *)
type kind =
  | Focused
  | Dependent
  | Dependency

type t = {
  focused: FilenameSet.t;
  dependents: FilenameSet.t;
  dependencies: FilenameSet.t;
}

let debug_equal a b =
  let { focused = a_focused; dependents = a_dependents; dependencies = a_dependencies } = a in
  let { focused = b_focused; dependents = b_dependents; dependencies = b_dependencies } = b in
  FilenameSet.equal a_focused b_focused
  && FilenameSet.equal a_dependents b_dependents
  && FilenameSet.equal a_dependencies b_dependencies

let empty =
  { focused = FilenameSet.empty; dependents = FilenameSet.empty; dependencies = FilenameSet.empty }

let is_empty { focused; dependents; dependencies } =
  FilenameSet.is_empty focused
  && FilenameSet.is_empty dependents
  && FilenameSet.is_empty dependencies

let of_focused_list focused =
  {
    focused = FilenameSet.of_list focused;
    dependents = FilenameSet.empty;
    dependencies = FilenameSet.empty;
  }

let cardinal { focused; dependents; dependencies } =
  FilenameSet.cardinal focused + FilenameSet.cardinal dependents + FilenameSet.cardinal dependencies

let mem key { focused; dependents; dependencies } =
  FilenameSet.mem key focused || FilenameSet.mem key dependents || FilenameSet.mem key dependencies

let add
    ?(focused = FilenameSet.empty)
    ?(dependents = FilenameSet.empty)
    ?(dependencies = FilenameSet.empty)
    checked =
  let focused = FilenameSet.union focused checked.focused in
  let dependents = FilenameSet.union dependents checked.dependents in
  let dependencies = FilenameSet.union dependencies checked.dependencies in
  (* ensure disjointness *)
  let dependents = FilenameSet.diff dependents focused in
  let dependencies = FilenameSet.diff dependencies focused in
  let dependencies = FilenameSet.diff dependencies dependents in
  { focused; dependents; dependencies }

let remove to_remove { focused; dependents; dependencies } =
  {
    focused = FilenameSet.diff focused to_remove;
    dependents = FilenameSet.diff dependents to_remove;
    dependencies = FilenameSet.diff dependencies to_remove;
  }

let union { focused; dependents; dependencies } checked =
  add ~focused ~dependents ~dependencies checked

(** [diff a b] removes from [a] every key which exists in [b] and which has an equal or higher
    kind in [b] than it does in [a], where Focused > Dependent > Dependency. So

    {[
      diff
        { A: Focused, B: Focused,   C: Dependency, D: Dependent }
        { A: Focused, B: Dependent, C: Dependent}
      = { B: Focused, D: Dependent }
    ]}
 *)
let diff { focused; dependents; dependencies } b =
  let focused = FilenameSet.diff focused b.focused in
  let dependents = FilenameSet.diff dependents b.focused in
  let dependents = FilenameSet.diff dependents b.dependents in
  let dependencies = FilenameSet.diff dependencies b.focused in
  let dependencies = FilenameSet.diff dependencies b.dependents in
  let dependencies = FilenameSet.diff dependencies b.dependencies in
  { focused; dependents; dependencies }

let filter ~f { focused; dependents; dependencies } =
  {
    focused = FilenameSet.filter (fun key -> f key Focused) focused;
    dependents = FilenameSet.filter (fun key -> f key Dependent) dependents;
    dependencies = FilenameSet.filter (fun key -> f key Dependency) dependencies;
  }

let partition_dependencies { focused; dependents; dependencies } =
  ({ empty with dependencies }, { empty with focused; dependents })

let is_focused kind = kind = Focused

let is_dependent kind = kind = Dependent

let is_dependency kind = kind = Dependency

(* Gives you a FilenameSet of all the checked files *)
let all { focused; dependents; dependencies } =
  FilenameSet.union focused (FilenameSet.union dependents dependencies)

(* Gives you a FilenameSet of all the focused files *)
let focused { focused; _ } = focused

let focused_cardinal { focused; _ } = FilenameSet.cardinal focused

(* Gives you a FilenameSet of all the dependent files *)
let dependents { dependents; _ } = dependents

let dependents_cardinal { dependents; _ } = FilenameSet.cardinal dependents

(* Gives you a FilenameSet of all the dependency files *)
let dependencies { dependencies; _ } = dependencies

let dependencies_cardinal { dependencies; _ } = FilenameSet.cardinal dependencies

let mem_focused x { focused; _ } = FilenameSet.mem x focused

let mem_dependent x { dependents; _ } = FilenameSet.mem x dependents

let mem_dependency x { dependencies; _ } = FilenameSet.mem x dependencies

(* Helper function for debugging *)
let debug_to_string ?limit =
  let string_of_set set =
    let files =
      Utils_js.FilenameSet.elements set
      |> Base.List.map ~f:(fun f -> spf "\"%s\"" (File_key.to_string f))
    in
    let files =
      match limit with
      | None -> files
      | Some n -> ListUtils.first_upto_n n (fun t -> Some (spf "[shown %d/%d]" n t)) files
    in
    String.concat "\n" files
  in
  fun checked ->
    Printf.sprintf
      "Focused:\n%s\nDependents:\n%s\nDependencies:\n%s"
      (checked |> focused |> string_of_set)
      (checked |> dependents |> string_of_set)
      (checked |> dependencies |> string_of_set)

let debug_counts_to_string checked =
  Printf.sprintf
    "Focused: %d, Dependents: %d, Dependencies: %d"
    (focused_cardinal checked)
    (dependents_cardinal checked)
    (dependencies_cardinal checked)
