(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

(* sketch of baseline incremental alg:

   Note: this is for strict mode only, which is the easy case,
   since it doesn't require a transitive merge of all modules
   connected via nonlocal type flows.

   Total case over set of modules M:
    1. for all m:M, do local inference on m, producing graph m.g
    2. for all m:M, for all r:m.required,
      perform substitution of r.export into m.g, producing m.g'

   Incremental case, given previously established modules m:
    1. Mt = (touched) reparse of all m:M where m.file has been updated
    2. for all m:Mt do local inference on m, producing m.g
    3. Mx = (export) m:Mt where m.export or m.name have changed
    4. Md = m:M where m.file has been deleted
    5. Mr = (recheck) m:Mt | (M where !empty(m.required & (Mx | Md))
      (all touched modules plus all others whose required sets
       include any export changes or deletions)
    5. for all m:Mr, for all r:m.required,
        perform substitution of r.export into m.g, producing m.g'
*)

(** A note on terminology.

    We say that a file F1 "depends on" another file F2 if F1 has a
    require/import that "resolves" to a module "provided by" F2.

    The "depends on" relation induces edges in a "dependency graph" with files
    as nodes. This dependency graph is often represented as a map from files to
    sets of files they depend on.

    When F1 depends on F2, i.e., there is an edge from F1 to F2 in
    the dependency graph, we say that
    - F2 is a "direct dependency" of F1
    - F1 is a "direct dependent" of F2.

    When F1 transitively depends on F2, i.e., there is a path from F1 to F2 in
    the dependency graph, we say that
    - F2 is in "all dependencies" of F1
    - F1 is in "all dependents" of F2.

    Sometimes we drop the word "all," so in general "dependencies" and
    "dependents" on their own should be taken to mean "all dependencies" and
    "all dependents." Correspondingly, sometimes we use the qualifier "direct"
    to describe non-transitive dependencies and non-transitive dependents.

    Also, sometimes we use the word "reverse dependency" instead of "dependent."

    How do we calculate the dependency graph and the dependent graph? It comes
    down to understanding what "depends on" really means. Recall that F1
    "depends on" F2 when F1 depends on some resolved module that is provided by
    F2. Since the "provides by" function is not injective, maps from files to
    (sets of) modules and back that compose to give the dependency graph and the
    dependent graph are useful intermediate data structures.

 **)

(* Identify the direct dependents of new, changed, and deleted files.

   Files that must be rechecked include those that immediately or recursively
   depended on modules whose providers were affected by new, changed, or deleted
   files. The latter modules, marked "changed," are calculated earlier when
   picking providers.

   - candidates is the set of files which could be dependents. The returned sets will be subsets of
   the candidates set. For example, if we're calculating the dependents of all the changed files
   then this would be the set of unchanged files
   - root_files is the set of files for which we'd like to calculate dependents. This should be
   disjoint from candidates. If we wanted to calculate the dependents of all the changed files then
   this would be the set of changed files
   - root_modules is the set of modules for which we'd like to calculate dependents. If we wanted to
   calculate the dependents of all the changed files then this would be the set of module names
   which have new providers.

   Return the subset of candidates directly dependent on root_modules / root_files.
*)
let calc_unchanged_dependents =
  let module Heap = SharedMem.NewAPI in
  let job acc ms =
    (* The MultiWorker API is weird. All jobs get the `neutral` value as their
     * accumulator argument. We can exploit this to return a set from workers
     * while the server accumulates lists of sets. *)
    assert (acc = []);
    let open Parsing_heaps in
    let acc = ref FilenameSet.empty in
    let iter_f file =
      (* Skip dependents which have themselves changed, since changed files will
       * already be part of the recheck set. *)
      if not (Heap.file_changed file) then acc := FilenameSet.add (read_file_key file) !acc
    in
    let iter_m = iter_dependents iter_f in
    List.iter iter_m ms;
    !acc
  in
  fun workers changed_modules ->
    let next = MultiWorkerLwt.next workers (Modulename.Set.elements changed_modules) in
    let%lwt dependent_sets = MultiWorkerLwt.call workers ~job ~merge:List.cons ~neutral:[] ~next in
    Lwt.return (List.fold_left FilenameSet.union FilenameSet.empty dependent_sets)

(* Calculate module dependencies. Since this involves a lot of reading from
   shared memory, it is useful to parallelize this process (leading to big
   savings in init and recheck times). *)

(* A file is considered to implement a required module r only if the file is
   registered to provide r and the file is checked. Such a file must be merged
   before any file that requires module r, so this notion naturally gives rise
   to a dependency ordering among files for merging. *)
let implementation_file ~reader = function
  | Error _ -> None
  | Ok m ->
    (match Parsing_heaps.Mutator_reader.get_provider ~reader m with
    | Some f when Parsing_heaps.Mutator_reader.is_typed_file ~reader f ->
      Some (Parsing_heaps.read_file_key f)
    | _ -> None)

let file_dependencies ~reader file =
  let file_addr = Parsing_heaps.get_file_addr_unsafe file in
  let parse = Parsing_heaps.Mutator_reader.get_typed_parse_unsafe ~reader file file_addr in
  let sig_require_set =
    let module Heap = SharedMem.NewAPI in
    let module Bin = Type_sig_bin in
    let buf = Heap.type_sig_buf (Option.get (Heap.get_type_sig parse)) in
    Bin.fold_tbl Bin.read_str SSet.add buf (Bin.module_refs buf) SSet.empty
  in
  let resolved_modules =
    Parsing_heaps.Mutator_reader.get_resolved_modules_unsafe ~reader file parse
  in
  SMap.fold
    (fun mref m (sig_files, all_files) ->
      match implementation_file ~reader m with
      | Some f ->
        if SSet.mem mref sig_require_set then
          (FilenameSet.add f sig_files, FilenameSet.add f all_files)
        else
          (sig_files, FilenameSet.add f all_files)
      | None -> (sig_files, all_files))
    resolved_modules
    (FilenameSet.empty, FilenameSet.empty)

(* Calculates the dependency graph as a map from files to their dependencies.
 * Dependencies not in parsed are ignored. *)
let calc_partial_dependency_graph ~reader workers files ~parsed =
  let%lwt dependency_graph =
    MultiWorkerLwt.call
      workers
      ~job:
        (List.fold_left (fun dependency_info file ->
             let dependencies = file_dependencies ~reader file in
             FilenameMap.add file dependencies dependency_info
         )
        )
      ~neutral:FilenameMap.empty
      ~merge:FilenameMap.union
      ~next:(MultiWorkerLwt.next workers (FilenameSet.elements files))
  in
  let dependency_graph =
    FilenameMap.map
      (fun (sig_files, all_files) ->
        (FilenameSet.inter parsed sig_files, FilenameSet.inter parsed all_files))
      dependency_graph
  in
  Lwt.return dependency_graph

let calc_dependency_info ~reader workers ~parsed =
  let%lwt dependency_graph = calc_partial_dependency_graph ~reader workers parsed ~parsed in
  Lwt.return (Dependency_info.of_map dependency_graph)
