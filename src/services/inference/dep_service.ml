(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

   TODO: Scanning the dependency graph to find reverse dependencies like this is not good! To avoid
   this, we should maintain the reverse dependency graph carefully as well. The existing reverse
   dependency graph stored in the server's OCaml heap is not enough:
     - It does not track reverse dependencies for non-checked files
     - It does not track reverse dependencies for no-provider modules
     - It does not track reverse dependencies for "phantom" files

   IMPORTANT!!! The only state this function can read is the resolved requires! If you need this
                function to read any other state, make sure to update the DirectDependentFilesCache!
*)
let calc_direct_dependents_job acc (root_files, root_modules) =
  (* The MultiWorker API is weird. All jobs get the `neutral` value as their
   * accumulator argument. We can exploit this to return a set from workers
   * while the server accumulates lists of sets. *)
  assert (acc = []);
  let open Module_heaps in
  let root_files =
    List.fold_left
      (fun acc f ->
        match f with
        | File_key.SourceFile s
        | File_key.JsonFile s
        | File_key.ResourceFile s ->
          SSet.add s acc
        | File_key.LibFile _
        | File_key.Builtins ->
          acc)
      SSet.empty
      root_files
  in
  let root_modules = Modulename.Set.of_list root_modules in
  let dependents = ref FilenameSet.empty in
  Module_heaps.iter_resolved_requires (fun { file_key; resolved_modules; phantom_dependents; _ } ->
      if not (SSet.disjoint root_files phantom_dependents) then
        dependents := FilenameSet.add file_key !dependents
      else if SMap.exists (fun _ m -> Modulename.Set.mem m root_modules) resolved_modules then
        dependents := FilenameSet.add file_key !dependents
  );
  !dependents

let calc_direct_dependents workers ~candidates ~root_files ~root_modules =
  if FilenameSet.is_empty root_files && Modulename.Set.is_empty root_modules then
    (* If root_files and root_modules are empty then we can immediately return.
     * We know that the empty set has no direct or transitive dependencies. This
     * can save us a lot of time on very large repositories *)
    Lwt.return FilenameSet.empty
  else
    (* Find direct dependents via parallel heap scans, searching for dependents
     * of the changed files and modules. Note that we accumulate a list of sets
     * during the call, then merge the sets after. List.cons is much faster than
     * FilenameSet.union, so we can avoid blocking worker SEND this way. *)
    let next =
      MultiWorkerLwt.next2
        workers
        ~max_size:2000
        (FilenameSet.elements root_files)
        (Modulename.Set.elements root_modules)
    in
    let%lwt dependent_sets =
      MultiWorkerLwt.call workers ~job:calc_direct_dependents_job ~merge:List.cons ~neutral:[] ~next
    in
    let dependents = List.fold_left FilenameSet.union FilenameSet.empty dependent_sets in
    (* We are only interested in dependents which are also in `candidates` *)
    Lwt.return (FilenameSet.inter candidates dependents)

(* Calculate module dependencies. Since this involves a lot of reading from
   shared memory, it is useful to parallelize this process (leading to big
   savings in init and recheck times). *)

let checked_module ~reader ~audit m =
  m
  |> Module_heaps.Mutator_reader.get_file_unsafe ~reader ~audit
  |> Module_js.checked_file ~reader:(Abstract_state_reader.Mutator_state_reader reader) ~audit

(* A file is considered to implement a required module r only if the file is
   registered to provide r and the file is checked. Such a file must be merged
   before any file that requires module r, so this notion naturally gives rise
   to a dependency ordering among files for merging. *)
let implementation_file ~reader ~audit r =
  if Module_heaps.Mutator_reader.module_exists ~reader r && checked_module ~reader ~audit r then
    Some (Module_heaps.Mutator_reader.get_file_unsafe ~reader ~audit r)
  else
    None

let file_dependencies ~audit ~reader file =
  let file_sig = Parsing_heaps.Mutator_reader.get_file_sig_unsafe reader file in
  let require_set = File_sig.With_Loc.(require_set file_sig.module_sig) in
  let sig_require_set =
    let { Packed_type_sig.Module.module_refs = mrefs; _ } =
      Parsing_heaps.Mutator_reader.get_type_sig_unsafe reader file
    in
    let acc = ref SSet.empty in
    Type_sig_collections.Module_refs.iter (fun x -> acc := SSet.add x !acc) mrefs;
    !acc
  in
  let { Module_heaps.resolved_modules; _ } =
    Module_heaps.Mutator_reader.get_resolved_requires_unsafe ~reader ~audit file
  in
  SSet.fold
    (fun mref (sig_files, all_files) ->
      let m = SMap.find mref resolved_modules in
      match implementation_file ~reader m ~audit:Expensive.ok with
      | Some f ->
        if SSet.mem mref sig_require_set then
          (FilenameSet.add f sig_files, FilenameSet.add f all_files)
        else
          (sig_files, FilenameSet.add f all_files)
      | None -> (sig_files, all_files))
    require_set
    (FilenameSet.empty, FilenameSet.empty)

(* Calculates the dependency graph as a map from files to their dependencies.
 * Dependencies not in parsed are ignored. *)
let calc_partial_dependency_graph ~reader workers files ~parsed =
  let%lwt dependency_graph =
    MultiWorkerLwt.call
      workers
      ~job:
        (List.fold_left (fun dependency_info file ->
             let dependencies = file_dependencies ~audit:Expensive.ok ~reader file in
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
