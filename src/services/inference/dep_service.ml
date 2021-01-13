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

(* produce, given files in fileset:
   (1) a dependent (reverse dependency) map for those files:
       the key is the module provided by a file;
       the value is the subset of files which require that module directly
   (2) a subset of those files that phantom depend on root_fileset

   IMPORTANT!!! The only state this function can read is the resolved requires! If you need this
                function to read any other state, make sure to update the DirectDependentFilesCache!
*)
let calc_direct_dependents_utils ~reader workers fileset root_fileset =
  let open Module_heaps in
  let root_fileset =
    FilenameSet.fold
      (fun f root_fileset ->
        match f with
        | File_key.SourceFile s
        | File_key.JsonFile s
        | File_key.ResourceFile s ->
          SSet.add s root_fileset
        | File_key.LibFile _
        | File_key.Builtins ->
          root_fileset)
      root_fileset
      SSet.empty
  in
  (* Distribute work, looking up InfoHeap and ResolvedRequiresHeap once per file. *)
  let job =
    List.fold_left (fun acc f ->
        let { resolved_modules; phantom_dependents; _ } =
          Reader_dispatcher.get_resolved_requires_unsafe ~reader ~audit:Expensive.ok f
        in

        (* For every required module m, add f to the reverse dependency list for m,
           stored in `module_dependents_tbl`. This will be used downstream when computing
           direct_dependents, and also in calc_all_dependents.

           TODO: should generate this map once on startup, keep required_by in
           module records and update incrementally on recheck.
        *)
        let required = Modulename.Set.of_list (SMap.values resolved_modules) in
        (* If f's phantom dependents are in root_fileset, then add f to
           `resolution_path_files`. These are considered direct dependencies (in
           addition to others computed by direct_dependents downstream). *)
        let is_resolution_path_file =
          SSet.exists (fun f -> SSet.mem f root_fileset) phantom_dependents
        in
        (f, required, is_resolution_path_file) :: acc)
  in
  (* merge results *)
  let merge = List.rev_append in
  let%lwt result =
    MultiWorkerLwt.call
      workers
      ~job
      ~merge
      ~neutral:[]
      ~next:(MultiWorkerLwt.next workers (FilenameSet.elements fileset))
  in
  List.fold_left
    (fun (module_dependents, resolution_path_files) (f, required, is_resolution_path_file) ->
      let add_file = function
        | None -> Some [f]
        | Some fs -> Some (f :: fs)
      in
      let module_dependents =
        Modulename.Set.fold (fun r -> Modulename.Map.update r add_file) required module_dependents
      in
      let resolution_path_files =
        if is_resolution_path_file then
          FilenameSet.add f resolution_path_files
        else
          resolution_path_files
      in
      (module_dependents, resolution_path_files))
    (Modulename.Map.empty, FilenameSet.empty)
    result
  |> Lwt.return

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

  IMPORTANT!!! The only state this function can read is the resolved requires! If you need this
               function to read any other state, make sure to update the DirectDependentFilesCache!
*)
let calc_direct_dependents ~reader workers ~candidates ~root_files ~root_modules =
  if FilenameSet.is_empty root_files && Modulename.Set.is_empty root_modules then
    (* dependent_calc_utils is O(candidates), but if root_files and root_modules are empty then we
     * can immediately return. We know that the empty set has no direct or transitive dependencies.
     * This can save us a lot of time on very large repositories *)
    Lwt.return FilenameSet.empty
  else
    (* Get the modules provided by candidate files, the reverse dependency map
       for candidate files, and the subset of candidate files whose resolution
       paths may encounter new or changed modules. *)
      let%lwt (module_dependents, resolution_path_files) =
        calc_direct_dependents_utils ~reader workers candidates root_files
      in
      (* resolution_path_files, plus files that require root_modules *)
      let direct_dependents =
        Modulename.Set.fold
          (fun m acc ->
            match Modulename.Map.find_opt m module_dependents with
            | None -> acc
            | Some files -> List.fold_left (fun acc f -> FilenameSet.add f acc) acc files)
          root_modules
          resolution_path_files
      in
      Lwt.return direct_dependents

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

let file_dependencies ~options ~audit ~reader file =
  let file_sig = Parsing_heaps.Mutator_reader.get_file_sig_unsafe reader file in
  let require_set = File_sig.With_Loc.(require_set file_sig.module_sig) in
  let sig_require_set =
    if not (Options.new_signatures options) then
      let sig_file_sig = Parsing_heaps.Mutator_reader.get_sig_file_sig_unsafe reader file in
      File_sig.With_ALoc.(require_set sig_file_sig.module_sig)
    else
      let { Packed_type_sig.module_refs = mrefs; _ } =
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
let calc_partial_dependency_graph ~options ~reader workers files ~parsed =
  let%lwt dependency_graph =
    MultiWorkerLwt.call
      workers
      ~job:
        (List.fold_left (fun dependency_info file ->
             let dependencies = file_dependencies ~options ~audit:Expensive.ok ~reader file in
             FilenameMap.add file dependencies dependency_info))
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

let calc_dependency_info ~options ~reader workers ~parsed =
  let%lwt dependency_graph =
    calc_partial_dependency_graph ~options ~reader workers parsed ~parsed
  in
  Lwt.return (Dependency_info.of_map dependency_graph)
