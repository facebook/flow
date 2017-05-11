(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
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
   (1) a map from those files to modules
   (2) a dependent (reverse dependency) map for those files:
       the key is the module provided by a file;
       the value is the subset of files which require that module directly
   (3) a subset of those files that phantom depend on root_fileset
 *)
let dependent_calc_utils workers fileset root_fileset = Module_js.(
  (* Distribute work, looking up InfoHeap and ResolvedRequiresHeap once per file. *)
  let job = List.fold_left (fun (modules, module_dependent_map, resolution_path_files) f ->
    let resolved_requires = get_resolved_requires_unsafe ~audit:Expensive.ok f in
    let info = get_info_unsafe ~audit:Expensive.ok f in
    (* Add f |-> info._module to the `modules` map. This will be used downstream
       in calc_all_dependents.

       TODO: Why do we this here rather than there? This used to be an
       optimization, since InfoHeap and ResolvedRequiresHeap were
       together. Will clean up later.

       TODO: explore whether we can avoid creating this map on every recheck,
       instead maintaining the map incrementally and hopefully reusing large
       parts of it.
    *)
    let modules = FilenameMap.add f info._module modules in
    (* For every r in resolved_requires.required, add f to the reverse dependency list for r,
       stored in `module_dependent_map`. This will be used downstream when computing
       direct_dependents, and also in calc_all_dependents.

       TODO: should generate this map once on startup, keep required_by in
       module records and update incrementally on recheck.
    *)

    let module_dependent_map = NameSet.fold (fun r module_dependent_map ->
      NameMap.add r FilenameSet.(
        match NameMap.get r module_dependent_map with
        | None -> singleton f
        | Some files -> add f files
      ) module_dependent_map
    ) resolved_requires.required module_dependent_map in
    (* If f's phantom dependents are in root_fileset, then add f to
       `resolution_path_files`. These are considered direct dependencies (in
       addition to others computed by direct_dependents downstream). *)
    let resolution_path_files =
      if resolved_requires.phantom_dependents |> SSet.exists (fun f ->
        FilenameSet.mem (Loc.SourceFile f) root_fileset ||
        FilenameSet.mem (Loc.JsonFile f) root_fileset ||
        FilenameSet.mem (Loc.ResourceFile f) root_fileset
      ) then FilenameSet.add f resolution_path_files
      else resolution_path_files in
    modules, module_dependent_map, resolution_path_files
  ) in
  (* merge results *)
  let merge
      (modules1, module_dependent_map1, resolution_path_files1)
      (modules2, module_dependent_map2, resolution_path_files2) =
    FilenameMap.union modules1 modules2,
    NameMap.merge (fun _ x y ->
      match x, y with
      | Some v, None
      | None, Some v -> Some v
      | Some v, Some w -> Some (FilenameSet.union v w)
      | None, None -> None
    ) module_dependent_map1 module_dependent_map2,
    FilenameSet.union resolution_path_files1 resolution_path_files2
  in

  MultiWorker.call workers ~job ~merge
    ~neutral: Module_js.(FilenameMap.empty, NameMap.empty, FilenameSet.empty)
    ~next: (MultiWorker.next workers (FilenameSet.elements fileset))
)

(* given a reverse dependency map (from modules to the files which
   require them), generate the closure of the dependencies of a
   given fileset, using get_info_unsafe to map files to modules
 *)
let calc_all_dependents modules module_dependent_map fileset =
  let module_dependents m = Module_js.NameMap.get m module_dependent_map in
  let file_dependents f =
    let m = FilenameMap.find_unsafe f modules in
    let f_module = Module_js.eponymous_module f in
    (* In general, a file exports its module via two names. See Modulename for
       details. It suffices to note here that dependents of the file can use
       either of those names to import the module. *)
    match module_dependents m, module_dependents f_module with
      | None, None -> FilenameSet.empty
      | None, Some dependents
      | Some dependents, None -> dependents
      | Some dependents1, Some dependents2 -> FilenameSet.union dependents1 dependents2
  in
  let rec expand module_dependent_map fileset seen =
    FilenameSet.fold (fun f acc ->
      if FilenameSet.mem f !seen then acc else (
        seen := FilenameSet.add f !seen;
        let dependents = file_dependents f in
        FilenameSet.add f (FilenameSet.union acc (expand module_dependent_map dependents seen))
      )
    ) fileset FilenameSet.empty
  in expand module_dependent_map fileset (ref FilenameSet.empty)

(* Identify the direct and transitive dependents of new, changed, and deleted
   files.

   Files that must be rechecked include those that immediately or recursively
   depended on modules whose providers were affected by new, changed, or deleted
   files. The latter modules, marked "changed," are calculated earlier when
   picking providers.

   - unchanged is all unchanged files in the current state
   - new_or_changed is all files that have just been through local inference and
   all skipped files that were also new or unchanged
   - changed_modules is a conservative approximation of modules that no longer have
   the same providers, or whose providers are changed files

   Return the subset of unchanged transitively dependent on updates, and
   the subset directly dependent on them.
*)
let dependent_files workers ~unchanged ~new_or_changed ~changed_modules =
  (* Get the modules provided by unchanged files, the reverse dependency map
     for unchanged files, and the subset of unchanged files whose resolution
     paths may encounter new or changed modules. *)
  let modules,
    module_dependent_map,
    resolution_path_files
    = dependent_calc_utils workers unchanged new_or_changed in

  (* resolution_path_files, plus files that require changed_modules *)
  let direct_dependents = Module_js.(NameSet.fold (fun m acc ->
    match NameMap.get m module_dependent_map with
    | Some files -> FilenameSet.union acc files
    | None -> acc
    ) changed_modules resolution_path_files
  ) in

  (* (transitive dependents are re-merged, directs are also re-resolved) *)
  calc_all_dependents modules module_dependent_map direct_dependents,
  direct_dependents


(* Calculate module dependencies. Since this involves a lot of reading from
   shared memory, it is useful to parallelize this process (leading to big
   savings in init and recheck times). *)


let checked_module ~audit m = Module_js.(
  m |> get_file_unsafe ~audit |> checked_file ~audit
)

(* A file is considered to implement a required module r only if the file is
   registered to provide r and the file is checked. Such a file must be merged
   before any file that requires module r, so this notion naturally gives rise
   to a dependency ordering among files for merging. *)
let implementation_file ~audit r = Module_js.(
  if module_exists r && checked_module ~audit r
  then Some (get_file_unsafe ~audit r)
  else None
)

let file_dependencies ~audit file = Module_js.(
  let { required; _ } = get_resolved_requires_unsafe ~audit file in
  NameSet.fold (fun r files ->
    match implementation_file ~audit:Expensive.ok r with
    | Some f -> FilenameSet.add f files
    | None -> files
  ) required FilenameSet.empty
)

let calc_dependency_graph workers files =
  let dependency_graph = MultiWorker.call
    workers
    ~job: (List.fold_left (fun dependency_graph file ->
      FilenameMap.add file (file_dependencies ~audit:Expensive.ok file) dependency_graph
    ))
    ~neutral: FilenameMap.empty
    ~merge: FilenameMap.union
    ~next: (MultiWorker.next workers files) in
  dependency_graph |> FilenameMap.map (
    FilenameSet.filter (fun f -> FilenameMap.mem f dependency_graph))

let calc_all_dependencies =
  let rec loop dependency_graph =
    FilenameSet.fold (fun file acc ->
      match FilenameMap.get file dependency_graph with
      | Some files ->
        let files = FilenameSet.diff files acc in
        let acc = FilenameSet.union files acc in
        loop dependency_graph files acc
      | None -> acc
    ) in
  fun dependency_graph files ->
    loop dependency_graph files files
