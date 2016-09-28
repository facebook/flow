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

(* produce, given files in fileset:
   (1) a map from those files to modules
   (2) a reverse dependency map for those files:
       the key is the module provided by a file;
       the value is the subset of files which require that module directly
   (3) a subset of those files that phantom depend on root_fileset
 *)
let calc_dep_utils workers fileset root_fileset = Module_js.(
  (* Distribute work, looking up InfoHeap once per file. *)
  let job = List.fold_left (fun (modules, rdmap, resolution_path_files) f ->
    let info = get_module_info ~audit:Expensive.ok f in
    (* Add f |-> info._module to the `modules` map. This will be used downstream
       in dep_closure. Why do we this here rather than there? Just as an
       optimization, since we're reading InfoHeap here any way.

       TODO: explore whether we can avoid creating this map on every recheck,
       instead maintaining the map incrementally and hopefully reusing large
       parts of it.
    *)
    let modules = FilenameMap.add f info._module modules in
    (* For every r in info.required, add f to the reverse dependency list for r,
       stored in `rdmap`. This will be used downstream when computing
       direct_deps, and also in dep_closure.

       TODO: should generate this map once on startup, keep required_by in
       module infos and update incrementally on recheck.
    *)

    let rdmap = NameSet.fold (fun r rdmap ->
      NameMap.add r FilenameSet.(
        match NameMap.get r rdmap with
        | None -> singleton f
        | Some files -> add f files
      ) rdmap
    ) info.required rdmap in
    (* If f's phantom dependents are in root_fileset, then add f to
       `resolution_path_files`. These are considered direct dependencies (in
       addition to others computed by direct_deps downstream). *)
    let resolution_path_files =
      if info.phantom_dependents |> SSet.exists (fun f ->
        FilenameSet.mem (Loc.SourceFile f) root_fileset ||
        FilenameSet.mem (Loc.JsonFile f) root_fileset ||
        FilenameSet.mem (Loc.ResourceFile f) root_fileset
      ) then FilenameSet.add f resolution_path_files
      else resolution_path_files in
    modules, rdmap, resolution_path_files
  ) in
  (* merge results *)
  let merge
      (modules1, rdmap1, resolution_path_files1)
      (modules2, rdmap2, resolution_path_files2) =
    FilenameMap.union modules1 modules2,
    NameMap.merge (fun _ x y ->
      match x, y with
      | Some v, None
      | None, Some v -> Some v
      | Some v, Some w -> Some (FilenameSet.union v w)
      | None, None -> None
    ) rdmap1 rdmap2,
    FilenameSet.union resolution_path_files1 resolution_path_files2
  in

  MultiWorker.call workers ~job ~merge
    ~neutral: Module_js.(FilenameMap.empty, NameMap.empty, FilenameSet.empty)
    ~next: (MultiWorker.next workers (FilenameSet.elements fileset))
)

(* given a reverse dependency map (from modules to the files which
   require them), generate the closure of the dependencies of a
   given fileset, using get_module_info to map files to modules
 *)
let dep_closure modules rdmap fileset =
  let deps_of_module m = Module_js.NameMap.get m rdmap in
  let deps_of_file f =
    let m = FilenameMap.find_unsafe f modules in
    let f_module = Modulename.Filename f in
    (* In general, a file exports its module via two names. See Modulename for
       details. It suffices to note here that dependents of the file can use
       either of those names to import the module. *)
    match deps_of_module m, deps_of_module f_module with
      | None, None -> FilenameSet.empty
      | None, Some deps
      | Some deps, None -> deps
      | Some deps1, Some deps2 -> FilenameSet.union deps1 deps2
  in
  let rec expand rdmap fileset seen =
    FilenameSet.fold (fun f acc ->
      if FilenameSet.mem f !seen then acc else (
        seen := FilenameSet.add f !seen;
        let deps = deps_of_file f in
        FilenameSet.add f (FilenameSet.union acc (expand rdmap deps seen))
      )
    ) fileset FilenameSet.empty
  in expand rdmap fileset (ref FilenameSet.empty)

(* Files that must be rechecked include those that immediately or recursively
   depend on modules that were added, deleted, or modified as a consequence of
   the files that were directly added, deleted, or modified. In general, the map
   from files to modules may not be preserved: deleting files may delete
   modules, adding files may add modules, and modifying files may do both.

   Identify the direct and transitive dependents of re-inferred files and
   removed modules.

   - unmodified_files is all unmodified files in the current state
   - inferred_files is all files that have just been through local inference
   - removed_modules is all modules whose infos have just been cleared

   Note that while removed_modules and (the modules provided by) inferred_files
   usually overlap, inferred_files will include providers of new modules, and
   removed_modules will include modules provided by deleted files.

   Return the subset of unmodified_files transitively dependent on changes,
   and the subset directly dependent on them.
*)
let dependent_files workers unmodified_files inferred_files removed_modules =
  (* Get the modules provided by unmodified files, the reverse dependency map
     for unmodified files, and the subset of unmodified files whose resolution
     paths may encounter newly inferred modules. *)
  (** [shared mem access] InfoHeap.get on unmodified_files for
      ._module, .required, .phantom_dependents **)
  let modules,
    reverse_deps,
    resolution_path_files
    = calc_dep_utils workers unmodified_files inferred_files in

  (* touched_modules includes removed modules and those provided by new files *)
  (** [shared mem access] InfoHeap.get on inferred_files for ._module **)
  let touched_modules = Module_js.(NameSet.union removed_modules (
    MultiWorker.call workers
      ~job: (List.fold_left (fun mods file ->
        let file_mods = get_module_names ~audit:Expensive.ok file in
        (* Add all module names exported by file *)
        List.fold_left (fun acc m -> NameSet.add m acc) mods file_mods
      ))
      ~neutral: NameSet.empty
      ~merge: NameSet.union
      ~next: (MultiWorker.next workers (FilenameSet.elements inferred_files))
  )) in

  (* files that require touched modules directly, or may resolve to
     modules provided by newly inferred files *)
  let direct_deps = Module_js.(NameSet.fold (fun m acc ->
    match NameMap.get m reverse_deps with
    | Some files -> FilenameSet.union acc files
    | None -> acc
    ) touched_modules resolution_path_files
  ) in

  (* (transitive dependents are re-merged, directs are also re-resolved) *)
  dep_closure modules reverse_deps direct_deps,
  direct_deps


(* Calculate module dependencies. Since this involves a lot of reading from
   shared memory, it is useful to parallelize this process (leading to big
   savings in init and recheck times). *)


let checked ~audit m = Module_js.(
  let info = m |> get_file ~audit |> get_module_info ~audit in
  info.checked
)

(* A file is considered to implement a required module r only if the file is
   registered to provide r and the file is checked. Such a file must be merged
   before any file that requires module r, so this notion naturally gives rise
   to a dependency ordering among files for merging. *)
let implementation_file ~audit r = Module_js.(
  if module_exists r && checked ~audit r
  then Some (get_file ~audit r)
  else None
)

let calc_dependencies workers files =
  let deps = MultiWorker.call
    workers
    ~job: (List.fold_left (fun deps file ->
      let { Module_js.required; _ } =
        Module_js.get_module_info ~audit:Expensive.ok file in
      let files = Module_js.NameSet.fold (fun r files ->
        match implementation_file ~audit:Expensive.ok r with
        | Some f -> FilenameSet.add f files
        | None -> files
      ) required FilenameSet.empty in
      FilenameMap.add file files deps
    ))
    ~neutral: FilenameMap.empty
    ~merge: FilenameMap.union
    ~next: (MultiWorker.next workers files) in
  deps |> FilenameMap.map (
    FilenameSet.filter (fun f -> FilenameMap.mem f deps))
