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

(* produce a reverse dependency map for the given fileset.
   keys are modules provided by the files in fileset.
   value for each key is the set of files which require that
   module directly.
 *)
let calc_reverse_deps workers fileset = Module_js.(
  (* distribute requires from a list of files into reverse-dep map *)
  let job = List.fold_left (fun rdmap f ->
    let reqs = (get_module_info f).required in
    NameSet.fold (fun r rdmap ->
      NameMap.add r FilenameSet.(
        match NameMap.get r rdmap with
        | None -> singleton f
        | Some files -> add f files
      ) rdmap
    ) reqs rdmap
  ) in
  (* merge two reverse-dependency maps *)
  let merge = NameMap.merge (fun _ x y ->
    match x, y with
    | Some v, None
    | None, Some v -> Some v
    | Some v, Some w -> Some (FilenameSet.union v w)
    | None, None -> None
  ) in
  MultiWorker.call workers ~job ~merge
    ~neutral: Module_js.NameMap.empty
    ~next: (Bucket.make (FilenameSet.elements fileset))
)

(* given a reverse dependency map (from modules to the files which
   require them), generate the closure of the dependencies of a
   given fileset, using get_module_info to map files to modules
 *)
let dep_closure rdmap fileset = FilenameSet.(
  let rec expand rdmap fileset seen =
    fold (fun f acc ->
      if mem f !seen then acc else (
        seen := add f !seen;
        let m = Module_js.((get_module_info f)._module) in
        add f (match Module_js.NameMap.get m rdmap with
          | None -> acc
          | Some deps -> union acc (expand rdmap deps seen)
        )
      )
    ) fileset empty
  in expand rdmap fileset (ref empty)
)

(* Files that must be rechecked include those that immediately or recursively
   depend on modules that were added, deleted, or modified as a consequence of
   the files that were directly added, deleted, or modified. In general, the map
   from files to modules may not be preserved: deleting files may delete
   modules, adding files may add modules, and modifying files may do both.

   Identify the direct and transitive dependents of re-inferred files and
   removed modules.

   - unmodified_files is all unmodified files in the current state
   - inferred_files is all files that have just been through local inference
   - touched_modules is all modules whose infos have just been cleared

   Note that while touched_modules and (the modules provided by) inferred_files
   usually overlap, inferred_files will include providers of new modules, and
   touched_modules will include modules provided by deleted files.

   Return the subset of unmodified_files transitively dependent on changes,
   and the subset directly dependent on them.
*)
let dependent_files workers unmodified_files inferred_files touched_modules =

  (* get reverse dependency map for unmodified files.
     TODO should generate this once on startup, keep required_by
     in module infos and update incrementally on recheck *)
  let reverse_deps = calc_reverse_deps workers unmodified_files in

  (* expand touched_modules to include those provided by new files *)
  let touched_modules = FilenameSet.fold Module_js.(fun file mods ->
    NameSet.add (get_module_name file) mods
  ) inferred_files touched_modules in

  (* files whose resolution paths may encounter newly inferred modules *)
  let resolution_path_files = MultiWorker.call workers
    ~job: (List.fold_left
      (Module_js.resolution_path_dependency inferred_files))
    ~neutral: FilenameSet.empty
    ~merge: FilenameSet.union
    ~next: (Bucket.make (FilenameSet.elements unmodified_files)) in

  (* files that require touched modules directly, or may resolve to
     modules provided by newly inferred files *)
  let direct_deps = Module_js.(NameSet.fold (fun m s ->
    match NameMap.get m reverse_deps with
    | Some files -> FilenameSet.union s files
    | None -> s
    ) touched_modules FilenameSet.empty
  ) |> FilenameSet.union resolution_path_files in

  (* (transitive dependents are re-merged, directs are also re-resolved) *)
  dep_closure reverse_deps direct_deps,
  direct_deps


(* Calculate module dependencies. Since this involves a lot of reading from
   shared memory, it is useful to parallelize this process (leading to big
   savings in init and recheck times). *)


let checked m = Module_js.(
  let info = m |> get_file |> get_module_info in
  info.checked
)

(* A file is considered to implement a required module r only if the file is
   registered to provide r and the file is checked. Such a file must be merged
   before any file that requires module r, so this notion naturally gives rise
   to a dependency ordering among files for merging. *)
let implementation_file r = Module_js.(
  if module_exists r && checked r
  then Some (get_file r)
  else None
)

let calc_dependencies_job =
  List.fold_left (fun deps file ->
    let { Module_js.required; _ } = Module_js.get_module_info file in
    let files = Module_js.NameSet.fold (fun r files ->
      match implementation_file r with
      | Some f -> FilenameSet.add f files
      | None -> files
    ) required FilenameSet.empty in
    FilenameMap.add file files deps
  )

let calc_dependencies workers files =
  let deps = MultiWorker.call
    workers
    ~job: calc_dependencies_job
    ~neutral: FilenameMap.empty
    ~merge: FilenameMap.union
    ~next: (Bucket.make files) in
  deps |> FilenameMap.map (
    FilenameSet.filter (fun f -> FilenameMap.mem f deps))
