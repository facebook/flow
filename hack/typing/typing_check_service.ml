(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

module Env = Typing_env

(*****************************************************************************)
(* The place where we store the shared data in cache *)
(*****************************************************************************)

module TypeCheckStore = GlobalStorage.Make(struct
  type t = TypecheckerOptions.t
end)

let neutral = [], Relative_path.Set.empty

(*****************************************************************************)
(* The job that will be run on the workers *)
(*****************************************************************************)

let type_fun tcopt x =
  try
    let fun_ = Naming_heap.FunHeap.find_unsafe x in
    Typing.fun_def tcopt x fun_;
  with Not_found -> ()

let type_class tcopt x =
  try
    let class_ = Naming_heap.ClassHeap.find_unsafe x in
    Typing.class_def tcopt x class_
  with Not_found -> ()

let check_typedef x =
  try
    let typedef = Naming_heap.TypedefHeap.find_unsafe x in
    Typing.typedef_def x typedef;
    Typing_variance.typedef x
  with Not_found ->
    ()

let check_const x =
  try
    match Typing_env.GConsts.get x with
    | None -> ()
    | Some declared_type ->
        let cst = Naming_heap.ConstHeap.find_unsafe x in
        match cst.Nast.cst_value with
        | Some v -> begin
          let filename = Pos.filename (fst cst.Nast.cst_name) in
          let dep = Typing_deps.Dep.GConst (snd cst.Nast.cst_name) in
          let env =
            Typing_env.empty TypecheckerOptions.default filename (Some dep) in
          let env = Typing_env.set_mode env cst.Nast.cst_mode in
          let env, value_type = Typing.expr env v in
          let env, dty = Typing_phase.localize_with_self env declared_type in
          let _env = Typing_utils.sub_type env dty value_type in
          ()
        end
        | None -> ()
  with Not_found ->
    ()

let check_file tcopt (errors, failed) (fn, file_infos) =
  let { FileInfo.n_funs; n_classes; n_types; n_consts } = file_infos in
  let errors', () = Errors.do_
    begin fun () ->
      SSet.iter (type_fun tcopt) n_funs;
      SSet.iter (type_class tcopt) n_classes;
      SSet.iter check_typedef n_types;
      SSet.iter check_const n_consts;
    end  in
  let failed =
    if errors' <> [] then Relative_path.Set.add fn failed else failed in
  List.rev_append errors' errors, failed

let check_files tcopt (errors, failed) fnl =
  SharedMem.invalidate_caches();
  let check_file =
    if !Utils.profile
    then (fun acc fn ->
      let t = Unix.gettimeofday () in
      let result = check_file tcopt acc fn in
      let t' = Unix.gettimeofday () in
      let msg =
        Printf.sprintf "%f %s [type-check]" (t' -. t)
          (Relative_path.suffix (fst fn)) in
      !Utils.log msg;
      result)
    else check_file tcopt in
  let errors, failed = List.fold_left fnl
    ~f:check_file ~init:(errors, failed) in
  errors, failed

let load_and_check_files acc fnl =
  let tcopt = TypeCheckStore.load() in
  check_files tcopt acc fnl

(*****************************************************************************)
(* Let's go! That's where the action is *)
(*****************************************************************************)

let parallel_check workers tcopt fnl =
  TypeCheckStore.store tcopt;
  let result =
    MultiWorker.call
      workers
      ~job:load_and_check_files
      ~neutral
      ~merge:Decl_service.merge_decl
      ~next:(Bucket.make fnl)
  in
  TypeCheckStore.clear();
  result

let go workers tcopt fast =
  let fnl = Relative_path.Map.elements fast in
  if List.length fnl < 10
  then check_files tcopt neutral fnl
  else parallel_check workers tcopt fnl
