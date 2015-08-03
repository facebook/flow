(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open ServerEnv
open Utils

module Json = Hh_json
module RP = Relative_path

type result = string Lint.t list

let output_json oc el =
  let errors_json = List.map el Lint.to_json in
  let res =
    Json.JAssoc [ "errors", Json.JList errors_json;
                  "version", Json.JString Build_id.build_id_ohai;
                ] in
  output_string oc (Json.json_to_string res);
  flush stderr

let output_text oc el =
  (* Essentially the same as type error output, except that we only have one
   * message per error, and no additional 'typing reasons' *)
  if el = []
  then output_string oc "No lint errors!\n"
  else begin
    let sl = List.map el Lint.to_string in
    List.iter sl begin fun s ->
      Printf.fprintf oc "%s\n%!" s;
    end
  end;
  flush oc

let lint _acc fnl =
  List.fold_left fnl ~f:begin fun acc fn ->
    let errs, () =
      Lint.do_ begin fun () ->
        Errors.ignore_ begin fun () ->
          Linting_service.lint fn (Sys_utils.cat (Relative_path.to_absolute fn))
        end
      end in
    errs @ acc
  end ~init:[]

let lint_all genv code =
  let root = ServerArgs.root genv.options in
  let next = compose
    (List.map ~f:(RP.create RP.Root))
    (Find.make_next_files FindUtils.is_php root) in
  let errs = MultiWorker.call
    genv.workers
    ~job:(fun acc fnl ->
      let lint_errs = lint acc fnl in
      List.filter lint_errs (fun err -> Lint.get_code err = code))
    ~merge:List.rev_append
    ~neutral:[]
    ~next in
  List.map errs Lint.to_absolute

let go genv fnl =
  let fnl = List.map fnl (Relative_path.create Relative_path.Root) in
  let errs =
    if List.length fnl > 10
    then
      MultiWorker.call
        genv.workers
        ~job:lint
        ~merge:List.rev_append
        ~neutral:[]
        ~next:(Bucket.make fnl)
    else
      lint [] fnl in
  let errs = List.sort begin fun x y ->
    Pos.compare (Lint.get_pos x) (Lint.get_pos y)
  end errs in
  List.map errs Lint.to_absolute
