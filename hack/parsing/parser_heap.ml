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

(*****************************************************************************)
(* Table containing all the HH_FIXMEs found in the source code.
 * Associates: filename => line_number, error_node_number map
 *)
(*****************************************************************************)

module HH_FIXMES = SharedMem.WithCache (Relative_path.S) (struct
  type t = Pos.t IMap.t IMap.t
  let prefix = Prefix.make()
end)

(*****************************************************************************)
(* We register the function that can look up a position and determine if
 * a given position is affected by an HH_FIXME. We use a reference to avoid
 * a cyclic dependency: everything depends on the Errors module (the module
 * defining all the errors), because of that making the Errors module call
 * into anything that isn't in the standard library is very unwise, because
 * that code won't be able to add errors.
 *)
(*****************************************************************************)

let () =
  Errors.is_hh_fixme := begin fun pos err_code ->
    let filename = Pos.filename pos in
    let line, _, _ = Pos.info_pos pos in
    match HH_FIXMES.get filename with
    | None -> false
    | Some fixme_map ->
        match IMap.get line fixme_map with
        | None -> false
        | Some code_map ->
            IMap.mem err_code code_map
  end

(*****************************************************************************)
(* Table containing all the Abstract Syntax Trees (cf ast.ml) for each file.*)
(*****************************************************************************)

module ParserHeap = SharedMem.NoCache (Relative_path.S) (struct
    type t = Ast.program
    let prefix = Prefix.make()
  end)

let find_class_in_file file_name class_name =
  match ParserHeap.get file_name with
  | None -> None
  | Some defs ->
    List.fold_left defs ~init:None ~f:begin fun acc def ->
      match def with
      | Ast.Class c when snd c.Ast.c_name = class_name -> Some c
      | _ -> acc
    end

let find_fun_in_file file_name fun_name =
  match ParserHeap.get file_name with
  | None -> None
  | Some defs ->
    List.fold_left defs ~init:None ~f:begin fun acc def ->
      match def with
      | Ast.Fun f when snd f.Ast.f_name = fun_name -> Some f
      | _ -> acc
    end

let find_typedef_in_file file_name name =
  match ParserHeap.get file_name with
  | None -> None
  | Some defs ->
    List.fold_left defs ~init:None ~f:begin fun acc def ->
      match def with
      | Ast.Typedef typedef when snd typedef.Ast.t_id = name -> Some typedef
      | _ -> acc
    end

let find_const_in_file file_name name =
  match ParserHeap.get file_name with
  | None -> None
  | Some defs ->
    List.fold_left defs ~init:None ~f:begin fun acc def ->
      match def with
      | Ast.Constant cst when snd cst.Ast.cst_name = name -> Some cst
      | _ -> acc
    end
