(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let summarize_class class_ acc =
  let class_name = Utils.strip_ns (snd class_.Ast.c_name) in
  let res_list =
    (Pos.to_absolute (fst class_.Ast.c_name), class_name, "class") :: acc in
  List.fold_left begin fun acc method_ ->
    match method_ with
      | Ast.Method m ->
          let desc =
            if List.mem (Ast.Static) m.Ast.m_kind then "static method"
            else "method"
          in
          (Pos.to_absolute (fst m.Ast.m_name),
           class_name ^ "::" ^ snd m.Ast.m_name, desc) :: acc
      | _ -> acc
  end res_list class_.Ast.c_body

let summarize_fun f acc =
  (Pos.to_absolute (fst f.Ast.f_name),
   Utils.strip_ns (snd f.Ast.f_name), "function") :: acc

let outline_ast ast =
  List.fold_left begin fun acc def ->
    match def with
    | Ast.Fun f -> summarize_fun f acc
    | Ast.Class c -> summarize_class c acc
    | _ -> acc
  end [] ast

let outline content =
  let {Parser_hack.ast; _} = Errors.ignore_ begin fun () ->
    Parser_hack.program Relative_path.default content
  end in
  outline_ast ast
