(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
open Utils

(*****************************************************************************)
(* Error. *)
(*****************************************************************************)

let report_error exn =
  let exn_str = Printexc.to_string exn in
  Printf.printf "Could not auto-complete because of errors: %s\n" exn_str;
  flush stdout;
  ()

let oldify_funs names =
  Naming_heap.FunHeap.oldify_batch names;
  Typing_env.Funs.oldify_batch names;
  ()

let oldify_classes names =
  Naming_heap.ClassHeap.oldify_batch names;
  Typing_env.Classes.oldify_batch names;
  ()

let revive funs classes =
  Naming_heap.FunHeap.revive_batch funs;
  Typing_env.Funs.revive_batch funs;
  Naming_heap.ClassHeap.revive_batch classes;
  Typing_env.Classes.revive_batch classes;
  ()

let declare path content =
  Autocomplete.auto_complete := false;
  Autocomplete.auto_complete_for_global := "";
  let declared_funs = ref SSet.empty in
  let declared_classes = ref SSet.empty in
  try
    Errors.ignore_ begin fun () ->
      let {Parser_hack.file_mode; comments; ast} =
        Parser_hack.program path content
      in
      let funs, classes = List.fold_left begin fun (funs, classes) def ->
        match def with
          | Ast.Fun f -> SSet.add (snd f.Ast.f_name) funs, classes
          | Ast.Class c -> funs, SSet.add (snd c.Ast.c_name) classes
          | _ -> funs, classes
      end (SSet.empty, SSet.empty) ast in
      oldify_funs funs;
      oldify_classes classes;
      List.iter begin fun def ->
        match def with
        | Ast.Fun f ->
            let tcopt = TypecheckerOptions.permissive in
            let nenv = Naming.empty tcopt in
            let f = Naming.fun_ nenv f in
            let fname = (snd f.Nast.f_name) in
            Typing.fun_decl nenv f;
            declared_funs := SSet.add fname !declared_funs;
        | Ast.Class c ->
            let tcopt = TypecheckerOptions.permissive in
            let nenv = Naming.empty tcopt in
            let c = Naming.class_ nenv c in
            let cname = snd c.Nast.c_name in
            declared_classes := SSet.add cname !declared_classes;
            Typing_decl.class_decl tcopt c;
            ()
        | _ -> ()
      end ast;
      !declared_funs, !declared_classes
    end
  with e ->
    report_error e;
    SSet.empty, SSet.empty

let fix_file_and_def path content = try
  Errors.ignore_ begin fun () ->
    let {Parser_hack.file_mode; comments; ast} =
      Parser_hack.program path content in
    List.iter begin fun def ->
      match def with
      | Ast.Fun f ->
          let tcopt = TypecheckerOptions.permissive in
          let nenv = Naming.empty tcopt in
          let f = Naming.fun_ nenv f in
          let filename = Pos.filename (fst f.Nast.f_name) in
          let tenv = Typing_env.empty tcopt filename in
          Typing.fun_def tenv nenv (snd f.Nast.f_name) f
      | Ast.Class c ->
          let tcopt = TypecheckerOptions.permissive in
          let nenv = Naming.empty tcopt in
          let c = Naming.class_ nenv c in
          let filename = Pos.filename (fst c.Nast.c_name) in
          let tenv = Typing_env.empty tcopt filename in
          let res = Typing.class_def tenv nenv (snd c.Nast.c_name) c in
          res
      | _ -> ()
    end ast;
  end
with e ->
  report_error e;
  ()

let recheck nenv fileinfo_l =
  SharedMem.invalidate_caches();
  List.iter begin fun defs ->
    ignore @@ Typing_check_utils.check_defs nenv defs
  end fileinfo_l

let check_file_input tcopt files_info fi =
  match fi with
  | ServerUtils.FileContent content ->
      let path = Relative_path.default in
      let funs, classes = declare path content in
      fix_file_and_def path content;
      revive funs classes;
      path
  | ServerUtils.FileName fn ->
      let path = Relative_path.create Relative_path.Root fn in
      let () = match Relative_path.Map.get path files_info with
      | Some fileinfo -> recheck tcopt [fileinfo]
      | None -> () in
      path
