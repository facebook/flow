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
open Reordered_argument_collections

(*****************************************************************************)
(* Error. *)
(*****************************************************************************)

let canon_set names =
  names
  |> SSet.elements
  |> List.map ~f:NamingGlobal.canon_key
  |> List.fold_left ~f:SSet.add ~init:SSet.empty

let report_error exn =
  let exn_str = Printexc.to_string exn in
  Printf.printf "Could not auto-complete because of errors: %s\n" exn_str;
  flush stdout;
  ()

let oldify_funs names =
  Naming_heap.FunPosHeap.oldify_batch names;
  Naming_heap.FunCanonHeap.oldify_batch @@ canon_set names;
  Typing_heap.Funs.oldify_batch names;
  ()

let oldify_classes names =
  Naming_heap.TypeIdHeap.oldify_batch names;
  Naming_heap.TypeCanonHeap.oldify_batch @@ canon_set names;
  Typing_heap.Classes.oldify_batch names;
  ()

let oldify_typedefs names =
  Naming_heap.TypeIdHeap.oldify_batch names;
  Naming_heap.TypeCanonHeap.oldify_batch @@ canon_set names;
  Typing_heap.Typedefs.oldify_batch names

let revive funs classes typedefs =
  Typing_heap.Funs.revive_batch funs;
  Naming_heap.FunPosHeap.revive_batch funs;
  Naming_heap.FunCanonHeap.revive_batch @@ canon_set funs;

  Typing_heap.Classes.revive_batch classes;
  Naming_heap.TypeIdHeap.revive_batch classes;
  Naming_heap.TypeCanonHeap.revive_batch @@ canon_set classes;

  Naming_heap.TypeIdHeap.revive_batch typedefs;
  Naming_heap.TypeCanonHeap.revive_batch @@ canon_set typedefs;
  Typing_heap.Typedefs.revive_batch typedefs

(* This will parse, declare and check all functions and classes in content
 * buffer.
 *
 * Declaring will overwrite definitions on shared heap, so before doing this,
 * the function will also "oldify" them (see functions above and
 * SharedMem.S.oldify_batch) - after working with local content is done,
 * original definitions can (and should) be restored using "revive".
 *)
let declare_and_check path content =
  let tcopt = TypecheckerOptions.permissive in
  Autocomplete.auto_complete := false;
  Autocomplete.auto_complete_for_global := "";
  let declared_funs = ref SSet.empty in
  let declared_classes = ref SSet.empty in
  let declared_typedefs = ref SSet.empty in
  try
    Errors.ignore_ begin fun () ->
      let {Parser_hack.file_mode; comments; ast} =
        Parser_hack.program path content
      in
      let funs, classes, typedefs =
        List.fold_left ast ~f:begin fun (funs, classes, typedefs) def ->
        match def with
          | Ast.Fun { Ast.f_name; _ } ->
            declared_funs := SSet.add !declared_funs (snd f_name);
            f_name::funs, classes, typedefs
          | Ast.Class { Ast.c_name; _ } ->
            declared_classes := SSet.add !declared_classes (snd c_name);
            funs, c_name::classes, typedefs
          | Ast.Typedef { Ast.t_id; _ } ->
            declared_typedefs := SSet.add !declared_typedefs (snd t_id);
            funs, classes, t_id::typedefs
          | _ -> funs, classes, typedefs
      end ~init:([], [], []) in

      oldify_funs !declared_funs;
      oldify_classes !declared_classes;
      oldify_typedefs !declared_typedefs;

      NamingGlobal.make_env ~funs ~classes ~typedefs ~consts:[];
      let nast = Naming.program tcopt ast in
      List.iter nast begin function
        | Nast.Fun f -> Decl.fun_decl f
        | Nast.Class c -> Decl.class_decl tcopt c
        | Nast.Typedef t -> Decl.typedef_decl t
        (* XXX should probably declare consts here too *)
        | _ -> ()
      end;
      (* We must run all the declaration steps first to ensure that the
       * typechecking below sees all the new declarations. Lazy decl
       * won't work in this case because we haven't put the new ASTs into
       * the parsing heap. *)
      List.iter nast begin function
        | Nast.Fun f -> Typing.fun_def tcopt f;
        | Nast.Class c -> Typing.class_def tcopt c;
        | Nast.Typedef t -> Typing.typedef_def t
        | _ -> ()
      end;
      !declared_funs, !declared_classes, !declared_typedefs
    end
  with e ->
    report_error e;
    SSet.empty, SSet.empty, SSet.empty

let recheck tcopt filetuple_l =
  SharedMem.invalidate_caches();
  List.iter filetuple_l begin fun (fn, defs) ->
    ignore @@ Typing_check_utils.check_defs tcopt fn defs
  end

let check_file_input tcopt files_info fi =
  match fi with
  | ServerUtils.FileContent content ->
      let path = Relative_path.default in
      let funs, classes, typedefs = declare_and_check path content in
      revive funs classes typedefs;
      path
  | ServerUtils.FileName fn ->
      let path = Relative_path.create Relative_path.Root fn in
      let () = match Relative_path.Map.get files_info path with
        | Some fileinfo -> recheck tcopt [(path, fileinfo)]
        | None -> () in
      path
