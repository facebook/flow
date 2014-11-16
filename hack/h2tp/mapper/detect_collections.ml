(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module M = Map_ast
module CE = Common_exns
module List = List_ext
open Ast
open Ast_ext

let map program =
  let errors = ref [] in

  let error p =
    let m = "collection found in no-collections mode." in
    errors := (CE.ConversionError (p, m)) :: !errors;
    () in

  let in_expr = function
    | (p, Collection _) -> error p
    | _ -> () in

  let rec detect_collection_hint = function
    | (_, Hoption h) -> detect_collection_hint h
    | (_, Hfun (hs, _, h)) -> List.iter detect_collection_hint (h::hs)
    | (_, Htuple hs) -> List.iter detect_collection_hint hs
    | (p, Happly ((_, name), hs)) ->
        if is_collection_str name
        then error p;
        List.iter detect_collection_hint hs
    | (_, Hshape sfields) ->
        List.iter (fun (_, h) -> detect_collection_hint h) sfields in

  let in_hintOpt = function
    | None -> ()
    | Some h -> detect_collection_hint h in

  let in_class_elt = function
    | ClassUse h | ClassTraitRequire (_, h) -> detect_collection_hint h
    | _ -> () in

  let in_class = function
    | {c_extends = e_hints; c_implements = i_hints; _} ->
        List.iter detect_collection_hint (e_hints @ i_hints) in

  let in_def = function
    | Typedef {t_kind = Alias h; _ }
    | Typedef {t_kind = NewType h; _ } -> detect_collection_hint h
    | _ -> () in

  let mapper = M.mk_program_mapper { M.default_mapper with
    M.k_class_ = (fun (k, _) c -> in_class c; k c);
    M.k_class_elt = (fun (k, _) _ elt -> in_class_elt elt; k elt);
    M.k_expr = (fun (k, _) e -> in_expr e; k e);
    M.k_hintOption = (fun (k, _) h -> in_hintOpt h; k h);
    M.k_def = (fun (k, _) def -> in_def def; k def);
  } in
  ignore (mapper program);
  if List.not_empty !errors
  then raise (CE.CompoundError !errors);
  program (* this mapper does no mapping *)
