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
open Utils
open Ast_ext


let map program =
  let errors = ref [] in

  let unsupported (p, m) =
    errors := (CE.ConversionError (p, m)) :: !errors;
    () in

  let detect_invalid_attributes p attributes =
    if (SMap.mem "__Memoize" attributes)
    then unsupported (p, "__Memoize is currently not supported.")
    else () in

  let detect_invalid_fun_kind p = function
    | FSync -> ()
    | _ -> unsupported (p, "async is currently not supported.") in

  let detect_invalid_fun {f_user_attributes; f_fun_kind; f_name = (p, _); _} =
    detect_invalid_attributes p f_user_attributes;
    detect_invalid_fun_kind p f_fun_kind in

  let detect_invalid_method {m_user_attributes; m_fun_kind; m_name = (p, _); _} =
    detect_invalid_attributes p m_user_attributes;
    detect_invalid_fun_kind p m_fun_kind in

  let detect_invalid_collection p name afields =
    match (base_collection_str name, afields) with
    | (Some "\\HH\\Pair", [AFvalue _; AFvalue _]) -> ()
    | (Some "\\HH\\Pair", _) ->
        unsupported (p, "Invalid initialization of Pair")
    | (Some "\\HH\\Map", _) | (Some "\\HH\\ImmMap", _) ->
        List.iter begin function
          | AFkvalue _ -> ()
          | AFvalue (p, _) ->
              unsupported (p, "Invalid Initialization of " ^ name)
          end afields
    | (None, _) ->
        unsupported (p, "Unsupported collection type " ^ name)
    | _ -> ()  in

  let detect_invalid_expr = function
    | (p, Await _) ->
        unsupported (p, "await is currently not supported.")
    | (_, Collection ((p, name), afields)) ->
        detect_invalid_collection p name afields;
        ()
    | _ -> () in

  let detect_invalid_class_var = function
    | ((p, _), Some (_, Collection _)) ->
        let m = "Collection initializers in instance variables are currently" ^
          " not supported." in
        unsupported (p, m)
    | _ -> () in

  let detect_invalid_class_elt = function
    | ClassVars (kinds, _, class_vars) when not (List.mem Static kinds) ->
        List.iter detect_invalid_class_var class_vars;
        ()
    | _ -> () in

  let detect_invalid_class = function
    | {c_name = (p, _); c_kind = Ctrait; c_implements = _::_; _} ->
      unsupported (p,
        "Traits implementing interfaces are currently not supported. ")
    | _ -> () in

  let mapper = M.mk_program_mapper { M.default_mapper with
    M.k_class_ = (fun (k, _) c ->
      detect_invalid_class c;
      k c);
    M.k_class_elt = (fun (k, _) _ elt ->
      detect_invalid_class_elt elt;
      k elt
      );
    M.k_fun_ = (fun (k, _) fun_ ->
      detect_invalid_fun fun_;
      k fun_);
    M.k_method_ = (fun (k, _) _ method_ ->
      detect_invalid_method method_;
      k method_);
    M.k_expr = (fun (k, _) e -> detect_invalid_expr e; k e);
  } in
  ignore (mapper program);
  if List.not_empty !errors
  then raise (CE.CompoundError !errors);
  program (* this mapper does no mapping *)
