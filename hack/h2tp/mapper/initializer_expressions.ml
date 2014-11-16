(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*
  This converts initializer expressions that are allowed in hack but not in
  php. Currently the only form that these take is as collections. Currently
  these can appear in 3 places 1) default arguments 2) initialized static
  variables 3) initialized instance variables.

  1. default arguments are initialized to a special constant that indicates
    missing arguments and then replaced inside the body of the function.
    Example:
      old: function foo($v = Vector{});
      new: function foo($v = \HACKLIB_UNINITIALIZED) {
        if ($v === \HACKLIB_UNINITIALIZED) {
          $v = new Vector();
        }
      }
  2. static properties are bundled into a special method that is called
    after the class definition to instantiate them. For more detailed examples
    look at the tests.
    Example:
      old: class Foo {
              private static $foo = Vector {1, 2};
            }
      new: class Foo {
              private static $foo;
              public static hacklib_initialize_statics() {
                self::$foo = Vector {1, 2};
              }
           }
           Foo::hacklib_initialize_statics();
  3. instance properties with initializers are currently not supported.
*)


module M = Map_ast
module CE = Common_exns
module List = List_ext
open Ast
open Ast_ext
open Utils

let static_init = "hacklib_initialize_statics"
let extract_class s = Str.split (Str.regexp_string "\\") s |> List.lst_unsafe

let split_param_and_initializer param = match param with
  | {param_expr = Some ((p, Collection _) as e) ; param_id; _} ->
        let new_exp = (p, Id (p, "\\HACKLIB_UNINIT")) in
        ({param with param_expr = Some new_exp;}, Some (param_id, e))
  | _ -> (param, None)

let initialize ((p, name), e) =
  let lvar_exp = (p, Lvar (p, name)) in
  If (
    (p, Binop (EQeqeq, lvar_exp, (p, Id (p, "\\HACKLIB_UNINIT")))),
    [Expr (assign p name e)],
    [Noop]
  )

let process_default_params params =
  let (params, inits) =
    List.map split_param_and_initializer params |> List.split in
  let inits =
    List.fold_right (fun v b -> (initialize v)::b) (filter_some inits) [] in
  (params, inits)

let convert_function f =
  match f with
  | {
      f_body;
      f_params;
      _ } -> let (f_params, inits) = process_default_params f_params in
             {f with f_body = (inits @ f_body); f_params;}

let convert_method c_kind m =
  match m with
   | {
      m_kind;
      m_params;
      m_body;
      _ } ->
            let (m_params, inits) = process_default_params m_params in
            let m_body = if List.mem Abstract m_kind || c_kind = Cinterface
                         then m_body
                         else (inits @ m_body) in
            {m with m_body; m_params; }


let extract_var_initializer var = match var with
  | (id, Some ((_, Collection _) as e)) -> ((id, None), Some (id, e))
  | _ -> (var, None)

let extract_class_initializer = function
  | ClassVars (kinds, hOpt, class_vars) when List.mem Static kinds ->
      let (class_vars, inits) =
        List.map extract_var_initializer class_vars |> List.split in
      let inits = filter_some inits in
      (ClassVars (kinds, hOpt, class_vars), inits)
  | elt -> (elt, [])

let chain_initializer (p, name) =
  Expr (call_self_func p (static_init ^ "_" ^ name) [])

let init_static ((p, name), e) =
  Expr (assign_self p name e)

let get_uses c_body =
  map_filter begin function
    | ClassUse (p, Happly ((_, name), _)) -> Some (p, extract_class name)
    | _ -> None
  end c_body

let is_ctor = function
  | Method {m_name = (_, "__construct"); _} -> true
  | _ -> false

let call_used_traits elts =
  let uses = get_uses elts in
  List.map chain_initializer uses

let create_static_initialize {c_kind; c_name = (p, cname); c_body; _} vars =
  let uses = get_uses c_body in
  if List.is_empty vars && c_kind <> Ctrait && List.is_empty uses
  then None
  else
    let (m_name, m_kind) = match c_kind with
    | Ctrait -> ((p, static_init ^ "_" ^ cname), [Protected; Static; Final])
    | _ -> ((p, static_init), [Public; Static;]) in
    let call_traits = call_used_traits c_body in
    let init_vars = List.map init_static vars in
    let m_body = call_traits @ init_vars in
    let mth = Method {default_method with m_kind; m_name; m_body;} in
    Some mth

let process_class_initializers ({c_body; c_kind; _} as class_) =
  let (elts, inits) =
    List.map extract_class_initializer c_body |> List.split in
  let inits = List.concat inits in
  let s_elts =
    create_static_initialize class_ inits in
  let elts = elts @ (filter_some [s_elts]) in
  (elts, Option.is_some s_elts && c_kind <> Ctrait)

let convert_class_ ({c_name = (p, cname); c_kind; _} as class_) =
  match c_kind with
  | Cabstract | Cnormal | Ctrait ->
    let (c_body, needsInitialize) = process_class_initializers class_ in
    let klass = Class {class_ with c_body;} in
    if not needsInitialize
    then [klass]
    else begin
      let initialize = Stmt (Expr (call_static_func p cname static_init [])) in
      [klass; initialize]
      end
  | Cenum | Cinterface -> [Class class_]

let convert_def = function
  | Class class_ ->
      convert_class_ class_
  | def -> [def]

let map =
  M.mk_program_mapper { M.default_mapper with
    M.k_fun_ = (fun (k, _) fun_ -> k (convert_function fun_));
    M.k_method_ =
      (fun (k, _) c_kind method_ -> k (convert_method c_kind method_));
    M.k_program =
      (fun (k, _) program -> k (List.concatMap convert_def program));
  }
