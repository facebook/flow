(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This erases all typehints from a hack ast.
    Example:
      old: function f<To>(@array<int, To> $foo, T $whatever): void {}
      new: function f($foo, $whatever) {}
   and typedefs
    Example:
      old: type MyInt = int;
      new: <nothing>
*)

module M = Map_ast
module CE = Common_exns
open Ast

(* This is meant to simplify hints when they appear inside extends,
implements or use statements *)
let simplify_hint (p, hint) = match hint with
  | Hoption _ | Hfun _ | Htuple _ | Hshape _ -> raise CE.Impossible
  | Happly (id, _) -> (p, Happly (id, []))


let modify_class ({c_extends; c_implements; _} as class_) =
  let c_extends = List.map simplify_hint c_extends in
  let c_implements = List.map simplify_hint c_implements in
  {class_ with c_tparams = []; c_extends; c_implements;}

let modify_use = function
  | ClassUse hint -> ClassUse (simplify_hint hint)
  | e -> e

let strip_typedefs = function
  | Typedef _ -> Stmt Noop
  | d -> d

let map =
  M.mk_program_mapper { M.default_mapper with
    M.k_hintOption = (fun (_, _) _ -> None);
    M.k_class_ = (fun (k, _) c -> k (modify_class c));
    M.k_class_elt = (fun (k, _) _ e -> k (modify_use e));
    M.k_fun_ = (fun (k, _) c -> k {c with f_tparams = []});
    M.k_method_ = (fun (k, _) _ c -> k {c with m_tparams = []});
    M.k_def = (fun (k, _) def -> k (strip_typedefs def));
  }
