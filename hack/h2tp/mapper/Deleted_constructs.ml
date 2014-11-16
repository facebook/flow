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
  This mapper deletes constructs that are unsupported in php and can be safely
  deleted to be valid php.

  1. Function and Method attributes. In the future we could choose
  to support Memoize and a few other constructs. Currently these are just
  deleted.
  Example:
    old: <<__Memoize>>
          function foo($k)
    new: function foo($k)

  2. Class attributes.
  Example:
    old: <<__ConsistentConstruct>
        class Foo {}
    new: class Foo {}

  3. Function Param Attributes
  Example:
    old: function foo(<<Foo>>$k) {}
    new: function foo($k) {}

  4. Trait and interface requirements
  Example:
    old: trait t {
          require extends C;
          require implements I;
        }
    new: trait t {}
*)

module M = Map_ast
open Ast
open Utils

let delete_method_attributes m =
  {m with m_user_attributes = SMap.empty}

let delete_function_attributes f =
  {f with f_user_attributes = SMap.empty}

let delete_param_attributes p =
  {p with param_user_attributes = SMap.empty}

let delete_class_attributes c =
  {c with c_user_attributes = SMap.empty}

let delete_requirement = function
  | ClassTraitRequire _  -> None
  | elt -> Some elt

let map =
  M.mk_program_mapper { M.default_mapper with
    M.k_method_ = (fun (k, _) _ m_ -> k (delete_method_attributes m_));
    M.k_fun_ = (fun (k, _) f_ -> k (delete_function_attributes f_));
    M.k_fun_param = (fun (k, _) p  -> k (delete_param_attributes p));
    M.k_class_ = (fun (k, _) c -> k (delete_class_attributes c));
    M.k_c_body = (fun (k, _) _ body ->
        k (map_filter delete_requirement body));
  }
