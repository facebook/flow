(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This converts enums into final classes with const definitions
    Example:
      old: enum Foo: int {a = 0; b = 1;}
      new: final class Foo {const a = 0; const b = 1;}
*)

module M = Map_ast
module CE = Common_exns
open Ast
open Ast_ext
open Utils

let create_class_vars p elts =
  map_filter (function | Const (_, csts) -> Some csts | _ -> None) elts |>
  List.concat |>
  List.map (fun ((p, name), e) -> AFkvalue ((p, String (p, name)), e)) |>
  fun afields -> ClassVars (
    [Private; Static;],
    None,
    ([
      (p, "hacklib_values"),
      Some (p, Array afields)]))

let modify_class c = match c with
  | {c_kind = Cenum; c_body; c_name = (p, _); _} ->
    let construct = Method { default_method with
      m_kind = [Private];
      m_name = (p, "__construct");
    }
    and data = create_class_vars p c_body
    and usestmt = ClassUse (p, Happly ((p, "\\HH\\HACKLIB_ENUM_LIKE"), [])) in
    { c with
        c_kind = Cnormal;
        c_enum = None;
        c_final = true;
        c_body = construct::data::usestmt::c_body;
      }
  | _ -> c

let map =
  M.mk_program_mapper { M.default_mapper with
    M.k_class_ = (fun (k, _) c -> k (modify_class c));
  }
