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
  This desugars lambda expressions in a hack ast and converts them to
    php style anonymous functions.
  Example:
    old: $b = 10; $x = $a ==> $a + $b
    new: $b = 10; $x = function ($a) use ($b) {
                        return $a + $b;
                       }
*)

module M = Map_ast
open Ast

let change_lambdas (k, _) = function
  | Lfun ({f_name = (p, _); _} as f)  ->
    let ss = Naming.uselist_lambda f in
    let uses = List.map (fun s -> ((p, s), false)) ss in
    k (Efun (f, uses))
  | e -> k e

let map =
  M.mk_program_mapper { M.default_mapper with
    M.k_expr_ = change_lambdas;
  }
