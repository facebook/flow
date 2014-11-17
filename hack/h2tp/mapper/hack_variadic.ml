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
  Hack allows variadic functions to use a trailing "..." to indicate that
  it calls func_get_args, func_get_arg or func_num_args.
  We just delete these trailing dots.
  Example:
    old: function foo($a, ...) {
    }
    new: function foo($a) {
    }
*)

module M = Map_ast
open Ast
open Utils

let strip_hack_variadic = function
  | {param_is_variadic = true; param_id = (_, "..."); _} -> None
  | p -> Some p

let map =
  M.mk_program_mapper { M.default_mapper with
    M.k_fun_params = (fun (k, _) params ->
      k (map_filter strip_hack_variadic params));
  }
