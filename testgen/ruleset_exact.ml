(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module S = Ast.Statement;;
module E = Ast.Expression;;
module T = Ast.Type;;
module P = Ast.Pattern;;
module Utils = Flowtestgen_utils;;
module FRandom = Utils.FRandom;;

(* Show how to use exact types. *)
open Ruleset_base;;

(* ESSENTIAL: Syntax type and related functions *)
module Syntax = Syntax_base;;

class ruleset_exact = object(self)
  inherit ruleset_base as super

  method! get_name () : string = "exact"

  method! weak_assert b = self#backtrack_on_false b

  method! is_subtype_obj (o1 : Loc.t T.Object.t) (o2 : Loc.t T.Object.t) =
    let open T.Object in
    if (o1.exact && o2.exact) then
      o1 = o2
    else if (o1.exact || o2.exact) then
      false
    else
      super#is_subtype_obj o1 o2

  method! get_all_rules () =
    [|self#rule_num_lit;
      self#rule_obj_lit 1 0;
      self#rule_vardecl_with_type;
      self#rule_prop_read;
      self#rule_prop_update;|]
  end

  class ruleset_random_exact = object
    inherit ruleset_exact
    method! weak_assert b =
      if (not b) && ((FRandom.rint 20) > 0) then raise Engine.Fail
  end
