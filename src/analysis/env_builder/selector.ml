(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type t =
  | Elem of {
      index: int;
      has_default: bool;
    }
  | Prop of {
      prop: string;
      prop_loc: ALoc.t;
      has_default: bool;
    }
  | Computed of {
      expression: (ALoc.t, ALoc.t) Ast.Expression.t;
      has_default: bool;
    }
  | ObjRest of {
      used_props: string list;
      after_computed: bool;
    }
  | ArrRest of int
  | Default

let to_string = function
  | Elem { index; _ } -> Utils_js.spf "[%d]" index
  | Prop { prop; _ } -> Utils_js.spf ".%s" prop
  | Computed _ -> ".[computed]"
  | ObjRest _ -> "{ ... }"
  | ArrRest _ -> "[...]"
  | Default -> "<with default>"
