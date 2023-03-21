(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

class ['M, 'T] infer_type_hoister =
  object (this)
    inherit ['M, 'T, 'M, 'T] Flow_polymorphic_ast_mapper.mapper as super

    val mutable infer_types_rev : ('T * ('M, 'T) Ast.Type.Infer.t) list = []

    method on_loc_annot l = l

    method on_type_annot l = l

    method collected_infer_types = List.rev infer_types_rev

    method! type_ t =
      match t with
      | (loc, Ast.Type.Infer infer_t) ->
        infer_types_rev <- List.cons (loc, infer_t) infer_types_rev;
        super#type_ t
      | ( _,
          Ast.Type.Conditional
            {
              Ast.Type.Conditional.check_type;
              extends_type = _;
              true_type;
              false_type;
              comments = _;
            }
        ) ->
        ignore @@ this#type_ check_type;
        (* Visiting of extends_type is intentionally skipped,
           since it should be internal to the nested conditional type. *)
        ignore @@ this#type_ true_type;
        ignore @@ this#type_ false_type;
        t
      | _ -> super#type_ t
  end

let hoist_infer_types t =
  let hoister = new infer_type_hoister in
  ignore @@ hoister#type_ t;
  hoister#collected_infer_types
