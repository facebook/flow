(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module ALocMap = Loc_collections.ALocMap

let polarity = function
  | Some (_, { Ast.Variance.kind = Ast.Variance.Plus; comments = _ }) -> Polarity.Positive
  | Some (_, { Ast.Variance.kind = Ast.Variance.Minus; comments = _ }) -> Polarity.Negative
  | Some (_, Ast.Variance.{ kind = InOut; comments = _ }) -> Polarity.Neutral
  | Some (_, Ast.Variance.{ kind = Readonly | Out; comments = _ }) -> Polarity.Positive
  | Some (_, Ast.Variance.{ kind = In; comments = _ }) -> Polarity.Negative
  | None -> Polarity.Neutral

class ['M, 'T, 'N, 'U] type_at_aloc_map_folder =
  object
    inherit ['M, 'M * 'T, 'N, 'N * 'U] Flow_polymorphic_ast_mapper.mapper

    val mutable map = ALocMap.empty

    method on_loc_annot x = x

    method on_type_annot x =
      let (loc, t) = x in
      map <- ALocMap.add loc t map;
      x

    method to_map = map
  end

class ['M, 'T, 'N, 'U] type_at_aloc_list_folder =
  object
    inherit ['M, 'M * 'T, 'N, 'N * 'U] Flow_polymorphic_ast_mapper.mapper

    val mutable l = []

    method on_loc_annot x = x

    method on_type_annot x =
      let (loc, t) = x in
      l <- (loc, t) :: l;
      x

    method to_list = l
  end

let typed_ast_to_map typed_ast : Type.t ALocMap.t =
  let folder = new type_at_aloc_map_folder in
  ignore (folder#program typed_ast);
  folder#to_map

let typed_ast_to_list typed_ast : (ALoc.t * Type.t) list =
  let folder = new type_at_aloc_list_folder in
  ignore (folder#program typed_ast);
  folder#to_list

(** Mappers
 *  Used to construct error nodes during type checking.
 *)

(* Error nodes are typed at `any`. Do not change this type as it might change
 * current behavior. *)
let error_mapper =
  object
    inherit [ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot loc = loc

    method on_type_annot loc = (loc, Type.AnyT.at (Type.AnyError None) loc)
  end

(* Used in skipped body due to placeholder function types. *)
let placeholder_mapper cx =
  object
    inherit [ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot loc = loc

    method on_type_annot loc = (loc, Context.mk_placeholder cx Reason.(mk_reason RAnyImplicit loc))
  end

(* Used in unimplemented cases or unsupported nodes *)
let unimplemented_mapper =
  object
    inherit [ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot loc = loc

    method on_type_annot loc = (loc, Type.(AnyT.at (Unsound Unimplemented)) loc)
  end

(* Code is not checked at all *)
let unchecked_mapper =
  object
    inherit [ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot loc = loc

    method on_type_annot loc = (loc, Type.(AnyT.at (Unsound Unchecked)) loc)
  end

let untyped_ast_mapper =
  object
    inherit [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot loc = loc

    method on_type_annot (loc, _) = loc
  end
