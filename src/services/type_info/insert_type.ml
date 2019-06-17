(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception UnknownTypeAtPoint of Loc.t
exception FailedToSerializeType of {location:Loc.t; ty:Ty.t; error_message:string}
exception FailedToNormalizeType of {location:Loc.t; ty:Type.t; error_message:string}
exception TypeAvailableAtPoint of {location:Loc.t; type_ast: (Loc.t, Loc.t) Flow_ast.Type.t}
exception UnknownAnnotation of Loc.t

(* This class maps each node that contains the target until a node is contained
   by the target *)
class mapper target normalize ty_lookup = object(this)
  inherit [Loc.t] Flow_ast_contains_mapper.mapper as super

  method private target_contains loc = Reason.in_range loc target
  method private target_contained_by = Reason.in_range target

  method private synth_type location =
    let (location, scheme) = ty_lookup location in
    match normalize location scheme with
    | Query_types.Success (_, ty) ->
      begin match Ty_serializer.type_ ty with
      | Ok type_ast -> (location, type_ast)
      | Error error_message ->
        raise @@ FailedToSerializeType {location; ty; error_message}
      end
    | Query_types.FailureUnparseable (location, ty, error_message) ->
      raise @@ FailedToNormalizeType {location; ty; error_message}
    | Query_types.FailureNoMatch -> failwith "TODO handle this error"

  method synth_type_annotation_hint loc =
    Flow_ast.Type.Available (this#synth_type loc)

  method! type_annotation_hint annot =
    let open Flow_ast.Type in
    match annot with
    | Missing location when this#target_contained_by location ->
      this#synth_type_annotation_hint location
    | Available (location, type_ast) when this#target_contained_by location ->
      raise @@ TypeAvailableAtPoint {location; type_ast;}
    | _ -> annot

  method! program p =
    let p' = super#program p in
    if p == p' then raise @@ UnknownAnnotation target;
    p'

  method! expression ((l, _) as e) =
    let open Flow_ast.Expression in
    if this#target_contained_by l
    then if this#target_contains l
      then (l, TypeCast TypeCast.{expression=e; annot=this#synth_type l})
      else super#expression e
    else e

  method loc_annot_contains_target = this#target_contained_by
end

let type_lookup_at_location typed_ast loc =
  match Typed_ast_utils.find_exact_match_annotation typed_ast (ALoc.of_loc loc) with
  | Some p -> p
  | None -> raise @@ UnknownTypeAtPoint loc
