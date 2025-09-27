(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

let prop_name_contains_target_loc ~target_loc =
  let open Ast.Type.Object in
  function
  | Property
      ( _,
        {
          Property.key =
            ( Ast.Expression.Object.Property.StringLiteral (loc, _)
            | Ast.Expression.Object.Property.NumberLiteral (loc, _)
            | Ast.Expression.Object.Property.BigIntLiteral (loc, _)
            | Ast.Expression.Object.Property.Identifier (loc, _)
            | Ast.Expression.Object.Property.PrivateName (loc, _)
            | Ast.Expression.Object.Property.Computed (loc, _) );
          _;
        }
      ) ->
    Loc.contains loc target_loc
  | SpreadProperty _ -> false
  | Indexer _ -> false
  | CallProperty _ -> false
  | InternalSlot _ -> false
  | MappedType _ -> false

type conversion_kind =
  | ConversionToReadOnlyArray
  | ConversionToReadOnlyObject
  | ConversionToReadOnlyMap
  | ConversionToReadOnlySet

class mapper ~ts_readonly_name target_loc =
  let readonly_array_name =
    if ts_readonly_name then
      "ReadonlyArray"
    else
      "$ReadOnlyArray"
  in
  let readonly_object_name =
    if ts_readonly_name then
      "Readonly"
    else
      "$ReadOnly"
  in
  let readonly_map_name =
    if ts_readonly_name then
      "ReadonlyMap"
    else
      "$ReadOnlyMap"
  in
  let readonly_set_name =
    if ts_readonly_name then
      "ReadonlySet"
    else
      "$ReadOnlySet"
  in
  object
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    val mutable conversion_kind = None

    method get_conversion_kind () = conversion_kind

    method! generic_type loc type_ =
      let open Ast.Type.Generic in
      let { id; targs; comments } = type_ in
      match (id, targs) with
      (* Skip already readonly objects. *)
      | ( ( Identifier.Unqualified (_, { Ast.Identifier.name = "$ReadOnly"; comments = _ })
          | Identifier.Unqualified (_, { Ast.Identifier.name = "Readonly"; comments = _ }) ),
          Some
            ( targs_loc,
              ( { Ast.Type.TypeArgs.arguments = [((_, Ast.Type.Object _) as targ)]; comments = _ }
              as targs
              )
            )
        ) ->
        (* We cannot just do super#generic_type, since we don't want to suggest converting to readonly
         * at foo for `$ReadOnly<{foo: string}>` *)
        let targ' = super#type_ targ in
        if targ == targ' then
          type_
        else
          let targs = Some (targs_loc, { targs with Ast.Type.TypeArgs.arguments = [targ'] }) in
          { id; targs; comments }
      (* Array<T> ~> ReadonlyArray<T> *)
      | ( Identifier.Unqualified (id_loc, { Ast.Identifier.name = "Array"; comments = id_comments }),
          Some (_, { Ast.Type.TypeArgs.arguments = [_]; comments = _ })
        )
        when Loc.contains id_loc target_loc || Loc.equal loc target_loc ->
        conversion_kind <- Some ConversionToReadOnlyArray;
        {
          id =
            Identifier.Unqualified
              (loc, { Ast.Identifier.name = readonly_array_name; comments = id_comments });
          targs;
          comments;
        }
      (* Map<T> ~> ReadonlyMap<T> *)
      | ( Identifier.Unqualified (id_loc, { Ast.Identifier.name = "Map"; comments = id_comments }),
          Some (_, { Ast.Type.TypeArgs.arguments = [_; _]; comments = _ })
        )
        when Loc.contains id_loc target_loc || Loc.equal loc target_loc ->
        conversion_kind <- Some ConversionToReadOnlyMap;
        {
          id =
            Identifier.Unqualified
              (loc, { Ast.Identifier.name = readonly_map_name; comments = id_comments });
          targs;
          comments;
        }
      (* Map<T> ~> ReadonlySet<T> *)
      | ( Identifier.Unqualified (id_loc, { Ast.Identifier.name = "Set"; comments = id_comments }),
          Some (_, { Ast.Type.TypeArgs.arguments = [_]; comments = _ })
        )
        when Loc.contains id_loc target_loc || Loc.equal loc target_loc ->
        conversion_kind <- Some ConversionToReadOnlySet;
        {
          id =
            Identifier.Unqualified
              (loc, { Ast.Identifier.name = readonly_set_name; comments = id_comments });
          targs;
          comments;
        }
      | _ -> super#generic_type loc type_

    method! type_ (loc, type_) =
      let open Ast.Type in
      match type_ with
      | Array { Array.argument; comments } when Loc.equal loc target_loc ->
        conversion_kind <- Some ConversionToReadOnlyArray;
        ( loc,
          Generic
            {
              Generic.id =
                Generic.Identifier.Unqualified
                  (loc, { Ast.Identifier.name = readonly_array_name; comments = None });
              targs = Some (Loc.none, Ast.Type.TypeArgs.{ arguments = [argument]; comments = None });
              comments;
            }
        )
      | Object ({ Object.properties; _ } as obj)
        when Base.List.exists ~f:(prop_name_contains_target_loc ~target_loc) properties
             || Loc.equal loc target_loc ->
        conversion_kind <- Some ConversionToReadOnlyObject;
        ( loc,
          Generic
            {
              Generic.id =
                Generic.Identifier.Unqualified
                  (loc, { Ast.Identifier.name = readonly_object_name; comments = None });
              targs =
                Some
                  (Loc.none, Ast.Type.TypeArgs.{ arguments = [(loc, Object obj)]; comments = None });
              comments = None;
            }
        )
      | _ -> super#type_ (loc, type_)
  end

let convert ~ts_readonly_name ast loc =
  let mapper = new mapper ~ts_readonly_name loc in
  let ast' = mapper#program ast in
  match mapper#get_conversion_kind () with
  | None -> None
  | Some conversion_kind ->
    if ast == ast' then
      None
    else
      Some (ast', conversion_kind)
