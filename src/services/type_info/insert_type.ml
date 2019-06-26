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
exception UnsupportedAnnotation of {location:Loc.t; error_message:string}


let is_point loc = Loc.(loc.start = loc._end)

(* This class maps each node that contains the target until a node is contained
   by the target *)
class mapper ~strict ~target ~normalize ~ty_lookup =
  let target_is_point = is_point target in
  object(this)
  inherit [Loc.t] Flow_ast_contains_mapper.mapper as super

  method private target_contains loc = Loc.contains target loc
  method private target_contained_by loc = Loc.contains loc target
  method private is_target loc = Loc.equal target loc
  method loc_annot_contains_target = this#target_contained_by

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

  method private synth_type_annotation_hint loc =
    Flow_ast.Type.Available (this#synth_type loc)

  (* If a type is missing and in the range of target then add a type annotation hint *)
  method private update_type_annotation_hint ?type_loc ?(check_loc=false) annot =
    let open Flow_ast.Type in
    match annot with
    | Missing location
      when (not check_loc) || this#target_contained_by location ->
      let type_loc = match type_loc with
                     | Some type_loc -> type_loc
                     | None -> location in
      this#synth_type_annotation_hint type_loc
    | Available (location, type_ast)
      when (not check_loc) || this#target_contained_by location ->
      raise @@ TypeAvailableAtPoint {location; type_ast;}
    | _ -> annot

  method! type_annotation_hint = this#update_type_annotation_hint ?type_loc:None ~check_loc:true

  method! class_extends location (extends: ('loc, 'loc) Flow_ast.Class.Extends.t') =
    match extends with
    | { Flow_ast.Class.Extends.targs = None; _} ->
      super#class_extends location extends
    | _ ->
      raise @@ UnsupportedAnnotation {location;
       error_message="Classes with type arguments"}

  method! function_param_pattern node =
    let open Flow_ast.Pattern in
    let open Flow_ast.Pattern.Identifier in
    match node with
    | loc, Identifier ({annot; _} as id)
      when this#is_target loc
        || (target_is_point && this#target_contained_by loc) ->
      if strict
      then raise @@ UnsupportedAnnotation {location=loc;
        error_message="Function parameter in strict mode."}
      else
        let annot = this#update_type_annotation_hint annot in
        loc, Identifier {id with annot}
    | _ -> super#function_param_pattern node

  method! class_element elem =
    let open Flow_ast.Class.Body in
    let open Flow_ast.Class.Property in
    let open Flow_ast.Expression.Object.Property in
    let update_property loc prop annot =
      let annot = this#update_type_annotation_hint annot in
      Property (loc, {prop with annot}) in
    match elem with
      | PrivateField (location, _) when this#is_target location ->
        raise @@ UnsupportedAnnotation {location;
          error_message="Private field"}
      | Property (loc, ({annot; _} as prop)) when this#is_target loc ->
        update_property loc prop annot
      | Property (loc, ({key=(Literal     (kloc, _)|
                              Identifier  (kloc, _)|
                              PrivateName (kloc, _));
                         annot; _} as prop))
        when this#is_target kloc
          || (target_is_point && this#target_contained_by kloc) ->
        if strict
        then raise @@ UnsupportedAnnotation {location=kloc;
          error_message="property key in strict mode"}
        else update_property loc prop annot
      | _ -> super#class_element elem

  method! variable_declarator ~kind decl =
    let open Flow_ast.Statement.VariableDeclaration in
    let open Flow_ast.Statement.VariableDeclaration.Declarator in
    let open Flow_ast.Pattern in
    let open Flow_ast.Pattern.Identifier in
    match kind, decl with
    (* In `const x = exp;` the error appears on exp *)
    | Const, (dloc, ({id = iloc, Identifier id; init = Some (type_loc, _);} as decl))
      (* Use is_target of initialization expression location
         because const signature verification errors point to expression *)
      when this#is_target type_loc ->
      let {annot; _} = id in
      let annot = this#update_type_annotation_hint ~type_loc annot in
      (dloc, {decl with id=(iloc, Identifier {id with annot})})
    | _ -> super#variable_declarator ~kind decl

  method! variable_declarator_pattern ~kind node =
    let open Flow_ast.Pattern in
    let open Flow_ast.Pattern.Identifier in
    let open Flow_ast.Statement.VariableDeclaration in
    let (loc, patt) = node in
    if not (this#target_contained_by loc) then node
    else match patt, kind with
    (* In `const x = exp;` for signature varification errors the error appears on the exp portion.
       When strict we only look for that error. *)
    | Identifier _,  Const when strict ->
      super#variable_declarator_pattern ~kind node
    | Identifier ({name; annot; _} as id), (Var | Let | Const)
      when target_is_point || this#is_target loc ->
      let (type_loc, _) = name in
      let annot = this#update_type_annotation_hint ~type_loc annot in
      (loc, Identifier {id with annot})
    | _ -> super#variable_declarator_pattern ~kind node

  method! expression ((l, _) as e) =
    let open Flow_ast.Expression in
    if this#target_contained_by l
    then if this#is_target l
      then (l, TypeCast TypeCast.{expression=e; annot=this#synth_type l})
      else super#expression e
    else e

  method! program p =
    let p' = super#program p in
    if p == p' then raise @@ UnknownAnnotation target;
    p'
end

let type_lookup_at_location typed_ast loc =
  match Typed_ast_utils.find_exact_match_annotation typed_ast (ALoc.of_loc loc) with
  | Some p -> p
  | None -> raise @@ UnknownTypeAtPoint loc
