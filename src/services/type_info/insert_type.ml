(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Utils = Insert_type_utils

type unexpected =
  | UnknownTypeAtPoint of Loc.t
  | FailedToSerialize of  {ty:Ty.t; error_message:string}
  | FailedToNormalizeNoMatch

type expected =
  | TypeAnnotationAtPoint of {location:Loc.t; type_ast: (Loc.t, Loc.t) Flow_ast.Type.t}
  | InvalidAnnotationTarget of Loc.t
  | UnsupportedAnnotation of {location:Loc.t; error_message:string}
  | TypeSizeLimitExceeded of {size_limit:int; size:int option;}
  | MulipleTypesPossibleAtPoint of {generalized:(Loc.t, Loc.t) Flow_ast.Type.t;
                                    specialized:(Loc.t, Loc.t) Flow_ast.Type.t}
  | FailedToValidateType of {error:Utils.validation_error; error_message:string}
  | FailedToTypeCheck of Errors.ConcreteLocPrintableErrorSet.t
  | FailedToNormalize of (Loc.t * string)

type errors =
  | Unexpected of unexpected
  | Expected of expected

exception FailedToInsertType of errors

let expected err = (FailedToInsertType (Expected err))

let unexpected err = (FailedToInsertType (Unexpected err))

let fail_when_ty_size_exceeds size_limit ty =
  match Ty_utils.size_of_type ~max:size_limit ty with
  | None ->
    raise @@ expected @@ TypeSizeLimitExceeded {size_limit; size=Ty_utils.size_of_type ty}
  | _ -> ()


(* This class maps each node that contains the target until a node is contained
   by the target *)
class mapper ~strict ?(size_limit=30) ~normalize ~ty_lookup target =
  let target_is_point = Utils.is_point target in
  object(this)
  inherit [Loc.t] Flow_ast_contains_mapper.mapper as super

  method private target_contains loc = Loc.contains target loc
  method private target_contained_by loc = Loc.contains loc target
  method private is_target loc = Loc.equal target loc
  method loc_annot_contains_target = this#target_contained_by

  method private synth_type location =
    let (location, scheme) = ty_lookup location in
    let ty = normalize location scheme in
    fail_when_ty_size_exceeds size_limit ty;
    let ty = Ty_utils.simplify_type ~simplify_empty:false ty in
    begin try
      Utils.validate_type ty
      with
      | Utils.Fatal error -> raise @@ expected @@
          FailedToValidateType{error; error_message=Utils.serialize_validation_error error}
    end;
    match Utils.serialize ~imports_react:true ty with
    | Ok type_ast -> (location, type_ast)
    | Error msg -> raise (unexpected (FailedToSerialize {ty; error_message=msg}))

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
      raise @@ expected
        @@ TypeAnnotationAtPoint {location; type_ast;}
    | _ -> annot

  method! type_annotation_hint = this#update_type_annotation_hint ?type_loc:None ~check_loc:true

  method! class_extends location (extends: ('loc, 'loc) Flow_ast.Class.Extends.t') =
    match extends with
    | { Flow_ast.Class.Extends.targs = None; _} ->
      super#class_extends location extends
    | _ ->
      raise @@ expected @@
        UnsupportedAnnotation {location; error_message="Classes with type arguments"}

  method! function_param_pattern node =
    let open Flow_ast.Pattern in
    let open Flow_ast.Pattern.Identifier in
    match node with
    | loc, Identifier ({annot; _} as id)
      when this#is_target loc
        || (target_is_point && this#target_contained_by loc) ->
      if strict
      then raise @@ expected
        @@ UnsupportedAnnotation {location=loc; error_message="Function parameter in strict mode."}
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
        raise @@ expected
          @@ UnsupportedAnnotation {location; error_message="Private field"}
      | Property (loc, ({annot; _} as prop)) when this#is_target loc ->
        update_property loc prop annot
      | Property (loc, ({key=(Literal     (kloc, _)|
                              Identifier  (kloc, _)|
                              PrivateName (kloc, _));
                         annot; _} as prop))
        when this#is_target kloc
          || (target_is_point && this#target_contained_by kloc) ->
        if strict
        then raise @@ expected
          @@ UnsupportedAnnotation {location=kloc; error_message="property key in strict mode"}
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
    if p == p' then raise @@ expected @@ InvalidAnnotationTarget target;
    p'
end

let type_lookup_at_location typed_ast loc =
  match Typed_ast_utils.find_exact_match_annotation typed_ast (ALoc.of_loc loc) with
  | Some p -> p
  | None -> raise @@ unexpected @@ UnknownTypeAtPoint loc

let normalize ~full_cx ~file_sig ~typed_ast ~expand_aliases ~omit_targ_defaults loc scheme =
  let open Query_types in
  match insert_type_normalize ~full_cx ~file_sig ~typed_ast
    ~expand_aliases ~omit_targ_defaults loc scheme
  with
  | FailureNoMatch -> raise @@ unexpected @@ FailedToNormalizeNoMatch
  | FailureUnparseable (loc, _, msg) ->
    raise @@ expected @@ FailedToNormalize (loc, msg)
  | Success (_, ty) -> ty

let type_to_string t =
  Js_layout_generator.type_ t
  |> Pretty_printer.print ~source_maps:None ~skip_endline:true
  |> Source.contents

let unexpected_error_to_string = function
  | UnknownTypeAtPoint _ ->
    "Couldn't locate a type for this annotation"
  | FailedToSerialize {error_message=msg; _} ->
    "couldn't print type: " ^ msg
  | FailedToNormalizeNoMatch ->
    "couldn't print type: couldn't locate a type for this annotation"


let expected_error_to_string = function
  | TypeAnnotationAtPoint {location; type_ast;} ->
    "Preexisiting type annotation at " ^ (Loc.to_string_no_source location)
    ^ ":" ^ (type_to_string type_ast)
  | InvalidAnnotationTarget location ->
    "Did not find an annotation at " ^ (Loc.to_string_no_source location)
  | UnsupportedAnnotation {location; error_message;} ->
    error_message ^ " found at " ^ (Loc.to_string_no_source location)
    ^ " is not currently supported"
  | TypeSizeLimitExceeded {size_limit; size;} ->
    "The type that would be generated "
    ^ (match size with
        | Some n -> "(size: " ^ (string_of_int n) ^ ") "
        | None -> "")
    ^ "exceeds the size limit (" ^ (string_of_int size_limit) ^ ")"
  | FailedToTypeCheck _ ->
    "Failed to typecheck file"
  | MulipleTypesPossibleAtPoint {generalized; specialized;} ->
    "Multiple types possible at point:\n" ^
    "    generalized type: " ^ (type_to_string generalized) ^ "\n" ^
    "    specialized type: " ^ (type_to_string specialized)
  | FailedToValidateType {error_message=msg; _} ->
    "Failed to validate type: " ^ msg
  | FailedToNormalize (_,msg) ->
    "couldn't print type: " ^ msg


let error_to_string = function
  | Unexpected err -> "flow autofix insert-type: " ^ (unexpected_error_to_string err)
  | Expected err -> "flow autofix insert-type: " ^ (expected_error_to_string err)
