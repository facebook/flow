(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Utils = Insert_type_utils
module Import = Insert_type_imports
module ImportsHelper = Import.ImportsHelper

type unexpected =
  | UnknownTypeAtPoint of Loc.t
  | NoFileInLocation of Loc.t
  | FailedToSerialize of {
      ty: Ty.t;
      error_message: string;
    }
  | FailedToNormalizeNoMatch

type expected =
  | TypeAnnotationAtPoint of {
      location: Loc.t;
      type_ast: (Loc.t, Loc.t) Flow_ast.Type.t;
    }
  | InvalidAnnotationTarget of Loc.t
  | UnsupportedAnnotation of {
      location: Loc.t;
      error_message: string;
    }
  | MultipleTypesPossibleAtPoint of {
      generalized: (Loc.t, Loc.t) Flow_ast.Type.t;
      specialized: (Loc.t, Loc.t) Flow_ast.Type.t;
    }
  | FailedToValidateType of {
      error: Utils.Error.validation_error;
      error_message: string;
    }
  | FailedToTypeCheck of Flow_errors_utils.ConcreteLocPrintableErrorSet.t
  | FailedToNormalize of (Loc.t * string)
  | FailedToImport of Insert_type_utils.Error.import_error

type errors =
  | Unexpected of unexpected
  | Expected of expected

exception FailedToInsertType of errors

let expected err = FailedToInsertType (Expected err)

let unexpected err = FailedToInsertType (Unexpected err)

let fail_on_ambiguity =
  let exception FoundAmbiguousType in
  let visitor =
    object
      inherit [_] Ty.endo_ty as super

      method! on_Arr () t =
        function
        | Ty.{ arr_literal = Some true; _ } -> raise FoundAmbiguousType
        | arr -> super#on_Arr () t arr

      method! on_Obj () t =
        function
        | Ty.{ obj_literal = Some true; _ } -> raise FoundAmbiguousType
        | obj -> super#on_Obj () t obj
    end
  in
  fun t ->
    match visitor#on_t () t with
    | exception FoundAmbiguousType -> None
    | t -> Some t

class generalize_temporary_types_mapper =
  object
    inherit [_] Ty.endo_ty as super

    method! on_Arr () t =
      function
      | Ty.{ arr_literal = Some true; _ } as arr ->
        let arr = Ty.{ arr with arr_literal = Some false } in
        super#on_Arr () (Ty.Arr arr) arr
      | arr -> super#on_Arr () t arr

    method! on_Obj () t =
      function
      | Ty.{ obj_kind = Ty.ExactObj; _ } as obj ->
        let obj = Ty.{ obj with obj_kind = Ty.InexactObj } in
        super#on_Obj () (Ty.Obj obj) obj
      | obj -> super#on_Obj () t obj
  end

let generalize_temporary_types = (new generalize_temporary_types_mapper)#on_t ()

let fixme_ambiguous_types =
  let visitor =
    object
      inherit [_] Ty.endo_ty as super

      method! on_Arr () t =
        function
        | Ty.{ arr_literal = Some true; _ } -> Utils.Builtins.flowfixme_ty_default
        | arr -> super#on_Arr () t arr

      method! on_Obj () t =
        function
        | Ty.{ obj_literal = Some true; _ } -> Utils.Builtins.flowfixme_ty_default
        | obj -> super#on_Obj () t obj
    end
  in
  visitor#on_t ()

let simplify = Ty_utils.simplify_type ~merge_kinds:true ~sort:true

(* Generate an equivalent Flow_ast.Type *)
let serialize
    ~cx
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~file_sig
    ~typed_ast
    ?(imports_react = false)
    ~exact_by_default
    loc
    ty =
  let mapper =
    new Utils.type_normalization_hardcoded_fixes_mapper
      ~cx
      ~loc_of_aloc
      ~get_ast_from_shared_mem
      ~file_sig
      ~typed_ast
      ~lint_severities:LintSettings.empty_severities
      ~suppress_types:SSet.empty (* Make autofix insert any *)
      ~imports_react
      ~generalize_maybe:true
      ~generalize_react_mixed_element:true
      ~add_warning:(fun _ _ -> ()
    )
  in
  mapper#on_t loc ty
  |> simplify
  |> Ty_serializer.(type_ { exact_by_default })
  |> Utils.patch_up_type_ast

let remove_ambiguous_types
    ~cx
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~file_sig
    ~typed_ast
    ~ambiguity_strategy
    ~exact_by_default
    ty
    loc =
  let open Autofix_options in
  match ambiguity_strategy with
  | Fail -> begin
    match fail_on_ambiguity ty with
    | Some t -> Ok t
    | None ->
      Error
        (MultipleTypesPossibleAtPoint
           {
             specialized =
               serialize
                 ~cx
                 ~loc_of_aloc
                 ~get_ast_from_shared_mem
                 ~file_sig
                 ~typed_ast
                 ~exact_by_default
                 loc
                 ty;
             generalized =
               generalize_temporary_types ty
               |> serialize
                    ~cx
                    ~loc_of_aloc
                    ~get_ast_from_shared_mem
                    ~file_sig
                    ~typed_ast
                    ~exact_by_default
                    loc;
           }
        )
  end
  | Generalize -> Ok (generalize_temporary_types ty)
  | Fixme -> Ok (fixme_ambiguous_types ty)
  | Suppress -> Ok Utils.Builtins.flowfixme_ty_default

let path_of_loc ?(error = Error "no path for location") (loc : Loc.t) : (string, string) result =
  match Loc.source loc with
  | Some src -> File_key.to_path src
  | None -> error

(* This class maps each node that contains the target until a node is contained
   by the target *)

class mapper ~strict ~synth_type ~casting_syntax target =
  let target_is_point = Utils.is_point target in
  object (this)
    inherit Flow_ast_contains_mapper.mapper target as super

    method private synth_type_annotation_hint loc =
      match synth_type loc with
      | Ok t -> Flow_ast.Type.Available t
      | Error _ -> Flow_ast.Type.Missing loc

    method private synth_return_type_annotation_hint loc =
      let open Flow_ast.Function.ReturnAnnot in
      match synth_type loc with
      | Ok t -> Available t
      | Error _ -> Missing loc

    (* If a type is missing and in the range of target then add a type annotation hint *)
    method private update_type_annotation_hint ?type_loc ?(check_loc = false) annot =
      let open Flow_ast.Type in
      match annot with
      | Missing location when (not check_loc) || this#target_contained_by location ->
        let type_loc =
          match type_loc with
          | Some type_loc -> type_loc
          | None -> location
        in
        this#synth_type_annotation_hint type_loc
      | Available (location, type_ast) when (not check_loc) || this#target_contained_by location ->
        raise @@ expected @@ TypeAnnotationAtPoint { location; type_ast }
      | _ -> annot

    method! type_annotation_hint = this#update_type_annotation_hint ?type_loc:None ~check_loc:true

    method! component_renders_annotation renders =
      let open Flow_ast.Type in
      match renders with
      | MissingRenders loc when this#target_contained_by loc ->
        (match synth_type loc with
        | Ok (l, (_, Renders r)) -> AvailableRenders (l, r)
        | Ok _
        | Error _ ->
          MissingRenders loc)
      | _ -> super#component_renders_annotation renders

    method! function_return_annotation return =
      let open Flow_ast.Function.ReturnAnnot in
      match return with
      | Missing loc when this#target_contained_by loc -> this#synth_return_type_annotation_hint loc
      | Available (location, type_ast) when this#target_contained_by location ->
        raise @@ expected @@ TypeAnnotationAtPoint { location; type_ast }
      | _ -> return

    method! function_param_pattern node =
      let open Flow_ast.Pattern in
      let open Flow_ast.Pattern.Identifier in
      match node with
      | (loc, Identifier ({ annot; _ } as id))
        when this#is_target loc || (target_is_point && this#target_contained_by loc) ->
        if strict then
          raise
          @@ expected
          @@ UnsupportedAnnotation
               { location = loc; error_message = "Function parameter in strict mode." }
        else
          let annot = this#update_type_annotation_hint annot in
          (loc, Identifier { id with annot })
      | _ -> super#function_param_pattern node

    method! class_element elem =
      let open Flow_ast.Class.Body in
      let open Flow_ast.Class.Property in
      let open Flow_ast.Expression.Object.Property in
      let update_property loc prop annot =
        let annot = this#update_type_annotation_hint annot in
        Property (loc, { prop with annot })
      in
      match elem with
      | PrivateField (location, _) when this#is_target location ->
        raise @@ expected @@ UnsupportedAnnotation { location; error_message = "Private field" }
      | Property (loc, ({ annot; _ } as prop)) when this#is_target loc ->
        update_property loc prop annot
      | Property
          ( loc,
            ( {
                key =
                  ( StringLiteral (kloc, _)
                  | NumberLiteral (kloc, _)
                  | BigIntLiteral (kloc, _)
                  | Identifier (kloc, _)
                  | PrivateName (kloc, _) );
                annot;
                _;
              } as prop
            )
          )
        when this#is_target kloc || (target_is_point && this#target_contained_by kloc) ->
        if strict then
          raise
          @@ expected
          @@ UnsupportedAnnotation
               { location = kloc; error_message = "property key in strict mode" }
        else
          update_property loc prop annot
      | _ -> super#class_element elem

    method! variable_declarator ~kind decl =
      let open Flow_ast.Statement.VariableDeclaration.Declarator in
      let open Flow_ast.Pattern in
      let open Flow_ast.Pattern.Identifier in
      match (kind, decl) with
      (* In `const x = exp;` the error appears on exp *)
      | ( Flow_ast.Variable.Const,
          (dloc, ({ id = (iloc, Identifier id); init = Some (type_loc, _) } as decl))
        )
      (* Use is_target of initialization expression location
         because const signature verification errors point to expression *)
        when this#is_target type_loc ->
        let { annot; _ } = id in
        let annot = this#update_type_annotation_hint ~type_loc annot in
        (dloc, { decl with id = (iloc, Identifier { id with annot }) })
      | _ -> super#variable_declarator ~kind decl

    method! variable_declarator_pattern ~kind node =
      let open Flow_ast.Pattern in
      let open Flow_ast.Pattern.Identifier in
      let open Flow_ast.Variable in
      let (loc, patt) = node in
      if not (this#target_contained_by loc) then
        node
      else
        match (patt, kind) with
        (* In `const x = exp;` for signature verification errors the error appears on the exp portion.
           When strict we only look for that error. *)
        | (Identifier _, Const) when strict -> super#variable_declarator_pattern ~kind node
        | (Identifier ({ name; annot; _ } as id), (Var | Let | Const))
          when target_is_point || this#is_target loc ->
          let (type_loc, _) = name in
          let annot = this#update_type_annotation_hint ~type_loc annot in
          (loc, Identifier { id with annot })
        | _ -> super#variable_declarator_pattern ~kind node

    method! expression ((l, _) as e) =
      let open Flow_ast.Expression in
      if this#target_contained_by l then
        if this#is_target l then
          let open Options.CastingSyntax in
          match (synth_type l, casting_syntax) with
          | (Error _, _) -> super#expression e
          | (Ok annot, Colon) -> (l, TypeCast TypeCast.{ expression = e; annot; comments = None })
          | (Ok annot, As)
          | (Ok annot, Both) ->
            (l, AsExpression AsExpression.{ expression = e; annot; comments = None })
        else
          super#expression e
      else
        e

    method! program p =
      let p' = super#program p in
      if p == p' then raise @@ expected @@ InvalidAnnotationTarget target;
      p'
  end

let type_lookup_at_location typed_ast loc =
  match Typed_ast_finder.find_exact_match_annotation typed_ast (ALoc.of_loc loc) with
  | Some p -> p
  | None -> raise @@ unexpected @@ UnknownTypeAtPoint loc

let normalize ~cx ~file_sig ~typed_ast ~omit_targ_defaults loc t =
  Query_types.(
    match insert_type_normalize ~cx ~file_sig ~typed_ast ~omit_targ_defaults loc t with
    | FailureNoMatch -> raise @@ unexpected @@ FailedToNormalizeNoMatch
    | FailureUnparseable (loc, _, msg) -> raise @@ expected @@ FailedToNormalize (loc, msg)
    | Success (_, ty) -> ty
  )

let synth_type
    ?(size_limit = 30)
    ~cx
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~file_sig
    ~typed_ast
    ~omit_targ_defaults
    ~ambiguity_strategy
    ~remote_converter
    type_loc
    t =
  let exact_by_default = Context.exact_by_default cx in
  let imports_react = ImportsHelper.imports_react file_sig in
  let process ty =
    match Utils.Validator.validate_type ~size_limit ~loc_of_aloc:ALoc.to_loc_exn ty with
    | (_, error :: _) ->
      (* TODO surface all errors *)
      let error_message = Utils.Error.serialize_validation_error error in
      let err = FailedToValidateType { error; error_message } in
      Error err
    | (_, []) ->
      remove_ambiguous_types
        ~cx
        ~loc_of_aloc
        ~get_ast_from_shared_mem
        ~file_sig
        ~typed_ast
        ~ambiguity_strategy
        ~exact_by_default
        ty
        type_loc
  in
  let t =
    Primitive_literal.convert_literal_type
      cx
      ~singleton_action:(fun _ -> Primitive_literal.DoNotKeep)
      t
  in
  let elt = normalize ~cx ~file_sig ~typed_ast ~omit_targ_defaults type_loc t in
  match Ty_utils.typify_elt elt with
  | None -> Error (FailedToNormalize (type_loc, "Non-type"))
  | Some ty -> begin
    match process ty with
    | Error err -> Error err
    | Ok ty ->
      (* If we can't fix the type annotation we insert the type anyway which will fail to typecheck
         but might provide a hint as to what the type is. *)
      let import_fixed_ty =
        match remote_converter#type_ ty with
        | Ok ty -> ty
        | Error e ->
          (* TODO: surface these errors to user *)
          Hh_logger.error "Insert type: %s" (Insert_type_utils.Error.serialize e);
          ty
      in
      let ast =
        serialize
          ~cx
          ~loc_of_aloc
          ~get_ast_from_shared_mem
          ~file_sig
          ~typed_ast
          ~imports_react
          ~exact_by_default
          type_loc
          import_fixed_ty
      in
      Ok (type_loc, ast)
  end

let type_to_string t =
  Js_layout_generator.type_ ~opts:Js_layout_generator.default_opts t
  |> Pretty_printer.print ~source_maps:None ~skip_endline:true
  |> Source.contents

let unexpected_error_to_string = function
  | NoFileInLocation _ -> "Target passed to insert-type without source in location"
  | UnknownTypeAtPoint _ -> "Couldn't locate a type for this annotation"
  | FailedToSerialize { error_message = msg; _ } -> "couldn't print type: " ^ msg
  | FailedToNormalizeNoMatch -> "couldn't print type: couldn't locate a type for this annotation"

let expected_error_to_string = function
  | TypeAnnotationAtPoint { location; type_ast } ->
    "Preexisting type annotation at "
    ^ Loc.to_string_no_source location
    ^ ": "
    ^ type_to_string type_ast
  | InvalidAnnotationTarget location ->
    "Did not find an annotation at " ^ Loc.to_string_no_source location
  | UnsupportedAnnotation { location; error_message } ->
    error_message ^ " found at " ^ Loc.to_string_no_source location ^ " is not currently supported"
  | FailedToTypeCheck _ -> "Failed to typecheck file"
  | MultipleTypesPossibleAtPoint { generalized; specialized } ->
    "Multiple types possible at point:\n"
    ^ "    generalized type: "
    ^ type_to_string generalized
    ^ "\n"
    ^ "    specialized type: "
    ^ type_to_string specialized
    ^ "\n"
  | FailedToValidateType { error = Utils.Error.TooBig { size_limit; size }; _ } ->
    "The type that would be generated (size: "
    ^ begin
        match size with
        | Some size -> string_of_int size
        | None -> ">" ^ string_of_int Utils.Validator.validate_type_too_big_max
      end
    ^ ") exceeds the size limit ("
    ^ string_of_int size_limit
    ^ ")"
  | FailedToValidateType { error_message = msg; _ } -> "Failed to validate type: " ^ msg
  | FailedToNormalize (_, msg) -> "couldn't print type: " ^ msg
  | FailedToImport e ->
    "failed to import needed type: " ^ Insert_type_utils.Error.serialize_import_error e

let error_to_string = function
  | Unexpected err -> "flow autofix insert-type: " ^ unexpected_error_to_string err
  | Expected err -> "flow autofix insert-type: " ^ expected_error_to_string err

let add_statement_after_directive_and_type_imports stmts new_imports =
  let open Flow_ast_differ in
  let (Partitioned { directives; imports; body }) = partition_imports stmts in
  directives @ new_imports @ imports @ body

let add_imports remote_converter stmts =
  let new_imports = remote_converter#to_import_stmts () in
  add_statement_after_directive_and_type_imports stmts new_imports

let insert_type_
    ~cx
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_module_info
    ~get_type_sig
    ~file_sig
    ~typed_ast
    ~omit_targ_defaults
    ~strict
    ~ambiguity_strategy
    ?remote_converter
    ast
    target
    loc_to_type =
  let file =
    match target.Loc.source with
    | Some source -> source
    | None -> raise (unexpected (NoFileInLocation target))
  in
  let (remote_converter, maybe_add_imports) =
    match remote_converter with
    | Some rc -> (rc, (fun x -> x)) (* External remote_converters must add there own imports *)
    | None ->
      let rc =
        new ImportsHelper.remote_converter
          ~loc_of_aloc
          ~file_options:(Context.file_options cx)
          ~get_haste_module_info
          ~get_type_sig
          ~iteration:0
          ~file
          ~reserved_names:SSet.empty
      in
      (rc, add_imports rc)
  in
  let synth_type location =
    synth_type
      ~cx
      ~loc_of_aloc
      ~get_ast_from_shared_mem
      ~file_sig
      ~typed_ast
      ~omit_targ_defaults
      ~ambiguity_strategy
      ~remote_converter
      location
      (loc_to_type location)
  in
  let casting_syntax = Context.casting_syntax cx in
  let mapper = new mapper ~strict ~synth_type ~casting_syntax target in
  let (loc, ast') = mapper#program ast in
  let statements = maybe_add_imports ast'.Flow_ast.Program.statements in
  (loc, { ast' with Flow_ast.Program.statements })

let insert_type
    ~cx
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_module_info
    ~get_type_sig
    ~file_sig
    ~typed_ast
    ~omit_targ_defaults
    ~strict
    ~ambiguity_strategy
    ?remote_converter
    ast
    target =
  insert_type_
    ~cx
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_module_info
    ~get_type_sig
    ~file_sig
    ~typed_ast
    ~omit_targ_defaults
    ~strict
    ~ambiguity_strategy
    ?remote_converter
    ast
    target
    (type_lookup_at_location typed_ast)

let insert_type_t
    ~cx
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_module_info
    ~get_type_sig
    ~file_sig
    ~typed_ast
    ~omit_targ_defaults
    ~strict
    ~ambiguity_strategy
    ?remote_converter
    ast
    target
    type_t =
  insert_type_
    ~cx
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_module_info
    ~get_type_sig
    ~file_sig
    ~typed_ast
    ~omit_targ_defaults
    ~strict
    ~ambiguity_strategy
    ?remote_converter
    ast
    target
    (fun _loc -> type_t
  )

let mk_diff ast new_ast = Flow_ast_differ.program ast new_ast

let mk_patch ~opts ast new_ast file_content =
  Replacement_printer.mk_patch_ast_differ ~opts (mk_diff ast new_ast) file_content
