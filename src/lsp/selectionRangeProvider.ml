(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

(** Provides LSP "selection ranges" for the textDocument/selectionRange API.

    Given a cursor position, it returns a tree of "selection ranges". A selection
    range is a range around the cursor position which the user might be interested
    in selecting. Typically, but not necessary, selection ranges correspond to the
    nodes of the syntax tree.

    https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_selectionRange *)

(** Finds a list of selection ranges for the given position. The returned list
    starts with the broadest range and each item in the list is a narrower child
    range. *)
class selection_range_finder ~position =
  object (this)
    inherit [Loc.t list, Loc.t] Flow_ast_visitor.visitor ~init:[] as super

    method add_loc loc =
      match acc with
      | hd :: _ when Loc.equal hd loc -> ()
      | _ -> this#update_acc (List.cons loc)

    method contains loc =
      let { Loc.start; _end; source = _ } = loc in
      Loc.pos_cmp start position <= 0 && Loc.pos_cmp position _end <= 0

    method test_loc : 'a. Loc.t -> (Loc.t -> 'a -> 'a) -> 'a -> 'a =
      fun loc f x ->
        if this#contains loc then (
          (* recurse first, so we end up with the parent at the head of the results *)
          let result = f loc x in
          this#add_loc loc;
          result
        ) else
          x

    method test : 'a. (Loc.t * 'a -> Loc.t * 'a) -> Loc.t * 'a -> Loc.t * 'a =
      fun f x ->
        let loc = fst x in
        if this#contains loc then (
          (* recurse first, so we end up with the parent at the head of the results *)
          let result = f x in
          this#add_loc loc;
          result
        ) else
          x

    (* including the whole program is important to ensure that all positions
       have at least one range. *)
    method! program x =
      let x = this#test super#program x in

      (* if `position` is within the docblock (or other toplevel comments) then
         add the whole file. *)
      let (loc, _) = x in
      (match acc with
      | [] -> this#add_loc { loc with Loc.start = { Loc.line = 1; column = 0 } }
      | hd :: _ when hd.Loc.start.Loc.line > 1 || hd.Loc.start.Loc.column > 0 ->
        this#add_loc { loc with Loc.start = { Loc.line = 1; column = 0 } }
      | _ -> ());

      x

    method! statement x = this#test super#statement x

    method! expression x = this#test super#expression x

    (* the arguments of a function call, including the parens *)
    method! call_arguments x = this#test super#call_arguments x

    (* the type args of a function call, including the <>'s *)
    method! call_type_args x = this#test super#call_type_args x

    (* the whole `catch (e) {}` *)
    method! catch_clause loc x = this#test_loc loc super#catch_clause x

    (* the whole class body, excluding `class Foo ...` *)
    method! class_body x = this#test super#class_body x

    (* the whole `extends Foo` clause in a class *)
    method! class_extends loc x = this#test_loc loc super#class_extends x

    (* the whole `implements IFoo<T>` clause in a class *)
    method! class_implements x = this#test super#class_implements x

    (* the `IFoo<T>` in `implements IFoo<T>` in a class *)
    method! class_implements_interface x = this#test super#class_implements_interface x

    method! class_method loc x = this#test_loc loc super#class_method x

    method! class_property loc x = this#test_loc loc super#class_property x

    method! class_private_field loc x = this#test_loc loc super#class_private_field x

    method! enum_body x = this#test super#enum_body x

    method! enum_defaulted_member x = this#test super#enum_defaulted_member x

    method! enum_boolean_member x = this#test super#enum_boolean_member x

    method! enum_number_member x = this#test super#enum_number_member x

    method! enum_string_member x = this#test super#enum_string_member x

    method! export_named_declaration_specifier x =
      this#test super#export_named_declaration_specifier x

    method! export_batch_specifier x = this#test super#export_batch_specifier x

    method! for_in_left_declaration x = this#test super#for_in_left_declaration x

    method! for_of_left_declaration x = this#test super#for_of_left_declaration x

    method! for_init_declaration x = this#test super#for_init_declaration x

    method! function_declaration loc x =
      let { Flow_ast.Function.sig_loc; _ } = x in
      let result = super#function_declaration loc x in
      if this#contains sig_loc then this#add_loc sig_loc;
      result

    method! function_expression_or_method loc x =
      let { Flow_ast.Function.sig_loc; _ } = x in
      let result = super#function_expression_or_method loc x in
      if this#contains sig_loc then this#add_loc sig_loc;
      result

    method! function_param_type x = this#test super#function_param_type x

    method! function_rest_param_type x = this#test super#function_rest_param_type x

    method! function_this_param_type x = this#test super#function_this_param_type x

    method! object_type_property_getter x = this#test super#object_type_property_getter x

    method! object_type_property_setter x = this#test super#object_type_property_setter x

    method! object_property_type x = this#test super#object_property_type x

    method! object_spread_property_type x = this#test super#object_spread_property_type x

    method! object_indexer_property_type x = this#test super#object_indexer_property_type x

    method! object_internal_slot_property_type x =
      this#test super#object_internal_slot_property_type x

    method! object_call_property_type x = this#test super#object_call_property_type x

    method! generic_qualified_identifier_type x =
      this#test super#generic_qualified_identifier_type x

    method! variance x = this#test super#variance x

    method! type_args x = this#test super#type_args x

    method! type_params x = this#test super#type_params x

    method! type_param x = this#test super#type_param x

    method! type_ x = this#test super#type_ x

    method! type_annotation x = this#test super#type_annotation x

    method! function_params x = this#test super#function_params x

    method! function_this_param x = this#test super#function_this_param x

    method! function_param x = this#test super#function_param x

    method! function_body x = this#test super#function_body x

    method! identifier x = this#test super#identifier x

    method! private_name x = this#test super#private_name x

    method! computed_key x = this#test super#computed_key x

    method! import_namespace_specifier ~import_kind loc x =
      this#test_loc loc (super#import_namespace_specifier ~import_kind) x

    method! jsx_opening_element x = this#test super#jsx_opening_element x

    method! jsx_closing_element x = this#test super#jsx_closing_element x

    method! jsx_spread_attribute loc x = this#test_loc loc super#jsx_spread_attribute x

    method! jsx_attribute x = this#test super#jsx_attribute x

    method! jsx_attribute_value_expression loc x =
      this#test_loc loc super#jsx_attribute_value_expression x

    method! jsx_attribute_value_literal loc x =
      this#test_loc loc super#jsx_attribute_value_literal x

    method! jsx_children x = this#test super#jsx_children x

    method! jsx_child x = this#test super#jsx_child x

    method! jsx_namespaced_name x = this#test super#jsx_namespaced_name x

    method! jsx_member_expression x = this#test super#jsx_member_expression x

    method! jsx_identifier x = this#test super#jsx_identifier x

    method! object_property x = this#test super#object_property x

    method! object_key_literal x = this#test super#object_key_literal x

    method! pattern ?kind patt = this#test (super#pattern ?kind) patt

    method! pattern_object_property ?kind p = this#test (super#pattern_object_property ?kind) p

    method! pattern_object_property_literal_key ?kind x =
      this#test (super#pattern_object_property_literal_key ?kind) x

    method! pattern_object_rest_property ?kind x =
      this#test (super#pattern_object_rest_property ?kind) x

    method! pattern_array_element ?kind x = this#test (super#pattern_array_element ?kind) x

    method! pattern_array_rest_element ?kind x =
      this#test (super#pattern_array_rest_element ?kind) x

    method! predicate x = this#test super#predicate x

    method! function_rest_param x = this#test super#function_rest_param x

    (* `...x`, separately from the `x` *)
    method! spread_element x = this#test super#spread_element x

    method! spread_property x = this#test super#spread_property x

    method! switch_case x = this#test super#switch_case x

    method! template_literal_element x = this#test super#template_literal_element x

    method! variable_declarator ~kind x = this#test (super#variable_declarator ~kind) x
  end

let selection_range_tree position ast =
  let finder = new selection_range_finder ~position in
  let rev_locs = finder#eval finder#program ast in
  List.fold
    ~f:(fun parent loc -> Some (Flow_lsp_conversions.selection_range_of_loc ?parent loc))
    ~init:None
    rev_locs

let provide_selection_ranges positions ast =
  let rev_results =
    List.fold_result
      ~f:(fun acc lsp_position ->
        let position = Flow_lsp_conversions.lsp_position_to_flow_position lsp_position in
        match selection_range_tree position ast with
        | Some range -> Ok (range :: acc)
        | None ->
          Error
            (Printf.sprintf
               "Invalid position: %d:%d"
               lsp_position.Lsp.line
               lsp_position.Lsp.character
            ))
      ~init:[]
      positions
  in
  match rev_results with
  | Ok rev_results ->
    let results = List.rev rev_results in
    Lsp.SelectionRangeResult results
  | Error message ->
    let e = Lsp.Error.{ code = InvalidParams; message; data = None } in
    Lsp.ErrorResult (e, "")
