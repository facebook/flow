(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Refactor_extract_utils
open Ast_builder

module TypeParamSet = struct
  include Flow_set.Make (struct
    type t = Type.typeparam

    let compare = Stdlib.compare
  end)

  let add_all list set = List.fold_left (fun acc t -> add t acc) set list
end

type attribute =
  | BoolAttribute
  | StringAttribute
  | EmptyAttribute
  | TypedAttribute of ALoc.t

let attribute_of_jsx_value = function
  | None -> BoolAttribute
  | Some (Ast.JSX.Attribute.StringLiteral _) -> StringAttribute
  | Some
      (Ast.JSX.Attribute.ExpressionContainer
        ( _,
          {
            Ast.JSX.ExpressionContainer.expression = Ast.JSX.ExpressionContainer.EmptyExpression;
            comments = _;
          }
        )
        ) ->
    EmptyAttribute
  | Some
      (Ast.JSX.Attribute.ExpressionContainer
        ( _,
          {
            Ast.JSX.ExpressionContainer.expression =
              Ast.JSX.ExpressionContainer.Expression ((typed_loc, _), _);
            comments = _;
          }
        )
        ) ->
    TypedAttribute typed_loc

exception FoundTarget of string * attribute SMap.t

class target_finder ~is_valid_target =
  object (_this)
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    method on_type_annot t = t

    method on_loc_annot l = l

    method! jsx_element annot expr =
      let open Ast.JSX in
      (match expr with
      | {
       opening_element =
         ( _,
           {
             Opening.name = Identifier ((id_loc, _), { Ast.JSX.Identifier.name; comments = _ });
             targs = None;
             self_closing = _;
             attributes;
           }
         );
       closing_element = _;
       children = (_, []);
       comments = _;
      }
        when name = String.capitalize_ascii name && is_valid_target id_loc ->
        let all_explicit_attributes =
          Base.List.fold_until attributes ~init:SMap.empty ~finish:Option.some ~f:(fun acc -> function
            | Opening.Attribute
                ( _,
                  {
                    Attribute.name =
                      Attribute.Identifier (_, { Ast.JSX.Identifier.name = prop_name; comments = _ });
                    value;
                  }
                ) ->
              Base.Continue_or_stop.Continue (SMap.add prop_name (attribute_of_jsx_value value) acc)
            | Opening.Attribute (_, { Attribute.name = Attribute.NamespacedName _; value = _ }) ->
              Base.Continue_or_stop.Stop None
            | Opening.SpreadAttribute _ -> Base.Continue_or_stop.Stop None
          )
        in
        (match all_explicit_attributes with
        | None -> ()
        | Some attributes -> raise (FoundTarget (name, attributes)))
      | _ -> ());
      super#jsx_element annot expr
  end

let stub
    ~ast
    ~cx
    ~file
    ~file_sig
    ~typed_ast
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_name
    ~get_type_sig
    cursor_loc =
  let open Base.Option.Let_syntax in
  let%bind (new_component_name, attributes) =
    let finder =
      let scope_info_lazy =
        lazy (Scope_builder.program ~enable_enums:(Context.enable_enums cx) ~with_types:true ast)
      in
      new target_finder ~is_valid_target:(fun id_loc ->
          let id_loc = loc_of_aloc id_loc in
          Loc.contains id_loc cursor_loc
          && not (Scope_api.is_local_use (Lazy.force scope_info_lazy) id_loc)
      )
    in
    try
      ignore @@ finder#program typed_ast;
      None
    with
    | FoundTarget (name, attributes) -> Some (name, attributes)
  in
  let { TypeSynthesizer.type_param_synthesizer; type_synthesizer; added_imports } =
    TypeSynthesizer.create_synthesizer_context
      ~cx
      ~file
      ~file_sig
      ~typed_ast
      ~loc_of_aloc
      ~get_ast_from_shared_mem
      ~get_haste_name
      ~get_type_sig
      ~locs:
        (SMap.fold
           (fun _ attr acc ->
             match attr with
             | BoolAttribute
             | StringAttribute
             | EmptyAttribute ->
               acc
             | TypedAttribute loc -> Loc_collections.LocSet.add (loc_of_aloc loc) acc)
           attributes
           Loc_collections.LocSet.empty
        )
    |> TypeSynthesizer.create_type_synthesizer_with_import_adder
  in
  let (tparams_set, typed_props) =
    SMap.fold
      (fun key attr (tparams_acc, props_acc) ->
        let (tparams, type_) =
          match attr with
          | BoolAttribute -> ([], (Loc.none, Ast.Type.Boolean { raw = `Boolean; comments = None }))
          | StringAttribute -> ([], (Loc.none, Ast.Type.String None))
          | EmptyAttribute -> ([], (Loc.none, Ast.Type.Mixed None))
          | TypedAttribute loc ->
            type_synthesizer (loc_of_aloc loc)
            |> Base.Result.ok
            |> Base.Option.join
            |> Base.Option.value ~default:([], (Loc.none, Ast.Type.Any None))
        in
        (TypeParamSet.add_all tparams tparams_acc, SMap.add key type_ props_acc))
      attributes
      (TypeParamSet.empty, SMap.empty)
  in
  let%bind tparams =
    let tparams = TypeParamSet.elements tparams_set in
    Base.List.fold_until
      tparams
      ~init:[]
      ~finish:(fun tparams ->
        if Base.List.is_empty tparams then
          Some None
        else
          Some (Some (Types.type_params (List.rev tparams))))
      ~f:(fun acc tparam ->
        match type_param_synthesizer tparam with
        | Ok tparam -> Base.Continue_or_stop.Continue (tparam :: acc)
        | Error _ -> Base.Continue_or_stop.Stop None)
  in
  let typed_props = SMap.elements typed_props in
  let added_imports = added_imports () in
  let component_declaration_statement =
    Statements.synthesized_component_declaration
      ~use_component_syntax:(Context.component_syntax cx)
      ?tparams
      ~typed_props
      ~body:[Statements.return None]
      new_component_name
  in
  let new_ast =
    let open Ast.Program in
    let (program_loc, program) = ast in
    let new_statements =
      Base.List.bind program.statements ~f:(fun ((loc, _) as s) ->
          if Loc.contains loc cursor_loc then
            [component_declaration_statement; s]
          else
            [s]
      )
    in
    (program_loc, { program with Ast.Program.statements = new_statements })
  in
  return
    {
      Refactor_extract.title =
        Utils_js.spf "Add missing React component declaration `%s`" new_component_name;
      new_ast;
      added_imports;
    }
