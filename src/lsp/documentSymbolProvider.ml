(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base.Option.Monad_infix
module Ast = Flow_ast

(** sometimes due to parse errors, we end up with empty names. hide them!
    in fact, VS Code throws out the entire response if any symbol name is falsy!
    https://github.com/microsoft/vscode/blob/afd102cbd2e17305a510701d7fd963ec2528e4ea/src/vs/workbench/api/common/extHostTypes.ts#L1068-L1072 *)
let name_opt = function
  | "" -> None
  | name -> Some name

let name_and_loc_of_identifier = function
  | (loc, { Ast.Identifier.name; _ }) -> name_opt name >>| fun name -> (name, loc)

let name_and_loc_of_private_name = function
  | (loc, { Ast.PrivateName.name; comments = _ }) -> name_opt name >>| fun name -> ("#" ^ name, loc)

let name_and_loc_of_key (key : (Loc.t, Loc.t) Ast.Expression.Object.Property.key) :
    (string * Loc.t) option =
  let open Ast.Expression.Object.Property in
  match key with
  | Literal (loc, { Ast.Literal.raw; _ }) -> name_opt raw >>| fun name -> (name, loc)
  | Identifier id -> name_and_loc_of_identifier id
  | PrivateName name -> name_and_loc_of_private_name name
  | Computed (_, _) -> None

let name_and_loc_of_pattern = function
  | (_, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; annot = _; optional = _ }) ->
    name_and_loc_of_identifier name
  | (_, Ast.Pattern.Object _)
  | (_, Ast.Pattern.Array _)
  | (_, Ast.Pattern.Expression _) ->
    None

let id_matches_name ~name = function
  | None -> true
  | Some (_, { Ast.Identifier.name = id_name; _ }) -> id_name = name

let special_initializer ~name expr =
  match expr with
  | (func_loc, Ast.Expression.Function ({ Ast.Function.id; _ } as func))
    when id_matches_name ~name id ->
    `Function (func_loc, func)
  | (class_loc, Ast.Expression.Class ({ Ast.Class.id; _ } as cls)) when id_matches_name ~name id ->
    `Class (class_loc, cls)
  | _ -> `Normal

let kind_of_property is_method =
  if is_method then
    Lsp.SymbolInformation.Method
  else
    Lsp.SymbolInformation.Property

let mk ~loc ~selection ~name ?detail ~kind ~children () =
  let range = Flow_lsp_conversions.loc_to_lsp_range loc in
  let selectionRange = Flow_lsp_conversions.loc_to_lsp_range selection in
  { Lsp.DocumentSymbol.name; detail; kind; deprecated = false; range; selectionRange; children }

class visitor =
  object (this)
    inherit [Lsp.DocumentSymbol.t list, Loc.t] Flow_ast_visitor.visitor ~init:[] as super

    method with_children
        : 't. ('t -> 't) -> 't -> f:(Lsp.DocumentSymbol.t list -> Lsp.DocumentSymbol.t) -> 't =
      fun g x ~f ->
        let prev = acc in
        this#set_acc [];
        let result = g x in
        let children = acc in
        this#set_acc (f children :: prev);
        result

    method add_with_children
        : 't.
          loc:Loc.t ->
          selection:Loc.t ->
          name:string ->
          ?detail:string ->
          kind:Lsp.SymbolInformation.symbolKind ->
          ('t -> 't) ->
          't ->
          unit =
      fun ~loc ~selection ~name ?detail ~kind g x ->
        let _ =
          this#with_children g x ~f:(fun children ->
              mk ~loc ~selection ~name ?detail ~kind ~children:(Some children) ()
          )
        in
        ()

    method add ~loc ~selection ~name ?detail ~kind () =
      let sym = mk ~loc ~selection ~name ?detail ~kind ~children:None () in
      this#update_acc (List.cons sym)

    method! variable_declarator
        ~kind (decl : (Loc.t, Loc.t) Ast.Statement.VariableDeclaration.Declarator.t) =
      let open Ast.Statement.VariableDeclaration.Declarator in
      let (loc, { id; init }) = decl in
      let (name, selection) =
        Base.Option.value ~default:("<var>", loc) (name_and_loc_of_pattern id)
      in
      let k = Lsp.SymbolInformation.Variable in
      (match init with
      | Some init -> this#visit_special_initializer ~loc ~name ~selection ~kind:k init
      | None ->
        this#add_with_children ~loc ~selection ~name ~kind:k (super#variable_declarator ~kind) decl);
      decl

    method! object_property (prop : (Loc.t, Loc.t) Ast.Expression.Object.Property.t) =
      let open Ast.Expression.Object.Property in
      (match prop with
      | (loc, Init { key; value; shorthand = _ }) ->
        Base.Option.iter (name_and_loc_of_key key) ~f:(fun (name, selection) ->
            let kind = Lsp.SymbolInformation.Property in
            this#visit_special_initializer ~loc ~name ~selection ~kind value
        )
      | (loc, Method { key; value = _ }) ->
        Base.Option.iter (name_and_loc_of_key key) ~f:(fun (name, selection) ->
            let kind = Lsp.SymbolInformation.Method in
            this#add_with_children ~loc ~selection ~name ~kind super#object_property prop
        )
      | (loc, Get { key; value = _; comments = _ }) ->
        Base.Option.iter (name_and_loc_of_key key) ~f:(fun (name, selection) ->
            let kind = Lsp.SymbolInformation.Property in
            this#add_with_children ~loc ~selection ~name ~kind super#object_property prop
        )
      | (loc, Set { key; value = _; comments = _ }) ->
        Base.Option.iter (name_and_loc_of_key key) ~f:(fun (name, selection) ->
            let kind = Lsp.SymbolInformation.Property in
            this#add_with_children ~loc ~selection ~name ~kind super#object_property prop
        ));
      prop

    method visit_special_initializer ~loc ~name ~selection ~kind value : unit =
      match special_initializer ~name value with
      | `Function (func_loc, func) ->
        let kind = Lsp.SymbolInformation.Method in
        this#add_with_children ~loc ~selection ~name ~kind (super#function_expression func_loc) func
      | `Class (class_loc, cls) ->
        let kind = Lsp.SymbolInformation.Class in
        this#add_with_children ~loc ~selection ~name ~kind (super#class_expression class_loc) cls
      | `Normal -> this#add_with_children ~loc ~selection ~name ~kind super#expression value

    method class_decl_or_expr f loc (cls : (Loc.t, Loc.t) Ast.Class.t) =
      let open Ast.Class in
      let { id; _ } = cls in
      let (name, selection) =
        Base.Option.value (id >>= name_and_loc_of_identifier) ~default:("<class>", loc)
      in
      this#add_with_children ~loc ~selection ~name ~kind:Lsp.SymbolInformation.Class (f loc) cls;
      cls

    method! class_declaration loc (cls : (Loc.t, Loc.t) Ast.Class.t) =
      this#class_decl_or_expr super#class_declaration loc cls

    method! class_expression loc (cls : (Loc.t, Loc.t) Ast.Class.t) =
      this#class_decl_or_expr super#class_expression loc cls

    method! class_method loc (meth : (Loc.t, Loc.t) Ast.Class.Method.t') =
      let open Ast.Class.Method in
      let { kind; key; _ } = meth in
      let (name, selection) =
        Base.Option.value ~default:("<method>", loc) (name_and_loc_of_key key)
      in
      let (kind, name) =
        match kind with
        | Constructor -> (Lsp.SymbolInformation.Constructor, name)
        | Method -> (Lsp.SymbolInformation.Method, name)
        | Get -> (Lsp.SymbolInformation.Property, "(get) " ^ name)
        | Set -> (Lsp.SymbolInformation.Property, "(set) " ^ name)
      in
      this#add_with_children ~loc ~selection ~name ~kind (super#class_method loc) meth;
      meth

    method! class_property loc (prop : (Loc.t, Loc.t) Ast.Class.Property.t') =
      let open Ast.Class.Property in
      let { key; value; _ } = prop in
      let (name, selection) =
        Base.Option.value ~default:("<property>", loc) (name_and_loc_of_key key)
      in
      let kind = Lsp.SymbolInformation.Property in
      (match value with
      | Initialized expr -> this#visit_special_initializer ~loc ~name ~selection ~kind expr
      | Declared
      | Uninitialized ->
        this#add_with_children ~loc ~selection ~name ~kind (super#class_property loc) prop);
      prop

    method! class_private_field loc (prop : (Loc.t, Loc.t) Ast.Class.PrivateField.t') =
      let open Ast.Class.PrivateField in
      let { key; _ } = prop in
      let (name, selection) =
        Base.Option.value ~default:("<property>", loc) (name_and_loc_of_private_name key)
      in
      let kind = Lsp.SymbolInformation.Property in
      this#add_with_children ~loc ~selection ~name ~kind (super#class_private_field loc) prop;
      prop

    method! declare_class loc (decl : (Loc.t, Loc.t) Ast.Statement.DeclareClass.t) =
      let open Ast.Statement.DeclareClass in
      let { id; _ } = decl in
      let (name, selection) =
        Base.Option.value ~default:("<class>", loc) (name_and_loc_of_identifier id)
      in
      let kind = Lsp.SymbolInformation.Class in
      this#add_with_children ~loc ~selection ~name ~kind (super#declare_class loc) decl;
      decl

    method! declare_function loc (decl : (Loc.t, Loc.t) Ast.Statement.DeclareFunction.t) =
      let open Ast.Statement.DeclareFunction in
      let { id; _ } = decl in
      let (name, selection) =
        Base.Option.value ~default:("<function>", loc) (name_and_loc_of_identifier id)
      in
      let kind = Lsp.SymbolInformation.Function in
      this#add_with_children ~loc ~selection ~name ~kind (super#declare_function loc) decl;
      decl

    method! declare_module loc (m : (Loc.t, Loc.t) Ast.Statement.DeclareModule.t) =
      let open Ast.Statement.DeclareModule in
      let { id; _ } = m in
      let (name, selection) =
        match id with
        | Identifier id ->
          Base.Option.value ~default:("<module>", loc) (name_and_loc_of_identifier id)
        | Literal (lit_loc, { Ast.StringLiteral.value; _ }) -> (Printf.sprintf "%S" value, lit_loc)
      in
      let kind = Lsp.SymbolInformation.Module in
      this#add_with_children ~loc ~selection ~name ~kind (super#declare_module loc) m;
      m

    method! declare_variable loc (decl : (Loc.t, Loc.t) Ast.Statement.DeclareVariable.t) =
      let open Ast.Statement.DeclareVariable in
      let { id; _ } = decl in
      let (name, selection) =
        Base.Option.value ~default:("<var>", loc) (name_and_loc_of_identifier id)
      in
      let kind = Lsp.SymbolInformation.Variable in
      this#add_with_children ~loc ~selection ~name ~kind (super#declare_variable loc) decl;
      decl

    method! enum_declaration loc (enum : (Loc.t, Loc.t) Ast.Statement.EnumDeclaration.t) =
      let open Ast.Statement.EnumDeclaration in
      let { id; _ } = enum in
      let (name, selection) =
        Base.Option.value ~default:("<enum>", loc) (name_and_loc_of_identifier id)
      in
      let kind = Lsp.SymbolInformation.Enum in
      this#add_with_children ~loc ~selection ~name ~kind (super#enum_declaration loc) enum;
      enum

    method! enum_member_identifier id =
      (match name_and_loc_of_identifier id with
      | Some (name, loc) ->
        let kind = Lsp.SymbolInformation.EnumMember in
        this#add ~loc ~selection:loc ~name ~kind ()
      | None -> ());
      id

    method! export_default_declaration loc decl =
      let open Ast.Statement.ExportDefaultDeclaration in
      let { default; _ } = decl in
      let name = "default" in
      let kind = Lsp.SymbolInformation.Variable in
      this#add_with_children
        ~loc
        ~selection:default
        ~name
        ~kind
        (super#export_default_declaration loc)
        decl;
      decl

    method function_decl_or_expr f loc (stmt : (Loc.t, Loc.t) Ast.Function.t) =
      let open Ast.Function in
      let { id; _ } = stmt in
      let (name, selection) =
        Base.Option.value ~default:("<function>", loc) (id >>= name_and_loc_of_identifier)
      in
      let kind = Lsp.SymbolInformation.Function in
      this#add_with_children ~loc ~selection ~name ~kind (f loc) stmt;
      stmt

    method! function_declaration loc stmt =
      this#function_decl_or_expr super#function_declaration loc stmt

    method! function_expression loc stmt =
      this#function_decl_or_expr super#function_expression loc stmt

    method! interface loc decl =
      let open Ast.Statement.Interface in
      let { id; _ } = decl in
      let (name, selection) =
        Base.Option.value ~default:("<interface>", loc) (name_and_loc_of_identifier id)
      in
      let kind = Lsp.SymbolInformation.Interface in
      this#add_with_children ~loc ~selection ~name ~kind (super#interface loc) decl;
      decl

    method! object_property_type (opt : (Loc.t, Loc.t) Ast.Type.Object.Property.t) =
      let open Ast.Type.Object.Property in
      let (loc, { key; _method; _ }) = opt in
      (match name_and_loc_of_key key with
      | Some (name, selection) ->
        let kind = kind_of_property _method in
        this#add_with_children ~loc ~selection ~name ~kind super#object_property_type opt
      | None -> ());
      opt

    method! object_indexer_property_type (opt : (Loc.t, Loc.t) Ast.Type.Object.Indexer.t) =
      let open Ast.Type.Object.Indexer in
      let (loc, { id; key; _ }) = opt in
      let (name, selection) =
        Base.Option.value ~default:("", fst key) (id >>= name_and_loc_of_identifier)
      in
      let name = Printf.sprintf "[%s]" name in
      let kind = Lsp.SymbolInformation.Property in
      this#add_with_children ~loc ~selection ~name ~kind super#object_indexer_property_type opt;
      opt

    method! object_internal_slot_property_type slot =
      let open Ast.Type.Object.InternalSlot in
      let (loc, { id; _method; _ }) = slot in
      let (name, selection) =
        Base.Option.value ~default:("", loc) (name_and_loc_of_identifier id)
      in
      let name = Printf.sprintf "[[%s]]" name in
      this#add_with_children
        ~loc
        ~selection
        ~name
        ~kind:(kind_of_property _method)
        super#object_internal_slot_property_type
        slot;
      slot

    method! object_call_property_type (call : (Loc.t, Loc.t) Ast.Type.Object.CallProperty.t) =
      (* TODO *)
      super#object_call_property_type call

    method! opaque_type loc (otype : (Loc.t, Loc.t) Ast.Statement.OpaqueType.t) =
      let open Ast.Statement.OpaqueType in
      let { id; _ } = otype in
      let (name, selection) =
        Base.Option.value ~default:("<opaque>", loc) (name_and_loc_of_identifier id)
      in
      let kind = Lsp.SymbolInformation.Variable in
      this#add_with_children ~loc ~selection ~name ~kind (super#opaque_type loc) otype;
      otype

    method! type_alias loc (stuff : (Loc.t, Loc.t) Ast.Statement.TypeAlias.t) =
      let open Ast.Statement.TypeAlias in
      let { id; _ } = stuff in
      let (name, selection) =
        Base.Option.value ~default:("<type>", loc) (name_and_loc_of_identifier id)
      in
      let kind = Lsp.SymbolInformation.Variable in
      this#add_with_children ~loc ~selection ~name ~kind (super#type_alias loc) stuff;
      stuff
  end

let provide_document_symbols program =
  let finder = new visitor in
  let rev_symbols = finder#eval finder#program program in
  Base.List.rev rev_symbols

let provide_symbol_information =
  let rec flatten ~uri ~containerName acc = function
    | [] -> acc
    | next :: todo ->
      let {
        Lsp.DocumentSymbol.name;
        detail = _;
        kind;
        deprecated = _;
        range;
        selectionRange = _;
        children;
      } =
        next
      in
      let location = { Lsp.Location.uri; range } in
      let info = { Lsp.SymbolInformation.name; kind; location; containerName } in
      let acc = info :: acc in
      let acc =
        match children with
        | Some children -> flatten ~uri ~containerName:(Some name) acc children
        | None -> acc
      in
      flatten ~uri ~containerName acc todo
  in
  fun ~uri program ->
    program |> provide_document_symbols |> flatten ~uri ~containerName:None [] |> Base.List.rev
