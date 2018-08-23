(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast_visitor

(* Resolve the AST nodes of CommonJS exported values, it mimics the ES modules interface
 * by differentiating between named and default values.
 *
 * NOTE: This logic is temporary and is intended to be replaced by `File_sig`, avoid
 *       using this module since any coding relying on it will need to be rewritten.
 *)

type export_values =
  | ExportFunction of { line_loc: Loc.t; func: (Loc.t * (Loc.t, Loc.t) Ast.Function.t) }
  | ExportClass of { line_loc: Loc.t; class_: (Loc.t * (Loc.t, Loc.t) Ast.Class.t) }
  | ExportExpression of { line_loc: Loc.t; expr: (Loc.t, Loc.t) Ast.Expression.t }

type module_exports = {
  default: export_values option;
  named: export_values SMap.t option
}

type non_resolveable_export_reason =
  (* The name for a named property could not be resolved, it was likely a
     non static computed property *)
  | NonResolveableName of { line_loc: Loc.t; loc: Loc.t }
  (* A non-analyzable export value, e.g. Getter, Setter  *)
  | DynamicExport of { line_loc: Loc.t; name: string option }
  (* The global `export` or `module` object has been overridden *)
  | ClobberedExport of { line_loc: Loc.t; }

type ast_node =
  | DefFunction of { line_loc: Loc.t; func: (Loc.t * (Loc.t, Loc.t) Ast.Function.t) }
  | DefClass of { line_loc: Loc.t; class_: (Loc.t * (Loc.t, Loc.t) Ast.Class.t) }
  | DefExpression of { line_loc: Loc.t; expr: (Loc.t, Loc.t) Ast.Expression.t }

let empty_module_exports = {
  default = None;
  named = None;
}

(* TODO: Create a generic LOC to AST map  *)
class ast_of_def_loc_visitor ~loc = object(this)
  inherit [ast_node option] visitor ~init:None as super

  method! statement stmt =
    let open Ast.Statement in
    if acc = None then match stmt with
    | line_loc, FunctionDeclaration ({ Ast.Function.id = Some (local_loc, _); _ } as func)
      when local_loc = loc ->
      this#update_acc (fun _ -> Some (DefFunction { line_loc; func = (line_loc, func) }));
      stmt
    | line_loc, ClassDeclaration ({ Ast.Class.id = Some (local_loc, _); _ } as class_)
      when local_loc = loc ->
      this#update_acc (fun _ -> Some (DefClass { line_loc; class_ = (line_loc, class_) }));
      stmt
    | line_loc, VariableDeclaration { VariableDeclaration.declarations; _ } ->
      List.iter (function
        | _, { VariableDeclaration.Declarator.
            id = (_, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
              name = (local_loc, _); _
            });
            init = Some expr
          } when local_loc = loc ->
          this#update_acc (fun _ -> Some (DefExpression { line_loc; expr }));
        | _ -> ()
      ) declarations;
      stmt
    | _ -> super#statement stmt
    else stmt
end

let ast_of_def_loc ~ast ~loc =
  let walk = new ast_of_def_loc_visitor ~loc in
  walk#eval walk#program ast

class exports_resolver ~ast = object(this)
  inherit [module_exports] visitor ~init:empty_module_exports as _super

  val scope_info = Scope_builder.program ast

  method private reset_cjs_exports () =
    this#update_acc (fun _ -> empty_module_exports)

  method private set_cjs_default_export export =
    (* Setting a default export will always clobber the named properties *)
    this#update_acc (fun _ -> { default = Some export; named = None })

  method private add_cjs_named_export name export =
    match name with
    | Some name ->
      this#update_acc (fun exports ->
        let named = Option.value exports.named ~default:SMap.empty in
        let named = SMap.add name export named in
        { exports with named = Some named }
      )
    (* TODO: Report non-resolvable exported names, currently they are dropped *)
    | None ->
      begin match export with
      | ExportFunction { line_loc; func = (loc, _) }
      | ExportClass { line_loc; class_ = (loc, _) }
      | ExportExpression { line_loc; expr = (loc, _) } ->
        this#add_non_resolveable_export (NonResolveableName { line_loc; loc })
      end

  method private add_non_resolveable_export (_reason: non_resolveable_export_reason) =
  (* TODO: Report non-resolvable exports, currently they are dropped *)
    ()

  method! declare_module _loc (m: (Loc.t, Loc.t) Ast.Statement.DeclareModule.t) =
    (* Don't walk into declare modules since they can define their own exports *)
    m

  method! statement (stmt: (Loc.t, Loc.t) Ast.Statement.t) =
    let open Ast.Statement in
    match stmt with
    | loc, Expression { Expression.
        expression = (_, Ast.Expression.Assignment expr);
        directive = None
      } ->
        this#assignment_with_loc loc expr;
        stmt
    (* No need to walk anything else *)
    | _ -> stmt

  method private assignment_with_loc (line_loc: Loc.t) (expr: (Loc.t, Loc.t) Ast.Expression.Assignment.t) =
    let open Ast.Expression in
    let { Assignment.operator; left; right } = expr in

    (* Handle exports *)
    match operator, left with
    (* exports = ... *)
    | Assignment.Assign, (module_loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
        name = (_, ("module" | "exports")); _
      })
      (* We only care about global scope *)
      when not (Scope_api.is_local_use scope_info module_loc) ->

      (* The assignment will override the global object referance, making it no longer exported *)
      this#reset_cjs_exports ();
      this#add_non_resolveable_export (ClobberedExport { line_loc })

    (* module.exports = ... *)
    | Assignment.Assign, (_, Ast.Pattern.Expression (_, Member { Member.
        _object = module_loc, Identifier (_, "module");
        property = Member.PropertyIdentifier (_, "exports"); _
      }))
      (* We only care about global scope *)
      when not (Scope_api.is_local_use scope_info module_loc) ->

      (* The assignment will clobber exports *)
      this#reset_cjs_exports ();
      this#process_default_export line_loc right;

    (* exports.foo = ... *)
    | Assignment.Assign, (_, Ast.Pattern.Expression (_, Member { Member.
        _object = module_loc, Identifier (_, "exports");
        property;
        computed = _;
      }))
    (* module.exports.foo = ... *)
    | Assignment.Assign, (_, Ast.Pattern.Expression (_, Member { Member.
        _object = _, Member { Member.
          _object = module_loc, Identifier (_, "module");
          property = Member.PropertyIdentifier (_, "exports"); _
        };
        property;
        computed = _;
      }))
      (* We only care about global scope *)
      when not (Scope_api.is_local_use scope_info module_loc) ->

      this#process_named_export
        line_loc
        (this#get_member_property_name property)
        right

    | _ -> ()

  method private process_default_export (line_loc: Loc.t) (expr: (Loc.t, Loc.t) Ast.Expression.t) =
    let open Ast.Expression in
    match expr with
    | _, Identifier (loc, _) ->
      begin match Scope_api.def_of_use scope_info loc with
      | { Scope_api.Def.locs = (loc, []); _ } ->
        begin match ast_of_def_loc ~ast ~loc with
        | Some (DefFunction { line_loc; func }) ->
          this#set_cjs_default_export (ExportFunction { line_loc; func })
        | Some (DefClass { line_loc; class_ }) ->
          this#set_cjs_default_export (ExportClass { line_loc; class_ });
          let (_, { Ast.Class.body = (_, { Ast.Class.Body.body }); _ }) = class_ in
          List.iter this#process_class_static_property_export body
        | Some (DefExpression { line_loc; expr }) ->
          this#process_default_export line_loc expr;
        | None ->
          (* Definition not found *)
          this#set_cjs_default_export (ExportExpression { line_loc; expr })
        end
      | _ ->
        (* Definition not found *)
        this#set_cjs_default_export (ExportExpression { line_loc; expr })
      end
    | _, Object { Object.properties; _ } ->
      List.iter this#process_object_property_export properties
    | loc, Function func
    | loc, ArrowFunction func ->
      this#set_cjs_default_export (ExportFunction { line_loc; func = (loc, func) })
      (* TODO: walk static properties, watch out for mutations *)
    | loc, Class ({ Ast.Class.body = (_, { Ast.Class.Body.body }); _ } as class_) ->
      this#set_cjs_default_export (ExportClass { line_loc; class_ = (loc, class_) });
      List.iter this#process_class_static_property_export body
    | _ ->
      this#set_cjs_default_export (ExportExpression { line_loc; expr })

  method private process_named_export (line_loc: Loc.t) (name: string option) (expr: (Loc.t, Loc.t) Ast.Expression.t) =
    let open Ast.Expression in
    match expr with
    | _, Identifier (loc, _) ->
      begin match Scope_api.def_of_use scope_info loc with
      | { Scope_api.Def.locs = (loc, []); _ } ->
        begin match ast_of_def_loc ~ast ~loc with
        | Some (DefFunction { line_loc; func }) ->
          this#add_cjs_named_export name (ExportFunction { line_loc; func })
        | Some (DefClass { line_loc; class_ }) ->
          this#add_cjs_named_export name (ExportClass { line_loc; class_ })
        | Some (DefExpression { line_loc; expr }) ->
          this#process_named_export line_loc name expr;
        | None ->
          (* Definition not found *)
          this#add_cjs_named_export name (ExportExpression { line_loc; expr })
        end
      | _ ->
        (* Definition not found *)
        this#add_cjs_named_export name (ExportExpression { line_loc; expr })
      end
    | loc, Function func
    | loc, ArrowFunction func ->
      this#add_cjs_named_export name (ExportFunction { line_loc; func = (loc, func) })
    | loc, Class class_ ->
      this#add_cjs_named_export name (ExportClass { line_loc; class_ = (loc, class_) })
    | _ ->
      this#add_cjs_named_export name (ExportExpression { line_loc; expr })

  method private process_object_property_export (prop: (Loc.t, Loc.t) Ast.Expression.Object.property) =
    let open Ast.Expression.Object in
    match prop with
    | Property (line_loc, Property.Init { key; value; shorthand = _ }) ->
      this#process_named_export line_loc (this#get_object_property_key_name key) value

    | Property (line_loc, Property.Method { key; value = func }) ->
      this#add_cjs_named_export
        (this#get_object_property_key_name key)
        (ExportFunction { line_loc; func })

    (* TODO: What do we do with setters or getters? *)
    | Property (line_loc, Property.Get { key; _ })
    | Property (line_loc, Property.Set { key; _ }) ->
      this#add_non_resolveable_export (DynamicExport {
        line_loc;
        name = (this#get_object_property_key_name key);
      })

    (* TODO: Follow local spread identifiers *)
    | SpreadProperty (line_loc, _) ->
      this#add_non_resolveable_export (DynamicExport { line_loc; name = None })

  method private process_class_static_property_export (el: (Loc.t, Loc.t) Ast.Class.Body.element) =
    let open Ast.Class in
    match el with
    | Body.Method (line_loc, { Method.static = true; key; value = func; kind = Method.Method; _ }) ->
      this#add_cjs_named_export
        (this#get_object_property_key_name key)
        (ExportFunction { line_loc; func })

    (* TODO: What do we do with setters or getters? *)
    | Body.Method (line_loc, { Method.static = true; key; _ }) ->
      this#add_non_resolveable_export (DynamicExport {
        line_loc;
        name = (this#get_object_property_key_name key);
      })

    | Body.Property (line_loc, { Property.static = true; key; value = Some value; _ }) ->
      this#process_named_export line_loc (this#get_object_property_key_name key) value

    | _ -> ()

  method private get_object_property_key_name (key: (Loc.t, Loc.t) Ast.Expression.Object.Property.key) =
    let open Ast.Expression.Object.Property in
    match key with
    (* Normal key *)
    | Identifier (_, id) -> Some id

    (* Is computed but just defined as a string *)
    | Literal (_, { Ast.Literal.value = Ast.Literal.String value; _ })
    | Computed (_, Ast.Expression.Literal { Ast.Literal.value = Ast.Literal.String value; _ })
      -> Some value

    (* Not exported *)
    | PrivateName _ -> None

    (* Anything else *)
    | _ -> None

  method private get_member_property_name (key: (Loc.t, Loc.t) Ast.Expression.Member.property) =
    let open Ast.Expression.Member in
    match key with
    (* Normal key *)
    | PropertyIdentifier (_, id) -> Some id

    (* Is computed but just defined as a string *)
    | PropertyExpression (_, Ast.Expression.Literal { Ast.Literal.value = Ast.Literal.String value; _ })
      -> Some value

    (* Not exported *)
    | PropertyPrivateName _ -> None

    (* Anything else *)
    | _ -> None

end

let program ~ast =
  let walk = new exports_resolver ~ast in
  walk#eval walk#program ast
