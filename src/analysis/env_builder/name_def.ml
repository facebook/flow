(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Reason
open Flow_ast_mapper
open Loc_collections

type for_kind =
  | In
  | Of of { await: bool }

type root =
  | Annotation of (ALoc.t, ALoc.t) Ast.Type.annotation
  | Value of (ALoc.t, ALoc.t) Ast.Expression.t
  | Contextual of ALoc.t
  | Catch
  | For of for_kind * (ALoc.t, ALoc.t) Ast.Expression.t

type selector =
  | Elem of int
  | Prop of {
      prop: string;
      has_default: bool;
    }
  | Computed of (ALoc.t, ALoc.t) Ast.Expression.t
  | ObjRest of {
      used_props: string list;
      after_computed: bool;
    }
  | ArrRest of int
  | Default of (ALoc.t, ALoc.t) Ast.Expression.t

type binding =
  | Root of root
  | Select of selector * binding

type import =
  | Named of {
      kind: Ast.Statement.ImportDeclaration.import_kind option;
      remote: string;
      remote_loc: ALoc.t;
      local: string;
    }
  | Namespace
  | Default of string

type def =
  | Binding of binding
  | OpAssign of {
      exp_loc: ALoc.t;
      op: Ast.Expression.Assignment.operator;
      rhs: (ALoc.t, ALoc.t) Ast.Expression.t;
    }
  | Update of {
      exp_loc: ALoc.t;
      op: Ast.Expression.Update.operator;
    }
  | Function of {
      fully_annotated: bool;
      function_: (ALoc.t, ALoc.t) Ast.Function.t;
    }
  | Class of {
      fully_annotated: bool;
      class_: (ALoc.t, ALoc.t) Ast.Class.t;
      class_loc: ALoc.t;
    }
  | DeclaredClass of ALoc.t * (ALoc.t, ALoc.t) Ast.Statement.DeclareClass.t
  | TypeAlias of ALoc.t * (ALoc.t, ALoc.t) Ast.Statement.TypeAlias.t
  | OpaqueType of ALoc.t * (ALoc.t, ALoc.t) Ast.Statement.OpaqueType.t
  | TypeParam of (ALoc.t, ALoc.t) Ast.Type.TypeParam.t
  | Interface of ALoc.t * (ALoc.t, ALoc.t) Ast.Statement.Interface.t
  | Enum of ALoc.t Ast.Statement.EnumDeclaration.body
  | Import of {
      import_kind: Ast.Statement.ImportDeclaration.import_kind;
      import: import;
      source: string;
      source_loc: ALoc.t;
    }

type map = (def * ALoc.t virtual_reason) ALocMap.t

module Destructure = struct
  open Ast.Pattern

  let type_of_pattern (_, p) =
    let open Ast.Pattern in
    match p with
    | Array { Array.annot = Ast.Type.Available t; _ }
    | Object { Object.annot = Ast.Type.Available t; _ }
    | Identifier { Identifier.annot = Ast.Type.Available t; _ } ->
      Some t
    | _ -> None

  let pattern_default acc = function
    | None -> acc
    | Some e -> Select (Default e, acc)

  let array_element acc i = Select (Elem i, acc)

  let array_rest_element acc i = Select (ArrRest i, acc)

  let object_named_property acc x ~has_default = Select (Prop { prop = x; has_default }, acc)

  let object_computed_property acc e = Select (Computed e, acc)

  let object_rest_property acc xs has_computed =
    Select (ObjRest { used_props = xs; after_computed = has_computed }, acc)

  let object_property acc xs key ~has_default =
    let open Ast.Pattern.Object in
    match key with
    | Property.Identifier (_, { Ast.Identifier.name = x; comments = _ }) ->
      let acc = object_named_property acc x ~has_default in
      (acc, x :: xs, false)
    | Property.Literal (_, { Ast.Literal.value = Ast.Literal.String x; _ }) ->
      let acc = object_named_property acc x ~has_default in
      (acc, x :: xs, false)
    | Property.Computed (_, { Ast.ComputedKey.expression; comments = _ }) ->
      let acc = object_computed_property acc expression in
      (acc, xs, true)
    | Property.Literal (_, _) -> (acc, xs, false)

  let identifier ~f acc (name_loc, { Ast.Identifier.name; _ }) =
    f name_loc (mk_reason (RIdentifier (OrdinaryName name)) name_loc) (Binding acc)

  let rec pattern ~f acc (_, p) =
    match p with
    | Array { Array.elements; annot = _; comments = _ } -> array_elements ~f acc elements
    | Object { Object.properties; annot = _; comments = _ } -> object_properties ~f acc properties
    | Identifier { Identifier.name = id; optional = _; annot = _ } -> identifier ~f acc id
    | Expression _ -> ()

  and array_elements ~f acc =
    let open Ast.Pattern.Array in
    Base.List.iteri ~f:(fun i -> function
      | Hole _ -> ()
      | Element (_, { Element.argument = p; default = d }) ->
        let acc = array_element acc i in
        let acc = pattern_default acc d in
        pattern ~f acc p
      | RestElement (_, { Ast.Pattern.RestElement.argument = p; comments = _ }) ->
        let acc = array_rest_element acc i in
        pattern ~f acc p
    )

  and object_properties =
    let open Ast.Pattern.Object in
    let prop ~f acc xs has_computed p =
      match p with
      | Property (_, { Property.key; pattern = p; default = d; shorthand = _ }) ->
        let has_default = d <> None in
        let (acc, xs, has_computed') = object_property acc xs key ~has_default in
        let acc = pattern_default acc d in
        pattern ~f acc p;
        (xs, has_computed || has_computed')
      | RestElement (_, { Ast.Pattern.RestElement.argument = p; comments = _ }) ->
        let acc = object_rest_property acc xs has_computed in
        pattern ~f acc p;
        (xs, false)
    in
    let rec loop ~f acc xs has_computed = function
      | [] -> ()
      | p :: ps ->
        let (xs, has_computed) = prop ~f acc xs has_computed p in
        loop ~f acc xs has_computed ps
    in
    (fun ~f acc ps -> loop ~f acc [] false ps)
end

let func_is_annotated { Ast.Function.return; _ } =
  match return with
  | Ast.Type.Missing _ -> false
  | Ast.Type.Available _ -> true

let def_of_function function_ =
  Function { fully_annotated = func_is_annotated function_; function_ }

let def_of_class loc ({ Ast.Class.body = (_, { Ast.Class.Body.body; _ }); _ } as class_) =
  let open Ast.Class.Body in
  let fully_annotated =
    Base.List.for_all
      ~f:(function
        | Method
            ( _,
              {
                Ast.Class.Method.key =
                  Ast.Expression.Object.Property.Identifier
                    (_, { Ast.Identifier.name = "constructor"; _ });
                _;
              }
            ) ->
          true
        | Method
            ( _,
              {
                Ast.Class.Method.key = Ast.Expression.Object.Property.Identifier _;
                value = (_, value);
                _;
              }
            ) ->
          func_is_annotated value
        | Method _ -> false
        | Property
            ( _,
              {
                Ast.Class.Property.key = Ast.Expression.Object.Property.Identifier _;
                annot = Ast.Type.Available _;
                _;
              }
            ) ->
          true
        | Property _ -> false
        | PrivateField (_, { Ast.Class.PrivateField.annot = Ast.Type.Available _; _ }) -> true
        | PrivateField (_, { Ast.Class.PrivateField.annot = Ast.Type.Missing _; _ }) -> false)
      body
  in
  Class { fully_annotated; class_; class_loc = loc }

class def_finder =
  object (this)
    inherit [map, ALoc.t] Flow_ast_visitor.visitor ~init:ALocMap.empty as super

    method add_binding loc reason src = this#update_acc (ALocMap.add loc (src, reason))

    method! variable_declarator ~kind decl =
      let open Ast.Statement.VariableDeclaration.Declarator in
      let (_, { id; init }) = decl in
      let source =
        match (Destructure.type_of_pattern id, init) with
        | (Some annot, _) -> Some (Annotation annot)
        | (None, Some init) -> Some (Value init)
        | (None, None) -> None
      in
      Base.Option.iter ~f:(fun acc -> Destructure.pattern ~f:this#add_binding (Root acc) id) source;
      super#variable_declarator ~kind decl

    method! declare_variable loc (decl : ('loc, 'loc) Ast.Statement.DeclareVariable.t) =
      let open Ast.Statement.DeclareVariable in
      let { id = (id_loc, { Ast.Identifier.name; _ }); annot; comments = _ } = decl in
      this#add_binding
        id_loc
        (mk_reason (RIdentifier (OrdinaryName name)) id_loc)
        (Binding (Root (Annotation annot)));
      super#declare_variable loc decl

    method! function_param (param : ('loc, 'loc) Ast.Function.Param.t) =
      let open Ast.Function.Param in
      let (loc, { argument; default }) = param in
      let source =
        match Destructure.type_of_pattern argument with
        | Some annot -> Annotation annot
        | None -> Contextual loc
      in
      let source = Destructure.pattern_default (Root source) default in
      Destructure.pattern ~f:this#add_binding source argument;
      super#function_param param

    method! function_rest_param (expr : ('loc, 'loc) Ast.Function.RestParam.t) =
      let open Ast.Function.RestParam in
      let (loc, { argument; _ }) = expr in
      let source =
        match Destructure.type_of_pattern argument with
        | Some annot -> Annotation annot
        | None -> Contextual loc
      in
      Destructure.pattern ~f:this#add_binding (Root source) argument;
      super#function_rest_param expr

    method! catch_clause_pattern pat =
      Destructure.pattern ~f:this#add_binding (Root Catch) pat;
      super#catch_clause_pattern pat

    method! function_expression loc expr =
      let open Ast.Function in
      let { id; async; generator; sig_loc; _ } = expr in
      begin
        match id with
        | Some (id_loc, _) ->
          this#add_binding id_loc (func_reason ~async ~generator sig_loc) (def_of_function expr)
        | None -> ()
      end;
      super#function_expression loc expr

    method! function_declaration loc expr =
      let open Ast.Function in
      let { id; async; generator; sig_loc; _ } = expr in
      begin
        match id with
        | Some (id_loc, _) ->
          this#add_binding id_loc (func_reason ~async ~generator sig_loc) (def_of_function expr)
        | None -> ()
      end;
      super#function_expression loc expr

    method! class_ loc expr =
      let open Ast.Class in
      let { id; _ } = expr in
      begin
        match id with
        | Some (id_loc, { Ast.Identifier.name; _ }) ->
          let name = OrdinaryName name in
          let reason = mk_reason (RType name) id_loc in
          this#add_binding id_loc reason (def_of_class loc expr)
        | None -> ()
      end;
      super#class_ loc expr

    method! declare_function loc (decl : ('loc, 'loc) Ast.Statement.DeclareFunction.t) =
      match Declare_function_utils.declare_function_to_function_declaration_simple loc decl with
      | Some stmt ->
        let _ = this#statement (loc, stmt) in
        decl
      | None ->
        let open Ast.Statement.DeclareFunction in
        let { id = (id_loc, _); annot; predicate = _; comments = _ } = decl in
        this#add_binding
          id_loc
          (func_reason ~async:false ~generator:false loc)
          (Binding (Root (Annotation annot)));
        super#declare_function loc decl

    method! declare_class loc (decl : ('loc, 'loc) Ast.Statement.DeclareClass.t) =
      let open Ast.Statement.DeclareClass in
      let { id = (id_loc, { Ast.Identifier.name; _ }); _ } = decl in
      this#add_binding
        id_loc
        (mk_reason (RClass (RIdentifier (OrdinaryName name))) loc)
        (DeclaredClass (loc, decl));
      super#declare_class loc decl

    method! assignment loc (expr : ('loc, 'loc) Ast.Expression.Assignment.t) =
      let open Ast.Expression.Assignment in
      let { operator; left = (_, lhs_node) as left; right; comments = _ } = expr in
      let () =
        match (operator, lhs_node) with
        | (None, _) -> Destructure.pattern ~f:this#add_binding (Root (Value right)) left
        | ( Some operator,
            Ast.Pattern.Identifier
              { Ast.Pattern.Identifier.name = (id_loc, { Ast.Identifier.name; _ }); _ }
          ) ->
          this#add_binding
            id_loc
            (mk_reason (RIdentifier (OrdinaryName name)) id_loc)
            (OpAssign { exp_loc = loc; op = operator; rhs = right })
        | _ -> ()
      in
      super#assignment loc expr

    method! update_expression loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Update.t) =
      let open Ast.Expression.Update in
      let { argument; operator; prefix = _; comments = _ } = expr in
      begin
        match argument with
        | (_, Ast.Expression.Identifier (id_loc, { Ast.Identifier.name; _ })) ->
          this#add_binding
            id_loc
            (mk_reason (RIdentifier (OrdinaryName name)) id_loc)
            (Update { exp_loc = loc; op = operator })
        | _ -> ()
      end;
      super#update_expression loc expr

    method! for_of_statement loc (stuff : ('loc, 'loc) Ast.Statement.ForOf.t) =
      let open Ast.Statement.ForOf in
      let { left; right; body = _; await; comments = _ } = stuff in
      begin
        match left with
        | LeftDeclaration
            ( _,
              {
                Ast.Statement.VariableDeclaration.declarations =
                  [(_, { Ast.Statement.VariableDeclaration.Declarator.id; _ })];
                _;
              }
            ) ->
          let source =
            match Destructure.type_of_pattern id with
            | Some annot -> Annotation annot
            | None -> For (Of { await }, right)
          in
          Destructure.pattern ~f:this#add_binding (Root source) id
        | LeftDeclaration _ -> failwith "Invalid AST structure"
        | LeftPattern pat ->
          Destructure.pattern ~f:this#add_binding (Root (For (Of { await }, right))) pat
      end;
      super#for_of_statement loc stuff

    method! for_in_statement loc (stuff : ('loc, 'loc) Ast.Statement.ForIn.t) =
      let open Ast.Statement.ForIn in
      let { left; right; body = _; each = _; comments = _ } = stuff in
      begin
        match left with
        | LeftDeclaration
            ( _,
              {
                Ast.Statement.VariableDeclaration.declarations =
                  [(_, { Ast.Statement.VariableDeclaration.Declarator.id; _ })];
                _;
              }
            ) ->
          let source =
            match Destructure.type_of_pattern id with
            | Some annot -> Annotation annot
            | None -> For (In, right)
          in
          Destructure.pattern ~f:this#add_binding (Root source) id
        | LeftDeclaration _ -> failwith "Invalid AST structure"
        | LeftPattern pat -> Destructure.pattern ~f:this#add_binding (Root (For (In, right))) pat
      end;
      super#for_in_statement loc stuff

    method! type_alias loc (alias : ('loc, 'loc) Ast.Statement.TypeAlias.t) =
      let open Ast.Statement.TypeAlias in
      let { id = (id_loc, { Ast.Identifier.name; _ }); _ } = alias in
      this#add_binding id_loc (mk_reason (RType (OrdinaryName name)) id_loc) (TypeAlias (loc, alias));
      super#type_alias loc alias

    method! opaque_type loc (otype : ('loc, 'loc) Ast.Statement.OpaqueType.t) =
      let open Ast.Statement.OpaqueType in
      let { id = (id_loc, { Ast.Identifier.name; _ }); _ } = otype in
      this#add_binding id_loc (mk_reason (ROpaqueType name) id_loc) (OpaqueType (loc, otype));
      super#opaque_type loc otype

    method! type_param (tparam : ('loc, 'loc) Ast.Type.TypeParam.t) =
      let open Ast.Type.TypeParam in
      let (_, { name = (name_loc, { Ast.Identifier.name; _ }); _ }) = tparam in
      this#add_binding name_loc (mk_reason (RType (OrdinaryName name)) name_loc) (TypeParam tparam);
      super#type_param tparam

    method! interface loc (interface : ('loc, 'loc) Ast.Statement.Interface.t) =
      let open Ast.Statement.Interface in
      let { id = (name_loc, _); _ } = interface in
      this#add_binding name_loc (mk_reason RInterfaceType loc) (Interface (loc, interface));
      super#interface loc interface

    method! enum_declaration loc (enum : ('loc, 'loc) Ast.Statement.EnumDeclaration.t) =
      let open Ast.Statement.EnumDeclaration in
      let { id = (name_loc, { Ast.Identifier.name; _ }); body; _ } = enum in
      this#add_binding name_loc (mk_reason (REnum name) loc) (Enum body);
      super#enum_declaration loc enum

    method! import_declaration loc (decl : ('loc, 'loc) Ast.Statement.ImportDeclaration.t) =
      let open Ast.Statement.ImportDeclaration in
      let {
        import_kind;
        source = (source_loc, { Ast.StringLiteral.value = source; _ });
        specifiers;
        default;
        comments = _;
      } =
        decl
      in
      begin
        match specifiers with
        | Some (ImportNamedSpecifiers specifiers) ->
          Base.List.iter
            ~f:(fun { kind; local; remote = (rem_id_loc, { Ast.Identifier.name = remote; _ }) } ->
              let (id_loc, name) =
                Base.Option.value_map
                  ~f:(fun (id_loc, { Ast.Identifier.name; _ }) -> (id_loc, name))
                  ~default:(rem_id_loc, remote)
                  local
              in
              this#add_binding
                id_loc
                (mk_reason (RNamedImportedType (source, name)) rem_id_loc)
                (Import
                   {
                     import_kind;
                     source;
                     source_loc;
                     import = Named { kind; remote; remote_loc = rem_id_loc; local = name };
                   }
                ))
            specifiers
        | Some (ImportNamespaceSpecifier (_, (id_loc, { Ast.Identifier.name; _ }))) ->
          let import_reason =
            let import_reason_desc =
              match import_kind with
              | ImportType -> RImportStarType name
              | ImportTypeof -> RImportStarTypeOf name
              | ImportValue -> RImportStar name
            in
            mk_reason import_reason_desc id_loc
          in
          this#add_binding
            id_loc
            import_reason
            (Import { import_kind; source; source_loc; import = Namespace })
        | None -> ()
      end;
      Base.Option.iter
        ~f:(fun (id_loc, { Ast.Identifier.name; _ }) ->
          let import_reason = mk_reason (RDefaultImportedType (name, source)) id_loc in
          this#add_binding
            id_loc
            import_reason
            (Import { import_kind; source; source_loc; import = Default name }))
        default;
      super#import_declaration loc decl
  end

let find_defs ast =
  let finder = new def_finder in
  finder#eval finder#program ast
