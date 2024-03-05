(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The parsing phase performs a single pass over the AST, building a parsed
 * signature. The parsed signature includes unresolved references which will be
 * resolved in a separate pass.
 *
 * When we see a definition in this pass, we make a note of the name, but do not
 * eagerly visit it's body. Instead, we visit definition bodies lazily during
 * the marking pass, which walks the graph of references starting at exports.
 *
 * This lazy walk is an optimization, to avoid walking parts of the AST which
 * are not reachable from the exports of a module.
 *)

open Type_sig
open Type_sig_collections
open Type_sig_options
module Ast = Flow_ast
module Option = Base.Option

(* This type encodes the fixed point of parsed signatures. Of particular note
 * here are the Ref and Err constructors.
 *
 * The Ref constructor encodes some name lookup which will be resolved in the
 * mark phase. The parsing phase builds up a scope as it descends into the AST,
 * which can be used to resolve the names to a definition once the program has
 * been fully parsed.
 *
 * The Err constructor encodes some failure in the parsing phase. These failures
 * are detected while visiting the AST, and thus are encoded directly in the
 * signature. The compaction step is responsible for collating these errors.
 *)
type 'loc parsed =
  | Annot of 'loc parsed_annot
  | Value of 'loc parsed_value
  | TyRef of 'loc tyname
  | TyRefApp of {
      loc: 'loc loc_node;
      name: 'loc tyname;
      targs: 'loc parsed list;
    }
  | AsyncVoidReturn of 'loc loc_node
  | ValRef of {
      type_only: bool;
      ref: 'loc ref;
    }
  | Err of 'loc loc_node * 'loc loc_node errno
  | BuiltinTyRef of {
      ref_loc: 'loc loc_node;
      name: string;
    }
  | Pattern of 'loc pattern_node
  | Eval of 'loc loc_node * 'loc parsed * 'loc parsed op
  | Require of {
      loc: 'loc loc_node;
      mref: module_ref_node;
    }
  | ImportDynamic of {
      loc: 'loc loc_node;
      mref: module_ref_node;
    }
  | ModuleRef of {
      loc: 'loc loc_node;
      mref: module_ref_node;
      legacy_interop: bool;
    }

and 'loc tyname =
  | Unqualified of 'loc ref
  | Qualified of {
      loc: 'loc loc_node;
      id_loc: 'loc loc_node;
      name: string;
      qualification: 'loc tyname;
    }

and 'loc parsed_value = ('loc loc_node, 'loc parsed) value

and 'loc parsed_annot = ('loc loc_node, 'loc parsed) annot

and 'loc parsed_def = ('loc loc_node, 'loc parsed) def

and 'loc export =
  | ExportRef of 'loc ref
  | ExportBinding of 'loc local_def_node
  | ExportDefault of {
      default_loc: 'loc loc_node;
      def: 'loc parsed;
    }
  | ExportDefaultBinding of {
      default_loc: 'loc loc_node;
      name: string;
      binding: 'loc local_def_node;
    }
  | ExportFrom of 'loc remote_ref_node

and 'loc export_type =
  | ExportTypeRef of 'loc ref
  | ExportTypeBinding of 'loc local_def_node
  | ExportTypeFrom of 'loc remote_ref_node

and 'loc module_kind =
  | UnknownModule
  | CJSModule of 'loc parsed
  | CJSModuleProps of ('loc loc_node * 'loc parsed) smap
  | CJSDeclareModule of 'loc local_def_node smap
  | ESModule of {
      names: 'loc export smap;
      stars: ('loc loc_node * module_ref_node) list;
    }

and 'loc ref =
  | Ref of {
      ref_loc: 'loc loc_node;
      name: string;
      scope: 'loc scope;
      mutable resolved: 'loc binding_node option;
    }

and 'loc exports =
  | Exports of {
      mutable kind: 'loc module_kind;
      mutable types: 'loc export_type smap;
      mutable type_stars: ('loc loc_node * module_ref_node) list;
      strict: bool;
      platform_availability_set: Platform_set.t option;
    }

(* The Global scope constructor is used only when parsing a library definition.
 * For implementation files, the top-level scope will be a Module. *)
and 'loc scope =
  | Global of {
      mutable values: 'loc binding_node SMap.t;
      mutable types: 'loc binding_node SMap.t;
      mutable modules: ('loc loc_node * 'loc exports) SMap.t;
    }
  | DeclareModule of {
      mutable values: 'loc binding_node SMap.t;
      mutable types: 'loc binding_node SMap.t;
      parent: 'loc scope;
      exports: 'loc exports;
    }
  | DeclareNamespace of {
      mutable values: 'loc binding_node SMap.t;
      mutable types: 'loc binding_node SMap.t;
      parent: 'loc scope;
    }
  | Module of {
      mutable values: 'loc binding_node SMap.t;
      mutable types: 'loc binding_node SMap.t;
      exports: 'loc exports;
    }
  | Lexical of {
      mutable values: 'loc binding_node SMap.t;
      mutable types: 'loc binding_node SMap.t;
      parent: 'loc scope;
    }
  | ConditionalTypeExtends of 'loc conditional_type_extends

and 'loc conditional_type_extends = {
  infer_type_names: 'loc SMap.t;
  mutable infer_tparams_rev: ('loc loc_node, 'loc parsed) tparam list;
  parent: 'loc scope;
}

(* When resolving names in the next phase, it will become possible to visit a
 * given binding multiple times, or even circularly. The node indirection here,
 * provided by the Compact_table module, deals with these cycles automatically.
 *
 * Bodies of definitions are stored as lazy thunks, to avoid parsing things
 * which are not reachable from exports.
 *)
and 'loc local_binding =
  | TypeBinding of {
      id_loc: 'loc loc_node;
      def: 'loc parsed_def Lazy.t;
    }
  | VarBinding of {
      id_loc: 'loc loc_node;
      name: string;
      def: 'loc parsed Lazy.t;
    }
  | LetConstBinding of {
      id_loc: 'loc loc_node;
      name: string;
      def: 'loc parsed Lazy.t;
    }
  | ConstRefBinding of {
      id_loc: 'loc loc_node;
      name: string;
      ref: 'loc ref;
    }
  | ConstFunBinding of {
      id_loc: 'loc loc_node;
      name: string;
      loc: 'loc loc_node;
      async: bool;
      generator: bool;
      def: ('loc loc_node, 'loc parsed) fun_sig Lazy.t;
      statics: ('loc loc_node * 'loc parsed) smap;
    }
  | ClassBinding of {
      id_loc: 'loc loc_node;
      name: string;
      def: ('loc loc_node, 'loc parsed) class_sig Lazy.t;
    }
  | DeclareClassBinding of {
      id_loc: 'loc loc_node;
      name: string;
      def: ('loc loc_node, 'loc parsed) declare_class_sig Lazy.t;
    }
  | FunBinding of {
      id_loc: 'loc loc_node;
      name: string;
      async: bool;
      generator: bool;
      fn_loc: 'loc loc_node;
      def: ('loc loc_node, 'loc parsed) fun_sig Lazy.t;
      statics: ('loc loc_node * 'loc parsed) smap;
    }
  | DeclareFunBinding of {
      name: string;
      defs_rev: ('loc loc_node * 'loc loc_node * ('loc loc_node, 'loc parsed) fun_sig Lazy.t) Nel.t;
    }
  | ComponentBinding of {
      id_loc: 'loc loc_node;
      name: string;
      fn_loc: 'loc loc_node;
      def: ('loc loc_node, 'loc parsed) component_sig Lazy.t option;
    }
  | EnumBinding of {
      id_loc: 'loc loc_node;
      name: string;
      def: (enum_rep * 'loc loc_node smap * bool) Lazy.t option;
    }
  | NamespaceBinding of {
      id_loc: 'loc loc_node;
      name: string;
      values: ('loc loc_node * 'loc parsed) smap;
      types: ('loc loc_node * 'loc parsed) smap;
    }

and 'loc remote_binding =
  | ImportBinding of {
      id_loc: 'loc loc_node;
      name: string;
      mref: module_ref_node;
      remote: string;
    }
  | ImportTypeBinding of {
      id_loc: 'loc loc_node;
      name: string;
      mref: module_ref_node;
      remote: string;
    }
  | ImportTypeofBinding of {
      id_loc: 'loc loc_node;
      name: string;
      mref: module_ref_node;
      remote: string;
    }
  | ImportNsBinding of {
      id_loc: 'loc loc_node;
      name: string;
      mref: module_ref_node;
    }
  | ImportTypeofNsBinding of {
      id_loc: 'loc loc_node;
      name: string;
      mref: module_ref_node;
    }

and 'loc pattern =
  | PDef of 'loc pattern_def_node Lazy.t
  | PropP of {
      def: 'loc pattern_node;
      id_loc: 'loc loc_node;
      name: string;
    }
  | ComputedP of {
      def: 'loc pattern_node;
      elem: 'loc pattern_def_node;
    }
  | UnsupportedLiteralP of 'loc loc_node
  | ObjRestP of {
      def: 'loc pattern_node;
      loc: 'loc loc_node;
      xs: string list;
    }
  | IndexP of {
      def: 'loc pattern_node;
      loc: 'loc loc_node;
      i: int;
    }
  | ArrRestP of {
      def: 'loc pattern_node;
      loc: 'loc loc_node;
      i: int;
    }

and 'loc binding_node =
  | LocalBinding of 'loc local_def_node
  | RemoteBinding of 'loc remote_ref_node

and 'loc loc_node = 'loc Locs.node

and 'loc local_def_node = 'loc local_binding Local_defs.node

and module_ref_node = string Module_refs.node

and 'loc remote_ref_node = 'loc remote_binding Remote_refs.node

and 'loc pattern_def_node = 'loc parsed Pattern_defs.node

and 'loc pattern_node = 'loc pattern Patterns.node

and 'loc tables = {
  locs: 'loc Locs.builder;
  local_defs: 'loc local_binding Local_defs.builder;
  module_refs: string Module_refs.Interned.builder;
  remote_refs: 'loc remote_binding Remote_refs.builder;
  pattern_defs: 'loc parsed Pattern_defs.builder;
  patterns: 'loc pattern Patterns.builder;
}

let ignore2 _ _ = ()

let create_tables () =
  {
    locs = Locs.create ();
    local_defs = Local_defs.create ();
    module_refs = Module_refs.Interned.create ();
    remote_refs = Remote_refs.create ();
    pattern_defs = Pattern_defs.create ();
    patterns = Patterns.create ();
  }

let push_loc tbls = Locs.push tbls.locs

let push_local_def tbls = Local_defs.push tbls.local_defs

let push_module_ref tbls = Module_refs.Interned.push tbls.module_refs

let push_remote_ref tbls = Remote_refs.push tbls.remote_refs

let push_pattern_def tbls = Pattern_defs.push tbls.pattern_defs

let push_pattern tbls = Patterns.push tbls.patterns

let splice tbls id_loc f = Locs.splice id_loc (fun locs -> f { tbls with locs })

let polarity = function
  | None -> Polarity.Neutral
  | Some (_, Ast.Variance.{ kind = InOut; comments = _ }) -> Polarity.Neutral
  | Some (_, Ast.Variance.{ kind = Readonly | Out; comments = _ }) -> Polarity.Positive
  | Some (_, Ast.Variance.{ kind = In; comments = _ }) -> Polarity.Negative
  | Some (_, { Ast.Variance.kind = Ast.Variance.Plus; comments = _ }) -> Polarity.Positive
  | Some (_, { Ast.Variance.kind = Ast.Variance.Minus; comments = _ }) -> Polarity.Negative

let id_name (_, { Ast.Identifier.name; comments = _ }) = name

let val_ref ~type_only scope ref_loc name =
  ValRef { type_only; ref = Ref { ref_loc; name; scope; resolved = None } }

let merge_accessors a b =
  match (a, b) with
  | (Get (get_loc, get_t), Set (set_loc, set_t))
  | (Set (set_loc, set_t), Get (get_loc, get_t))
  | (GetSet (get_loc, get_t, _, _), Set (set_loc, set_t))
  | (GetSet (_, _, set_loc, set_t), Get (get_loc, get_t)) ->
    GetSet (get_loc, get_t, set_loc, set_t)
  | (_, x) -> x

let extract_string_literal =
  let module E = Ast.Expression in
  let module S = Ast.StringLiteral in
  let module T = E.TemplateLiteral in
  function
  | E.StringLiteral { S.value = x; _ } -> Some x
  | E.TemplateLiteral { T.quasis; expressions = []; comments = _ } -> begin
    match quasis with
    | [(_, { T.Element.value = { T.Element.cooked = x; _ }; _ })] -> Some x
    | _ -> None
  end
  | _ -> None

let extract_number_literal =
  let module E = Ast.Expression in
  let module N = Ast.NumberLiteral in
  function
  | E.NumberLiteral { N.value = x; raw; _ } -> Some (x, raw)
  | _ -> None

(* The parser determines the type of a module based on the kinds of exports it
 * sees. All modules start out as an empty CJS module, but transition to an
 * explicit CJS module if we see module.exports or an ES module if we see import
 * or export statements.
 *
 * Conflicting information leads to a indeterminate module type, which this
 * phase simply ignores, as an error should be realized in another part of type
 * checking.
 *
 * Note that this logic is very similar to the logic found in File_sig and also
 * Module_info. Ideally these modules would share the one implementation of
 * this logic.
 *)
module Exports = struct
  let create ~strict ~platform_availability_set =
    Exports
      {
        kind = UnknownModule;
        types = SMap.empty;
        type_stars = [];
        strict;
        platform_availability_set;
      }

  let add name t (Exports e) =
    match e.kind with
    | ESModule { names; stars } ->
      let names = SMap.add name t names in
      e.kind <- ESModule { names; stars }
    | UnknownModule ->
      let names = SMap.singleton name t in
      let stars = [] in
      e.kind <- ESModule { names; stars }
    | CJSModule _
    | CJSModuleProps _
    | CJSDeclareModule _ ->
      (* indeterminate *)
      ()

  let add_star loc mref (Exports e) =
    match e.kind with
    | ESModule { names; stars } ->
      let stars = (loc, mref) :: stars in
      e.kind <- ESModule { names; stars }
    | UnknownModule ->
      let names = SMap.empty in
      let stars = [(loc, mref)] in
      e.kind <- ESModule { names; stars }
    | CJSModule _
    | CJSModuleProps _
    | CJSDeclareModule _ ->
      (* indeterminate *)
      ()

  let cjs_clobber t (Exports e) =
    match e.kind with
    | UnknownModule
    | CJSModule _
    | CJSModuleProps _
    | CJSDeclareModule _ ->
      e.kind <- CJSModule t
    | ESModule _ ->
      (* indeterminate *)
      ()

  let cjs_set_prop ~assign name prop (Exports e) =
    match e.kind with
    | UnknownModule
    | CJSDeclareModule _ ->
      let props = SMap.singleton name prop in
      e.kind <- CJSModuleProps props
    | CJSModuleProps props ->
      let props = SMap.add name prop props in
      e.kind <- CJSModuleProps props
    | CJSModule t -> e.kind <- CJSModule (assign name prop t)
    | ESModule _ ->
      (* indeterminate *)
      ()

  let cjs_declare_module_set_prop name prop (Exports e) =
    match e.kind with
    | UnknownModule ->
      let props = SMap.singleton name prop in
      e.kind <- CJSDeclareModule props
    | CJSDeclareModule props ->
      let props = SMap.add name prop props in
      e.kind <- CJSDeclareModule props
    | CJSModuleProps _
    | CJSModule _
    | ESModule _ ->
      (* indeterminate *)
      ()

  let add_type name t (Exports e) = e.types <- SMap.add name t e.types

  let add_type_star loc mref (Exports e) = e.type_stars <- (loc, mref) :: e.type_stars
end

module Scope = struct
  let create_global () = Global { values = SMap.empty; types = SMap.empty; modules = SMap.empty }

  let create_module ~strict ~platform_availability_set =
    Module
      {
        values = SMap.empty;
        types = SMap.empty;
        exports = Exports.create ~strict ~platform_availability_set;
      }

  let push_lex parent = Lexical { parent; values = SMap.empty; types = SMap.empty }

  let push_declare_namespace parent =
    DeclareNamespace { parent; values = SMap.empty; types = SMap.empty }

  let push_declare_module loc name parent =
    let exports = Exports.create ~strict:true ~platform_availability_set:None in
    begin
      match parent with
      | Global g ->
        g.modules <-
          SMap.update
            name
            (function
              | None -> Some (loc, exports)
              | Some existing -> Some existing)
            g.modules
      | _ -> ()
    end;
    DeclareModule { values = SMap.empty; types = SMap.empty; exports; parent }

  let parent_opt = function
    | DeclareModule { parent; _ }
    | Lexical { parent; _ }
    | ConditionalTypeExtends { parent; _ }
    | DeclareNamespace { parent; _ } ->
      Some parent
    | Global _
    | Module _ ->
      None

  let modify_exports f = function
    | Module scope -> f scope.exports
    | DeclareModule scope -> f scope.exports
    | DeclareNamespace _
    | Global _
    | Lexical _
    | ConditionalTypeExtends _ ->
      ()

  let builtins_exn = function
    | Global { values; types; modules } -> (values, types, modules)
    | DeclareModule _
    | DeclareNamespace _
    | Module _
    | Lexical _
    | ConditionalTypeExtends _ ->
      raise Not_found

  let exports_exn = function
    | Module { exports; _ } -> exports
    | Global _
    | DeclareModule _
    | DeclareNamespace _
    | Lexical _
    | ConditionalTypeExtends _ ->
      raise Not_found

  let block_scoped = function
    | TypeBinding _
    | VarBinding _ ->
      false
    | LetConstBinding _
    | ConstFunBinding _
    | ConstRefBinding _
    | FunBinding _
    | DeclareFunBinding _
    | ComponentBinding _
    | ClassBinding _
    | DeclareClassBinding _
    | EnumBinding _
    | NamespaceBinding _ ->
      true

  let value_binding kind id_loc name def =
    let open Ast.Variable in
    match kind with
    | Var -> VarBinding { id_loc; name; def }
    | Let
    | Const ->
      LetConstBinding { id_loc; name; def }

  let import_binding kind id_loc name mref ~remote =
    let module ID = Ast.Statement.ImportDeclaration in
    match kind with
    | ID.ImportValue -> ImportBinding { id_loc; name; mref; remote }
    | ID.ImportType -> ImportTypeBinding { id_loc; name; mref; remote }
    | ID.ImportTypeof -> ImportTypeofBinding { id_loc; name; mref; remote }

  let import_ns_binding kind id_loc name mref =
    let module ID = Ast.Statement.ImportDeclaration in
    match kind with
    | ID.ImportValue -> ImportNsBinding { id_loc; name; mref }
    | ID.ImportTypeof -> ImportTypeofNsBinding { id_loc; name; mref }
    | ID.ImportType -> failwith "unexpected import type *"

  let bind ~type_only scope name f =
    let bind_value values types =
      match SMap.find_opt name values with
      | Some _ -> SMap.update name f values
      | None ->
        (match SMap.find_opt name types with
        | None -> SMap.update name f values
        | Some _ -> values)
    in
    let bind_type values types =
      match SMap.find_opt name types with
      | Some _ -> SMap.update name f types
      | None ->
        (match SMap.find_opt name values with
        | None -> SMap.update name f types
        | Some _ -> types)
    in
    if type_only then
      match scope with
      | Global scope -> scope.types <- bind_type scope.values scope.types
      | DeclareModule scope -> scope.types <- bind_type scope.values scope.types
      | DeclareNamespace scope -> scope.types <- bind_type scope.values scope.types
      | Module scope -> scope.types <- bind_type scope.values scope.types
      | Lexical scope -> scope.types <- bind_type scope.values scope.types
      | ConditionalTypeExtends _ -> ()
    else
      match scope with
      | Global scope -> scope.values <- bind_value scope.values scope.types
      | DeclareModule scope -> scope.values <- bind_value scope.values scope.types
      | DeclareNamespace scope -> scope.values <- bind_value scope.values scope.types
      | Module scope -> scope.values <- bind_value scope.values scope.types
      | Lexical scope -> scope.values <- bind_value scope.values scope.types
      | ConditionalTypeExtends _ -> ()

  let rec lookup_value scope name =
    match scope with
    | Global { values; _ }
    | Module { values; _ } ->
      Base.Option.map ~f:(fun binding -> (binding, scope)) (SMap.find_opt name values)
    | ConditionalTypeExtends { parent; _ } -> lookup_value parent name
    | DeclareModule { parent; values; _ }
    | DeclareNamespace { parent; values; _ }
    | Lexical { parent; values; types = _ } ->
      (match SMap.find_opt name values with
      | Some binding -> Some (binding, scope)
      | None -> lookup_value parent name)

  let rec lookup_type scope name =
    let lookup_scope name values types =
      match SMap.find_opt name types with
      | Some _ as v -> v
      | _ -> SMap.find_opt name values
    in
    match scope with
    | Global { values; types; _ } ->
      Base.Option.map ~f:(fun binding -> (binding, scope)) (lookup_scope name values types)
    | Module { values; types; _ } ->
      Base.Option.map ~f:(fun binding -> (binding, scope)) (lookup_scope name values types)
    | ConditionalTypeExtends { parent; _ } -> lookup_type parent name
    | DeclareModule { parent; values; types; _ }
    | DeclareNamespace { parent; values; types }
    | Lexical { parent; values; types } ->
      (match lookup_scope name values types with
      | Some binding -> Some (binding, scope)
      | None -> lookup_type parent name)

  let rec find_host scope b =
    match scope with
    | Global _
    | DeclareModule _
    | DeclareNamespace _
    | Module _ ->
      scope
    | ConditionalTypeExtends { parent; _ } -> find_host parent b
    | Lexical { parent; _ } ->
      if block_scoped b then
        scope
      else
        find_host parent b

  let rec scope_of_infer_name scope name loc =
    match scope with
    | Global _
    | DeclareModule _
    | DeclareNamespace _
    | Module _ ->
      None
    | Lexical { parent; _ } -> scope_of_infer_name parent name loc
    | ConditionalTypeExtends ({ infer_type_names; _ } as scope) ->
      (match SMap.find_opt name infer_type_names with
      | Some l when l = loc -> Some scope
      | _ -> None)

  let bind_local scope tbls name def k =
    let host = find_host scope def in
    bind host name (function
        | Some _ as existing_binding -> existing_binding
        | None ->
          let node = push_local_def tbls def in
          k name node;
          Some (LocalBinding node)
        )

  let bind_remote scope tbls name ref =
    bind scope name (function
        | Some _ as existing_binding -> existing_binding
        | None ->
          let node = push_remote_ref tbls ref in
          Some (RemoteBinding node)
        )

  let bind_type scope tbls id_loc name def =
    bind_local ~type_only:true scope tbls name (TypeBinding { id_loc; def })

  let bind_class scope tbls id_loc name def =
    bind_local ~type_only:false scope tbls name (ClassBinding { id_loc; name; def })

  let bind_declare_class scope tbls id_loc name def =
    bind_local ~type_only:false scope tbls name (DeclareClassBinding { id_loc; name; def })

  let bind_enum scope tbls id_loc name def =
    bind_local ~type_only:false scope tbls name (EnumBinding { id_loc; name; def })

  (* Function declarations preceded by declared functions are taken to have the
   * type of the declared functions. This is a weird special case aimed to
   * support overloaded signatures. *)
  let bind_function scope tbls id_loc fn_loc name ~async ~generator ~hook:_ def k =
    bind ~type_only:false scope name (fun binding_opt ->
        match binding_opt with
        | None ->
          let statics = SMap.empty in
          let def : _ local_binding =
            FunBinding { id_loc; name; async; generator; fn_loc; def; statics }
          in
          let node = push_local_def tbls def in
          k name node;
          Some (LocalBinding node)
        | Some (RemoteBinding _) -> binding_opt
        | Some (LocalBinding node) ->
          (match Local_defs.value node with
          | DeclareFunBinding _ -> k name node
          | _ -> ());
          binding_opt
    )

  (* Multiple declared functions with the same name in the same scope define an
   * overloaded function. Note that declared functions are block scoped, so we
   * don't need to walk the scope chain since the scope argument is certainly
   * the host scope. *)
  let bind_declare_function scope tbls id_loc fn_loc name def k =
    bind ~type_only:false scope name (fun binding_opt ->
        match binding_opt with
        | None ->
          let defs_rev = Nel.one (id_loc, fn_loc, def) in
          let def = DeclareFunBinding { name; defs_rev } in
          let node = push_local_def tbls def in
          k name node;
          Some (LocalBinding node)
        | Some (RemoteBinding _) -> binding_opt
        | Some (LocalBinding node) ->
          Local_defs.modify node (function
              | DeclareFunBinding { name; defs_rev } ->
                k name node;
                let defs_rev = Nel.cons (id_loc, fn_loc, def) defs_rev in
                DeclareFunBinding { name; defs_rev }
              | def -> def
              );
          binding_opt
    )

  let bind_component scope tbls id_loc fn_loc name def =
    bind_local ~type_only:false scope tbls name (ComponentBinding { id_loc; fn_loc; name; def })

  let bind_var scope tbls kind id_loc name def =
    bind_local ~type_only:false scope tbls name (value_binding kind id_loc name def)

  let bind_const scope tbls id_loc name def =
    bind_local ~type_only:false scope tbls name (LetConstBinding { id_loc; name; def })

  let bind_const_ref scope tbls id_loc name ref_loc ref_name ref_scope =
    let ref = Ref { ref_loc; name = ref_name; scope = ref_scope; resolved = None } in
    bind_local ~type_only:false scope tbls name (ConstRefBinding { id_loc; name; ref })

  let bind_const_fun scope tbls id_loc name loc ~async ~generator def =
    let statics = SMap.empty in
    bind_local
      ~type_only:false
      scope
      tbls
      name
      (ConstFunBinding { id_loc; name; loc; async; generator; def; statics })

  let bind_import scope tbls kind id_loc ~local ~remote mref =
    let mref = push_module_ref tbls mref in
    let type_only =
      let open Ast.Statement.ImportDeclaration in
      match kind with
      | ImportValue -> false
      | ImportType
      | ImportTypeof ->
        true
    in
    bind_remote ~type_only scope tbls local (import_binding kind id_loc local mref ~remote)

  let bind_import_ns scope tbls kind id_loc name mref =
    let mref = push_module_ref tbls mref in
    let type_only =
      let open Ast.Statement.ImportDeclaration in
      match kind with
      | ImportValue -> false
      | ImportType
      | ImportTypeof ->
        true
    in
    bind_remote ~type_only scope tbls name (import_ns_binding kind id_loc name mref)

  let rec assign_binding =
    let f prop_name prop def =
      match def with
      | TypeBinding _
      | VarBinding _
      | LetConstBinding _
      | ClassBinding _
      | DeclareClassBinding _
      | DeclareFunBinding _
      | EnumBinding _
      | ComponentBinding _
      | NamespaceBinding _ ->
        def
      | FunBinding fn ->
        let statics = SMap.add prop_name prop fn.statics in
        FunBinding { fn with statics }
      | ConstFunBinding fn ->
        let statics = SMap.add prop_name prop fn.statics in
        ConstFunBinding { fn with statics }
      | ConstRefBinding { ref = Ref { name = ref_name; scope; _ }; _ } ->
        assign_binding prop_name prop ref_name scope;
        def
    in
    fun prop_name prop ref_name scope ->
      match lookup_value scope ref_name with
      | None -> ()
      | Some (RemoteBinding _, _) -> ()
      | Some (LocalBinding node, found_scope) ->
        if scope == found_scope then
          Local_defs.modify node (f prop_name prop)
        else
          ()

  let assign prop_name prop = function
    | Value (FunExpr fn) ->
      let statics = SMap.add prop_name prop fn.statics in
      Value (FunExpr { fn with statics })
    | ValRef { type_only = false; ref = Ref { name = ref_name; scope; _ } } as vref ->
      assign_binding prop_name prop ref_name scope;
      vref
    | x -> x

  let export_ref scope tbls kind ~local ~exported =
    let (ref_loc, name, exported) =
      match exported with
      | None ->
        let (id_loc, { Ast.Identifier.name; comments = _ }) = local in
        (id_loc, name, name)
      | Some (id_loc, { Ast.Identifier.name = exported; comments = _ }) ->
        let (_, { Ast.Identifier.name; comments = _ }) = local in
        (id_loc, name, exported)
    in
    let ref_loc = push_loc tbls ref_loc in
    let ref = Ref { ref_loc; name; scope; resolved = None } in
    let f =
      match kind with
      | Ast.Statement.ExportType -> Exports.add_type exported (ExportTypeRef ref)
      | Ast.Statement.ExportValue -> Exports.add exported (ExportRef ref)
    in
    modify_exports f scope

  let export_binding scope kind name binding =
    let f =
      match kind with
      | Ast.Statement.ExportType -> Exports.add_type name (ExportTypeBinding binding)
      | Ast.Statement.ExportValue -> Exports.add name (ExportBinding binding)
    in
    modify_exports f scope

  let export_default scope default_loc def =
    let f = Exports.add "default" (ExportDefault { default_loc; def }) in
    modify_exports f scope

  let export_default_binding scope default_loc name binding =
    let f = Exports.add "default" (ExportDefaultBinding { default_loc; name; binding }) in
    modify_exports f scope

  let export_from scope tbls kind mref ~local ~exported =
    let mref = push_module_ref tbls mref in
    let (id_loc, name, remote) =
      match exported with
      | None ->
        let (id_loc, { Ast.Identifier.name; comments = _ }) = local in
        (id_loc, name, name)
      | Some (id_loc, { Ast.Identifier.name; comments = _ }) ->
        let (_, { Ast.Identifier.name = remote; comments = _ }) = local in
        (id_loc, name, remote)
    in
    let id_loc = push_loc tbls id_loc in
    let f =
      match kind with
      | Ast.Statement.ExportType ->
        let node = push_remote_ref tbls (ImportTypeBinding { id_loc; name; mref; remote }) in
        Exports.add_type name (ExportTypeFrom node)
      | Ast.Statement.ExportValue ->
        let node = push_remote_ref tbls (ImportBinding { id_loc; name; mref; remote }) in
        Exports.add name (ExportFrom node)
    in
    modify_exports f scope

  let export_ns scope tbls kind mref id_loc name =
    match kind with
    | Ast.Statement.ExportType -> failwith "unexpected export type * as"
    | Ast.Statement.ExportValue ->
      let mref = push_module_ref tbls mref in
      let node = push_remote_ref tbls (ImportNsBinding { id_loc; name; mref }) in
      modify_exports (Exports.add name (ExportFrom node)) scope

  let export_star scope tbls kind loc mref =
    let mref = push_module_ref tbls mref in
    let add =
      match kind with
      | Ast.Statement.ExportType -> Exports.add_type_star
      | Ast.Statement.ExportValue -> Exports.add_star
    in
    modify_exports (add loc mref) scope

  let cjs_clobber scope t = modify_exports (Exports.cjs_clobber t) scope

  let cjs_set_prop scope name prop = modify_exports (Exports.cjs_set_prop ~assign name prop) scope

  (* a `declare module` that has no explicit exports via `declare module.exports =` or
     `declare exports` defaults to exporting everything as CJS named properties. *)
  let finalize_declare_module_exports_exn = function
    | DeclareModule { values; types; exports; parent = _ } as scope ->
      (match exports with
      | Exports { kind = ESModule _; _ } ->
        (* has explicit exports so do nothing here *)
        ()
      | Exports { kind = CJSModule _ | CJSModuleProps _; _ } ->
        (* Always auto export all local types for CJS declare modules *)
        modify_exports
          (fun exports ->
            SMap.iter
              (fun name binding ->
                match binding with
                | LocalBinding node ->
                  (match Local_defs.value node with
                  | TypeBinding _ -> Exports.add_type name (ExportTypeBinding node) exports
                  | _ -> ())
                | RemoteBinding _ -> ())
              types)
          scope
      | Exports { kind = UnknownModule; _ } ->
        (* add a CJS export for each declared binding *)
        modify_exports
          (fun exports ->
            SMap.iter
              (fun name binding ->
                match binding with
                | LocalBinding node ->
                  (match Local_defs.value node with
                  | VarBinding _
                  | LetConstBinding _
                  | ConstRefBinding _
                  | ConstFunBinding _
                  | ClassBinding _
                  | DeclareClassBinding _
                  | FunBinding _
                  | DeclareFunBinding _
                  | ComponentBinding _
                  | EnumBinding _
                  | NamespaceBinding _ ->
                    Exports.cjs_declare_module_set_prop name node exports
                  | TypeBinding _ -> ())
                | RemoteBinding _ -> ())
              values)
          scope;
        modify_exports
          (fun exports ->
            SMap.iter
              (fun name binding ->
                match binding with
                | LocalBinding node ->
                  (match Local_defs.value node with
                  | VarBinding _
                  | LetConstBinding _
                  | ConstRefBinding _
                  | ConstFunBinding _
                  | ClassBinding _
                  | DeclareClassBinding _
                  | FunBinding _
                  | DeclareFunBinding _
                  | ComponentBinding _
                  | EnumBinding _
                  | NamespaceBinding _ ->
                    ()
                  | TypeBinding _ -> Exports.add_type name (ExportTypeBinding node) exports)
                | RemoteBinding _ -> ())
              types)
          scope
      | Exports { kind = CJSDeclareModule _; _ } ->
        (* is already the right kind? shouldn't happen *)
        failwith "only call finalize_declare_module_exports_exn once per DeclareModule")
    | DeclareNamespace _
    | Global _
    | Module _
    | Lexical _
    | ConditionalTypeExtends _ ->
      failwith "expected DeclareModule to still be the scope"

  (* a `declare namespace` exports every binding. *)
  let finalize_declare_namespace_exn ~is_type_only scope tbls id_loc name =
    match scope with
    | DeclareNamespace { values; types; parent; _ } ->
      let values =
        SMap.fold
          (fun name binding acc ->
            match binding with
            | LocalBinding node ->
              let local_binding = Local_defs.value node in
              (match local_binding with
              | VarBinding { id_loc; _ }
              | LetConstBinding { id_loc; _ }
              | ConstRefBinding { id_loc; _ }
              | ConstFunBinding { id_loc; _ }
              | ClassBinding { id_loc; _ }
              | DeclareClassBinding { id_loc; _ }
              | FunBinding { id_loc; _ }
              | ComponentBinding { id_loc; _ }
              | EnumBinding { id_loc; _ }
              | NamespaceBinding { id_loc; _ } ->
                SMap.add name (id_loc, val_ref ~type_only:false scope id_loc name) acc
              | DeclareFunBinding { defs_rev; _ } ->
                let (id_loc, _, _) = Nel.last defs_rev in
                SMap.add name (id_loc, val_ref ~type_only:false scope id_loc name) acc
              | TypeBinding _ -> acc)
            | RemoteBinding _ -> acc)
          values
          SMap.empty
      in
      let types =
        SMap.fold
          (fun name binding acc ->
            match binding with
            | LocalBinding node ->
              let local_binding = Local_defs.value node in
              (match local_binding with
              | VarBinding _
              | LetConstBinding _
              | ConstRefBinding _
              | ConstFunBinding _
              | ClassBinding _
              | DeclareClassBinding _
              | FunBinding _
              | ComponentBinding _
              | EnumBinding _
              | NamespaceBinding _
              | DeclareFunBinding _ ->
                acc
              | TypeBinding { id_loc; _ } ->
                SMap.add name (id_loc, val_ref ~type_only:true scope id_loc name) acc)
            | RemoteBinding _ -> acc)
          types
          SMap.empty
      in
      bind_local
        ~type_only:is_type_only
        parent
        tbls
        name
        (NamespaceBinding { id_loc; name; values; types })
    | _ -> failwith "The scope must be lexical"
end

module ObjAnnotAcc = struct
  type 'loc prop = ('loc loc_node, 'loc parsed) obj_annot_prop

  type 'loc dict = 'loc parsed obj_annot_dict

  type 'loc elem = ('loc loc_node, 'loc parsed) obj_spread_annot_elem

  type 'loc t = {
    dict: 'loc dict option;
    props: 'loc prop SMap.t;
    tail: 'loc elem list;
    proto: ('loc loc_node * 'loc parsed) option;
    calls_rev: 'loc parsed list;
  }

  let empty = { dict = None; props = SMap.empty; tail = []; proto = None; calls_rev = [] }

  let empty_slice = ObjSpreadAnnotSlice { dict = None; props = SMap.empty }

  let head_slice { dict; props; _ } =
    if dict = None && SMap.is_empty props then
      None
    else
      Some (ObjSpreadAnnotSlice { dict; props })

  let map_props f acc = { acc with props = f acc.props }

  let add_field name id_loc polarity t acc =
    let f = SMap.add name (ObjAnnotField (id_loc, t, polarity)) in
    map_props f acc

  let add_method name id_loc fn_loc def acc =
    let f = SMap.add name (ObjAnnotMethod { id_loc; fn_loc; def }) in
    map_props f acc

  let add_accessor_helper x = function
    | Some (ObjAnnotAccess x') -> Some (ObjAnnotAccess (merge_accessors x' x))
    | _ -> Some (ObjAnnotAccess x)

  let add_accessor name x acc =
    let f = SMap.update name (add_accessor_helper x) in
    map_props f acc

  let add_dict d acc =
    match acc.dict with
    | Some _ -> acc (* invalid: multiple indexers *)
    | None -> { acc with dict = Some d }

  let add_call t acc =
    match (acc.proto, acc.calls_rev) with
    | (Some _, _) -> acc (* invalid: call after proto *)
    | (None, ts) -> { acc with calls_rev = t :: ts }

  let add_proto x acc =
    match (acc.proto, acc.calls_rev) with
    | (Some _, _) -> acc (* invalid: multiple proto *)
    | (_, _ :: _) -> acc (* invalid: proto after call *)
    | _ -> { acc with proto = Some x }

  let add_spread t acc =
    let tail =
      match head_slice acc with
      | None -> acc.tail
      | Some slice -> slice :: acc.tail
    in
    let tail = ObjSpreadAnnotElem t :: tail in
    { acc with dict = None; props = SMap.empty; tail }

  let elements_rev acc =
    match head_slice acc with
    | Some slice -> (slice, acc.tail)
    | None ->
      (match acc.tail with
      | [] -> (empty_slice, [])
      | x :: xs -> (x, xs))

  let object_type loc ~exact acc =
    match elements_rev acc with
    | (ObjSpreadAnnotSlice { dict; props }, []) ->
      let proto =
        match (acc.calls_rev, acc.proto) with
        | (t :: ts, _) -> ObjAnnotCallable { ts_rev = (t, ts) }
        | (_, Some (loc, t)) -> ObjAnnotExplicitProto (loc, t)
        | _ -> ObjAnnotImplicitProto
      in
      let obj_kind =
        match dict with
        | Some d -> IndexedObj d
        | None ->
          if exact then
            ExactObj
          else
            InexactObj
      in
      Annot (ObjAnnot { loc; props; proto; obj_kind })
    | elems_rev -> Annot (ObjSpreadAnnot { loc; exact; elems_rev })
end

module ClassAcc = struct
  type 'loc prop = ('loc loc_node, 'loc parsed) obj_value_prop

  type 'loc t = {
    static: 'loc prop SMap.t;
    proto: 'loc prop SMap.t;
    own: 'loc prop SMap.t;
  }

  let empty = { static = SMap.empty; proto = SMap.empty; own = SMap.empty }

  let map_static f acc = { acc with static = f acc.static }

  let map_proto f acc = { acc with proto = f acc.proto }

  let map_own f acc = { acc with own = f acc.own }

  let add_field ~static name id_loc polarity t acc =
    let f = SMap.add name (ObjValueField (id_loc, t, polarity)) in
    if static then
      map_static f acc
    else
      map_own f acc

  let add_method ~static name id_loc fn_loc ~async ~generator def acc =
    let f = SMap.add name (ObjValueMethod { id_loc; fn_loc; async; generator; def }) in
    if static then
      map_static f acc
    else
      map_proto f acc

  let add_accessor_helper x = function
    | Some (ObjValueAccess x') -> Some (ObjValueAccess (merge_accessors x' x))
    | _ -> Some (ObjValueAccess x)

  let add_accessor ~static name x acc =
    let f = SMap.update name (add_accessor_helper x) in
    if static then
      map_static f acc
    else
      map_proto f acc

  let class_def tparams extends implements { static; proto; own } =
    ClassSig
      { tparams; extends; implements; static_props = static; proto_props = proto; own_props = own }
end

module DeclareClassAcc = struct
  type 'loc prop = ('loc loc_node, 'loc parsed) interface_prop

  type 'loc calls = 'loc parsed list

  type 'loc t = {
    static: 'loc prop SMap.t;
    proto: 'loc prop SMap.t;
    own: 'loc prop SMap.t;
    scalls: 'loc calls;
    calls: 'loc calls;
    dict: 'loc parsed obj_annot_dict option;
    static_dict: 'loc parsed obj_annot_dict option;
  }

  let empty =
    {
      static = SMap.empty;
      proto = SMap.empty;
      own = SMap.empty;
      scalls = [];
      calls = [];
      dict = None;
      static_dict = None;
    }

  let map_static f acc = { acc with static = f acc.static }

  let map_proto f acc = { acc with proto = f acc.proto }

  let map_own f acc = { acc with own = f acc.own }

  let add_field ~static name id_loc polarity t acc =
    let f = SMap.add name (InterfaceField (Some id_loc, t, polarity)) in
    if static then
      map_static f acc
    else
      map_own f acc

  let add_indexer ~static dict acc =
    if static then
      match acc.static_dict with
      | Some _ -> acc (* invalid: multiple indexers *)
      | None -> { acc with static_dict = Some dict }
    else
      match acc.dict with
      | Some _ -> acc (* invalid: multiple indexers *)
      | None -> { acc with dict = Some dict }

  let add_proto_field name id_loc polarity t acc =
    map_proto (SMap.add name (InterfaceField (Some id_loc, t, polarity))) acc

  let append_method_helper m = function
    | Some (InterfaceMethod ms) -> Some (InterfaceMethod (Nel.cons m ms))
    | _ -> Some (InterfaceMethod (Nel.one m))

  let append_method ~static name id_loc fn_loc def acc =
    let f = SMap.update name (append_method_helper (id_loc, fn_loc, def)) in
    if static then
      map_static f acc
    else
      map_proto f acc

  let add_accessor_helper x = function
    | Some (InterfaceAccess x') -> Some (InterfaceAccess (merge_accessors x' x))
    | _ -> Some (InterfaceAccess x)

  let add_accessor ~static name x acc =
    let f = SMap.update name (add_accessor_helper x) in
    if static then
      map_static f acc
    else
      map_proto f acc

  let append_call ~static t acc =
    let f = List.cons t in
    if static then
      { acc with scalls = f acc.scalls }
    else
      { acc with calls = f acc.calls }

  let declare_class_def tparams extends mixins implements acc =
    DeclareClassSig
      {
        tparams;
        extends;
        mixins;
        implements;
        static_props = acc.static;
        own_props = acc.own;
        proto_props = acc.proto;
        static_calls = acc.scalls;
        calls = acc.calls;
        dict = acc.dict;
        static_dict = acc.static_dict;
      }
end

module InterfaceAcc = struct
  type 'loc prop = ('loc loc_node, 'loc parsed) interface_prop

  type 'loc t = {
    props: 'loc prop SMap.t;
    calls: 'loc parsed list;
    dict: 'loc parsed obj_annot_dict option;
  }

  let empty = { props = SMap.empty; calls = []; dict = None }

  let map_props f acc = { acc with props = f acc.props }

  let add_field name id_loc polarity t acc =
    let f = SMap.add name (InterfaceField (Some id_loc, t, polarity)) in
    map_props f acc

  let add_indexer dict acc =
    match acc.dict with
    | Some _ -> acc (* invalid: multiple indexers *)
    | None -> { acc with dict = Some dict }

  let append_method_helper m = function
    | Some (InterfaceMethod ms) -> Some (InterfaceMethod (Nel.cons m ms))
    | _ -> Some (InterfaceMethod (Nel.one m))

  let append_method name id_loc fn_loc def acc =
    let f = SMap.update name (append_method_helper (id_loc, fn_loc, def)) in
    map_props f acc

  let add_accessor_helper x = function
    | Some (InterfaceAccess x') -> Some (InterfaceAccess (merge_accessors x' x))
    | _ -> Some (InterfaceAccess x)

  let add_accessor name x acc =
    let f = SMap.update name (add_accessor_helper x) in
    map_props f acc

  let append_call t acc =
    let f = List.cons t in
    { acc with calls = f acc.calls }

  let interface_def extends { props; calls; dict } = InterfaceSig { extends; props; calls; dict }
end

module ObjectLiteralAcc = struct
  type 'loc prop = ('loc loc_node, 'loc parsed) obj_value_prop

  type 'loc elem = ('loc loc_node, 'loc parsed) obj_value_spread_elem

  type 'loc t = {
    proto: ('loc loc_node * 'loc parsed) option;
    props: 'loc prop SMap.t;
    tail: 'loc elem list;
  }

  let empty = { proto = None; props = SMap.empty; tail = [] }

  let head_slice acc =
    if SMap.is_empty acc.props then
      None
    else
      Some (ObjValueSpreadSlice acc.props)

  let add_proto x acc = { acc with proto = Some x }

  let map_props f acc = { acc with props = f acc.props }

  let add_field name id_loc t acc =
    let f = SMap.add name (ObjValueField (id_loc, t, Polarity.Neutral)) in
    map_props f acc

  let add_method name id_loc fn_loc ~async ~generator def acc =
    let f = SMap.add name (ObjValueMethod { id_loc; fn_loc; async; generator; def }) in
    map_props f acc

  let add_accessor_helper x = function
    | Some (ObjValueAccess x') -> Some (ObjValueAccess (merge_accessors x' x))
    | _ -> Some (ObjValueAccess x)

  let add_accessor name x acc =
    let f = SMap.update name (add_accessor_helper x) in
    map_props f acc

  let add_spread t acc =
    let tail =
      match head_slice acc with
      | None -> acc.tail
      | Some slice -> slice :: acc.tail
    in
    { acc with props = SMap.empty; tail = ObjValueSpreadElem t :: tail }

  let elems_rev acc =
    match head_slice acc with
    | None -> acc.tail
    | Some slice -> slice :: acc.tail

  let object_lit loc ~frozen acc =
    match elems_rev acc with
    | [] -> Value (ObjLit { loc; frozen; proto = acc.proto; props = SMap.empty })
    | [ObjValueSpreadSlice props] -> Value (ObjLit { loc; frozen; proto = acc.proto; props })
    | x :: xs -> Value (ObjSpreadLit { loc; frozen; proto = acc.proto; elems_rev = (x, xs) })
end

module T = Ast.Type

let rec sequence f = function
  | [] -> failwith "unexpected empty sequence"
  | [expr] -> f expr
  | _ :: exprs -> sequence f exprs

let rec annot opts scope tbls xs (loc, t) =
  let (_, annot) = annot_with_loc opts scope tbls xs (loc, t) in
  annot

and annot_with_loc opts scope tbls xs (loc, t) =
  let loc = push_loc tbls loc in
  let annot =
    match t with
    | T.Any _ -> Annot (Any loc)
    | T.Mixed _ -> Annot (Mixed loc)
    | T.Empty _ -> Annot (Empty loc)
    | T.Void _ -> Annot (Void loc)
    | T.Null _ -> Annot (Null loc)
    | T.Symbol _ -> Annot (Symbol loc)
    | T.Number _ -> Annot (Number loc)
    | T.BigInt _ -> Annot (BigInt loc)
    | T.String _ -> Annot (String loc)
    | T.Boolean _ -> Annot (Boolean loc)
    | T.Unknown _ -> Annot (Mixed loc)
    | T.Never _ -> Annot (Empty loc)
    | T.Undefined _ -> Annot (Void loc)
    | T.StringLiteral { Ast.StringLiteral.value; _ } -> Annot (SingletonString (loc, value))
    | T.NumberLiteral { Ast.NumberLiteral.value; raw; _ } ->
      Annot (SingletonNumber (loc, value, raw))
    | T.BigIntLiteral { Ast.BigIntLiteral.value; raw; _ } ->
      Annot (SingletonBigInt (loc, value, raw))
    | T.BooleanLiteral { Ast.BooleanLiteral.value; _ } -> Annot (SingletonBoolean (loc, value))
    | T.Nullable { T.Nullable.argument; _ } -> Annot (Maybe (loc, annot opts scope tbls xs argument))
    | T.Array { T.Array.argument; _ } -> Annot (Array (loc, annot opts scope tbls xs argument))
    | T.Conditional t -> conditional_type opts scope tbls xs (loc, t)
    | T.Infer t -> infer_type opts scope tbls xs (loc, t)
    | T.Function f ->
      let def = function_type opts scope tbls xs f in
      Annot (FunAnnot (loc, def))
    | T.Component { T.Component.tparams; params; renders; _ } when opts.enable_component_syntax ->
      let def = component_type opts scope tbls tparams params renders in
      Annot (ComponentAnnot (loc, def))
    | T.Component _ -> Annot (Any loc)
    | T.Object o -> object_type opts scope tbls xs loc o
    | T.Interface
        {
          T.Interface.body = (_, { T.Object.properties; exact = _; inexact = _; comments = _ });
          extends;
          comments = _;
        } ->
      let def = interface_def opts scope tbls xs extends properties in
      Annot (InlineInterface (loc, def))
    | T.Generic g -> maybe_special_generic opts scope tbls xs loc g
    | T.IndexedAccess { T.IndexedAccess._object; index; _ } ->
      let obj = annot opts scope tbls xs _object in
      (match index with
      | (_, T.StringLiteral { Ast.StringLiteral.value; _ }) ->
        Annot (PropertyType { loc; obj; prop = value })
      | _ ->
        let elem = annot opts scope tbls xs index in
        Annot (ElementType { loc; obj; elem }))
    | T.OptionalIndexedAccess ia ->
      let (_, result) = optional_indexed_access opts scope tbls xs (loc, ia) in
      result
    | T.Tuple { T.Tuple.elements; _ } ->
      let elems_rev = List.rev_map (tuple_element opts scope tbls xs) elements in
      Annot (Tuple { loc; elems_rev })
    | T.Union { T.Union.types = (t0, t1, ts); _ } ->
      let t0 = annot opts scope tbls xs t0 in
      let t1 = annot opts scope tbls xs t1 in
      let ts_rev = List.rev_map (annot opts scope tbls xs) ts in
      Annot (Union { loc; t0; t1; ts = List.rev ts_rev })
    | T.Intersection { T.Intersection.types = (t0, t1, ts); _ } ->
      let t0 = annot opts scope tbls xs t0 in
      let t1 = annot opts scope tbls xs t1 in
      let ts_rev = List.rev_map (annot opts scope tbls xs) ts in
      Annot (Intersection { loc; t0; t1; ts = List.rev ts_rev })
    | T.Typeof { T.Typeof.argument = t; targs; _ } -> typeof opts scope tbls xs loc t targs
    | T.Renders { T.Renders.operator_loc = _; comments = _; argument; variant } ->
      let t = annot opts scope tbls xs argument in
      Annot (Renders (loc, t, variant))
    | T.Keyof { T.Keyof.argument; comments = _ } ->
      let t = annot opts scope tbls xs argument in
      Annot (Keys (loc, t))
    | T.ReadOnly { T.ReadOnly.argument = (_, T.Tuple _) as argument; comments = _ } ->
      let t = annot opts scope tbls xs argument in
      Annot (ReadOnly (loc, t))
    | T.ReadOnly
        { T.ReadOnly.argument = (_, T.Array { T.Array.argument; comments = _ }); comments = _ } ->
      let t = annot opts scope tbls xs argument in
      Annot (ReadOnlyArray (loc, t))
    | T.ReadOnly _ -> Annot (Any loc)
    | T.Exists _ -> Annot (Exists loc)
  in
  (loc, annot)

and typeof =
  let rec finish opts scope tbls xs typeof_loc t qname targs = function
    | [] ->
      let targs =
        Option.map
          ~f:(fun (_, { T.TypeArgs.arguments; _ }) -> List.map (annot opts scope tbls xs) arguments)
          targs
      in
      Annot (Typeof { loc = typeof_loc; qname; t; targs })
    | (id_loc, x) :: chain ->
      let id_loc = push_loc tbls id_loc in
      let t = Eval (id_loc, t, GetProp x) in
      finish opts scope tbls xs typeof_loc t (x :: qname) targs chain
  in
  let rec loop opts scope tbls xs typeof_loc targs chain = function
    | T.Typeof.Target.Qualified
        (_, { T.Typeof.Target.qualification; id = (id_loc, { Ast.Identifier.name; comments = _ }) })
      ->
      loop opts scope tbls xs typeof_loc targs ((id_loc, name) :: chain) qualification
    | T.Typeof.Target.Unqualified id ->
      let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
      let id_loc = push_loc tbls id_loc in
      let t = val_ref ~type_only:false scope id_loc name in
      finish opts scope tbls xs typeof_loc t [name] targs chain
  in
  (fun opts scope tbls xs typeof_loc expr targs -> loop opts scope tbls xs typeof_loc targs [] expr)

and tuple_element opts scope tbls xs (loc, el) =
  let loc = push_loc tbls loc in
  match el with
  | Ast.Type.Tuple.UnlabeledElement t ->
    let t = annot opts scope tbls xs t in
    TupleElement { loc; name = None; t; polarity = Polarity.Neutral; optional = false }
  | Ast.Type.Tuple.LabeledElement
      { Ast.Type.Tuple.LabeledElement.name; annot = t; variance; optional } ->
    let t = annot opts scope tbls xs t in
    let t =
      if optional then
        Annot (Optional t)
      else
        t
    in
    let name = Some (id_name name) in
    TupleElement { loc; name; t; polarity = polarity variance; optional }
  | Ast.Type.Tuple.SpreadElement { Ast.Type.Tuple.SpreadElement.name; annot = t } ->
    let t = annot opts scope tbls xs t in
    let name = Base.Option.map ~f:id_name name in
    TupleSpread { loc; name; t }

and type_guard_opt opts scope tbls xs guard =
  let (_, { T.TypeGuard.asserts = _; guard = (x, t_opt); _ }) = guard in
  match t_opt with
  | Some t ->
    let (loc, { Ast.Identifier.name; _ }) = x in
    let loc = push_loc tbls loc in
    Some ((loc, name), annot opts scope tbls xs t)
  | None ->
    (* TODO(pvekris) support assert type guards in type_sig_parse *)
    None

and type_guard_or_predicate_of_type_guard opts scope tbls xs guard =
  match type_guard_opt opts scope tbls xs guard with
  | Some p -> Some (TypeGuard p)
  | None -> None

and return_annot opts scope tbls xs = function
  | T.Function.TypeAnnotation r -> (annot opts scope tbls xs r, None)
  | T.Function.TypeGuard ((loc, _) as g) ->
    let loc = push_loc tbls loc in
    let guard = type_guard_or_predicate_of_type_guard opts scope tbls xs g in
    (Annot (Boolean loc), guard)

and function_type opts scope tbls xs f =
  let module F = T.Function in
  let {
    F.tparams = tps;
    params = (_, { F.Params.params = ps; rest = rp; this_; comments = _ });
    return = r;
    hook;
    comments = _;
  } =
    f
  in
  let (xs, tparams) = tparams opts scope tbls xs tps in
  let this_param = function_type_this_param opts scope tbls xs this_ in
  let params = function_type_params opts scope tbls xs ps in
  let rest_param = function_type_rest_param opts scope tbls xs rp in
  let (return, predicate) = return_annot opts scope tbls xs r in
  let hook =
    if hook then
      HookAnnot
    else
      NonHook
  in
  FunSig { tparams; params; rest_param; this_param; return; predicate; hook }

and function_component_type_param opts scope tbls xs t optional =
  let t = annot opts scope tbls xs t in
  if optional then
    Annot (Optional t)
  else
    t

and function_type_params =
  let module F = T.Function in
  let param opts scope tbls xs (_, p) =
    let { F.Param.name = id; annot = t; optional } = p in
    let name =
      match id with
      | None -> None
      | Some id -> Some (id_name id)
    in
    let t = function_component_type_param opts scope tbls xs t optional in
    FunParam { name; t }
  in
  let rec loop opts scope tbls xs acc = function
    | [] -> List.rev acc
    | p :: ps ->
      let p = param opts scope tbls xs p in
      loop opts scope tbls xs (p :: acc) ps
  in
  (fun opts scope tbls xs ps -> loop opts scope tbls xs [] ps)

and function_type_rest_param opts scope tbls xs =
  let module F = T.Function in
  function
  | None -> None
  | Some (loc, { F.RestParam.argument = p; comments = _ }) ->
    let loc = push_loc tbls loc in
    let (_, { F.Param.name = id; annot = t; optional }) = p in
    ignore optional;
    (* allowed by the parser, but semantically void *)
    let name =
      match id with
      | None -> None
      | Some id -> Some (id_name id)
    in
    let t = annot opts scope tbls xs t in
    Some (FunRestParam { name; loc; t })

and function_type_this_param opts scope tbls xs =
  let module F = T.Function in
  function
  | None -> None
  | Some (_, { F.ThisParam.annot = (_, t); comments = _ }) ->
    let t = annot opts scope tbls xs t in
    Some t

and component_type =
  let param opts scope tbls xs (_, p) =
    let module P = Ast.Type.Component.Param in
    let { P.name; annot = (_, annot); optional; _ } = p in
    let (name, name_loc) =
      match name with
      | Ast.Statement.ComponentDeclaration.Param.Identifier (loc, { Ast.Identifier.name; _ })
      | Ast.Statement.ComponentDeclaration.Param.StringLiteral
          (loc, { Ast.StringLiteral.value = name; _ }) ->
        (name, loc)
    in
    let name_loc = push_loc tbls name_loc in
    let t = function_component_type_param opts scope tbls xs annot optional in
    ComponentParam { name; name_loc; t }
  in
  let rec params opts scope tbls xs acc = function
    | [] -> List.rev acc
    | p :: ps ->
      let p = param opts scope tbls xs p in
      params opts scope tbls xs (p :: acc) ps
  in
  let rest_param opts scope tbls xs (_, p) =
    let module P = Ast.Type.Component.RestParam in
    let { P.annot = t; _ } = p in
    let t = annot opts scope tbls xs t in
    ComponentRestParam { t }
  in
  let renders opts scope tbls xs ret =
    match ret with
    | Ast.Type.AvailableRenders
        (loc, { Ast.Type.Renders.operator_loc = _; comments = _; variant; argument }) ->
      let loc = push_loc tbls loc in
      let t = annot opts scope tbls xs argument in
      Annot (Renders (loc, t, variant))
    | Ast.Type.MissingRenders loc ->
      let loc = push_loc tbls loc in
      Annot
        (Renders
           ( loc,
             maybe_special_unqualified_generic opts scope tbls xs loc None loc "React$Node",
             Ast.Type.Renders.Normal
           )
        )
  in
  fun opts
      scope
      tbls
      tps
      (loc, { Ast.Type.Component.Params.params = ps; rest = rp; comments = _ })
      r ->
    let (xs, tparams) = tparams opts scope tbls SSet.empty tps in
    let loc = push_loc tbls loc in
    let params = params opts scope tbls xs [] ps in
    let rest_param =
      match rp with
      | Some p -> Some (rest_param opts scope tbls xs p)
      | None -> None
    in
    let renders = renders opts scope tbls xs r in
    ComponentSig { params_loc = loc; tparams; params; rest_param; renders }

and getter_type opts scope tbls xs id_loc f =
  let module F = T.Function in
  let { F.return = r; _ } = f in
  Get (id_loc, return_annot opts scope tbls xs r |> fst)

and setter_type opts scope tbls xs id_loc f =
  let module F = T.Function in
  let { F.params = (_, { F.Params.params; _ }); _ } = f in
  match params with
  | [(_, p)] ->
    let { F.Param.annot = t; optional; _ } = p in
    let t = annot opts scope tbls xs t in
    let t =
      if optional then
        Annot (Optional t)
      else
        t
    in
    Set (id_loc, t)
  | _ -> failwith "unexpected setter"

and indexer opts scope tbls xs dict =
  let { T.Object.Indexer.id; key = k; value = v; static = _; variance; comments = _ } = dict in
  let name =
    match id with
    | None -> None
    | Some id -> Some (id_name id)
  in
  let key = annot opts scope tbls xs k in
  let value = annot opts scope tbls xs v in
  ObjDict { name; polarity = polarity variance; key; value }

and object_type =
  let module O = T.Object in
  let module Acc = ObjAnnotAcc in
  let add_method opts scope tbls xs acc id_loc name = function
    | (fn_loc, T.Function f) ->
      let fn_loc = push_loc tbls fn_loc in
      let id_loc = push_loc tbls id_loc in
      let def = function_type opts scope tbls xs f in
      Acc.add_method name id_loc fn_loc def acc
    | _ -> failwith "unexpected method"
  in
  let prop opts scope tbls xs acc p =
    let { O.Property.key; value; optional; static = _; proto = _; _method; variance; comments = _ }
        =
      p
    in
    match value with
    | O.Property.Init t ->
      let module P = Ast.Expression.Object.Property in
      begin
        match key with
        | P.Identifier (id_loc, { Ast.Identifier.name; comments = _ })
        | P.StringLiteral (id_loc, { Ast.StringLiteral.value = name; _ }) ->
          if _method then
            add_method opts scope tbls xs acc id_loc name t
          else
            let id_loc = push_loc tbls id_loc in
            let loc = push_loc tbls (fst t) in
            let t = annot opts scope tbls xs t in
            if name = "__proto__" && (not optional) && variance = None then
              Acc.add_proto (loc, t) acc
            else
              let t =
                if optional then
                  Annot (Optional t)
                else
                  t
              in
              Acc.add_field name id_loc (polarity variance) t acc
        | P.NumberLiteral _
        | P.BigIntLiteral _
        | P.PrivateName _
        | P.Computed _ ->
          acc (* unsupported object keys *)
      end
    | O.Property.Get (_, f) ->
      let module P = Ast.Expression.Object.Property in
      begin
        match key with
        | P.Identifier (id_loc, { Ast.Identifier.name; comments = _ }) ->
          let id_loc = push_loc tbls id_loc in
          let getter = getter_type opts scope tbls xs id_loc f in
          Acc.add_accessor name getter acc
        | _ -> acc (* unsupported getter syntax *)
      end
    | O.Property.Set (_, f) ->
      let module P = Ast.Expression.Object.Property in
      begin
        match key with
        | P.Identifier (id_loc, { Ast.Identifier.name; comments = _ }) ->
          let id_loc = push_loc tbls id_loc in
          let setter = setter_type opts scope tbls xs id_loc f in
          Acc.add_accessor name setter acc
        | _ -> acc (* unsupported setter syntax *)
      end
  in
  let spread opts scope tbls xs acc p =
    let { O.SpreadProperty.argument = t; comments = _ } = p in
    Acc.add_spread (annot opts scope tbls xs t) acc
  in
  let mapped_type opts scope tbls xs p =
    let (loc, { O.MappedType.source_type; prop_type; key_tparam; variance; optional; comments = _ })
        =
      p
    in
    let loc = push_loc tbls loc in
    (* The source type does not have the key_tparam in scope, but we need to parse locs in syntax
     * order or we will violate type sig invariants. I keep track of old xs to make sure we don't
     * accidentally shadow a tparam in source_type *)
    match optional with
    | Ast.Type.Object.MappedType.(
        PlusOptional | Ast.Type.Object.MappedType.Optional | NoOptionalFlag) ->
      let (key_loc, key_name) =
        let ( _,
              {
                T.TypeParam.name = (name_loc, { Ast.Identifier.name; comments = _ });
                bound = _;
                bound_kind = _;
                variance = _;
                default = _;
              }
            ) =
          key_tparam
        in
        (push_loc tbls name_loc, name)
      in

      let (source_type, inline_keyof) =
        match source_type with
        | (_, Ast.Type.Keyof { Ast.Type.Keyof.argument; comments = _ }) ->
          (annot opts scope tbls xs argument, true)
        | t -> (annot opts scope tbls xs t, false)
      in

      let (key_tparam, xs) =
        ( TParam
            {
              name_loc = key_loc;
              name = key_name;
              polarity = Polarity.Neutral;
              bound = None;
              default = None;
            },
          SSet.add key_name xs
        )
      in
      let property_type = annot opts scope tbls xs prop_type in
      Annot
        (MappedTypeAnnot
           {
             loc;
             source_type;
             property_type;
             key_tparam;
             variance = polarity variance;
             optional;
             inline_keyof;
           }
        )
    | _ -> Annot (Any loc)
  in

  let dict opts scope tbls xs acc p =
    let dict = indexer opts scope tbls xs p in
    Acc.add_dict dict acc
  in
  let call opts scope tbls xs acc p =
    let { O.CallProperty.value = (fn_loc, f); static = _; comments = _ } = p in
    let fn_loc = push_loc tbls fn_loc in
    let def = function_type opts scope tbls xs f in
    let t = Annot (FunAnnot (fn_loc, def)) in
    Acc.add_call t acc
  in
  let slot opts scope tbls xs acc p =
    let { O.InternalSlot.id; value; optional; static = _; _method = _; comments = _ } = p in
    let name = id_name id in
    if name = "call" then
      let t = annot opts scope tbls xs value in
      let t =
        if optional then
          Annot (Optional t)
        else
          t
      in
      Acc.add_call t acc
    else
      acc
    (* unsupported slot name *)
  in
  fun opts scope tbls xs loc o ->
    let { O.exact; inexact; properties; comments = _ } = o in
    let exact = exact || ((not inexact) && opts.exact_by_default) in
    (* Mapped types do not allow extra properties yet. We syntactically match
     * on objects with only a mapped type property and make other objects
     * including a mapped type `any`.
     *)
    match properties with
    | [O.MappedType p] -> mapped_type opts scope tbls xs p
    | _ ->
      let acc =
        List.fold_left
          (fun acc objprop ->
            match acc with
            | None -> None
            | Some acc ->
              (match objprop with
              | O.Property (_, p) -> Some (prop opts scope tbls xs acc p)
              | O.SpreadProperty (_, p) -> Some (spread opts scope tbls xs acc p)
              | O.Indexer (_, p) -> Some (dict opts scope tbls xs acc p)
              | O.CallProperty (_, p) -> Some (call opts scope tbls xs acc p)
              | O.InternalSlot (_, p) -> Some (slot opts scope tbls xs acc p)
              | O.MappedType _ -> None))
          (Some Acc.empty)
          properties
      in
      (match acc with
      | None -> Annot (Any loc)
      | Some acc -> Acc.object_type loc ~exact acc)

and interface_def opts scope tbls xs extends properties =
  let module Acc = InterfaceAcc in
  let extends =
    List.map
      (fun (loc, g) ->
        let loc = push_loc tbls loc in
        generic opts scope tbls xs loc g)
      extends
  in
  Acc.empty |> interface_props opts scope tbls xs properties |> Acc.interface_def extends

and interface_props =
  let module O = Ast.Type.Object in
  let module Acc = InterfaceAcc in
  let prop opts scope tbls xs acc p =
    let { O.Property.key; value; optional; static = _; proto = _; _method; variance; comments = _ }
        =
      p
    in
    let module P = Ast.Expression.Object.Property in
    match key with
    | P.StringLiteral _
    | P.NumberLiteral _
    | P.BigIntLiteral _
    | P.PrivateName _
    | P.Computed _ ->
      acc (* unsupported interface keys *)
    | P.Identifier (id_loc, { Ast.Identifier.name; comments = _ }) ->
      (match (_method, value) with
      | (true, O.Property.Init (fn_loc, Ast.Type.Function fn)) ->
        let fn_loc = push_loc tbls fn_loc in
        let id_loc = push_loc tbls id_loc in
        let def = function_type opts scope tbls xs fn in
        Acc.append_method name id_loc fn_loc def acc
      | (true, _) -> acc (* unexpected non-function method *)
      | (false, O.Property.Init t) ->
        let id_loc = push_loc tbls id_loc in
        let t = annot opts scope tbls xs t in
        let t =
          if optional then
            Annot (Optional t)
          else
            t
        in
        let polarity = polarity variance in
        Acc.add_field name id_loc polarity t acc
      | (_, O.Property.Get (_, fn)) ->
        let id_loc = push_loc tbls id_loc in
        let getter = getter_type opts scope tbls xs id_loc fn in
        Acc.add_accessor name getter acc
      | (_, O.Property.Set (_, fn)) ->
        let id_loc = push_loc tbls id_loc in
        let setter = setter_type opts scope tbls xs id_loc fn in
        Acc.add_accessor name setter acc)
  in
  let interface_indexer opts scope tbls xs acc p =
    let i = indexer opts scope tbls xs p in
    Acc.add_indexer i acc
  in
  let call opts scope tbls xs acc p =
    let { O.CallProperty.value = (fn_loc, fn); static = _; comments = _ } = p in
    let fn_loc = push_loc tbls fn_loc in
    let def = function_type opts scope tbls xs fn in
    let t = Annot (FunAnnot (fn_loc, def)) in
    Acc.append_call t acc
  in
  let slot opts scope tbls xs acc p =
    let { O.InternalSlot.id; value; optional; static = _; _method = _; comments = _ } = p in
    let name = id_name id in
    if name = "call" then
      let t = annot opts scope tbls xs value in
      let t =
        if optional then
          Annot (Optional t)
        else
          t
      in
      Acc.append_call t acc
    else
      acc
    (* unsupported slot name *)
  in
  fun opts scope tbls xs properties acc ->
    List.fold_left
      (fun acc -> function
        | O.Property (_, p) -> prop opts scope tbls xs acc p
        | O.Indexer (_, p) -> interface_indexer opts scope tbls xs acc p
        | O.CallProperty (_, p) -> call opts scope tbls xs acc p
        | O.InternalSlot (_, p) -> slot opts scope tbls xs acc p
        | O.SpreadProperty _
        | O.MappedType _ ->
          (* no spread or mapped types in interface *)
          acc)
      acc
      properties

and declare_class_props =
  let module O = Ast.Type.Object in
  let module Acc = DeclareClassAcc in
  let prop opts scope tbls xs acc p =
    let { O.Property.key; value; optional; static; proto; _method; variance; comments = _ } = p in
    let module P = Ast.Expression.Object.Property in
    match key with
    | P.StringLiteral _
    | P.NumberLiteral _
    | P.BigIntLiteral _
    | P.PrivateName _
    | P.Computed _ ->
      acc (* unsupported interface / declare class keys *)
    | P.Identifier (id_loc, { Ast.Identifier.name; comments = _ }) ->
      (match (_method, value) with
      | (true, O.Property.Init (fn_loc, Ast.Type.Function fn)) ->
        let fn_loc = push_loc tbls fn_loc in
        let id_loc = push_loc tbls id_loc in
        let def = function_type opts scope tbls xs fn in
        Acc.append_method ~static name id_loc fn_loc def acc
      | (true, _) -> acc (* unexpected non-function method *)
      | (false, O.Property.Init t) ->
        let id_loc = push_loc tbls id_loc in
        let t = annot opts scope tbls xs t in
        let t =
          if optional then
            Annot (Optional t)
          else
            t
        in
        let polarity = polarity variance in
        if proto then
          Acc.add_proto_field name id_loc polarity t acc
        else
          Acc.add_field ~static name id_loc polarity t acc
      | (_, O.Property.Get (_, fn)) ->
        let id_loc = push_loc tbls id_loc in
        let getter = getter_type opts scope tbls xs id_loc fn in
        Acc.add_accessor ~static name getter acc
      | (_, O.Property.Set (_, fn)) ->
        let id_loc = push_loc tbls id_loc in
        let setter = setter_type opts scope tbls xs id_loc fn in
        Acc.add_accessor ~static name setter acc)
  in
  let class_indexer opts scope tbls xs acc p =
    let { O.Indexer.static; _ } = p in
    let i = indexer opts scope tbls xs p in
    Acc.add_indexer ~static i acc
  in
  let call opts scope tbls xs acc p =
    let { O.CallProperty.value = (fn_loc, fn); static; comments = _ } = p in
    let fn_loc = push_loc tbls fn_loc in
    let def = function_type opts scope tbls xs fn in
    let t = Annot (FunAnnot (fn_loc, def)) in
    Acc.append_call ~static t acc
  in
  let slot opts scope tbls xs acc p =
    let { O.InternalSlot.id; value; optional; static; _method = _; comments = _ } = p in
    let name = id_name id in
    if name = "call" then
      let t = annot opts scope tbls xs value in
      let t =
        if optional then
          Annot (Optional t)
        else
          t
      in
      Acc.append_call ~static t acc
    else
      acc
    (* unsupported slot name *)
  in
  fun opts scope tbls xs properties acc ->
    List.fold_left
      (fun acc -> function
        | O.Property (_, p) -> prop opts scope tbls xs acc p
        | O.Indexer (_, p) -> class_indexer opts scope tbls xs acc p
        | O.CallProperty (_, p) -> call opts scope tbls xs acc p
        | O.InternalSlot (_, p) -> slot opts scope tbls xs acc p
        | O.SpreadProperty _
        | O.MappedType _ ->
          (* no spread or mapped types in interface / declare class *)
          acc)
      acc
      properties

and nominal_type opts scope tbls xs loc name = function
  | None -> TyRef name
  | Some (_, { Ast.Type.TypeArgs.arguments = targs; comments = _ }) ->
    let targs = List.map (annot opts scope tbls xs) targs in
    TyRefApp { loc; name; targs }

and generic_id =
  let rec finish tbls tyname = function
    | [] -> Ok tyname
    | (loc, id_loc, name) :: chain ->
      let id_loc = push_loc tbls id_loc in
      let tyname = Qualified { loc; id_loc; name; qualification = tyname } in
      finish tbls tyname chain
  in
  fun scope tbls xs chain ->
    let module G = T.Generic in
    function
    | G.Identifier.Qualified (qloc, { G.Identifier.qualification; id }) ->
      let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
      let qloc = push_loc tbls qloc in
      let chain = (qloc, id_loc, name) :: chain in
      generic_id scope tbls xs chain qualification
    | G.Identifier.Unqualified id ->
      let (ref_loc, { Ast.Identifier.name; comments = _ }) = id in
      let ref_loc = push_loc tbls ref_loc in
      (* Type params in scope should be handled before generic_id. *)
      if SSet.mem name xs then
        Error ref_loc
      else
        let tyname = Unqualified (Ref { ref_loc; name; scope; resolved = None }) in
        finish tbls tyname chain

and generic opts scope tbls xs loc g =
  let module G = T.Generic in
  let { G.id; targs; comments = _ } = g in
  match generic_id scope tbls xs [] id with
  | Ok tyname -> nominal_type opts scope tbls xs loc tyname targs
  | Error loc -> Err (loc, CheckError)

and maybe_special_generic opts scope tbls xs loc g =
  let module G = T.Generic in
  let { G.id; targs; comments = _ } = g in
  match id with
  | G.Identifier.Qualified (qloc, { G.Identifier.qualification; id }) ->
    let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
    let qloc = push_loc tbls qloc in
    (match generic_id scope tbls xs [(qloc, id_loc, name)] qualification with
    | Ok tyname -> nominal_type opts scope tbls xs loc tyname targs
    | Error loc -> Err (loc, CheckError))
  | G.Identifier.Unqualified (ref_loc, { Ast.Identifier.name; comments = _ }) ->
    let ref_loc = push_loc tbls ref_loc in
    maybe_special_unqualified_generic opts scope tbls xs loc targs ref_loc name

and maybe_special_unqualified_generic opts scope tbls xs loc targs ref_loc =
  let open Ast.Type.TypeArgs in
  function
  | name when SSet.mem name opts.suppress_types -> Annot (Any loc)
  | name when SSet.mem name xs ->
    (* TODO: error if targs <> None *)
    Annot (Bound { ref_loc; name })
  | name when (not opts.for_builtins) && Option.is_some (Scope.lookup_type scope name) ->
    let name = Unqualified (Ref { ref_loc; name; scope; resolved = None }) in
    nominal_type opts scope tbls xs loc name targs
  | "Array" -> begin
    match targs with
    | Some (_, { arguments = [t]; _ }) ->
      let t = annot opts scope tbls xs t in
      Annot (Array (loc, t))
    | _ -> Err (loc, CheckError)
  end
  | "Class" -> begin
    match targs with
    | Some (_, { arguments = [t]; _ }) ->
      let t = annot opts scope tbls xs t in
      Annot (ClassT (loc, t))
    | _ -> Err (loc, CheckError)
  end
  | "Function"
  | "function"
  | "Object" -> begin
    match targs with
    | None -> Annot (Any loc)
    | _ -> Err (loc, CheckError)
  end
  | "Function$Prototype$Apply" -> begin
    match targs with
    | None -> Annot (Function_apply loc)
    | _ -> Err (loc, CheckError)
  end
  | "Function$Prototype$Bind" -> begin
    match targs with
    | None -> Annot (Function_bind loc)
    | _ -> Err (loc, CheckError)
  end
  | "Function$Prototype$Call" -> begin
    match targs with
    | None -> Annot (Function_call loc)
    | _ -> Err (loc, CheckError)
  end
  | "Object$Assign" -> begin
    match targs with
    | None -> Annot (Object_assign loc)
    | _ -> Err (loc, CheckError)
  end
  | "Object$GetPrototypeOf" -> begin
    match targs with
    | None -> Annot (Object_getPrototypeOf loc)
    | _ -> Err (loc, CheckError)
  end
  | "Object$SetPrototypeOf" -> begin
    match targs with
    | None -> Annot (Object_setPrototypeOf loc)
    | _ -> Err (loc, CheckError)
  end
  | "NoInfer" -> begin
    match targs with
    | Some (_, { arguments = [t]; _ }) ->
      let t = annot opts scope tbls xs t in
      Annot (NoInfer t)
    | _ -> Err (loc, CheckError)
  end
  | "$TEMPORARY$number" -> begin
    match targs with
    | Some (_, { arguments = [(loc, T.NumberLiteral { Ast.NumberLiteral.value; raw; _ })]; _ }) ->
      let loc = push_loc tbls loc in
      Annot (TEMPORARY_Number (loc, value, raw))
    | _ -> Err (loc, CheckError)
  end
  | "$TEMPORARY$string" -> begin
    match targs with
    | Some (_, { arguments = [(loc, T.StringLiteral { Ast.StringLiteral.value = s; _ })]; _ }) ->
      let loc = push_loc tbls loc in
      if opts.max_literal_len = 0 || String.length s <= opts.max_literal_len then
        Annot (TEMPORARY_String (loc, s))
      else
        Annot (TEMPORARY_LongString loc)
    | _ -> Err (loc, CheckError)
  end
  | "$TEMPORARY$boolean" -> begin
    match targs with
    | Some (_, { arguments = [(loc, T.BooleanLiteral { Ast.BooleanLiteral.value; _ })]; _ }) ->
      let loc = push_loc tbls loc in
      Annot (TEMPORARY_Boolean (loc, value))
    | _ -> Err (loc, CheckError)
  end
  | "$TEMPORARY$object" -> begin
    match targs with
    | Some (_, { arguments = [t]; _ }) ->
      let t = annot opts scope tbls xs t in
      Annot (TEMPORARY_Object t)
    | _ -> Err (loc, CheckError)
  end
  | "$TEMPORARY$array" -> begin
    match targs with
    | Some (_, { arguments = [t]; _ }) ->
      let t = annot opts scope tbls xs t in
      Annot (TEMPORARY_Array (loc, t))
    | _ -> Err (loc, CheckError)
  end
  | "$ReadOnlyArray" -> begin
    match targs with
    | Some (_, { arguments = [t]; _ }) ->
      let t = annot opts scope tbls xs t in
      Annot (ReadOnlyArray (loc, t))
    | _ -> Err (loc, CheckError)
  end
  | "$PropertyType" -> begin
    match targs with
    | Some (_, { arguments = [obj; (_, T.StringLiteral { Ast.StringLiteral.value; _ })]; _ }) ->
      let obj = annot opts scope tbls xs obj in
      Annot (PropertyType { loc; obj; prop = value })
    | Some (_, { arguments = [_; (loc, _)]; _ }) ->
      let loc = push_loc tbls loc in
      Err (loc, CheckError)
    | _ -> Err (loc, CheckError)
  end
  | "$ElementType" -> begin
    match targs with
    | Some (_, { arguments = [obj; elem]; _ }) ->
      let obj = annot opts scope tbls xs obj in
      let elem = annot opts scope tbls xs elem in
      Annot (ElementType { loc; obj; elem })
    | _ -> Err (loc, CheckError)
  end
  | "$NonMaybeType" -> begin
    match targs with
    | Some (_, { arguments = [t]; _ }) ->
      let t = annot opts scope tbls xs t in
      Annot (NonMaybeType (loc, t))
    | _ -> Err (loc, CheckError)
  end
  | "$Shape" -> Annot (Any loc)
  | "$Diff" -> begin
    match targs with
    | Some (_, { arguments = [t1; t2]; _ }) ->
      let t1 = annot opts scope tbls xs t1 in
      let t2 = annot opts scope tbls xs t2 in
      Annot (Diff (loc, t1, t2))
    | _ -> Err (loc, CheckError)
  end
  | "$ReadOnly" -> begin
    match targs with
    | Some (_, { arguments = [t]; _ }) ->
      let t = annot opts scope tbls xs t in
      Annot (ReadOnly (loc, t))
    | _ -> Err (loc, CheckError)
  end
  | "$Partial" -> Annot (Any loc)
  | "Partial" -> begin
    match targs with
    | Some (_, { arguments = [t]; _ }) ->
      let t = annot opts scope tbls xs t in
      Annot (Partial (loc, t))
    | _ -> Err (loc, CheckError)
  end
  | "Required" -> begin
    match targs with
    | Some (_, { arguments = [t]; _ }) ->
      let t = annot opts scope tbls xs t in
      Annot (Required (loc, t))
    | _ -> Err (loc, CheckError)
  end
  | "$Keys" -> begin
    match targs with
    | Some (_, { arguments = [t]; _ }) ->
      let t = annot opts scope tbls xs t in
      Annot (Keys (loc, t))
    | _ -> Err (loc, CheckError)
  end
  | "$Values" -> begin
    match targs with
    | Some (_, { arguments = [t]; _ }) ->
      let t = annot opts scope tbls xs t in
      Annot (Values (loc, t))
    | _ -> Err (loc, CheckError)
  end
  | "$Exact" -> begin
    match targs with
    | Some (_, { arguments = [t]; _ }) ->
      let t = annot opts scope tbls xs t in
      Annot (Exact (loc, t))
    | _ -> Err (loc, CheckError)
  end
  | "$Rest" -> begin
    match targs with
    | Some (_, { arguments = [t1; t2]; _ }) ->
      let t1 = annot opts scope tbls xs t1 in
      let t2 = annot opts scope tbls xs t2 in
      Annot (Rest (loc, t1, t2))
    | _ -> Err (loc, CheckError)
  end
  | "$Exports" -> begin
    match targs with
    | Some (_, { arguments = [(_, T.StringLiteral { Ast.StringLiteral.value; _ })]; _ }) ->
      Annot (ExportsT (loc, value))
    | Some (_, { arguments = [(loc, _)]; _ }) -> Err (push_loc tbls loc, CheckError)
    | _ -> Err (loc, CheckError)
  end
  | "$Call" -> begin
    match targs with
    | Some (_, { arguments = fn :: args; _ }) ->
      let fn = annot opts scope tbls xs fn in
      let args = List.map (annot opts scope tbls xs) args in
      Annot (Call { loc; fn; args })
    | _ -> Err (loc, CheckError)
  end
  | "$TupleMap" -> begin
    match targs with
    | Some (_, { arguments = [tup; fn]; _ }) ->
      let tup = annot opts scope tbls xs tup in
      let fn = annot opts scope tbls xs fn in
      Annot (TupleMap { loc; tup; fn })
    | _ -> Err (loc, CheckError)
  end
  | "$ObjMap" -> begin
    match targs with
    | Some (_, { arguments = [obj; fn]; _ }) ->
      let obj = annot opts scope tbls xs obj in
      let fn = annot opts scope tbls xs fn in
      Annot (ObjMap { loc; obj; fn })
    | _ -> Err (loc, CheckError)
  end
  | "$ObjMapi" -> begin
    match targs with
    | Some (_, { arguments = [obj; fn]; _ }) ->
      let obj = annot opts scope tbls xs obj in
      let fn = annot opts scope tbls xs fn in
      Annot (ObjMapi { loc; obj; fn })
    | _ -> Err (loc, CheckError)
  end
  | "$KeyMirror" -> begin
    match targs with
    | Some (_, { arguments = [obj]; _ }) ->
      let obj = annot opts scope tbls xs obj in
      Annot (ObjKeyMirror { loc; obj })
    | _ -> Err (loc, CheckError)
  end
  | "$ObjMapConst" -> begin
    match targs with
    | Some (_, { arguments = [obj; t]; _ }) ->
      let obj = annot opts scope tbls xs obj in
      let t = annot opts scope tbls xs t in
      Annot (ObjMapConst { loc; obj; t })
    | _ -> Err (loc, CheckError)
  end
  | "$CharSet" -> begin
    match targs with
    | Some (_, { arguments = [(_, T.StringLiteral { Ast.StringLiteral.value; _ })]; _ }) ->
      Annot (CharSet (loc, value))
    | Some (_, { arguments = [(loc, _)]; _ }) ->
      let loc = push_loc tbls loc in
      Err (loc, CheckError)
    | _ -> Err (loc, CheckError)
  end
  | "React$AbstractComponent" -> begin
    match targs with
    | Some (_, { arguments = config :: targs; _ }) ->
      let config = annot opts scope tbls xs config in
      (match targs with
      | [] -> Annot (ReactAbstractComponent { loc; config; instance = None; renders = None })
      | [instance] ->
        let instance = Some (annot opts scope tbls xs instance) in
        Annot (ReactAbstractComponent { loc; config; instance; renders = None })
      | [instance; renders] ->
        let instance = Some (annot opts scope tbls xs instance) in
        let renders = Some (annot opts scope tbls xs renders) in
        Annot (ReactAbstractComponent { loc; config; instance; renders })
      | _ -> Err (loc, CheckError))
    | _ -> Err (loc, CheckError)
  end
  | "React$Config" -> begin
    match targs with
    | Some (_, { arguments = [props; default]; _ }) ->
      let props = annot opts scope tbls xs props in
      let default = annot opts scope tbls xs default in
      Annot (ReactConfig { loc; props; default })
    | _ -> Err (loc, CheckError)
  end
  | "React$CreateClass" -> begin
    match targs with
    | None -> Annot (ReactCreateClass loc)
    | _ -> Err (loc, CheckError)
  end
  | "React$CreateElement" -> begin
    match targs with
    | None -> Annot (ReactCreateElement loc)
    | _ -> Err (loc, CheckError)
  end
  | "React$CloneElement" -> begin
    match targs with
    | None -> Annot (ReactCloneElement loc)
    | _ -> Err (loc, CheckError)
  end
  | "React$ElementFactory" -> begin
    match targs with
    | Some (_, { arguments = [t]; _ }) ->
      let t = annot opts scope tbls xs t in
      Annot (ReactElementFactory (loc, t))
    | _ -> Err (loc, CheckError)
  end
  | "React$ElementProps" -> begin
    match targs with
    | Some (_, { arguments = [t]; _ }) ->
      let t = annot opts scope tbls xs t in
      Annot (ReactElementProps (loc, t))
    | _ -> Err (loc, CheckError)
  end
  | "React$ElementConfig" -> begin
    match targs with
    | Some (_, { arguments = [t]; _ }) ->
      let t = annot opts scope tbls xs t in
      Annot (ReactElementConfig (loc, t))
    | _ -> Err (loc, CheckError)
  end
  | "React$ElementRef" -> begin
    match targs with
    | Some (_, { arguments = [t]; _ }) ->
      let t = annot opts scope tbls xs t in
      Annot (ReactElementRef (loc, t))
    | _ -> Err (loc, CheckError)
  end
  | "$Compose" -> begin
    match targs with
    | None -> Annot (Compose loc)
    | _ -> Err (loc, CheckError)
  end
  | "$ComposeReverse" -> begin
    match targs with
    | None -> Annot (ComposeReverse loc)
    | _ -> Err (loc, CheckError)
  end
  | "$Flow$DebugPrint" -> begin
    match targs with
    | None -> Annot (FlowDebugPrint loc)
    | _ -> Err (loc, CheckError)
  end
  | "$Flow$DebugThrow" -> begin
    match targs with
    | None -> Annot (FlowDebugThrow loc)
    | _ -> Err (loc, CheckError)
  end
  | "$Flow$DebugSleep" -> begin
    match targs with
    | None -> Annot (FlowDebugSleep loc)
    | _ -> Err (loc, CheckError)
  end
  | "$Flow$EnforceOptimized" -> begin
    match targs with
    | Some (_, { arguments = [t]; _ }) -> annot opts scope tbls xs t
    | _ -> Err (loc, CheckError)
  end
  | "Readonly" ->
    if opts.enable_ts_syntax then
      match targs with
      | Some (_, { arguments = [t]; _ }) ->
        let t = annot opts scope tbls xs t in
        Annot (ReadOnly (loc, t))
      | _ -> Err (loc, CheckError)
    else
      Annot (Any loc)
  | "ReadonlyArray" ->
    if opts.enable_ts_syntax then
      match targs with
      | Some (_, { arguments = [t]; _ }) ->
        let t = annot opts scope tbls xs t in
        Annot (ReadOnlyArray (loc, t))
      | _ -> Err (loc, CheckError)
    else
      Annot (Any loc)
  | "NonNullable" ->
    if opts.enable_ts_syntax then
      match targs with
      | Some (_, { arguments = [t]; _ }) ->
        let t = annot opts scope tbls xs t in
        Annot (NonMaybeType (loc, t))
      | _ -> Err (loc, CheckError)
    else
      Annot (Any loc)
  | "ReadonlyMap" when not opts.enable_ts_syntax -> Annot (Any loc)
  | "ReadonlySet" when not opts.enable_ts_syntax -> Annot (Any loc)
  | name ->
    let name = Unqualified (Ref { ref_loc; name; scope; resolved = None }) in
    nominal_type opts scope tbls xs loc name targs

and tparam opts scope tbls xs tp =
  let bound opts scope tbls xs = function
    | Ast.Type.Available (_, t) -> Some (annot opts scope tbls xs t)
    | Ast.Type.Missing _ -> None
  in
  let default opts scope tbls xs = function
    | Some t -> Some (annot opts scope tbls xs t)
    | None -> None
  in
  let module T = T.TypeParam in
  let ( _,
        {
          T.name = (name_loc, { Ast.Identifier.name; comments = _ });
          bound = b;
          bound_kind = _;
          variance = v;
          default = d;
        }
      ) =
    tp
  in
  let name_loc = push_loc tbls name_loc in
  let bound = bound opts scope tbls xs b in
  let default = default opts scope tbls xs d in
  TParam { name_loc; name; polarity = polarity v; bound; default }

and tparams =
  let rec loop opts scope tbls tparams_loc xs acc = function
    | [] ->
      ( xs,
        begin
          match List.rev acc with
          | [] -> Mono
          | tp :: tps -> Poly (tparams_loc, tp, tps)
        end
      )
    | tp :: tps ->
      let (TParam { name; _ } as tp) = tparam opts scope tbls xs tp in
      let xs = SSet.add name xs in
      loop opts scope tbls tparams_loc xs (tp :: acc) tps
  in
  fun opts scope tbls xs -> function
    | None -> (xs, Mono)
    | Some (tparams_loc, { Ast.Type.TypeParams.params = tps; comments = _ }) ->
      let tparams_loc = push_loc tbls tparams_loc in
      loop opts scope tbls tparams_loc xs [] tps

and conditional_type
    opts
    scope
    tbls
    xs
    (loc, { T.Conditional.check_type; extends_type; true_type; false_type; comments = _ }) =
  let check_type = annot opts scope tbls xs check_type in
  let distributive_tparam =
    match check_type with
    | Annot (Bound { ref_loc = name_loc; name }) ->
      (* If check type is a bound type, then this is a distributive conditional type. *)
      Some
        (TParam
           { name_loc; name; polarity = Polarity.Neutral; bound = Some check_type; default = None }
        )
    | _ -> None
  in
  let hoisted_infer_types = Infer_type_hoister.hoist_infer_types extends_type in
  let (infer_type_names, true_type_xs) =
    Base.List.fold
      hoisted_infer_types
      ~init:(SMap.empty, xs)
      ~f:(fun
           (infer_type_names, true_type_xs)
           ( _,
             {
               T.Infer.tparam = (loc, { T.TypeParam.name = (_, { Ast.Identifier.name; _ }); _ });
               comments = _;
             }
           )
         -> (SMap.add name loc infer_type_names, SSet.add name true_type_xs)
    )
  in
  let conditional_type_extends = { infer_type_names; infer_tparams_rev = []; parent = scope } in
  let (extends_type_loc, extends_type) =
    annot_with_loc opts (ConditionalTypeExtends conditional_type_extends) tbls xs extends_type
  in
  let infer_tparams =
    match List.rev conditional_type_extends.infer_tparams_rev with
    | [] -> Mono
    | tp :: tps -> Poly (extends_type_loc, tp, tps)
  in
  let true_type = annot opts scope tbls true_type_xs true_type in
  let false_type = annot opts scope tbls xs false_type in
  Annot
    (Conditional
       { loc; distributive_tparam; infer_tparams; check_type; extends_type; true_type; false_type }
    )

and infer_type
    opts
    scope
    tbls
    xs
    ( loc,
      {
        T.Infer.tparam = (tp_loc, { T.TypeParam.name = (_, { Ast.Identifier.name; _ }); _ }) as tp;
        comments = _;
      }
    ) =
  match Scope.scope_of_infer_name scope name tp_loc with
  | None -> Annot (Any loc)
  | Some infer_scope ->
    let (TParam { name_loc; _ }) =
      (* There can be duplicate infer type names. We will skip later ones,
         since the first one wins when there are bound conflicts. *)
      match
        Base.List.find infer_scope.infer_tparams_rev ~f:(fun (TParam { name = n; _ }) -> n = name)
      with
      | Some tp -> tp
      | None ->
        let tp = tparam opts scope tbls xs tp in
        infer_scope.infer_tparams_rev <- tp :: infer_scope.infer_tparams_rev;
        tp
    in
    Annot (Bound { ref_loc = name_loc; name })

and optional_indexed_access
    opts scope tbls xs (loc, { T.OptionalIndexedAccess.indexed_access; optional }) =
  let { T.IndexedAccess._object; index; comments = _ } = indexed_access in
  let (obj_loc, obj) =
    match _object with
    | (loc, T.OptionalIndexedAccess ia) ->
      let loc = push_loc tbls loc in
      let (obj, _) = optional_indexed_access opts scope tbls xs (loc, ia) in
      (loc, obj)
    | _ -> annot_with_loc opts scope tbls xs _object
  in
  let index = annot opts scope tbls xs index in
  let non_maybe_result =
    if optional then
      Annot (OptionalIndexedAccessNonMaybeType { loc; obj; index })
    else
      Annot (ElementType { loc; obj; elem = index })
  in
  let result =
    Annot (OptionalIndexedAccessResultType { loc; non_maybe_result; void_loc = obj_loc })
  in
  (non_maybe_result, result)

let annot_or_hint ~sort ~err_loc opts scope tbls xs = function
  | Ast.Type.Available (_, t) -> annot opts scope tbls xs t
  | Ast.Type.Missing loc ->
    let err_loc =
      match err_loc with
      | Some err_loc -> err_loc
      | None -> push_loc tbls loc
    in
    Err (err_loc, SigError (Signature_error.ExpectedAnnotation (err_loc, sort)))

let function_return_annot ~sort ~err_loc opts scope tbls xs = function
  | Ast.Function.ReturnAnnot.Available (_, t) -> annot opts scope tbls xs t
  | Ast.Function.ReturnAnnot.TypeGuard (loc, _) ->
    (* This is only used for getters, where type guards are not allowed *)
    let loc = push_loc tbls loc in
    Annot (Any loc)
  | Ast.Function.ReturnAnnot.Missing loc ->
    let err_loc =
      match err_loc with
      | Some err_loc -> err_loc
      | None -> push_loc tbls loc
    in
    Err (err_loc, SigError (Signature_error.ExpectedAnnotation (err_loc, sort)))

let class_implements =
  let module C = Ast.Class in
  let rec loop opts scope tbls xs acc = function
    | [] -> List.rev acc
    | (loc, { C.Implements.Interface.id; targs }) :: rest ->
      let (ref_loc, { Ast.Identifier.name; comments = _ }) = id in
      let loc = push_loc tbls loc in
      let ref_loc = push_loc tbls ref_loc in
      let name = Unqualified (Ref { ref_loc; name; scope; resolved = None }) in
      let t = nominal_type opts scope tbls xs loc name targs in
      loop opts scope tbls xs (t :: acc) rest
  in
  fun opts scope tbls xs -> function
    | None -> []
    | Some (_, { C.Implements.interfaces; comments = _ }) -> loop opts scope tbls xs [] interfaces

let getter_def opts scope tbls xs id_loc f =
  let module F = Ast.Function in
  let { F.return = r; _ } = f in
  let t =
    function_return_annot
      ~err_loc:None (* use location of Missing annotation *)
      ~sort:Expected_annotation_sort.FunctionReturn
      opts
      scope
      tbls
      xs
      r
  in
  Get (id_loc, t)

let setter_def opts scope tbls xs id_loc f =
  let module F = Ast.Function in
  let module P = Ast.Pattern in
  let { F.params = (_, { F.Params.params; _ }); _ } = f in
  match params with
  | [
   (param_loc, { F.Param.argument = (_, P.Identifier { P.Identifier.annot = t; _ }); default = _ });
  ] ->
    let param_loc = push_loc tbls param_loc in
    let t =
      annot_or_hint
        ~err_loc:(Some param_loc)
        ~sort:Expected_annotation_sort.Identifier
        opts
        scope
        tbls
        xs
        t
    in
    Set (id_loc, t)
  | _ -> failwith "unexpected setter"

let module_ref_literal tbls loc { Ast.ModuleRefLiteral.value; prefix_len; legacy_interop; _ } =
  let mref = push_module_ref tbls (Base.String.drop_prefix value prefix_len) in
  ModuleRef { loc; mref; legacy_interop }

let string_literal opts loc s =
  if opts.max_literal_len = 0 || String.length s <= opts.max_literal_len then
    Value (StringLit (loc, s))
  else
    Value (LongStringLit loc)

let template_literal opts loc quasis =
  let module T = Ast.Expression.TemplateLiteral in
  match quasis with
  | [(_, { T.Element.value = { T.Element.raw = _; cooked = s }; tail = _ })] ->
    string_literal opts loc s
  | _ -> Value (StringVal loc)

let graphql_literal opts tbls loc quasi =
  let module_prefix = opts.relay_integration_module_prefix in
  match Graphql.extract_module_name ~module_prefix quasi with
  | Ok module_name ->
    let mref = push_module_ref tbls module_name in
    Require { loc; mref }
  | Error _ -> Annot (Any loc)

let key_mirror =
  let module O = Ast.Expression.Object in
  let module P = O.Property in
  let module Acc = ObjectLiteralAcc in
  let rec loop tbls loc acc = function
    | [] -> Acc.object_lit loc ~frozen:false acc
    | p :: ps ->
      (match p with
      | O.Property
          ( _,
            P.Init
              {
                key =
                  ( P.Identifier (id_loc, { Ast.Identifier.name; comments = _ })
                  | P.StringLiteral (id_loc, { Ast.StringLiteral.value = name; _ }) );
                value = _;
                shorthand = _;
              }
          ) ->
        let id_loc = push_loc tbls id_loc in
        let acc =
          if name = "__proto__" then
            acc
          else
            let t = Annot (SingletonString (id_loc, name)) in
            Acc.add_field name id_loc t acc
        in
        loop tbls loc acc ps
      | O.Property (prop_loc, _)
      | O.SpreadProperty (prop_loc, _) ->
        let prop_loc = push_loc tbls prop_loc in
        Err (loc, SigError (Signature_error.UnexpectedObjectKey (loc, prop_loc))))
  in
  (fun tbls loc properties -> loop tbls loc Acc.empty properties)

let jsx_element opts tbls loc elem =
  let module J = Ast.JSX in
  let { J.opening_element; closing_element = _; children = _; comments = _ } = elem in
  let (_, { J.Opening.name; targs = _; self_closing = _; attributes = _ }) = opening_element in
  match (name, opts.facebook_fbt) with
  | (J.Identifier (ref_loc, { J.Identifier.name = "fbt"; comments = _ }), Some custom_jsx_type) ->
    let ref_loc = push_loc tbls ref_loc in
    BuiltinTyRef { ref_loc; name = custom_jsx_type }
  | _ ->
    Err
      ( loc,
        SigError
          (Signature_error.UnexpectedExpression (loc, Flow_ast_utils.ExpressionSort.JSXElement))
      )

let binary loc lhs_t rhs_t op =
  let open Ast.Expression.Binary in
  match op with
  | Equal
  | NotEqual
  | StrictEqual
  | StrictNotEqual
  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  | In
  | Instanceof ->
    Value (BooleanVal loc)
  | LShift
  | RShift
  | RShift3
  | Minus
  | Mult
  | Exp
  | Div
  | Mod
  | BitOr
  | Xor
  | BitAnd
  | Plus ->
    Eval (loc, lhs_t, Arith (op, rhs_t))

let rec expression opts scope tbls (loc, expr) =
  let module E = Ast.Expression in
  let loc = push_loc tbls loc in
  match expr with
  | E.StringLiteral { Ast.StringLiteral.value; raw = _; comments = _ } ->
    string_literal opts loc value
  | E.NumberLiteral { Ast.NumberLiteral.value; raw; comments = _ } ->
    Value (NumberLit (loc, value, raw))
  | E.BigIntLiteral { Ast.BigIntLiteral.value; raw; comments = _ } ->
    Value (BigIntLit (loc, value, raw))
  | E.BooleanLiteral { Ast.BooleanLiteral.value; comments = _ } -> Value (BooleanLit (loc, value))
  | E.NullLiteral _comments -> Value (NullLit loc)
  | E.RegExpLiteral _ ->
    (* This can probably be easily supported by referencing the builtin type, as
     * we do in statement.ml. TODO: write a test and implement this. *)
    Err
      ( loc,
        SigError (Signature_error.UnexpectedExpression (loc, Flow_ast_utils.ExpressionSort.Literal))
      )
  | E.ModuleRefLiteral lit -> module_ref_literal tbls loc lit
  | E.TaggedTemplate
      {
        E.TaggedTemplate.tag = (_, E.Identifier (_, { Ast.Identifier.name = "graphql"; _ }));
        quasi;
        comments = _;
      }
    when opts.enable_relay_integration ->
    graphql_literal opts tbls loc quasi
  | E.TemplateLiteral { E.TemplateLiteral.quasis; expressions = _; comments = _ } ->
    template_literal opts loc quasis
  | E.Identifier id ->
    let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
    let id_loc = push_loc tbls id_loc in
    val_ref ~type_only:false scope id_loc name
  | E.Member { E.Member._object; property; comments = _ } ->
    member opts scope tbls _object loc property
  | E.Class c -> begin
    match c.Ast.Class.id with
    | Some (id_loc, { Ast.Identifier.name; comments = _ }) ->
      let id_loc = push_loc tbls id_loc in
      let scope = Scope.push_lex scope in
      let def = lazy (splice tbls id_loc (fun tbls -> class_def opts scope tbls c)) in
      Scope.bind_class scope tbls id_loc name def ignore2;
      val_ref ~type_only:false scope id_loc name
    | None ->
      let def = class_def opts scope tbls c in
      Value (ClassExpr (loc, def))
  end
  | E.Function f ->
    let { Ast.Function.id; async; generator; sig_loc; _ } = f in
    let sig_loc = push_loc tbls sig_loc in
    begin
      match id with
      | Some (id_loc, { Ast.Identifier.name; comments = _ }) ->
        let id_loc = push_loc tbls id_loc in
        let scope = Scope.push_lex scope in
        let def =
          lazy (splice tbls id_loc (fun tbls -> function_def opts scope tbls SSet.empty loc f))
        in
        Scope.bind_function scope tbls id_loc sig_loc name ~async ~generator ~hook:false def ignore2;
        val_ref ~type_only:false scope id_loc name
      | None ->
        let def = function_def opts scope tbls SSet.empty loc f in
        let statics = SMap.empty in
        Value (FunExpr { loc = sig_loc; async; generator; def; statics })
    end
  | E.ArrowFunction f ->
    let { Ast.Function.async; generator; _ } = f in
    let def = function_def opts scope tbls SSet.empty loc f in
    let statics = SMap.empty in
    Value (FunExpr { loc; async; generator; def; statics })
  | E.TypeCast { E.TypeCast.expression = _; annot = (_, t); comments = _ } ->
    let open Options.CastingSyntax in
    (match opts.casting_syntax with
    | Colon
    | Both ->
      annot opts scope tbls SSet.empty t
    | As -> Annot (Any loc))
  | E.AsExpression { E.AsExpression.expression = _; annot = (_, t); comments = _ } ->
    let open Options.CastingSyntax in
    (match opts.casting_syntax with
    | Colon -> Annot (Any loc)
    | As
    | Both ->
      annot opts scope tbls SSet.empty t)
  | E.AsConstExpression { E.AsConstExpression.expression = e; comments = _ } -> begin
    match expression opts scope tbls e with
    | Value v -> Value (AsConst v)
    | e -> e
  end
  | E.TSSatisfies _ ->
    Err
      ( loc,
        SigError
          (Signature_error.UnexpectedExpression (loc, Flow_ast_utils.ExpressionSort.Satisfies))
      )
  | E.Object { E.Object.properties; comments = _ } ->
    object_literal opts scope tbls loc ~frozen:false properties
  | E.Array { E.Array.elements; comments = _ } -> array_literal opts scope tbls loc elements
  | E.Unary { E.Unary.operator; argument; comments = _ } -> begin
    match operator with
    | E.Unary.Await ->
      (* This is already a parse error *)
      let e = Signature_error.UnexpectedExpression (loc, Flow_ast_utils.ExpressionSort.Unary) in
      Err (loc, SigError e)
    | _ ->
      let t = expression opts scope tbls argument in
      Eval (loc, t, Unary operator)
  end
  | E.Binary { E.Binary.operator; left; right; comments = _ } ->
    let lhs_t = expression opts scope tbls left in
    let rhs_t = expression opts scope tbls right in
    binary loc lhs_t rhs_t operator
  | E.Update { E.Update.operator = _; argument; prefix = _; comments = _ } ->
    let t = expression opts scope tbls argument in
    Eval (loc, t, Update)
  | E.Sequence { E.Sequence.expressions; comments = _ } ->
    sequence (expression opts scope tbls) expressions
  | E.Assignment { E.Assignment.operator; right; _ } -> begin
    match operator with
    | None ->
      (* This is sketchy: the RHS may have side effects that are not tracked! *)
      expression opts scope tbls right
    | Some _ ->
      Err
        ( loc,
          SigError
            (Signature_error.UnexpectedExpression (loc, Flow_ast_utils.ExpressionSort.Assignment))
        )
  end
  | E.Call
      {
        E.Call.callee = (_, E.Identifier (_, { Ast.Identifier.name = "require"; comments = _ }));
        targs;
        arguments;
        comments = _;
      } ->
    (* TODO: We should only special-case this logic if "require" is not aliased
     * in the current scope. i.e., it should resolves to a builtin. The current
     * signature builder does not do this, so we should probably fix that first.
     *)
    let mref =
      match (targs, arguments) with
      | (None, (_, { E.ArgList.arguments = [E.Expression (_, e)]; comments = _ })) ->
        extract_string_literal e
      | _ -> None
    in
    begin
      match mref with
      | Some mref ->
        let mref = push_module_ref tbls mref in
        Require { loc; mref }
      | None ->
        (* error cases: explicit targs / non-literal require *)
        Annot (Any loc)
    end
  | E.Call
      {
        E.Call.callee =
          ( _,
            E.Member
              {
                E.Member._object =
                  (_, E.Identifier (_, { Ast.Identifier.name = "Object"; comments = _ }));
                property =
                  E.Member.PropertyIdentifier (_, { Ast.Identifier.name = "freeze"; comments = _ });
                comments = _;
              }
          );
        targs = None;
        arguments =
          ( _,
            {
              E.ArgList.arguments =
                [E.Expression (obj_loc, E.Object { E.Object.properties; comments = _ })];
              comments = _;
            }
          );
        comments = _;
      } ->
    (* TODO: Similar to the "require" case above, we should only special-case
     * this call "Object" is not in scope. Again, we should fix the existing
     * signature builder first. *)
    let obj_loc = push_loc tbls obj_loc in
    object_literal opts scope tbls obj_loc ~frozen:true properties
  | E.Call
      {
        E.Call.callee = (_, E.Identifier (_, { Ast.Identifier.name = "keyMirror"; comments = _ }));
        targs = None;
        arguments =
          ( _,
            {
              E.ArgList.arguments =
                [E.Expression (obj_loc, E.Object { E.Object.properties; comments = _ })];
              comments = _;
            }
          );
        comments = _;
      }
    when opts.facebook_keyMirror ->
    (* TODO: In general, "keyMirror" could resolve to anything. It might not be
     * possible to ensure that it resolves to the expected function. If not,
     * we should document this limitation along with the flag to enable this
     * behavior. *)
    let obj_loc = push_loc tbls obj_loc in
    key_mirror tbls obj_loc properties
  | E.JSXElement elem -> jsx_element opts tbls loc elem
  | E.Import { E.Import.argument = (_, e); comments = _ } -> begin
    match extract_string_literal e with
    | None ->
      (* error case: non-literal require *)
      Annot (Any loc)
    | Some mref ->
      let mref = push_module_ref tbls mref in
      ImportDynamic { loc; mref }
  end
  | E.Call _ ->
    Err
      ( loc,
        SigError (Signature_error.UnexpectedExpression (loc, Flow_ast_utils.ExpressionSort.Call))
      )
  | E.Conditional _ ->
    Err
      ( loc,
        SigError
          (Signature_error.UnexpectedExpression (loc, Flow_ast_utils.ExpressionSort.Conditional))
      )
  | E.JSXFragment _ ->
    Err
      ( loc,
        SigError
          (Signature_error.UnexpectedExpression (loc, Flow_ast_utils.ExpressionSort.JSXFragment))
      )
  | E.Logical _ ->
    Err
      ( loc,
        SigError (Signature_error.UnexpectedExpression (loc, Flow_ast_utils.ExpressionSort.Logical))
      )
  | E.MetaProperty _ ->
    Err
      ( loc,
        SigError
          (Signature_error.UnexpectedExpression (loc, Flow_ast_utils.ExpressionSort.MetaProperty))
      )
  | E.New _ ->
    Err
      (loc, SigError (Signature_error.UnexpectedExpression (loc, Flow_ast_utils.ExpressionSort.New)))
  | E.OptionalCall _ ->
    Err
      ( loc,
        SigError
          (Signature_error.UnexpectedExpression (loc, Flow_ast_utils.ExpressionSort.OptionalCall))
      )
  | E.OptionalMember _ ->
    Err
      ( loc,
        SigError
          (Signature_error.UnexpectedExpression (loc, Flow_ast_utils.ExpressionSort.OptionalMember))
      )
  | E.Super _ ->
    Err
      ( loc,
        SigError (Signature_error.UnexpectedExpression (loc, Flow_ast_utils.ExpressionSort.Super))
      )
  | E.TaggedTemplate _ ->
    Err
      ( loc,
        SigError
          (Signature_error.UnexpectedExpression (loc, Flow_ast_utils.ExpressionSort.TaggedTemplate))
      )
  | E.This _ ->
    Err
      ( loc,
        SigError (Signature_error.UnexpectedExpression (loc, Flow_ast_utils.ExpressionSort.This))
      )
  | E.Yield _ ->
    Err
      ( loc,
        SigError (Signature_error.UnexpectedExpression (loc, Flow_ast_utils.ExpressionSort.Yield))
      )

and pattern opts scope tbls f def (_, p) =
  let module P = Ast.Pattern in
  match p with
  | P.Identifier { P.Identifier.name = id; annot = _; optional = _ } ->
    let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
    let id_loc = push_loc tbls id_loc in
    f id_loc name def
  | P.Object { P.Object.properties; annot = _; comments = _ } ->
    object_pattern opts scope tbls f def properties
  | P.Array { P.Array.elements; annot = _; comments = _ } ->
    array_pattern opts scope tbls f def elements
  | P.Expression _ -> failwith "unexpected expression pattern"

and object_pattern =
  let module O = Ast.Pattern.Object in
  let prop opts scope tbls f def xs = function
    | O.Property (_, { O.Property.key; pattern = p; default = _; shorthand = _ }) ->
      let (xs, def) =
        match key with
        | O.Property.Identifier (id_loc, { Ast.Identifier.name; comments = _ })
        | O.Property.StringLiteral (id_loc, { Ast.StringLiteral.value = name; _ }) ->
          let id_loc = push_loc tbls id_loc in
          let def = push_pattern tbls (PropP { id_loc; name; def }) in
          (name :: xs, def)
        | O.Property.Computed (_, { Ast.ComputedKey.expression = expr; comments = _ }) ->
          let t = expression opts scope tbls expr in
          let elem = push_pattern_def tbls t in
          let def = push_pattern tbls (ComputedP { elem; def }) in
          (xs, def)
        | O.Property.NumberLiteral (loc, _)
        | O.Property.BigIntLiteral (loc, _) ->
          let loc = push_loc tbls loc in
          let def = push_pattern tbls (UnsupportedLiteralP loc) in
          (xs, def)
      in
      pattern opts scope tbls f def p;
      xs
    | O.RestElement (loc, { Ast.Pattern.RestElement.argument = p; comments = _ }) ->
      let loc = push_loc tbls loc in
      let def = push_pattern tbls (ObjRestP { loc; xs; def }) in
      pattern opts scope tbls f def p;
      xs
  in
  let rec loop opts scope tbls f def xs = function
    | [] -> ()
    | p :: ps ->
      let xs = prop opts scope tbls f def xs p in
      loop opts scope tbls f def xs ps
  in
  (fun opts scope tbls f def props -> loop opts scope tbls f def [] props)

and array_pattern =
  let module A = Ast.Pattern.Array in
  let elem opts scope tbls f def i = function
    | A.Hole _ -> ()
    | A.Element (loc, { A.Element.argument = p; default = _ }) ->
      let loc = push_loc tbls loc in
      let def = push_pattern tbls (IndexP { loc; i; def }) in
      pattern opts scope tbls f def p
    | A.RestElement (loc, { Ast.Pattern.RestElement.argument = p; comments = _ }) ->
      let loc = push_loc tbls loc in
      let def = push_pattern tbls (ArrRestP { loc; i; def }) in
      pattern opts scope tbls f def p
  in
  let rec loop opts scope tbls f def i = function
    | [] -> ()
    | e :: es ->
      elem opts scope tbls f def i e;
      loop opts scope tbls f def (succ i) es
  in
  (fun opts scope tbls f def elems -> loop opts scope tbls f def 0 elems)

and member =
  let module E = Ast.Expression in
  let module M = E.Member in
  let prop_op opts scope tbls = function
    | M.PropertyIdentifier id -> GetProp (id_name id)
    | M.PropertyExpression expr ->
      let t = expression opts scope tbls expr in
      GetElem t
    | M.PropertyPrivateName _ -> failwith "unexpected private name outside class"
  in
  let rec finish opts scope tbls t = function
    | [] -> t
    | (loc, property) :: chain ->
      let op = prop_op opts scope tbls property in
      finish opts scope tbls (Eval (loc, t, op)) chain
  in
  let rec loop ~toplevel_loc opts scope tbls chain (loc, expr) =
    match expr with
    | E.Identifier (id_loc, { Ast.Identifier.name; comments = _ }) ->
      let id_loc = push_loc tbls id_loc in
      let t = val_ref ~type_only:false scope id_loc name in
      finish opts scope tbls t chain
    | E.Member { E.Member._object; property; comments = _ } ->
      let loc = push_loc tbls loc in
      loop ~toplevel_loc opts scope tbls ((loc, property) :: chain) _object
    | _ ->
      Err
        ( toplevel_loc,
          SigError
            (Signature_error.UnexpectedExpression
               (toplevel_loc, Flow_ast_utils.ExpressionSort.Member)
            )
        )
  in
  (fun opts scope tbls obj loc prop -> loop ~toplevel_loc:loc opts scope tbls [(loc, prop)] obj)

and param opts scope tbls xs loc patt ~bind_names default =
  let module P = Ast.Pattern in
  match patt with
  | P.Identifier { P.Identifier.name = id; annot = t; optional } ->
    let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
    let loc = push_loc tbls id_loc in
    let t =
      annot_or_hint
        ~err_loc:(Some loc)
        ~sort:Expected_annotation_sort.Identifier
        opts
        scope
        tbls
        xs
        t
    in
    let name_t =
      if optional && default = None then
        Annot (Optional t)
      else
        t
    in
    let t =
      if optional || default <> None then
        Annot (Optional t)
      else
        t
    in
    let scope =
      if bind_names then (
        let scope = Scope.push_lex scope in
        Scope.bind_var scope tbls Ast.Variable.Let loc name (lazy name_t) ignore2;
        scope
      ) else
        scope
    in
    (Some name, scope, t)
  | P.Object { P.Object.annot = t; properties = _; comments = _ }
  | P.Array { P.Array.annot = t; elements = _; comments = _ } ->
    let patt_with_loc = (loc, patt) in
    let loc = push_loc tbls loc in
    let lazy_t =
      lazy
        (annot_or_hint
           ~err_loc:(Some loc)
           ~sort:
             (match patt with
             | P.Object _ -> Expected_annotation_sort.ObjectPattern
             | _ -> Expected_annotation_sort.ArrayPattern)
           opts
           scope
           tbls
           xs
           t
        )
    in
    let scope =
      if bind_names then (
        let scope = Scope.push_lex scope in
        let f id_loc name p =
          let def = lazy (Pattern p) in
          Scope.bind_var scope tbls Ast.Variable.Let id_loc name def ignore2
        in
        let pattern_def = Lazy.map (push_pattern_def tbls) lazy_t in
        let def = push_pattern tbls (PDef pattern_def) in
        pattern opts scope tbls f def patt_with_loc;
        scope
      ) else
        scope
    in
    let t = Lazy.force lazy_t in
    let t =
      if default <> None then
        Annot (Optional t)
      else
        t
    in
    (None, scope, t)
  | P.Expression _ -> failwith "unexpected expression pattern"

and rest_param opts scope tbls xs param_loc p =
  let module P = Ast.Pattern in
  match p with
  | P.Identifier { P.Identifier.name = (id_loc, _) as id; annot = t; optional = _ } ->
    let loc = push_loc tbls param_loc in
    let name = Some (id_name id) in
    let id_loc = push_loc tbls id_loc in
    let t =
      annot_or_hint
        ~err_loc:(Some id_loc)
        ~sort:Expected_annotation_sort.Identifier
        opts
        scope
        tbls
        xs
        t
    in
    Some (name, loc, t)
  | P.Object { P.Object.annot = t; _ } ->
    let loc = push_loc tbls param_loc in
    let t =
      annot_or_hint
        ~err_loc:(Some loc)
        ~sort:Expected_annotation_sort.ObjectPattern
        opts
        scope
        tbls
        xs
        t
    in
    Some (None, loc, t)
  | P.Array { P.Array.annot = t; _ } ->
    let loc = push_loc tbls param_loc in
    let t =
      annot_or_hint
        ~err_loc:(Some loc)
        ~sort:Expected_annotation_sort.ArrayPattern
        opts
        scope
        tbls
        xs
        t
    in
    Some (None, loc, t)
  | P.Expression _ -> None

and function_def_helper =
  let module F = Ast.Function in
  let rec params opts scope tbls xs acc = function
    | [] -> List.rev acc
    | p :: ps ->
      let (loc, { F.Param.argument = (_, patt); default }) = p in
      let (name, scope, t) = param opts scope tbls xs loc patt ~bind_names:true default in
      let p = FunParam { name; t } in
      params opts scope tbls xs (p :: acc) ps
  in
  let this_param opts scope tbls xs = function
    | None -> None
    | Some (_, { F.ThisParam.annot = (_, t); comments = _ }) ->
      let t = annot opts scope tbls xs t in
      Some t
    (* unexpected rest param pattern *)
  in
  let return opts scope tbls xs ~async ~generator ~constructor body ret =
    if constructor then
      match ret with
      | Ast.Function.ReturnAnnot.Available (loc, _)
      | Ast.Function.ReturnAnnot.Missing loc
      | Ast.Function.ReturnAnnot.TypeGuard (loc, _) ->
        let loc = push_loc tbls loc in
        (Annot (Void loc), None)
    else
      match ret with
      | Ast.Function.ReturnAnnot.Available (_, t) -> (annot opts scope tbls xs t, None)
      | Ast.Function.ReturnAnnot.Missing loc ->
        let loc = push_loc tbls loc in
        if generator || not (Signature_utils.Procedure_decider.is body) then
          let err =
            Err
              ( loc,
                SigError
                  (Signature_error.ExpectedAnnotation (loc, Expected_annotation_sort.FunctionReturn))
              )
          in
          (err, None)
        else if async then
          (AsyncVoidReturn loc, None)
        else
          (Annot (Void loc), None)
      | Ast.Function.ReturnAnnot.TypeGuard (loc, guard) ->
        let loc = push_loc tbls loc in
        let guard = type_guard_opt opts scope tbls xs guard in
        (Annot (Boolean loc), guard)
  in
  let predicate opts scope tbls ps body =
    let module P = Ast.Type.Predicate in
    let module S = Ast.Statement in
    function
    | None -> None
    | Some (_, { P.kind = P.Declared _; _ }) ->
      (* declared predicate no allowed in function declaration *)
      None
    | Some (_, { P.kind = P.Inferred; _ }) ->
      (match body with
      | F.BodyBlock
          ( _,
            {
              S.Block.body =
                [(loc, S.Return { S.Return.argument = Some expr; comments = _; return_out = _ })];
              comments = _;
            }
          )
      | F.BodyExpression ((loc, _) as expr) ->
        let loc = push_loc tbls loc in
        let pnames =
          List.fold_left
            (fun acc p ->
              let (_, { Ast.Function.Param.argument = (_, patt); _ }) = p in
              match patt with
              | Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; _ } ->
                let (_, { Ast.Identifier.name; _ }) = name in
                SSet.add name acc
              | _ -> acc)
            SSet.empty
            ps
        in
        Some (loc, predicate opts scope tbls pnames expr)
      | _ -> None)
  in
  fun opts scope tbls xs ~constructor fun_loc f ->
    let {
      F.id;
      tparams = tps;
      params = (_, { F.Params.params = ps; rest = rp; this_; comments = _ });
      body;
      return = r;
      predicate = p;
      async;
      generator;
      hook;
      sig_loc = _;
      comments = _;
    } =
      f
    in
    let (xs, tparams) = tparams opts scope tbls xs tps in
    let this_param = this_param opts scope tbls xs this_ in
    let params = params opts scope tbls xs [] ps in
    let rest_param =
      match rp with
      | Some (param_loc, { F.RestParam.argument = (_, p); comments = _ }) ->
        let rp = rest_param opts scope tbls xs param_loc p in
        Base.Option.map ~f:(fun (name, loc, t) -> FunRestParam { name; loc; t }) rp
      | None -> None
    in
    let (return, type_guard_opt) =
      return opts scope tbls xs ~async ~generator ~constructor body r
    in
    let predicate =
      let open Option.Let_syntax in
      match type_guard_opt with
      | Some (loc, p) ->
        (* Type-guard and %checks cannot coexist (parse error) *)
        Some (TypeGuard (loc, p))
      | None ->
        let%map (loc, p) = predicate opts scope tbls ps body p in
        Predicate (loc, p)
    in
    let hook =
      match id with
      | _ when hook -> HookDecl fun_loc
      | Some (_, { Ast.Identifier.name; _ })
        when opts.hooklike_functions && Flow_ast_utils.hook_name name ->
        AnyHook
      | _ -> NonHook
    in
    FunSig { tparams; params; rest_param; this_param; return; predicate; hook }

and function_def = function_def_helper ~constructor:false

and constructor_def = function_def_helper ~constructor:true

and component_def =
  let module C = Ast.Statement.ComponentDeclaration in
  let param opts scope tbls xs (loc, p) =
    let { C.Param.name; local = (_, patt); default; _ } = p in
    let (name, name_loc) =
      match name with
      | C.Param.Identifier (loc, { Ast.Identifier.name; _ })
      | C.Param.StringLiteral (loc, { Ast.StringLiteral.value = name; _ }) ->
        (name, loc)
    in
    let name_loc = push_loc tbls name_loc in
    let (_, _, t) = param opts scope tbls xs loc patt ~bind_names:false default in
    ComponentParam { name; name_loc; t }
  in
  let rec params opts scope tbls xs acc = function
    | [] -> List.rev acc
    | p :: ps ->
      let p = param opts scope tbls xs p in
      params opts scope tbls xs (p :: acc) ps
  in
  let renders opts scope tbls xs ret =
    match ret with
    | Ast.Type.AvailableRenders
        (loc, { Ast.Type.Renders.operator_loc = _; comments = _; variant; argument }) ->
      let loc = push_loc tbls loc in
      let t = annot opts scope tbls xs argument in
      Annot (Renders (loc, t, variant))
    | Ast.Type.MissingRenders loc ->
      let loc = push_loc tbls loc in
      Annot
        (Renders
           ( loc,
             maybe_special_unqualified_generic opts scope tbls xs loc None loc "React$Node",
             Ast.Type.Renders.Normal
           )
        )
  in
  fun opts scope tbls f ->
    let {
      C.id = _;
      tparams = tps;
      params = (loc, { C.Params.params = ps; rest = rp; comments = _ });
      body = _;
      renders = r;
      sig_loc = _;
      comments = _;
    } =
      f
    in
    let (xs, tparams) = tparams opts scope tbls SSet.empty tps in
    let loc = push_loc tbls loc in
    let params = params opts scope tbls xs [] ps in
    let rest_param =
      match rp with
      | Some (param_loc, { C.RestParam.argument = (_, p); comments = _ }) ->
        let rp = rest_param opts scope tbls xs param_loc p in
        Base.Option.map ~f:(fun (_, _, t) -> ComponentRestParam { t }) rp
      | None -> None
    in
    let renders = renders opts scope tbls xs r in
    ComponentSig { params_loc = loc; tparams; params; rest_param; renders }

and declare_component_def opts scope tbls f =
  let { Ast.Statement.DeclareComponent.id = _; tparams = tps; params; renders = r; comments = _ } =
    f
  in
  component_type opts scope tbls tps params r

and predicate opts scope tbls pnames =
  let open Option.Let_syntax in
  let module E = Ast.Expression in
  let module I = Ast.Identifier in
  let refinement_key (_, { I.name = id_name; _ }) =
    if SSet.mem id_name pnames then
      Some id_name
    else
      None
  in
  let refinement_prop = function
    | E.Member.PropertyIdentifier (_, { I.name; _ }) -> Some name
    | E.Member.PropertyExpression (_, expr) -> extract_string_literal expr
    | E.Member.PropertyPrivateName _ -> None
  in
  let typeof_typename = function
    | "boolean" -> Some `Boolean
    | "function" -> Some `Function
    | "number" -> Some `Number
    | "bigint" -> Some `BigInt
    | "object" -> Some `Object
    | "string" -> Some `String
    | "symbol" -> Some `Symbol
    | "undefined" -> Some `Undefined
    | _ -> None
  in
  let refine_id test id =
    let%map key = refinement_key id in
    match test with
    | `Exists -> ExistsP key
    | `Instanceof right ->
      let t = expression opts scope tbls right in
      InstanceofP (key, t)
    | `IsArray -> ArrP key
    | `Eq (eq_loc, sense, eq_test) ->
      let pred =
        match eq_test with
        | `Typeof `Boolean -> BoolP (key, eq_loc)
        | `Typeof `Function -> FunP key
        | `Typeof `Number -> NumP (key, eq_loc)
        | `Typeof `BigInt -> BigIntP (key, eq_loc)
        | `Typeof `Object -> ObjP key
        | `Typeof `String -> StrP (key, eq_loc)
        | `Typeof `Symbol -> SymbolP (key, eq_loc)
        | `Typeof `Undefined -> VoidP key
        | `String (loc, x) -> SingletonStrP (key, loc, sense, x)
        | `Number (loc, x, raw) -> SingletonNumP (key, loc, sense, x, raw)
        | `BigInt (loc, x, raw) -> SingletonBigIntP (key, loc, sense, x, raw)
        | `Bool (loc, x) -> SingletonBoolP (key, loc, x)
        | `Null _ -> NullP key
        | `Void _ -> VoidP key
        | `Maybe -> MaybeP key
      in
      if sense then
        pred
      else
        NotP pred
  in
  let refine_member test id prop =
    let%bind key = refinement_key id in
    let%bind prop = refinement_prop prop in
    match test with
    | `Exists -> None
    | `Instanceof _ -> None
    | `IsArray -> None
    | `Eq (_, sense, eq_test) ->
      let%map pred =
        match eq_test with
        | `Typeof _ -> None
        | `String (loc, x) -> Some (SentinelStrP (key, prop, loc, x))
        | `Number (loc, x, raw) -> Some (SentinelNumP (key, prop, loc, x, raw))
        | `BigInt (loc, x, raw) -> Some (SentinelBigIntP (key, prop, loc, x, raw))
        | `Bool (loc, x) -> Some (SentinelBoolP (key, prop, loc, x))
        | `Null loc -> Some (SentinelNullP (key, prop, loc))
        | `Void loc -> Some (SentinelVoidP (key, prop, loc))
        | `Maybe -> None
      in
      if sense then
        pred
      else
        NotP pred
  in
  let refine test (_, expr) =
    match expr with
    | E.Identifier id -> refine_id test id
    | E.Member { E.Member._object = (_, E.Identifier id); property; _ } ->
      refine_member test id property
    | _ ->
      (* TODO: OptionalMember *)
      None
  in
  let eq_test ~strict ~sense eq_loc left right =
    let module T = E.TemplateLiteral in
    match (left, right) with
    | ((_, E.Unary { E.Unary.operator = E.Unary.Typeof; argument = expr; _ }), (_, lit))
    | ((_, lit), (_, E.Unary { E.Unary.operator = E.Unary.Typeof; argument = expr; _ })) ->
      let%bind string_literal = extract_string_literal lit in
      let%bind typename = typeof_typename string_literal in
      refine (`Eq (eq_loc, sense, `Typeof typename)) expr
    | ((loc, E.StringLiteral { Ast.StringLiteral.value; _ }), expr)
    | (expr, (loc, E.StringLiteral { Ast.StringLiteral.value; _ }))
      when strict ->
      let loc = push_loc tbls loc in
      refine (`Eq (eq_loc, sense, `String (loc, value))) expr
    | ((loc, E.BooleanLiteral { Ast.BooleanLiteral.value; _ }), expr)
    | (expr, (loc, E.BooleanLiteral { Ast.BooleanLiteral.value; _ }))
      when strict ->
      let loc = push_loc tbls loc in
      refine (`Eq (eq_loc, sense, `Bool (loc, value))) expr
    | ((loc, E.NumberLiteral { Ast.NumberLiteral.value; raw; _ }), expr)
    | (expr, (loc, E.NumberLiteral { Ast.NumberLiteral.value; raw; _ }))
      when strict ->
      let loc = push_loc tbls loc in
      refine (`Eq (eq_loc, sense, `Number (loc, value, raw))) expr
    | ((loc, E.BigIntLiteral { Ast.BigIntLiteral.value; raw; _ }), expr)
    | (expr, (loc, E.BigIntLiteral { Ast.BigIntLiteral.value; raw; _ }))
      when strict ->
      let loc = push_loc tbls loc in
      refine (`Eq (eq_loc, sense, `BigInt (loc, value, raw))) expr
    | ((loc, E.NullLiteral _), expr)
    | (expr, (loc, E.NullLiteral _)) ->
      let test =
        if strict then
          let loc = push_loc tbls loc in
          `Null loc
        else
          `Maybe
      in
      refine (`Eq (eq_loc, sense, test)) expr
    | ((loc, E.TemplateLiteral { T.quasis; expressions = []; _ }), expr)
    | (expr, (loc, E.TemplateLiteral { T.quasis; expressions = []; _ }))
      when strict ->
      (match quasis with
      | [(_, { T.Element.value = { T.Element.cooked = x; _ }; _ })] ->
        let loc = push_loc tbls loc in
        refine (`Eq (eq_loc, sense, `String (loc, x))) expr
      | _ -> None)
    | ((loc, E.Unary { E.Unary.operator = E.Unary.Minus; argument = (_, lit); _ }), expr)
    | (expr, (loc, E.Unary { E.Unary.operator = E.Unary.Minus; argument = (_, lit); _ }))
      when strict ->
      let%bind (x, raw) = extract_number_literal lit in
      let loc = push_loc tbls loc in
      refine (`Eq (eq_loc, sense, `Number (loc, -.x, "-" ^ raw))) expr
    | ((loc, E.Unary { E.Unary.operator = E.Unary.Void; _ }), expr)
    | (expr, (loc, E.Unary { E.Unary.operator = E.Unary.Void; _ }))
    | ((loc, E.Identifier (_, { I.name = "undefined"; _ })), expr)
    | (expr, (loc, E.Identifier (_, { I.name = "undefined"; _ }))) ->
      (* TODO: should not refine if undefined is shadowed *)
      let eq_test =
        if strict then
          let loc = push_loc tbls loc in
          `Void loc
        else
          `Maybe
      in
      refine (`Eq (eq_loc, sense, eq_test)) expr
    | ((_, E.Member { E.Member._object = (_, E.Identifier id); property; _ }), expr)
    | (expr, (_, E.Member { E.Member._object = (_, E.Identifier id); property; _ })) ->
      let%bind key = refinement_key id in
      let%map prop = refinement_prop property in
      let t = expression opts scope tbls expr in
      SentinelExprP (key, prop, t)
    | _ -> None
  in
  let logical expr1 expr2 =
    let open E.Logical in
    function
    | And ->
      let p1 = predicate opts scope tbls pnames expr1 in
      let p2 = predicate opts scope tbls pnames expr2 in
      Option.merge p1 p2 ~f:(fun p1 p2 -> AndP (p1, p2))
    | Or ->
      let p1 = predicate opts scope tbls pnames expr1 in
      let p2 = predicate opts scope tbls pnames expr2 in
      Option.merge p1 p2 ~f:(fun p1 p2 -> OrP (p1, p2))
    | NullishCoalesce -> None
  in
  let unary expr =
    let module U = E.Unary in
    function
    | U.Not ->
      let%map p = predicate opts scope tbls pnames expr in
      NotP p
    | U.Minus
    | U.Plus
    | U.BitNot
    | U.Typeof
    | U.Void
    | U.Delete
    | U.Await ->
      None
  in
  let binary loc left right =
    let open E.Binary in
    function
    | Instanceof -> refine (`Instanceof right) left
    | Equal -> eq_test ~strict:false ~sense:true loc left right
    | StrictEqual -> eq_test ~strict:true ~sense:true loc left right
    | NotEqual -> eq_test ~strict:false ~sense:false loc left right
    | StrictNotEqual -> eq_test ~strict:true ~sense:false loc left right
    | LessThan
    | LessThanEqual
    | GreaterThan
    | GreaterThanEqual
    | LShift
    | RShift
    | RShift3
    | Plus
    | Minus
    | Mult
    | Exp
    | Div
    | Mod
    | BitOr
    | Xor
    | BitAnd
    | In ->
      None
  in
  let targ = function
    | E.CallTypeArg.Implicit (loc, _) -> ImplicitArg (push_loc tbls loc)
    | E.CallTypeArg.Explicit t -> ExplicitArg (annot opts scope tbls pnames t)
  in
  let targs_ = function
    | None -> None
    | Some (_, { E.CallTypeArgs.arguments; _ }) -> Some (Base.List.map arguments ~f:targ)
  in
  let arg_list args =
    Base.List.map args.E.ArgList.arguments ~f:(function
        | E.Expression e -> Some (Arg (expression opts scope tbls e))
        | E.Spread _ -> None
        )
    |> Base.Option.all
  in
  let call callee targs args =
    let finish = function
      | [] -> None
      | x :: xs ->
        let t = expression opts scope tbls callee in
        let targs = targs_ targs in
        let args_opt = arg_list args in
        Base.Option.map args_opt ~f:(fun args -> LatentP (t, targs, args, (x, xs)))
    in
    let f acc i = function
      | E.Expression (_, E.Identifier id) ->
        Some
          (match refinement_key id with
          | Some key -> (key, i) :: acc
          | None -> acc)
      | E.Expression _ -> Some acc
      | E.Spread _ -> None
    in
    let rec loop acc i = function
      | [] -> finish acc
      | arg :: args ->
        let%bind acc = f acc i arg in
        loop acc (i + 1) args
    in
    match (callee, args) with
    | ( ( _,
          E.Member
            {
              E.Member._object = (_, E.Identifier (_, { I.name = "Array"; comments = _ }));
              property = E.Member.PropertyIdentifier (_, { I.name = "isArray"; comments = _ });
              comments = _;
            }
        ),
        { E.ArgList.arguments = [E.Expression arg]; comments = _ }
      ) ->
      refine `IsArray arg
    | (_, { E.ArgList.arguments; comments = _ }) -> loop [] 0 arguments
  in
  fun (loc, expr) ->
    let loc = push_loc tbls loc in
    match expr with
    | E.Identifier id -> refine_id `Exists id
    | E.Logical { E.Logical.operator; left; right; comments = _ } -> logical left right operator
    | E.Unary { E.Unary.operator; argument; comments = _ } -> unary argument operator
    | E.Binary { E.Binary.operator; left; right; comments = _ } -> binary loc left right operator
    | E.Call { E.Call.callee; targs; arguments = (_, args); comments = _ } -> call callee targs args
    | _ -> None

and class_def =
  let module C = Ast.Class in
  let module Acc = ClassAcc in
  let mk_extends opts scope tbls xs = function
    | None -> ClassImplicitExtends
    | Some (loc, { C.Extends.expr; targs; comments = _ }) ->
      let loc = push_loc tbls loc in
      let t = expression opts scope tbls expr in
      (match targs with
      | None -> ClassExplicitExtends { loc; t }
      | Some (_, { Ast.Type.TypeArgs.arguments = targs; comments = _ }) ->
        let targs = List.map (annot opts scope tbls xs) targs in
        ClassExplicitExtendsApp { loc; t; targs })
  in
  let props opts scope tbls xs elements acc =
    List.fold_left
      (fun acc ->
        let module P = Ast.Expression.Object.Property in
        function
        | C.Body.Method
            ( fn_loc,
              {
                C.Method.key = P.Identifier (id_loc, { Ast.Identifier.name; comments = _ });
                value = (_, fn);
                kind;
                static;
                decorators = _;
                comments = _;
              }
            ) ->
          if opts.munge && Signature_utils.is_munged_property_string name then
            acc
          else begin
            match kind with
            | C.Method.Method ->
              let { Ast.Function.async; generator; _ } = fn in
              let fn_loc = push_loc tbls fn_loc in
              let id_loc = push_loc tbls id_loc in
              let def = function_def opts scope tbls xs fn_loc fn in
              Acc.add_method ~static name id_loc fn_loc ~async ~generator def acc
            | C.Method.Constructor ->
              let { Ast.Function.async; generator; _ } = fn in
              let fn_loc = push_loc tbls fn_loc in
              let id_loc = push_loc tbls id_loc in
              let def = constructor_def opts scope tbls xs fn_loc fn in
              Acc.add_method ~static name id_loc fn_loc ~async ~generator def acc
            | C.Method.Get ->
              let id_loc = push_loc tbls id_loc in
              let getter = getter_def opts scope tbls xs id_loc fn in
              Acc.add_accessor ~static name getter acc
            | C.Method.Set ->
              let id_loc = push_loc tbls id_loc in
              let setter = setter_def opts scope tbls xs id_loc fn in
              Acc.add_accessor ~static name setter acc
          end
        | C.Body.Property
            ( prop_loc,
              {
                C.Property.key = P.Identifier (id_loc, { Ast.Identifier.name; comments = _ });
                annot = t;
                value;
                static;
                variance;
                decorators = _;
                comments = _;
              }
            ) ->
          if opts.munge && Signature_utils.is_munged_property_string name then
            acc
          else
            let (id_loc, t) =
              match t with
              | Ast.Type.Available (_, t) ->
                let id_loc = push_loc tbls id_loc in
                (id_loc, annot opts scope tbls xs t)
              | Ast.Type.Missing _ ->
                let prop_loc = push_loc tbls prop_loc in
                let id_loc = push_loc tbls id_loc in
                let res =
                  match value with
                  | C.Property.Initialized e -> expression opts scope tbls e
                  | C.Property.Declared
                  | C.Property.Uninitialized ->
                    Err
                      ( prop_loc,
                        SigError
                          (Signature_error.ExpectedAnnotation
                             (prop_loc, Expected_annotation_sort.Property { name })
                          )
                      )
                in
                (id_loc, res)
            in
            Acc.add_field ~static name id_loc (polarity variance) t acc
        | C.Body.PrivateField _ -> acc (* private fields are unreachable from exports *)
        | C.Body.Method
            (_, { C.Method.key = P.StringLiteral _ | P.NumberLiteral _ | P.BigIntLiteral _; _ })
        | C.Body.Property
            (_, { C.Property.key = P.StringLiteral _ | P.NumberLiteral _ | P.BigIntLiteral _; _ })
          ->
          acc (* unsupported literal method/field *)
        | C.Body.Method (_, { C.Method.key = P.Computed _; _ })
        | C.Body.Property (_, { C.Property.key = P.Computed _; _ }) ->
          acc (* unsupported computed method/field *)
        | C.Body.Method (_, { C.Method.key = P.PrivateName _; _ })
        | C.Body.Property (_, { C.Property.key = P.PrivateName _; _ }) ->
          acc
        (* unexpected non-private method/field with private name *))
      acc
      elements
  in
  fun opts scope tbls decl ->
    let {
      Ast.Class.id = _;
      body = (_, { Ast.Class.Body.body = elements; comments = _ });
      tparams = tps;
      extends;
      implements;
      class_decorators = _;
      comments = _;
    } =
      decl
    in
    let (xs, tparams) = tparams opts scope tbls SSet.empty tps in
    let xs = SSet.add "this" xs in
    let extends = mk_extends opts scope tbls xs extends in
    let implements = class_implements opts scope tbls xs implements in
    Acc.empty |> props opts scope tbls xs elements |> Acc.class_def tparams extends implements

and object_literal =
  let module O = Ast.Expression.Object in
  let module P = O.Property in
  let module Acc = ObjectLiteralAcc in
  let prop opts scope tbls acc prop_loc = function
    | P.Init
        {
          key =
            ( P.Identifier (id_loc, { Ast.Identifier.name; comments = _ })
            | P.StringLiteral (id_loc, { Ast.StringLiteral.value = name; _ }) );
          value;
          shorthand = _;
        } ->
      let id_loc = push_loc tbls id_loc in
      let loc = push_loc tbls (fst value) in
      let t = expression opts scope tbls value in
      if name = "__proto__" then
        Acc.add_proto (loc, t) acc
      else
        Acc.add_field name id_loc t acc
    | P.Method
        {
          key =
            ( P.Identifier (id_loc, { Ast.Identifier.name; comments = _ })
            | P.StringLiteral (id_loc, { Ast.StringLiteral.value = name; _ }) );
          value = (_, fn);
        } ->
      let { Ast.Function.async; generator; _ } = fn in
      let fn_loc = push_loc tbls prop_loc in
      let id_loc = push_loc tbls id_loc in
      let def = function_def opts scope tbls SSet.empty fn_loc fn in
      Acc.add_method name id_loc fn_loc ~async ~generator def acc
    | P.Get
        {
          key =
            ( P.Identifier (id_loc, { Ast.Identifier.name; comments = _ })
            | P.StringLiteral (id_loc, { Ast.StringLiteral.value = name; _ }) );
          value = (_, fn);
          comments = _;
        } ->
      let id_loc = push_loc tbls id_loc in
      let getter = getter_def opts scope tbls SSet.empty id_loc fn in
      Acc.add_accessor name getter acc
    | P.Set
        {
          key =
            ( P.Identifier (id_loc, { Ast.Identifier.name; comments = _ })
            | P.StringLiteral (id_loc, { Ast.StringLiteral.value = name; _ }) );
          value = (_, fn);
          comments = _;
        } ->
      let id_loc = push_loc tbls id_loc in
      let setter = setter_def opts scope tbls SSet.empty id_loc fn in
      Acc.add_accessor name setter acc
    | P.Init { key = P.NumberLiteral _ | P.BigIntLiteral _; _ }
    | P.Method { key = P.NumberLiteral _ | P.BigIntLiteral _; _ }
    | P.Get { key = P.NumberLiteral _ | P.BigIntLiteral _; _ }
    | P.Set { key = P.NumberLiteral _ | P.BigIntLiteral _; _ } ->
      acc (* unsupported non-string literal key *)
    | P.Get { key = P.Computed _; _ }
    | P.Set { key = P.Computed _; _ } ->
      acc (* unsupported computed get/set key *)
    | P.Init { key = P.Computed _; _ }
    | P.Method { key = P.Computed _; _ } ->
      failwith "unexpected computed field key" (* handled in loop below *)
    | P.Init { key = P.PrivateName _; _ }
    | P.Method { key = P.PrivateName _; _ }
    | P.Get { key = P.PrivateName _; _ }
    | P.Set { key = P.PrivateName _; _ } ->
      failwith "unexpected private field in object literal"
  in
  let spread opts scope tbls acc p =
    let { O.SpreadProperty.argument = e; comments = _ } = p in
    let t = expression opts scope tbls e in
    Acc.add_spread t acc
  in
  let rec loop opts scope tbls loc ~frozen acc = function
    | [] -> Acc.object_lit loc ~frozen acc
    | p :: ps ->
      (match p with
      | O.SpreadProperty (_, p) ->
        let acc = spread opts scope tbls acc p in
        loop opts scope tbls loc ~frozen acc ps
      | O.Property (prop_loc, P.Init { key = P.Computed _; _ })
      | O.Property (prop_loc, P.Method { key = P.Computed _; _ }) ->
        (* TODO: Instead of stopping at the first unexpected key, we should
         * accumulate a list of all signature errors in the object literal, so
         * the user can fix all of the errors at once instead of one-by-one. *)
        let prop_loc = push_loc tbls prop_loc in
        Err (loc, SigError (Signature_error.UnexpectedObjectKey (loc, prop_loc)))
      | O.Property (prop_loc, p) ->
        let acc = prop opts scope tbls acc prop_loc p in
        loop opts scope tbls loc ~frozen acc ps)
  in
  fun opts scope tbls loc ~frozen properties ->
    loop opts scope tbls loc ~frozen Acc.empty properties

and array_literal =
  let module E = Ast.Expression in
  let finish loc = function
    | [] -> Err (loc, SigError (Signature_error.EmptyArray loc))
    | t :: ts -> Value (ArrayLit (loc, t, ts))
  in
  let rec loop opts scope tbls loc acc = function
    | [] -> finish loc (List.rev acc)
    | elem :: elems ->
      (match elem with
      | E.Array.Hole _ -> Err (loc, SigError (Signature_error.UnexpectedArrayHole loc))
      | E.Array.Expression expr ->
        let t = expression opts scope tbls expr in
        loop opts scope tbls loc (t :: acc) elems
      | E.Array.Spread (spread_loc, _) ->
        let spread_loc = push_loc tbls spread_loc in
        Err (loc, SigError (Signature_error.UnexpectedArraySpread (loc, spread_loc))))
  in
  (fun opts scope tbls loc elements -> loop opts scope tbls loc [] elements)

let rec member_expr_of_generic_id scope tbls chain =
  let module G = Ast.Type.Generic in
  function
  | G.Identifier.Qualified (loc, { G.Identifier.qualification; id }) ->
    let loc = push_loc tbls loc in
    let name = id_name id in
    let chain = (loc, name) :: chain in
    member_expr_of_generic_id scope tbls chain qualification
  | G.Identifier.Unqualified id ->
    let (ref_loc, { Ast.Identifier.name; comments = _ }) = id in
    let ref_loc = push_loc tbls ref_loc in
    List.fold_left
      (fun t (loc, name) -> Eval (loc, t, GetProp name))
      (val_ref ~type_only:true scope ref_loc name)
      chain

let declare_class_def =
  let module Acc = DeclareClassAcc in
  let is_object_builtin_libdef id =
    let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
    name = "Object"
    &&
    match Loc.source id_loc with
    | None -> false
    | Some source -> File_key.is_lib_file source
  in
  let mk_extends opts scope tbls xs id = function
    | None ->
      if is_object_builtin_libdef id then
        ObjectPrototypeExtendsNull
      else
        ClassImplicitExtends
    | Some (loc, { Ast.Type.Generic.id; targs; comments = _ }) ->
      let loc = push_loc tbls loc in
      let t = member_expr_of_generic_id scope tbls [] id in
      (match targs with
      | None -> ClassExplicitExtends { loc; t }
      | Some (_, { Ast.Type.TypeArgs.arguments = targs; comments = _ }) ->
        let targs = List.map (annot opts scope tbls xs) targs in
        ClassExplicitExtendsApp { loc; t; targs })
  in
  let rec mk_mixins opts scope tbls xs acc = function
    | [] -> List.rev acc
    | (loc, { Ast.Type.Generic.id; targs; comments = _ }) :: ms ->
      let loc = push_loc tbls loc in
      let t = member_expr_of_generic_id scope tbls [] id in
      let m =
        match targs with
        | None -> ClassMixin { loc; t }
        | Some (_, { Ast.Type.TypeArgs.arguments = targs; comments = _ }) ->
          let targs = List.map (annot opts scope tbls xs) targs in
          ClassMixinApp { loc; t; targs }
      in
      mk_mixins opts scope tbls xs (m :: acc) ms
  in
  fun opts scope tbls decl ->
    let {
      Ast.Statement.DeclareClass.id;
      tparams = tps;
      body = (_, { Ast.Type.Object.properties; exact = _; inexact = _; comments = _ });
      extends;
      mixins;
      implements;
      comments = _;
    } =
      decl
    in
    let (xs, tparams) = tparams opts scope tbls SSet.empty tps in
    let xs = SSet.add "this" xs in
    let extends = mk_extends opts scope tbls xs id extends in
    let mixins = mk_mixins opts scope tbls xs [] mixins in
    let implements = class_implements opts scope tbls xs implements in
    Acc.empty
    |> declare_class_props opts scope tbls xs properties
    |> Acc.declare_class_def tparams extends mixins implements

let type_alias_decl opts scope tbls decl =
  let {
    Ast.Statement.TypeAlias.id = (id_loc, { Ast.Identifier.name; comments = _ });
    tparams = tps;
    right = t;
    comments = _;
  } =
    decl
  in
  let id_loc = push_loc tbls id_loc in
  let def =
    lazy
      (splice tbls id_loc (fun tbls ->
           let (xs, tparams) = tparams opts scope tbls SSet.empty tps in
           let body = annot opts scope tbls xs t in
           TypeAlias { id_loc; name; tparams; body }
       )
      )
  in
  Scope.bind_type scope tbls id_loc name def

let opaque_type_decl opts scope tbls decl =
  let {
    Ast.Statement.OpaqueType.id = (id_loc, { Ast.Identifier.name; comments = _ });
    tparams = tps;
    supertype;
    impltype;
    comments = _;
  } =
    decl
  in
  let id_loc = push_loc tbls id_loc in
  let def =
    lazy
      (splice tbls id_loc (fun tbls ->
           let (xs, tparams) = tparams opts scope tbls SSet.empty tps in
           let bound = Base.Option.map ~f:(annot opts scope tbls xs) supertype in
           let body = Base.Option.map ~f:(annot opts scope tbls xs) impltype in
           OpaqueType { id_loc; name; tparams; bound; body }
       )
      )
  in
  Scope.bind_type scope tbls id_loc name def

let rec const_var_init_decl opts scope tbls id_loc name k expr =
  let module E = Ast.Expression in
  match expr with
  (* const x = id *)
  | (_, E.Identifier (ref_loc, { Ast.Identifier.name = ref_name; comments = _ })) ->
    let ref_loc = push_loc tbls ref_loc in
    Scope.bind_const_ref scope tbls id_loc name ref_loc ref_name scope k
  (* const x = function *)
  | (_, E.Function f) ->
    let { Ast.Function.id = fn_id; async; generator; sig_loc; _ } = f in
    let sig_loc = push_loc tbls sig_loc in
    begin
      match fn_id with
      | Some (fn_id_loc, { Ast.Identifier.name = fn_name; comments = _ }) ->
        let fn_id_loc = push_loc tbls fn_id_loc in
        let fn_scope = Scope.push_lex scope in
        let def =
          lazy
            (splice tbls fn_id_loc (fun tbls ->
                 function_def opts fn_scope tbls SSet.empty fn_id_loc f
             )
            )
        in
        Scope.bind_function
          fn_scope
          tbls
          fn_id_loc
          sig_loc
          fn_name
          ~async
          ~generator
          ~hook:false
          def
          ignore2;
        Scope.bind_const_ref scope tbls id_loc name fn_id_loc fn_name fn_scope k
      | None ->
        let def =
          lazy (splice tbls sig_loc (fun tbls -> function_def opts scope tbls SSet.empty sig_loc f))
        in
        Scope.bind_const_fun scope tbls id_loc name sig_loc ~async ~generator def k
    end
  (* const x = arrow function *)
  | (loc, E.ArrowFunction f) ->
    let { Ast.Function.async; generator; _ } = f in
    let loc = push_loc tbls loc in
    let def = lazy (splice tbls loc (fun tbls -> function_def opts scope tbls SSet.empty loc f)) in
    Scope.bind_const_fun scope tbls id_loc name loc ~async ~generator def k
  (* const x = a, b *)
  | (_, E.Sequence { E.Sequence.expressions; comments = _ }) ->
    sequence (const_var_init_decl opts scope tbls id_loc name k) expressions
  (* const x = ... fallback *)
  | _ ->
    let def = lazy (splice tbls id_loc (fun tbls -> expression opts scope tbls expr)) in
    Scope.bind_const scope tbls id_loc name def k

let variable_decl opts scope tbls kind k decl =
  let module V = Ast.Statement.VariableDeclaration in
  let module P = Ast.Pattern in
  let (_, { V.Declarator.id = p; init }) = decl in
  match snd p with
  | P.Identifier { P.Identifier.name; annot; optional = _ } ->
    let (id_loc, { Ast.Identifier.name; comments = _ }) = name in
    let id_loc = push_loc tbls id_loc in
    begin
      match (kind, annot, init) with
      (* const x = ... special cases *)
      | (Ast.Variable.Const, Ast.Type.Missing _, Some expr) ->
        const_var_init_decl opts scope tbls id_loc name k expr
      | _ ->
        let def =
          lazy
            (splice tbls id_loc (fun tbls ->
                 annot_or_hint
                   ~err_loc:(Some id_loc)
                   ~sort:(Expected_annotation_sort.VariableDefinition { name })
                   opts
                   scope
                   tbls
                   SSet.empty
                   annot
             )
            )
        in
        Scope.bind_var scope tbls kind id_loc name def k
    end
  | P.Object { P.Object.annot; _ }
  | P.Array { P.Array.annot; _ } ->
    let f id_loc name p =
      let def = lazy (Pattern p) in
      Scope.bind_var scope tbls kind id_loc name def k
    in
    let splice_loc_ref = ref None in
    let pattern_def =
      lazy
        (let splice_loc = Option.value_exn !splice_loc_ref in
         let def =
           splice tbls splice_loc (fun tbls ->
               match (kind, annot, init) with
               | (Ast.Variable.Const, Ast.Type.Missing _, Some expr) ->
                 expression opts scope tbls expr
               | _ ->
                 annot_or_hint
                   ~err_loc:None
                   ~sort:Expected_annotation_sort.ArrayPattern
                   opts
                   scope
                   tbls
                   SSet.empty
                   annot
           )
         in
         push_pattern_def tbls def
        )
    in
    let def = push_pattern tbls (PDef pattern_def) in
    pattern opts scope tbls f def p;
    splice_loc_ref := Some (Locs.tail_exn tbls.locs)
  | P.Expression _ -> failwith "unexpected expression pattern"

let variable_decls opts scope tbls decl k =
  let { Ast.Statement.VariableDeclaration.kind; declarations; comments = _ } = decl in
  List.iter (variable_decl opts scope tbls kind k) declarations

let class_decl opts scope tbls decl =
  let id = Base.Option.value_exn decl.Ast.Class.id in
  let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
  let id_loc = push_loc tbls id_loc in
  let def = lazy (splice tbls id_loc (fun tbls -> class_def opts scope tbls decl)) in
  Scope.bind_class scope tbls id_loc name def

let function_decl opts scope tbls decl =
  let { Ast.Function.id; async; generator; hook; sig_loc; _ } = decl in
  let (id_loc, { Ast.Identifier.name; comments = _ }) = Base.Option.value_exn id in
  let sig_loc = push_loc tbls sig_loc in
  let id_loc = push_loc tbls id_loc in
  let def =
    lazy (splice tbls id_loc (fun tbls -> function_def opts scope tbls SSet.empty id_loc decl))
  in
  Scope.bind_function scope tbls id_loc sig_loc name ~async ~generator ~hook def

let component_decl opts scope tbls decl =
  let {
    Ast.Statement.ComponentDeclaration.id = (id_loc, { Ast.Identifier.name; comments = _ });
    sig_loc;
    _;
  } =
    decl
  in
  let sig_loc = push_loc tbls sig_loc in
  let id_loc = push_loc tbls id_loc in
  let def =
    if opts.enable_component_syntax then
      Some (lazy (splice tbls id_loc (fun tbls -> component_def opts scope tbls decl)))
    else
      None
  in
  Scope.bind_component scope tbls id_loc sig_loc name def

let declare_component_decl opts scope tbls c_loc decl =
  let { Ast.Statement.DeclareComponent.id = (id_loc, { Ast.Identifier.name; comments = _ }); _ } =
    decl
  in
  let sig_loc = push_loc tbls c_loc in
  let id_loc = push_loc tbls id_loc in
  let def =
    if opts.enable_component_syntax then
      Some (lazy (splice tbls id_loc (fun tbls -> declare_component_def opts scope tbls decl)))
    else
      None
  in
  Scope.bind_component scope tbls id_loc sig_loc name def

let declare_variable_decl opts scope tbls decl =
  let { Ast.Statement.DeclareVariable.id; annot = (_, t); kind; comments = _ } = decl in
  let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
  let id_loc = push_loc tbls id_loc in
  let def = lazy (splice tbls id_loc (fun tbls -> annot opts scope tbls SSet.empty t)) in
  Scope.bind_var scope tbls kind id_loc name def

let declare_function_decl opts scope tbls decl =
  let {
    Ast.Statement.DeclareFunction.id = (id_loc, { Ast.Identifier.name; comments = _ });
    annot = (_, (fn_loc, t));
    predicate = p;
    comments = _;
  } =
    decl
  in
  let id_loc = push_loc tbls id_loc in
  let fn_loc = push_loc tbls fn_loc in
  let def =
    lazy
      (splice tbls fn_loc (fun tbls ->
           let module T = Ast.Type in
           match t with
           | T.Function
               {
                 T.Function.tparams = tps;
                 params = (_, { T.Function.Params.params = ps; rest = rp; this_; comments = _ });
                 return = r;
                 hook;
                 comments = _;
               } ->
             let (xs, tparams) = tparams opts scope tbls SSet.empty tps in
             let this_param = function_type_this_param opts scope tbls xs this_ in
             let params = function_type_params opts scope tbls xs ps in
             let rest_param = function_type_rest_param opts scope tbls xs rp in
             let (return, type_guard) = return_annot opts scope tbls xs r in
             let predicate =
               let module P = T.Predicate in
               match (type_guard, p) with
               | (Some (TypeGuard _), _) -> type_guard
               | (_, None) -> None
               | (_, Some (loc, { P.kind = P.Declared expr; _ })) ->
                 let loc = push_loc tbls loc in
                 let pnames =
                   List.fold_left
                     (fun acc p ->
                       let (_, { Ast.Type.Function.Param.name; _ }) = p in
                       match name with
                       | None -> acc
                       | Some (_, { Ast.Identifier.name; _ }) -> SSet.add name acc)
                     SSet.empty
                     ps
                 in
                 Some (Predicate (loc, predicate opts scope tbls pnames expr))
               | (_, Some (_, { P.kind = P.Inferred; _ })) ->
                 (* inferred predicate not allowed in declared function *)
                 None
             in
             let hook =
               if hook then
                 HookAnnot
               else if opts.hooklike_functions && Flow_ast_utils.hook_name name then
                 AnyHook
               else
                 NonHook
             in
             FunSig { tparams; params; rest_param; this_param; return; predicate; hook }
           | _ -> failwith "unexpected declare function annot"
       )
      )
  in
  Scope.bind_declare_function scope tbls id_loc fn_loc name def

let declare_class_decl opts scope tbls decl =
  let (id_loc, { Ast.Identifier.name; comments = _ }) = decl.Ast.Statement.DeclareClass.id in
  let id_loc = push_loc tbls id_loc in
  let def = lazy (splice tbls id_loc (fun tbls -> declare_class_def opts scope tbls decl)) in
  Scope.bind_declare_class scope tbls id_loc name def

let namespace_decl
    opts
    scope
    tbls
    ~is_type_only
    ~visit_statement
    {
      Ast.Statement.DeclareNamespace.id = (id_loc, { Ast.Identifier.name; _ });
      body = (_, { Ast.Statement.Block.body = stmts; comments = _ });
      comments = _;
    } =
  let id_loc = push_loc tbls id_loc in
  let stmts =
    Base.List.filter stmts ~f:(fun (_, stmt) ->
        Flow_ast_utils.acceptable_statement_in_declaration_context ~in_declare_namespace:true stmt
        |> Base.Result.is_ok
    )
  in
  let scope = Scope.push_declare_namespace scope in
  List.iter (visit_statement opts scope tbls) stmts;
  Scope.finalize_declare_namespace_exn ~is_type_only scope tbls id_loc name

let import_decl _opts scope tbls decl =
  let module I = Ast.Statement.ImportDeclaration in
  let {
    I.source = (_, { Ast.StringLiteral.value = mref; _ });
    default;
    specifiers;
    import_kind = kind;
    comments = _;
  } =
    decl
  in
  begin
    match default with
    | None -> ()
    | Some
        {
          I.identifier = (id_loc, { Ast.Identifier.name = local; comments = _ });
          remote_default_name_def_loc = _;
        } ->
      let id_loc = push_loc tbls id_loc in
      Scope.bind_import scope tbls kind id_loc ~local ~remote:"default" mref
  end;
  match specifiers with
  | None -> ()
  | Some (I.ImportNamespaceSpecifier (_, id)) ->
    let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
    let id_loc = push_loc tbls id_loc in
    Scope.bind_import_ns scope tbls kind id_loc name mref
  | Some (I.ImportNamedSpecifiers specifiers) ->
    List.iter
      (fun specifier ->
        let {
          I.kind = kind_opt;
          local = local_opt;
          remote = (remote_id_loc, { Ast.Identifier.name = remote; comments = _ });
          remote_name_def_loc = _;
        } =
          specifier
        in
        let kind =
          match kind_opt with
          | Some k -> k
          | None -> kind
        in
        let (local_id_loc, local) =
          match local_opt with
          | Some (id_loc, { Ast.Identifier.name; comments = _ }) -> (id_loc, name)
          | None -> (remote_id_loc, remote)
        in
        let local_id_loc = push_loc tbls local_id_loc in
        Scope.bind_import scope tbls kind local_id_loc ~local ~remote mref)
      specifiers

let interface_decl opts scope tbls decl =
  let {
    Ast.Statement.Interface.id = (id_loc, { Ast.Identifier.name; comments = _ });
    tparams = tps;
    body = (_, { Ast.Type.Object.properties; exact = _; inexact = _; comments = _ });
    extends;
    comments = _;
  } =
    decl
  in
  let id_loc = push_loc tbls id_loc in
  let def =
    lazy
      (splice tbls id_loc (fun tbls ->
           let (xs, tparams) = tparams opts scope tbls SSet.empty tps in
           let def = interface_def opts scope tbls xs extends properties in
           Interface { id_loc; name; tparams; def }
       )
      )
  in
  Scope.bind_type scope tbls id_loc name def

let enum_decl =
  let module E = Ast.Statement.EnumDeclaration in
  let defaulted_member tbls members member =
    let (loc, { E.DefaultedMember.id }) = member in
    let (_, { Ast.Identifier.name; comments = _ }) = id in
    let loc = push_loc tbls loc in
    SMap.add name loc members
  in
  let initialized_member f tbls (rep, members) member =
    let (loc, { E.InitializedMember.id; init }) = member in
    let (_, { Ast.Identifier.name; comments = _ }) = id in
    let loc = push_loc tbls loc in
    let rep = f rep init in
    let members = SMap.add name loc members in
    (rep, members)
  in
  let boolean_rep lit init =
    let (_, { Ast.BooleanLiteral.value; _ }) = init in
    match lit with
    | None -> Some value
    | Some _ -> None
  in
  let string_rep truthy init =
    let (_, { Ast.StringLiteral.value; _ }) = init in
    if value = "" then
      false
    else
      truthy
  in
  let number_rep truthy init =
    let (_, { Ast.NumberLiteral.value; _ }) = init in
    if value = 0. then
      false
    else
      truthy
  in
  let bigint_rep truthy init =
    let (_, { Ast.BigIntLiteral.value; _ }) = init in
    if value = Some 0L then
      false
    else
      truthy
  in
  let boolean_member = initialized_member boolean_rep in
  let string_member = initialized_member string_rep in
  let number_member = initialized_member number_rep in
  let bigint_member = initialized_member bigint_rep in
  let defaulted_members tbls = List.fold_left (defaulted_member tbls) SMap.empty in
  let initialized_members tbls f rep = List.fold_left (f tbls) (rep, SMap.empty) in
  let string_enum_def tbls has_unknown_members = function
    | E.StringBody.Initialized members ->
      let (truthy, members) = initialized_members tbls string_member true members in
      (StringRep { truthy }, members, has_unknown_members)
    | E.StringBody.Defaulted members ->
      let members = defaulted_members tbls members in
      (StringRep { truthy = true }, members, has_unknown_members)
  in
  let enum_def tbls = function
    | E.BooleanBody { E.BooleanBody.members; has_unknown_members; _ } ->
      let (lit, members) = initialized_members tbls boolean_member None members in
      (BoolRep lit, members, has_unknown_members)
    | E.NumberBody { E.NumberBody.members; has_unknown_members; _ } ->
      let (truthy, members) = initialized_members tbls number_member true members in
      (NumberRep { truthy }, members, has_unknown_members)
    | E.StringBody { E.StringBody.members; has_unknown_members; _ } ->
      string_enum_def tbls has_unknown_members members
    | E.SymbolBody { E.SymbolBody.members; has_unknown_members; _ } ->
      let members = defaulted_members tbls members in
      (SymbolRep, members, has_unknown_members)
    | E.BigIntBody { E.BigIntBody.members; has_unknown_members; _ } ->
      let (truthy, members) = initialized_members tbls bigint_member true members in
      (BigIntRep { truthy }, members, has_unknown_members)
  in
  fun opts scope tbls decl ->
    let { E.id; body = (_, body); comments = _ } = decl in
    let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
    let id_loc = push_loc tbls id_loc in
    let def =
      if opts.enable_enums then
        Some (lazy (splice tbls id_loc (fun tbls -> enum_def tbls body)))
      else
        None
    in
    Scope.bind_enum scope tbls id_loc name def

let export_named_decl opts scope tbls kind stmt =
  let module S = Ast.Statement in
  let decl =
    match stmt with
    | S.FunctionDeclaration f -> function_decl opts scope tbls f
    | S.ClassDeclaration c -> class_decl opts scope tbls c
    | S.TypeAlias t -> type_alias_decl opts scope tbls t
    | S.OpaqueType t -> opaque_type_decl opts scope tbls t
    | S.InterfaceDeclaration i -> interface_decl opts scope tbls i
    | S.VariableDeclaration decl -> variable_decls opts scope tbls decl
    | S.EnumDeclaration e -> enum_decl opts scope tbls e
    | S.ComponentDeclaration c -> component_decl opts scope tbls c
    | _ -> failwith "unexpected export declaration"
  in
  decl (Scope.export_binding scope kind)

let declare_export_decl opts scope tbls default =
  let module S = Ast.Statement in
  let module D = S.DeclareExportDeclaration in
  let export_maybe_default_binding =
    match default with
    | None -> Scope.export_binding scope S.ExportValue
    | Some default_loc -> Scope.export_default_binding scope default_loc
  in
  function
  | D.Variable (_, v) ->
    declare_variable_decl opts scope tbls v (Scope.export_binding scope S.ExportValue)
  | D.Function (_, f) -> declare_function_decl opts scope tbls f export_maybe_default_binding
  | D.Class (_, c) -> declare_class_decl opts scope tbls c export_maybe_default_binding
  | D.Component (loc, c) ->
    declare_component_decl opts scope tbls loc c export_maybe_default_binding
  | D.Enum (_, enum) -> enum_decl opts scope tbls enum (Scope.export_binding scope S.ExportValue)
  | D.DefaultType t ->
    let default_loc = Base.Option.value_exn default in
    let def = annot opts scope tbls SSet.empty t in
    Scope.export_default scope default_loc def
  | D.NamedType (_, t) -> type_alias_decl opts scope tbls t (Scope.export_binding scope S.ExportType)
  | D.NamedOpaqueType (_, t) ->
    opaque_type_decl opts scope tbls t (Scope.export_binding scope S.ExportType)
  | D.Interface (_, i) -> interface_decl opts scope tbls i (Scope.export_binding scope S.ExportType)

let export_default_decl =
  let module S = Ast.Statement in
  let module D = S.ExportDefaultDeclaration in
  let export_default_class opts scope tbls default_loc loc decl =
    match decl.Ast.Class.id with
    | Some _ -> class_decl opts scope tbls decl (Scope.export_default_binding scope default_loc)
    | None ->
      let def = class_def opts scope tbls decl in
      let def = Value (ClassExpr (loc, def)) in
      Scope.export_default scope default_loc def
  in
  let export_default_fun opts scope tbls default_loc loc decl =
    let { Ast.Function.id; async; generator; _ } = decl in
    match id with
    | Some _ -> function_decl opts scope tbls decl (Scope.export_default_binding scope default_loc)
    | None ->
      let def = function_def opts scope tbls SSet.empty loc decl in
      let statics = SMap.empty in
      let def = Value (FunExpr { loc; async; generator; def; statics }) in
      Scope.export_default scope default_loc def
  in
  fun opts scope tbls decl ->
    let { D.default = default_loc; declaration; comments = _ } = decl in
    let default_loc = push_loc tbls default_loc in
    match declaration with
    | D.Declaration (loc, S.ClassDeclaration decl) ->
      let loc = push_loc tbls loc in
      export_default_class opts scope tbls default_loc loc decl
    | D.Declaration (loc, S.FunctionDeclaration decl) ->
      let loc = push_loc tbls loc in
      export_default_fun opts scope tbls default_loc loc decl
    | D.Declaration (_, S.EnumDeclaration decl) ->
      enum_decl opts scope tbls decl (Scope.export_default_binding scope default_loc)
    | D.Declaration (_, S.ComponentDeclaration c) ->
      component_decl opts scope tbls c (Scope.export_default_binding scope default_loc)
    | D.Declaration _ -> failwith "unexpected default export declaration"
    | D.Expression expr ->
      let def = expression opts scope tbls expr in
      Scope.export_default scope default_loc def

let export_specifiers scope tbls kind source =
  let module S = Ast.Statement in
  let module E = S.ExportNamedDeclaration in
  let source =
    match source with
    | Some (_, { Ast.StringLiteral.value; raw = _; comments = _ }) -> Some value
    | None -> None
  in
  function
  | E.ExportBatchSpecifier (_, Some id) ->
    let mref = Base.Option.value_exn source in
    let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
    let id_loc = push_loc tbls id_loc in
    Scope.export_ns scope tbls kind mref id_loc name
  | E.ExportBatchSpecifier (loc, None) ->
    let loc = push_loc tbls loc in
    let mref = Base.Option.value_exn source in
    Scope.export_star scope tbls kind loc mref
  | E.ExportSpecifiers specifiers ->
    List.iter
      (fun (_, { E.ExportSpecifier.local; exported }) ->
        match source with
        | None -> Scope.export_ref scope tbls kind ~local ~exported
        | Some mref -> Scope.export_from scope tbls kind mref ~local ~exported)
      specifiers

let assignment =
  let module E = Ast.Expression in
  let module A = E.Assignment in
  let module P = Ast.Pattern in
  let module I = Ast.Identifier in
  fun opts scope tbls { A.operator; left = (_, left); right; comments = _ } ->
    match (operator, left) with
    (* module.exports = ... *)
    | ( None,
        P.Expression
          ( _,
            E.Member
              {
                E.Member._object =
                  (_, E.Identifier (_, { I.name = "module" as object_name; comments = _ }));
                property = E.Member.PropertyIdentifier (_, { I.name = "exports"; comments = _ });
                comments = _;
              }
          )
      )
      when Scope.lookup_value scope object_name = None ->
      let t = expression opts scope tbls right in
      Scope.cjs_clobber scope t
    (* exports.foo = ... *)
    | ( None,
        P.Expression
          ( _,
            E.Member
              {
                E.Member._object =
                  (_, E.Identifier (_, { I.name = "exports" as object_name; comments = _ }));
                property = E.Member.PropertyIdentifier (id_loc, { I.name; comments = _ });
                comments = _;
              }
          )
      )
    (* module.exports.foo = ... *)
    | ( None,
        P.Expression
          ( _,
            E.Member
              {
                E.Member._object =
                  ( _,
                    E.Member
                      {
                        E.Member._object =
                          (_, E.Identifier (_, { I.name = "module" as object_name; comments = _ }));
                        property =
                          E.Member.PropertyIdentifier (_, { I.name = "exports"; comments = _ });
                        comments = _;
                      }
                  );
                property = E.Member.PropertyIdentifier (id_loc, { I.name; comments = _ });
                comments = _;
              }
          )
      )
      when Scope.lookup_value scope object_name = None ->
      let id_loc = push_loc tbls id_loc in
      let t = expression opts scope tbls right in
      Scope.cjs_set_prop scope name (id_loc, t)
    (* id.foo = ... *)
    | ( None,
        P.Expression
          ( _,
            E.Member
              {
                E.Member._object = (_, E.Identifier (_, { I.name; comments = _ }));
                property = E.Member.PropertyIdentifier (id_loc, { I.name = prop_name; comments = _ });
                comments = _;
              }
          )
      ) ->
      let id_loc = push_loc tbls id_loc in
      let t = expression opts scope tbls right in
      Scope.assign_binding prop_name (id_loc, t) name scope
    | _ ->
      (* TODO: Assignment expressions can alter the actual type of something in
       * a way that is currently invisible to the signature extractor. *)
      ()

let rec statement opts scope tbls (loc, stmt) =
  let module S = Ast.Statement in
  match stmt with
  | S.TypeAlias decl -> type_alias_decl opts scope tbls decl ignore2
  | S.DeclareTypeAlias decl -> type_alias_decl opts scope tbls decl ignore2
  | S.OpaqueType decl -> opaque_type_decl opts scope tbls decl ignore2
  | S.DeclareOpaqueType decl -> opaque_type_decl opts scope tbls decl ignore2
  | S.ClassDeclaration decl -> class_decl opts scope tbls decl ignore2
  | S.DeclareClass decl -> declare_class_decl opts scope tbls decl ignore2
  | S.DeclareComponent decl -> declare_component_decl opts scope tbls loc decl ignore2
  | S.InterfaceDeclaration decl -> interface_decl opts scope tbls decl ignore2
  | S.DeclareInterface decl -> interface_decl opts scope tbls decl ignore2
  | S.FunctionDeclaration decl -> function_decl opts scope tbls decl ignore2
  | S.ComponentDeclaration decl -> component_decl opts scope tbls decl ignore2
  | S.DeclareFunction decl -> declare_function_decl opts scope tbls decl ignore2
  | S.ImportDeclaration decl ->
    (match scope with
    | Global _ ->
      (* this is illegal. it should be caught in the parser. *)
      ()
    | _ -> import_decl opts scope tbls decl)
  | S.ExportNamedDeclaration decl ->
    let module E = S.ExportNamedDeclaration in
    let { E.export_kind = kind; source; specifiers; declaration; comments = _ } = decl in
    begin
      match declaration with
      | None -> ()
      | Some (_, stmt) -> export_named_decl opts scope tbls kind stmt
    end;
    begin
      match specifiers with
      | None -> ()
      | Some specifiers -> export_specifiers scope tbls kind source specifiers
    end
  (* walk lex scopes to collect hoisted names in scope *)
  | S.Block { S.Block.body; _ } ->
    let scope = Scope.push_lex scope in
    List.iter (statement opts scope tbls) body
  | S.DoWhile { S.DoWhile.body; _ } -> statement opts scope tbls body
  | S.For { S.For.init; body; _ } ->
    let scope =
      match init with
      | None -> scope
      | Some (S.For.InitExpression _) -> scope
      | Some (S.For.InitDeclaration (_, decl)) ->
        let scope = Scope.push_lex scope in
        variable_decls opts scope tbls decl ignore2;
        scope
    in
    statement opts scope tbls body
  | S.ForIn { S.ForIn.left; body; _ } ->
    let scope =
      match left with
      | S.ForIn.LeftPattern _ -> scope
      | S.ForIn.LeftDeclaration (_, decl) ->
        let scope = Scope.push_lex scope in
        variable_decls opts scope tbls decl ignore2;
        scope
    in
    statement opts scope tbls body
  | S.ForOf { S.ForOf.left; body; _ } ->
    let scope =
      match left with
      | S.ForOf.LeftPattern _ -> scope
      | S.ForOf.LeftDeclaration (_, decl) ->
        let scope = Scope.push_lex scope in
        variable_decls opts scope tbls decl ignore2;
        scope
    in
    statement opts scope tbls body
  | S.If { S.If.consequent; alternate; _ } ->
    statement opts scope tbls consequent;
    begin
      match alternate with
      | None -> ()
      | Some (_, { S.If.Alternate.body; comments = _ }) -> statement opts scope tbls body
    end
  | S.Switch { S.Switch.cases; _ } ->
    let scope = Scope.push_lex scope in
    List.iter
      (fun (_, { S.Switch.Case.consequent; _ }) -> List.iter (statement opts scope tbls) consequent)
      cases
  | S.Try { S.Try.block = b; handler; finalizer; comments = _ } ->
    let block (loc, b) = statement opts scope tbls (loc, S.Block b) in
    block b;
    begin
      match handler with
      | None -> ()
      | Some (_, { S.Try.CatchClause.body; _ }) -> block body
    end;
    begin
      match finalizer with
      | None -> ()
      | Some b -> block b
    end
  | S.While { S.While.body; _ } -> statement opts scope tbls body
  | S.Labeled { S.Labeled.body; _ } -> statement opts scope tbls body
  | S.VariableDeclaration decl -> variable_decls opts scope tbls decl ignore2
  | S.DeclareVariable decl -> declare_variable_decl opts scope tbls decl ignore2
  | S.DeclareExportDeclaration decl ->
    let module D = S.DeclareExportDeclaration in
    let { D.default; declaration; specifiers; source; comments = _ } = decl in
    let default = Option.map ~f:(fun loc -> push_loc tbls loc) default in
    begin
      match declaration with
      | None -> ()
      | Some decl -> declare_export_decl opts scope tbls default decl
    end;
    begin
      match specifiers with
      | None -> ()
      | Some specifiers -> export_specifiers scope tbls S.ExportValue source specifiers
    end
  | S.ExportDefaultDeclaration decl -> export_default_decl opts scope tbls decl
  | S.Expression
      { S.Expression.expression = (_, Ast.Expression.Assignment a); directive = _; comments = _ } ->
    assignment opts scope tbls a
  | S.DeclareModuleExports { S.DeclareModuleExports.annot = (_, t); comments = _ } ->
    let t = annot opts scope tbls SSet.empty t in
    Scope.cjs_clobber scope t
  | S.DeclareModule { S.DeclareModule.id; body; comments = _ } ->
    let (loc, name) =
      match id with
      | S.DeclareModule.Identifier ((id_loc, _) as id) -> (push_loc tbls id_loc, id_name id)
      | S.DeclareModule.Literal (id_loc, { Ast.StringLiteral.value; _ }) ->
        (push_loc tbls id_loc, value)
    in
    let scope = Scope.push_declare_module loc name scope in
    let (_, { S.Block.body = stmts; comments = _ }) = body in
    let visit_statement ((_, stmt') as stmt) =
      match
        Flow_ast_utils.acceptable_statement_in_declaration_context ~in_declare_namespace:false stmt'
      with
      | Ok () -> statement opts scope tbls stmt
      | Error _ -> ()
    in
    List.iter visit_statement stmts;
    Scope.finalize_declare_module_exports_exn scope
  | S.DeclareNamespace decl ->
    let is_type_only = Flow_ast_utils.is_type_only_declaration_statement (loc, stmt) in
    namespace_decl opts scope tbls ~is_type_only ~visit_statement:statement decl ignore2
  | S.DeclareEnum decl
  | S.EnumDeclaration decl ->
    enum_decl opts scope tbls decl ignore2
  (* unsupported *)
  | S.With _ -> ()
  (* statements that won't introduce a top-level type or name in module scope *)
  | S.Break _
  | S.Continue _
  | S.Debugger _
  | S.Empty _
  | S.Expression _
  | S.Return _
  | S.Throw _ ->
    ()
