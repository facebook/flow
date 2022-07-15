(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Hint_api
open Reason
open Flow_ast_mapper
open Loc_collections
module EnvMap = Env_api.EnvMap

type cond_context =
  | NonConditionalContext
  | SwitchConditionalTest of {
      case_test_reason: reason;
      switch_discriminant_reason: reason;
    }
  | OtherConditionalTest

type scope_kind =
  | Ordinary (* function or module *)
  | Async (* async function *)
  | Generator (* generator function *)
  | AsyncGenerator (* async generator function *)
  | Module (* module scope *)
  | Predicate (* predicate function *)
  | Ctor

type class_stack = ALoc.t list

type for_kind =
  | In
  | Of of { await: bool }

(* A map from location of tparam to name. *)
type tparams_map = string ALocMap.t

type hint_node =
  | AnnotationHint of tparams_map * (ALoc.t, ALoc.t) Ast.Type.annotation
  | ValueHint of (ALoc.t, ALoc.t) Ast.Expression.t
  | ProvidersHint of ALoc.t Nel.t

type root =
  | Annotation of {
      tparams_map: tparams_map;
      optional: bool;
      is_assignment: bool;
      annot: (ALoc.t, ALoc.t) Ast.Type.annotation;
    }
  | Value of (ALoc.t, ALoc.t) Ast.Expression.t
  | Contextual of Reason.reason * hint_node hint
  | Catch
  | For of for_kind * (ALoc.t, ALoc.t) Ast.Expression.t

type selector =
  | Elem of int
  | Prop of {
      prop: string;
      prop_loc: ALoc.t;
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

type generator_annot = {
  tparams_map: tparams_map;
  return_annot: (ALoc.t, ALoc.t) Ast.Type.annotation;
  async: bool;
}

type def =
  | Binding of binding
  | ChainExpression of cond_context * (ALoc.t, ALoc.t) Ast.Expression.t
  | RefiExpression of (ALoc.t, ALoc.t) Ast.Expression.t
  | MemberAssign of {
      member_loc: ALoc.t;
      member: (ALoc.t, ALoc.t) Ast.Expression.Member.t;
      rhs: (ALoc.t, ALoc.t) Ast.Expression.t;
    }
  | OpAssign of {
      exp_loc: ALoc.t;
      lhs: (ALoc.t, ALoc.t) Ast.Pattern.t;
      op: Ast.Expression.Assignment.operator;
      rhs: (ALoc.t, ALoc.t) Ast.Expression.t;
    }
  | Update of {
      exp_loc: ALoc.t;
      lhs_member: (ALoc.t, ALoc.t) Ast.Expression.t option;
      op: Ast.Expression.Update.operator;
    }
  | Function of {
      synthesizable_from_annotation: bool;
      function_loc: ALoc.t;
      function_: (ALoc.t, ALoc.t) Ast.Function.t;
      tparams_map: tparams_map;
    }
  | Class of {
      missing_annotations: reason list;
      class_: (ALoc.t, ALoc.t) Ast.Class.t;
      class_loc: ALoc.t;
    }
  | DeclaredClass of ALoc.t * (ALoc.t, ALoc.t) Ast.Statement.DeclareClass.t
  | TypeAlias of ALoc.t * (ALoc.t, ALoc.t) Ast.Statement.TypeAlias.t
  | OpaqueType of ALoc.t * (ALoc.t, ALoc.t) Ast.Statement.OpaqueType.t
  | TypeParam of tparams_map * (ALoc.t, ALoc.t) Ast.Type.TypeParam.t
  | ThisTypeParam of tparams_map * ALoc.t option
  | Interface of ALoc.t * (ALoc.t, ALoc.t) Ast.Statement.Interface.t
  | Enum of ALoc.t * ALoc.t Ast.Statement.EnumDeclaration.body
  | Import of {
      import_kind: Ast.Statement.ImportDeclaration.import_kind;
      import: import;
      source: string;
      source_loc: ALoc.t;
    }
  | GeneratorNext of generator_annot option

type map = (def * scope_kind * class_stack * ALoc.t virtual_reason) EnvMap.t

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

  let object_named_property acc prop_loc x ~has_default =
    Select (Prop { prop = x; prop_loc; has_default }, acc)

  let object_computed_property acc e = Select (Computed e, acc)

  let object_rest_property acc xs has_computed =
    Select (ObjRest { used_props = xs; after_computed = has_computed }, acc)

  let object_property acc xs key ~has_default =
    let open Ast.Pattern.Object in
    match key with
    | Property.Identifier (loc, { Ast.Identifier.name = x; comments = _ }) ->
      let acc = object_named_property acc loc x ~has_default in
      (acc, x :: xs, false)
    | Property.Literal (loc, { Ast.Literal.value = Ast.Literal.String x; _ }) ->
      let acc = object_named_property acc loc x ~has_default in
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

let func_params_missing_annotations
    ~allow_this ((param_loc, { Ast.Function.Params.params; rest; this_; _ }) as all_params) body =
  let is_annotated p = p |> Destructure.type_of_pattern |> Base.Option.is_some in
  let params =
    Base.List.concat_map params ~f:(fun (_, { Ast.Function.Param.argument; _ }) ->
        if is_annotated argument then
          []
        else
          Flow_ast_utils.fold_bindings_of_pattern
            (fun acc (loc, { Ast.Identifier.name; _ }) ->
              mk_reason (RParameter (Some name)) loc :: acc)
            []
            argument
    )
  in
  let rest =
    Base.Option.value_map rest ~default:[] ~f:(fun (_, { Ast.Function.RestParam.argument; _ }) ->
        if is_annotated argument then
          []
        else
          Flow_ast_utils.fold_bindings_of_pattern
            (fun acc (loc, { Ast.Identifier.name; _ }) ->
              mk_reason (RRestParameter (Some name)) loc :: acc)
            []
            argument
    )
  in
  let this_ =
    if
      allow_this
      || (not @@ Signature_utils.This_finder.found_this_in_body_or_params body all_params)
      || Base.Option.is_some this_
    then
      []
    else
      [mk_reason (RImplicitThis (RFunction RNormal)) param_loc]
  in
  params @ rest @ this_

let func_missing_annotations ~allow_this ({ Ast.Function.return; generator; params; body; _ } as f)
    =
  let params = func_params_missing_annotations ~allow_this params body in
  match return with
  | Ast.Type.Available _ -> params
  | Ast.Type.Missing loc ->
    if (not (Nonvoid_return.might_have_nonvoid_return ALoc.none f)) && not generator then
      params
    else
      mk_reason RReturn loc :: params

let func_is_synthesizable_from_annotation ~allow_this ({ Ast.Function.predicate; _ } as f) =
  Base.Option.is_none predicate && Base.List.is_empty (func_missing_annotations ~allow_this f)

let def_of_function tparams_map function_loc function_ =
  Function
    {
      synthesizable_from_annotation =
        func_is_synthesizable_from_annotation ~allow_this:false function_;
      function_loc;
      function_;
      tparams_map;
    }

let def_of_class loc ({ Ast.Class.body = (_, { Ast.Class.Body.body; _ }); _ } as class_) =
  let open Ast.Class.Body in
  let missing_annotations =
    Base.List.concat_map
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
          []
        | Method
            ( _,
              {
                Ast.Class.Method.key = Ast.Expression.Object.Property.Identifier _;
                value = (_, value);
                _;
              }
            ) ->
          func_missing_annotations ~allow_this:true value
        | Method (loc, _) -> [mk_reason (RMethod None) loc]
        | Property
            ( _,
              {
                Ast.Class.Property.key = Ast.Expression.Object.Property.Identifier _;
                annot = Ast.Type.Available _;
                _;
              }
            ) ->
          []
        | Property
            ( _,
              {
                Ast.Class.Property.key = Ast.Expression.Object.Property.Identifier _;
                annot = Ast.Type.Missing _;
                value =
                  Ast.Class.Property.Initialized
                    (_, (Ast.Expression.Function fn | Ast.Expression.ArrowFunction fn));
                _;
              }
            ) ->
          func_missing_annotations ~allow_this:true fn
        | Property
            ( _,
              {
                Ast.Class.Property.key = Ast.Expression.Object.Property.Identifier _;
                annot = Ast.Type.Missing _;
                value = Ast.Class.Property.Initialized (_, Ast.Expression.Literal _);
                _;
              }
            ) ->
          []
        | Property
            ( _,
              {
                Ast.Class.Property.key =
                  Ast.Expression.Object.Property.Identifier (_, { Ast.Identifier.name; _ });
                annot = Ast.Type.Missing loc;
                value = Ast.Class.Property.Initialized _;
                _;
              }
            )
        | Property
            ( loc,
              {
                Ast.Class.Property.key =
                  Ast.Expression.Object.Property.Identifier (_, { Ast.Identifier.name; _ });
                annot = Ast.Type.Missing _;
                value = Ast.Class.Property.(Uninitialized | Declared);
                _;
              }
            ) ->
          (* To correspond with other local inference annotation locations,
             point at the annot if it's initialized and the whole property if it's not *)
          [mk_reason (RProperty (Some (OrdinaryName name))) loc]
        | Property (loc, _) -> [mk_reason (RProperty None) loc]
        | PrivateField (_, { Ast.Class.PrivateField.annot = Ast.Type.Available _; _ }) -> []
        | PrivateField
            ( _,
              {
                Ast.Class.PrivateField.key = (_, { Ast.PrivateName.name; _ });
                annot = Ast.Type.Missing loc;
                value = Ast.Class.Property.Initialized _;
                _;
              }
            )
        | PrivateField
            ( loc,
              {
                Ast.Class.PrivateField.key = (_, { Ast.PrivateName.name; _ });
                annot = Ast.Type.Missing _;
                value = Ast.Class.Property.(Uninitialized | Declared);
                _;
              }
            ) ->
          [mk_reason (RPrivateProperty name) loc])
      body
  in
  Class { missing_annotations; class_; class_loc = loc }

let func_scope_kind ?key { Ast.Function.async; generator; predicate; _ } =
  match (async, generator, predicate, key) with
  | ( false,
      false,
      None,
      Some
        (Ast.Expression.Object.Property.Identifier (_, { Ast.Identifier.name = "constructor"; _ }))
    ) ->
    Ctor
  | (true, true, None, _) -> AsyncGenerator
  | (true, false, None, _) -> Async
  | (false, true, None, _) -> Generator
  | (false, false, Some _, _) -> Predicate
  | (false, false, None, _) -> Ordinary
  | _ -> (* Invalid, default to ordinary and hopefully error elsewhere *) Ordinary

module Eq_test = Eq_test.Make (Scope_api.With_ALoc) (Ssa_api.With_ALoc) (Env_api.With_ALoc)

class def_finder env_entries providers toplevel_scope =
  object (this)
    inherit [map, ALoc.t] Flow_ast_visitor.visitor ~init:EnvMap.empty as super

    val mutable tparams : tparams_map = ALocMap.empty

    val mutable scope_kind : scope_kind = toplevel_scope

    val mutable class_stack : class_stack = []

    val mutable return_hint_stack : hint_node hint list = []

    method add_tparam loc name = tparams <- ALocMap.add loc name tparams

    method add_binding kind_and_loc reason src =
      this#update_acc (EnvMap.add kind_and_loc (src, scope_kind, class_stack, reason))

    method add_ordinary_binding loc = this#add_binding (Env_api.OrdinaryNameLoc, loc)

    method private in_scope : 'a 'b. ('a -> 'b) -> scope_kind -> 'a -> 'b =
      fun f scope' node ->
        let scope0 = scope_kind in
        scope_kind <- scope';
        let res = f node in
        scope_kind <- scope0;
        res

    method private in_new_tparams_env : 'a. ?keep:bool -> (unit -> 'a) -> 'a =
      fun ?(keep = false) f ->
        let old_tparams = tparams in
        if not keep then tparams <- ALocMap.empty;
        let result = f () in
        tparams <- old_tparams;
        result

    method! variable_declarator ~kind decl =
      let open Ast.Statement.VariableDeclaration.Declarator in
      let (_, { id; init }) = decl in
      let (source, hint) =
        match (Destructure.type_of_pattern id, init) with
        | (Some annot, _) ->
          ( Some
              (Annotation
                 { tparams_map = ALocMap.empty; optional = false; is_assignment = true; annot }
              ),
            Hint_t (AnnotationHint (ALocMap.empty, annot))
          )
        | (None, Some init) -> (Some (Value init), Hint_None)
        | (None, None) -> (None, Hint_None)
      in
      Base.Option.iter
        ~f:(fun acc -> Destructure.pattern ~f:this#add_ordinary_binding (Root acc) id)
        source;
      ignore @@ this#variable_declarator_pattern ~kind id;
      Base.Option.iter init ~f:(fun init ->
          this#visit_expression ~hint ~cond:NonConditionalContext init
      );
      decl

    method! declare_variable loc (decl : ('loc, 'loc) Ast.Statement.DeclareVariable.t) =
      let open Ast.Statement.DeclareVariable in
      let { id = (id_loc, { Ast.Identifier.name; _ }); annot; comments = _ } = decl in
      this#add_ordinary_binding
        id_loc
        (mk_reason (RIdentifier (OrdinaryName name)) id_loc)
        (Binding
           (Root
              (Annotation
                 { tparams_map = ALocMap.empty; optional = false; is_assignment = true; annot }
              )
           )
        );
      super#declare_variable loc decl

    method! function_param _ = failwith "Should be visited by visit_function_param"

    method private visit_function_param ~hint (param : ('loc, 'loc) Ast.Function.Param.t) =
      let open Ast.Function.Param in
      let (loc, { argument; default }) = param in
      let optional =
        match argument with
        | (_, Ast.Pattern.Identifier { Ast.Pattern.Identifier.optional; _ }) -> optional
        | _ -> false
      in
      let source =
        match Destructure.type_of_pattern argument with
        | Some annot -> Annotation { tparams_map = tparams; optional; is_assignment = false; annot }
        | None ->
          let reason =
            match argument with
            | ( _,
                Ast.Pattern.Identifier
                  { Ast.Pattern.Identifier.name = (_, { Ast.Identifier.name; _ }); _ }
              ) ->
              mk_reason (RParameter (Some name)) loc
            | _ -> mk_reason RDestructuring loc
          in
          Contextual (reason, hint)
      in
      let source = Destructure.pattern_default (Root source) default in
      Destructure.pattern ~f:this#add_ordinary_binding source argument;
      ignore @@ super#function_param param

    method private visit_function_rest_param ~hint (expr : ('loc, 'loc) Ast.Function.RestParam.t) =
      let open Ast.Function.RestParam in
      let (loc, { argument; comments = _ }) = expr in
      let source =
        match Destructure.type_of_pattern argument with
        | Some annot ->
          Annotation { tparams_map = tparams; optional = false; is_assignment = false; annot }
        | None ->
          let reason =
            match argument with
            | ( _,
                Ast.Pattern.Identifier
                  { Ast.Pattern.Identifier.name = (_, { Ast.Identifier.name; _ }); _ }
              ) ->
              mk_reason (RRestParameter (Some name)) loc
            | _ ->
              (* TODO: This should be a parse error, but we only produce an internal
                 error in statement.ml. *)
              mk_reason (RCustom "contextual variable") loc
          in
          Contextual (reason, hint)
      in
      Destructure.pattern ~f:this#add_ordinary_binding (Root source) argument;
      ignore @@ super#function_rest_param expr

    method! catch_clause_pattern pat =
      Destructure.pattern ~f:this#add_ordinary_binding (Root Catch) pat;
      super#catch_clause_pattern pat

    method private visit_function_expr ~func_hint loc expr =
      let { Ast.Function.id; async; generator; sig_loc; _ } = expr in
      let scope_kind = func_scope_kind expr in
      this#in_new_tparams_env (fun () ->
          this#visit_function ~scope_kind ~func_hint expr;
          match id with
          | Some (id_loc, _) ->
            this#add_ordinary_binding
              id_loc
              (func_reason ~async ~generator sig_loc)
              (def_of_function tparams loc expr)
          | None -> ()
      )

    method! function_expression loc expr =
      this#visit_function_expr ~func_hint:Hint_None loc expr;
      expr

    method! function_declaration loc expr =
      let { Ast.Function.id; async; generator; sig_loc; _ } = expr in
      let scope_kind = func_scope_kind expr in
      this#in_new_tparams_env (fun () ->
          this#visit_function ~func_hint:Hint_None ~scope_kind expr;
          match id with
          | Some (id_loc, _) ->
            this#add_ordinary_binding
              id_loc
              (func_reason ~async ~generator sig_loc)
              (def_of_function tparams loc expr)
          | None -> ()
      );
      expr

    method! function_type loc ft =
      this#in_new_tparams_env ~keep:true (fun () -> super#function_type loc ft)

    method! function_ _ expr =
      let scope_kind = func_scope_kind expr in
      this#in_new_tparams_env (fun () -> this#visit_function ~scope_kind ~func_hint:Hint_None expr);
      expr

    method private visit_function ~scope_kind ~func_hint expr =
      this#in_scope
        (fun () ->
          let {
            Ast.Function.id = _;
            params = (_, { Ast.Function.Params.params = params_list; rest; comments = _; this_ = _ });
            body;
            async;
            generator;
            predicate;
            return;
            tparams = fun_tparams;
            sig_loc = _;
            comments = _;
          } =
            expr
          in
          Base.Option.iter fun_tparams ~f:(fun tparams -> ignore @@ this#type_params tparams);
          Base.List.iteri
            ~f:(fun i ->
              this#visit_function_param ~hint:(decompose_hint (Decomp_FuncParam i) func_hint))
            params_list;
          Base.Option.iter
            ~f:
              (this#visit_function_rest_param
                 ~hint:(decompose_hint (Decomp_FuncRest (List.length params_list)) func_hint)
              )
            rest;
          ignore @@ this#type_annotation_hint return;

          let return_hint =
            match return with
            | Ast.Type.Available annot -> Hint_t (AnnotationHint (ALocMap.empty, annot))
            | Ast.Type.Missing _ -> decompose_hint Decomp_FuncReturn func_hint
          in
          let old_stack = return_hint_stack in
          return_hint_stack <- return_hint :: return_hint_stack;
          let body_loc =
            match body with
            | Ast.Function.BodyBlock (loc, block) ->
              ignore @@ this#block loc block;
              loc
            | Ast.Function.BodyExpression ((loc, _) as expr) ->
              this#visit_expression ~hint:return_hint ~cond:NonConditionalContext expr;
              loc
          in
          return_hint_stack <- old_stack;

          begin
            if generator then
              let (loc, gen) =
                match return with
                | Ast.Type.Missing loc -> (loc, None)
                | Ast.Type.Available ((loc, _) as return_annot) ->
                  (loc, Some { tparams_map = tparams; return_annot; async })
              in

              this#add_ordinary_binding loc (mk_reason (RCustom "next") body_loc) (GeneratorNext gen)
          end;

          Base.Option.iter predicate ~f:(fun (_, { Ast.Type.Predicate.kind; comments = _ }) ->
              match kind with
              | Ast.Type.Predicate.Inferred -> ()
              | Ast.Type.Predicate.Declared expr ->
                this#visit_expression ~hint:Hint_None ~cond:NonConditionalContext expr
          ))
        scope_kind
        ()

    method! class_ loc expr =
      let open Ast.Class in
      let { id; body; tparams = class_tparams; extends; implements; class_decorators; comments = _ }
          =
        expr
      in
      this#in_new_tparams_env (fun () ->
          let old_stack = class_stack in
          class_stack <- loc :: class_stack;
          this#in_scope
            (fun () ->
              Flow_ast_visitor.run_opt this#class_identifier id;
              Flow_ast_visitor.run_opt this#type_params class_tparams;
              let () =
                let reason =
                  match id with
                  | Some (name_loc, { Ast.Identifier.name; comments = _ }) ->
                    mk_reason (RType (OrdinaryName name)) name_loc
                  | None -> mk_reason (RType (InternalName "*default*")) loc
                in
                this#add_ordinary_binding
                  loc
                  reason
                  (ThisTypeParam (tparams, Base.Option.map ~f:fst class_tparams));
                this#add_tparam loc "this"
              in
              ignore @@ this#class_body body;
              Flow_ast_visitor.run_opt (Flow_ast_mapper.map_loc this#class_extends) extends;
              Flow_ast_visitor.run_opt this#class_implements implements;
              Flow_ast_visitor.run_list this#class_decorator class_decorators;
              ())
            Ordinary
            ();
          class_stack <- old_stack;
          begin
            match id with
            | Some (id_loc, { Ast.Identifier.name; _ }) ->
              let name = OrdinaryName name in
              let reason = mk_reason (RType name) id_loc in
              this#add_ordinary_binding id_loc reason (def_of_class loc expr)
            | None -> ()
          end;
          expr
      )

    method! class_property _loc (prop : ('loc, 'loc) Ast.Class.Property.t') =
      let open Ast.Class.Property in
      let { key; value; annot; static = _; variance; comments = _ } = prop in
      ignore @@ this#object_key key;
      ignore @@ this#type_annotation_hint annot;
      let hint =
        match annot with
        | Ast.Type.Available annot -> Hint_t (AnnotationHint (ALocMap.empty, annot))
        | Ast.Type.Missing _ -> Hint_None
      in
      this#visit_class_property_value ~hint value;
      ignore @@ this#variance_opt variance;
      prop

    method! class_private_field _loc (prop : ('loc, 'loc) Ast.Class.PrivateField.t') =
      let open Ast.Class.PrivateField in
      let { key; value; annot; static = _; variance; comments = _ } = prop in
      ignore @@ this#private_name key;
      ignore @@ this#type_annotation_hint annot;
      let hint =
        match annot with
        | Ast.Type.Available annot -> Hint_t (AnnotationHint (ALocMap.empty, annot))
        | Ast.Type.Missing _ -> Hint_None
      in
      this#visit_class_property_value ~hint value;
      ignore @@ this#variance_opt variance;
      prop

    method private visit_class_property_value ~hint value =
      let open Ast.Class.Property in
      match value with
      | Declared -> ()
      | Uninitialized -> ()
      | Initialized x -> this#visit_expression ~cond:NonConditionalContext ~hint x

    method! class_method _loc (meth : ('loc, 'loc) Ast.Class.Method.t') =
      let open Ast.Class.Method in
      let { kind = _; key; value = (_, value); static = _; decorators; comments = _ } = meth in
      let _ = this#object_key key in
      let scope_kind = func_scope_kind ~key value in
      let () =
        this#in_new_tparams_env ~keep:true (fun () ->
            this#visit_function ~scope_kind ~func_hint:Hint_None value
        )
      in
      let (_ : _ list) = map_list this#class_decorator decorators in
      meth

    method! declare_function loc (decl : ('loc, 'loc) Ast.Statement.DeclareFunction.t) =
      match Declare_function_utils.declare_function_to_function_declaration_simple loc decl with
      | Some stmt ->
        let _ = this#statement (loc, stmt) in
        decl
      | None ->
        let open Ast.Statement.DeclareFunction in
        let { id = (id_loc, _); annot; predicate = _; comments = _ } = decl in
        this#add_ordinary_binding
          id_loc
          (func_reason ~async:false ~generator:false loc)
          (Binding
             (Root
                (Annotation
                   { tparams_map = ALocMap.empty; optional = false; is_assignment = false; annot }
                )
             )
          );
        super#declare_function loc decl

    method! declare_class loc (decl : ('loc, 'loc) Ast.Statement.DeclareClass.t) =
      let open Ast.Statement.DeclareClass in
      let { id = (id_loc, { Ast.Identifier.name; _ }); _ } = decl in
      this#add_ordinary_binding
        id_loc
        (mk_reason (RClass (RIdentifier (OrdinaryName name))) loc)
        (DeclaredClass (loc, decl));
      super#declare_class loc decl

    method! assignment loc (expr : ('loc, 'loc) Ast.Expression.Assignment.t) =
      let open Ast.Expression.Assignment in
      let { operator; left = (lhs_loc, lhs_node) as left; right; comments = _ } = expr in
      let () =
        match (operator, lhs_node) with
        | (None, Ast.Pattern.Expression (member_loc, Ast.Expression.Member member)) ->
          (* Use super member to visit sub-expressions to avoid record a read of the member. *)
          ignore @@ super#member member_loc member;
          this#add_ordinary_binding
            member_loc
            (mk_pattern_reason left)
            (MemberAssign { member_loc; member; rhs = right })
        | (None, _) -> Destructure.pattern ~f:this#add_ordinary_binding (Root (Value right)) left
        | ( Some operator,
            Ast.Pattern.Identifier
              { Ast.Pattern.Identifier.name = (id_loc, { Ast.Identifier.name; _ }); _ }
          ) ->
          this#add_ordinary_binding
            id_loc
            (mk_reason (RIdentifier (OrdinaryName name)) id_loc)
            (OpAssign { exp_loc = loc; lhs = left; op = operator; rhs = right })
        | (Some operator, Ast.Pattern.Expression ((def_loc, _) as e)) ->
          (* In op_assign, the LHS will also be read. *)
          let cond =
            match operator with
            | AndAssign
            | OrAssign ->
              OtherConditionalTest
            | _ -> NonConditionalContext
          in
          this#visit_expression ~cond ~hint:Hint_None e;
          this#add_ordinary_binding
            def_loc
            (mk_pattern_reason left)
            (OpAssign { exp_loc = loc; lhs = left; op = operator; rhs = right })
        | _ -> ()
      in
      let is_provider =
        Flow_ast_utils.fold_bindings_of_pattern
          (fun acc (loc, _) -> acc || Env_api.Provider_api.is_provider providers loc)
          false
          left
      in
      let hint =
        if is_provider then
          Hint_None
        else
          match lhs_node with
          | Ast.Pattern.Identifier _ ->
            Env_api.Provider_api.providers_of_def providers lhs_loc
            |> Base.Option.value_map ~f:(fun x -> x.Env_api.Provider_api.providers) ~default:[]
            |> Base.List.map ~f:(fun { Env_api.Provider_api.reason; _ } ->
                   Reason.aloc_of_reason reason
               )
            |> Nel.of_list
            |> Base.Option.value_map ~default:Hint_None ~f:(fun providers ->
                   Hint_t (ProvidersHint providers)
               )
          | Ast.Pattern.Expression
              (_, Ast.Expression.Member { Ast.Expression.Member._object; property; comments = _ })
            ->
            (match property with
            | Ast.Expression.Member.PropertyIdentifier (_, { Ast.Identifier.name; comments = _ }) ->
              decompose_hint (Decomp_ObjProp name) (Hint_t (ValueHint _object))
            | Ast.Expression.Member.PropertyPrivateName _ ->
              (* TODO create a hint based on the current class. *)
              Hint_None
            | Ast.Expression.Member.PropertyExpression _ ->
              decompose_hint Decomp_ObjComputed (Hint_t (ValueHint _object)))
          | _ ->
            (* TODO create a hint based on the lhs pattern *)
            Hint_t (AnnotationHint (ALocMap.empty, (lhs_loc, (lhs_loc, Ast.Type.Any None))))
      in
      this#visit_expression ~hint ~cond:NonConditionalContext right;
      expr

    method! update_expression loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Update.t) =
      let open Ast.Expression.Update in
      let { argument; operator; prefix = _; comments = _ } = expr in
      begin
        match argument with
        | (_, Ast.Expression.Identifier (id_loc, { Ast.Identifier.name; _ })) ->
          this#add_ordinary_binding
            id_loc
            (mk_reason (RIdentifier (OrdinaryName name)) id_loc)
            (Update { exp_loc = loc; lhs_member = None; op = operator })
        | (def_loc, Ast.Expression.Member _) ->
          this#add_ordinary_binding
            def_loc
            (mk_expression_reason argument)
            (Update { exp_loc = loc; lhs_member = Some argument; op = operator })
        | _ -> ()
      end;
      super#update_expression loc expr

    method! return _loc (stmt : ('loc, 'loc) Ast.Statement.Return.t) =
      let open Ast.Statement.Return in
      let { argument; comments = _; return_out = _ } = stmt in
      Base.Option.iter argument ~f:(fun argument ->
          this#visit_expression
            ~hint:(Base.Option.value ~default:Hint_None (Base.List.hd return_hint_stack))
            ~cond:NonConditionalContext
            argument
      );
      stmt

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
            | Some annot ->
              Annotation
                { tparams_map = ALocMap.empty; optional = false; is_assignment = true; annot }
            | None -> For (Of { await }, right)
          in
          Destructure.pattern ~f:this#add_ordinary_binding (Root source) id
        | LeftDeclaration _ -> failwith "Invalid AST structure"
        | LeftPattern pat ->
          Destructure.pattern ~f:this#add_ordinary_binding (Root (For (Of { await }, right))) pat
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
            | Some annot ->
              Annotation
                { tparams_map = ALocMap.empty; optional = false; is_assignment = true; annot }
            | None -> For (In, right)
          in
          Destructure.pattern ~f:this#add_ordinary_binding (Root source) id
        | LeftDeclaration _ -> failwith "Invalid AST structure"
        | LeftPattern pat ->
          Destructure.pattern ~f:this#add_ordinary_binding (Root (For (In, right))) pat
      end;
      super#for_in_statement loc stuff

    method! for_statement _ (stmt : ('loc, 'loc) Ast.Statement.For.t) =
      let open Ast.Statement.For in
      let { init; test; update; body; comments = _ } = stmt in
      Base.Option.iter init ~f:(fun init -> ignore @@ this#for_statement_init init);
      Base.Option.iter test ~f:(this#visit_expression ~hint:Hint_None ~cond:OtherConditionalTest);
      Base.Option.iter update ~f:(this#visit_expression ~hint:Hint_None ~cond:OtherConditionalTest);
      ignore @@ this#statement body;
      stmt

    method! while_ _loc (stmt : ('loc, 'loc) Ast.Statement.While.t) =
      let open Ast.Statement.While in
      let { test; body; comments = _ } = stmt in
      this#visit_expression ~hint:Hint_None ~cond:OtherConditionalTest test;
      ignore @@ this#statement body;
      stmt

    method! do_while _loc (stmt : ('loc, 'loc) Ast.Statement.DoWhile.t) =
      let open Ast.Statement.DoWhile in
      let { body; test; comments = _ } = stmt in
      ignore @@ this#statement body;
      this#visit_expression ~hint:Hint_None ~cond:OtherConditionalTest test;
      stmt

    method! if_statement _ (stmt : ('loc, 'loc) Ast.Statement.If.t) =
      let open Ast.Statement.If in
      let { test; consequent; alternate; comments = _ } = stmt in
      this#visit_expression ~hint:Hint_None ~cond:OtherConditionalTest test;
      ignore @@ this#if_consequent_statement ~has_else:(alternate <> None) consequent;
      Base.Option.iter alternate ~f:(fun (loc, alternate) ->
          ignore @@ this#if_alternate_statement loc alternate
      );
      stmt

    method! type_alias loc (alias : ('loc, 'loc) Ast.Statement.TypeAlias.t) =
      let open Ast.Statement.TypeAlias in
      let { id = (id_loc, { Ast.Identifier.name; _ }); _ } = alias in
      this#add_ordinary_binding
        id_loc
        (mk_reason (RType (OrdinaryName name)) id_loc)
        (TypeAlias (loc, alias));
      this#in_new_tparams_env (fun () -> super#type_alias loc alias)

    method! opaque_type loc (otype : ('loc, 'loc) Ast.Statement.OpaqueType.t) =
      let open Ast.Statement.OpaqueType in
      let { id = (id_loc, { Ast.Identifier.name; _ }); _ } = otype in
      this#add_ordinary_binding
        id_loc
        (mk_reason (ROpaqueType name) id_loc)
        (OpaqueType (loc, otype));
      this#in_new_tparams_env (fun () -> super#opaque_type loc otype)

    method! type_param (tparam : ('loc, 'loc) Ast.Type.TypeParam.t) =
      let open Ast.Type.TypeParam in
      let (_, { name = (name_loc, { Ast.Identifier.name; _ }); _ }) = tparam in
      this#add_ordinary_binding
        name_loc
        (mk_reason (RType (OrdinaryName name)) name_loc)
        (TypeParam (tparams, tparam));
      this#add_tparam name_loc name;
      super#type_param tparam

    method! interface loc (interface : ('loc, 'loc) Ast.Statement.Interface.t) =
      let open Ast.Statement.Interface in
      let { id = (name_loc, _); _ } = interface in
      this#add_ordinary_binding name_loc (mk_reason RInterfaceType loc) (Interface (loc, interface));
      this#in_new_tparams_env (fun () -> super#interface loc interface)

    method! enum_declaration loc (enum : ('loc, 'loc) Ast.Statement.EnumDeclaration.t) =
      let open Ast.Statement.EnumDeclaration in
      let { id = (name_loc, { Ast.Identifier.name; _ }); body; _ } = enum in
      this#add_ordinary_binding name_loc (mk_reason (REnum name) name_loc) (Enum (loc, body));
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
              this#add_ordinary_binding
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
          this#add_ordinary_binding
            id_loc
            import_reason
            (Import { import_kind; source; source_loc; import = Namespace })
        | None -> ()
      end;
      Base.Option.iter
        ~f:(fun (id_loc, { Ast.Identifier.name; _ }) ->
          let import_reason = mk_reason (RDefaultImportedType (name, source)) id_loc in
          this#add_ordinary_binding
            id_loc
            import_reason
            (Import { import_kind; source; source_loc; import = Default name }))
        default;
      super#import_declaration loc decl

    method! call _ expr =
      let open Ast.Expression.Call in
      let {
        callee;
        targs;
        arguments = (_, { Ast.Expression.ArgList.arguments; comments = _ });
        comments = _;
      } =
        expr
      in
      this#visit_expression ~hint:Hint_None ~cond:NonConditionalContext callee;
      Base.Option.iter targs ~f:(fun targs -> ignore @@ this#call_type_args targs);
      let call_argumemts_hint = Hint_t (ValueHint callee) in
      Base.List.iteri arguments ~f:(fun i arg ->
          let hint = decompose_hint (Decomp_FuncParam i) call_argumemts_hint in
          match arg with
          | Ast.Expression.Expression expr ->
            this#visit_expression ~hint ~cond:NonConditionalContext expr
          | Ast.Expression.Spread (_, spread) ->
            this#visit_expression
              ~hint
              ~cond:NonConditionalContext
              spread.Ast.Expression.SpreadElement.argument
      );
      expr

    method! new_ _ expr =
      let open Ast.Expression.New in
      let { callee; targs; arguments; comments = _ } = expr in
      this#visit_expression ~hint:Hint_None ~cond:NonConditionalContext callee;
      Base.Option.iter targs ~f:(fun targs -> ignore @@ this#call_type_args targs);
      let call_argumemts_hint = decompose_hint Decomp_CallNew (Hint_t (ValueHint callee)) in
      arguments
      |> Base.Option.value_map
           ~default:[]
           ~f:(fun (_, { Ast.Expression.ArgList.arguments; comments = _ }) -> arguments
         )
      |> Base.List.iteri ~f:(fun i arg ->
             let hint = decompose_hint (Decomp_FuncParam i) call_argumemts_hint in
             match arg with
             | Ast.Expression.Expression expr ->
               this#visit_expression ~hint ~cond:NonConditionalContext expr
             | Ast.Expression.Spread (_, spread) ->
               this#visit_expression
                 ~hint
                 ~cond:NonConditionalContext
                 spread.Ast.Expression.SpreadElement.argument
         );
      expr

    method! member _ _ = failwith "Should be visited by visit_member_expression"

    method private visit_member_expression ~cond loc mem =
      begin
        match EnvMap.find_opt_ordinary loc env_entries with
        | Some (Env_api.AssigningWrite reason) ->
          this#add_ordinary_binding
            loc
            reason
            (ChainExpression (cond, (loc, Ast.Expression.Member mem)))
        | _ -> ()
      end;
      ignore @@ super#member loc mem

    method! optional_member _ _ = failwith "Should be visited by visit_optional_member_expression"

    method private visit_optional_member_expression ~cond loc mem =
      begin
        match EnvMap.find_opt_ordinary loc env_entries with
        | Some (Env_api.AssigningWrite reason) ->
          this#add_ordinary_binding
            loc
            reason
            (ChainExpression (cond, (loc, Ast.Expression.OptionalMember mem)))
        | _ -> ()
      end;
      let open Ast.Expression.OptionalMember in
      let { member; optional = _; filtered_out = _ } = mem in
      ignore @@ super#member loc member

    method! type_cast _ expr =
      let open Ast.Expression.TypeCast in
      let { expression; annot; comments = _ } = expr in
      this#visit_expression
        ~hint:(Hint_t (AnnotationHint (ALocMap.empty, annot)))
        ~cond:NonConditionalContext
        expression;
      ignore @@ this#type_annotation annot;
      expr

    method! unary_expression _ expr =
      let open Flow_ast.Expression.Unary in
      let { argument; operator; comments = _ } = expr in
      let cond =
        match operator with
        | Not -> OtherConditionalTest
        | _ -> NonConditionalContext
      in
      this#visit_expression ~hint:Hint_None ~cond argument;
      expr

    method! jsx_element _ expr =
      let open Ast.JSX in
      let {
        opening_element =
          (_, { Opening.name = opening_name; self_closing = _; attributes = opening_attributes });
        closing_element = _;
        children;
        comments = _;
      } =
        expr
      in
      let hint =
        match opening_name with
        | Ast.JSX.Identifier (loc, { Ast.JSX.Identifier.name; comments }) ->
          Hint_t
            (ValueHint (loc, Ast.Expression.Identifier (loc, { Ast.Identifier.name; comments })))
        | Ast.JSX.NamespacedName _ -> Hint_None
        | Ast.JSX.MemberExpression member ->
          let rec jsx_title_member_to_expression member =
            let (mloc, member) = member in
            let _object =
              match member.Ast.JSX.MemberExpression._object with
              | Ast.JSX.MemberExpression.MemberExpression member ->
                jsx_title_member_to_expression member
              | Ast.JSX.MemberExpression.Identifier
                  (loc, { Ast.JSX.Identifier.name = "this"; comments }) ->
                (loc, Ast.Expression.This { Ast.Expression.This.comments })
              | Ast.JSX.MemberExpression.Identifier (loc, { Ast.JSX.Identifier.name; comments }) ->
                (loc, Ast.Expression.Identifier (loc, { Ast.Identifier.name; comments }))
            in
            let property =
              let open Ast.JSX.MemberExpression in
              let (loc, { Ast.JSX.Identifier.name; comments }) = member.property in
              (loc, { Ast.Identifier.name; comments })
            in
            ( mloc,
              Ast.Expression.Member
                {
                  Ast.Expression.Member._object;
                  property = Ast.Expression.Member.PropertyIdentifier property;
                  comments = None;
                }
            )
          in
          Hint_t (ValueHint (jsx_title_member_to_expression member))
      in
      Base.List.iter opening_attributes ~f:(function
          | Opening.Attribute (_, { Attribute.name; value }) ->
            let hint =
              match name with
              | Ast.JSX.Attribute.Identifier (_, { Ast.JSX.Identifier.name; comments = _ }) ->
                decompose_hint (Decomp_ObjProp name) hint
              | Ast.JSX.Attribute.NamespacedName _ -> Hint_None
            in
            Base.Option.iter value ~f:(fun value ->
                match value with
                | Attribute.Literal _ -> ()
                | Attribute.ExpressionContainer (_, expr) -> this#visit_jsx_expression ~hint expr
            )
          | Opening.SpreadAttribute (_, { SpreadAttribute.argument; comments = _ }) ->
            this#visit_expression
              ~hint:(decompose_hint Decomp_ObjSpread hint)
              ~cond:NonConditionalContext
              argument
          );
      this#visit_jsx_children ~hint:(decompose_hint (Decomp_ObjProp "children") hint) children;
      expr

    method private visit_jsx_expression ~hint expr =
      let open Ast.JSX.ExpressionContainer in
      let { expression; comments = _ } = expr in
      match expression with
      | Expression expr -> this#visit_expression ~hint ~cond:NonConditionalContext expr
      | EmptyExpression -> ()

    method private visit_jsx_children ~hint (_, children) =
      let single_child =
        match children with
        | [_] -> true
        | _ -> false
      in
      Base.List.iteri children ~f:(fun i (loc, child) ->
          let hint =
            if single_child then
              hint
            else
              decompose_hint (Decomp_ArrElement i) hint
          in
          match child with
          | Ast.JSX.Element elem -> ignore @@ this#jsx_element loc elem
          | Ast.JSX.Fragment frag -> ignore @@ this#jsx_fragment loc frag
          | Ast.JSX.ExpressionContainer expr -> this#visit_jsx_expression ~hint expr
          | Ast.JSX.SpreadChild _ -> () (* Unsupported syntax *)
          | Ast.JSX.Text _ -> ()
      )

    method! expression expr =
      this#visit_expression ~hint:Hint_None ~cond:NonConditionalContext expr;
      expr

    method private visit_expression ~hint ~cond ((loc, expr) as exp) =
      begin
        match EnvMap.find_opt (Env_api.ExpressionLoc, loc) env_entries with
        | Some (Env_api.AssigningWrite reason) ->
          this#add_binding (Env_api.ExpressionLoc, loc) reason (RefiExpression exp)
        | _ -> ()
      end;
      begin
        match EnvMap.find_opt (Env_api.ArrayProviderLoc, loc) env_entries with
        | Some (Env_api.AssigningWrite reason) ->
          this#add_binding (Env_api.ArrayProviderLoc, loc) reason (RefiExpression exp)
        | _ -> ()
      end;
      match expr with
      | Ast.Expression.Array expr -> this#visit_array_expression ~array_hint:hint expr
      | Ast.Expression.ArrowFunction x ->
        let scope_kind = func_scope_kind x in
        this#in_new_tparams_env (fun () -> this#visit_function ~func_hint:hint ~scope_kind x)
      | Ast.Expression.Function x -> this#visit_function_expr ~func_hint:hint loc x
      | Ast.Expression.Object expr -> this#visit_object_expression ~object_hint:hint expr
      | Ast.Expression.Member m -> this#visit_member_expression ~cond loc m
      | Ast.Expression.OptionalMember m -> this#visit_optional_member_expression ~cond loc m
      | Ast.Expression.Binary expr -> this#visit_binary_expression ~cond expr
      | Ast.Expression.Logical expr -> this#visit_logical_expression ~cond expr
      | Ast.Expression.Assignment _
      | Ast.Expression.Call _
      | Ast.Expression.Class _
      | Ast.Expression.Comprehension _
      | Ast.Expression.Conditional _
      | Ast.Expression.Generator _
      | Ast.Expression.Identifier _
      | Ast.Expression.Import _
      | Ast.Expression.JSXElement _
      | Ast.Expression.JSXFragment _
      | Ast.Expression.Literal _
      | Ast.Expression.MetaProperty _
      | Ast.Expression.New _
      | Ast.Expression.OptionalCall _
      | Ast.Expression.Sequence _
      | Ast.Expression.Super _
      | Ast.Expression.TaggedTemplate _
      | Ast.Expression.TemplateLiteral _
      | Ast.Expression.This _
      | Ast.Expression.TypeCast _
      | Ast.Expression.Unary _
      | Ast.Expression.Update _
      | Ast.Expression.Yield _ ->
        ignore @@ super#expression exp

    method! array _ _ = failwith "Should be visited by visit_array_expression"

    method private visit_array_expression ~array_hint expr =
      let { Ast.Expression.Array.elements; comments = _ } = expr in
      Base.List.iteri elements ~f:(fun i element ->
          match element with
          | Ast.Expression.Array.Expression expr ->
            this#visit_expression
              ~hint:(decompose_hint (Decomp_ArrElement i) array_hint)
              ~cond:NonConditionalContext
              expr
          | Ast.Expression.Array.Spread (_, spread) ->
            this#visit_expression
              ~hint:(decompose_hint (Decomp_ArrSpread i) array_hint)
              ~cond:NonConditionalContext
              spread.Ast.Expression.SpreadElement.argument
          | Ast.Expression.Array.Hole _ -> ()
      )

    method! conditional _ expr =
      let open Ast.Expression.Conditional in
      let { test; consequent; alternate; comments = _ } = expr in
      this#visit_expression ~hint:Hint_None ~cond:OtherConditionalTest test;
      this#visit_expression ~hint:Hint_None ~cond:NonConditionalContext consequent;
      this#visit_expression ~hint:Hint_None ~cond:NonConditionalContext alternate;
      expr

    method! binary _ _ = failwith "Should be visited by visit_binary_expression"

    method private visit_binary_expression ~cond expr =
      let open Ast.Expression.Binary in
      let { operator; left; right; comments = _ } = expr in
      match (operator, cond) with
      | ( (Equal | NotEqual | StrictEqual | StrictNotEqual),
          (SwitchConditionalTest _ | OtherConditionalTest)
        ) ->
        Eq_test.visit_eq_test
          ~on_type_of_test:(fun _ expr value _ _ ->
            this#visit_expression ~hint:Hint_None ~cond expr;
            ignore @@ this#expression value)
          ~on_literal_test:(fun ~strict:_ ~sense:_ _ expr _ value ->
            this#visit_expression ~hint:Hint_None ~cond expr;
            ignore @@ this#expression value)
          ~on_null_test:(fun ~sense:_ ~strict:_ _ expr value ->
            this#visit_expression ~hint:Hint_None ~cond expr;
            ignore @@ this#expression value)
          ~on_void_test:(fun ~sense:_ ~strict:_ ~check_for_bound_undefined:_ _ expr value ->
            this#visit_expression ~hint:Hint_None ~cond expr;
            ignore @@ this#expression value)
          ~on_member_eq_other:(fun expr value ->
            this#visit_expression ~hint:Hint_None ~cond expr;
            ignore @@ this#expression value)
          ~on_other_eq_member:(fun value expr ->
            this#visit_expression ~hint:Hint_None ~cond expr;
            ignore @@ this#expression value)
          ~is_switch_cond_context:
            (match cond with
            | SwitchConditionalTest _ -> false
            | _ -> true)
          ~strict:false
          ~sense:false
          ~on_other_eq_test:(fun left right ->
            ignore @@ this#expression left;
            ignore @@ this#expression right)
          ALoc.none
          left
          right
      | _ ->
        ignore @@ this#expression left;
        ignore @@ this#expression right

    method! logical _ _ = failwith "Should be visited by visit_logical_expression"

    method private visit_logical_expression ~cond expr =
      let open Ast.Expression.Logical in
      let { operator; left; right; comments = _ } = expr in
      let (left_cond, right_hint) =
        match operator with
        | And -> (OtherConditionalTest, Hint_None)
        | Or -> (OtherConditionalTest, Hint_t (ValueHint left))
        | NullishCoalesce -> (cond, Hint_t (ValueHint left))
      in
      this#visit_expression ~hint:Hint_None ~cond:left_cond left;
      this#visit_expression ~hint:right_hint ~cond right

    method! object_ _ _ = failwith "Should be visited by visit_object_expression"

    method private visit_object_expression ~object_hint expr =
      let open Ast.Expression.Object in
      let { properties; comments = _ } = expr in
      let visit_object_key_and_compute_hint = function
        | Ast.Expression.Object.Property.Literal
            (_, { Ast.Literal.value = Ast.Literal.String name; _ }) ->
          decompose_hint (Decomp_ObjProp name) object_hint
        | Ast.Expression.Object.Property.Literal _ -> Hint_None
        | Ast.Expression.Object.Property.Identifier (_, { Ast.Identifier.name; comments = _ }) ->
          decompose_hint (Decomp_ObjProp name) object_hint
        | Ast.Expression.Object.Property.PrivateName _ -> Hint_None (* Illegal syntax *)
        | Ast.Expression.Object.Property.Computed computed ->
          let (_, { Ast.ComputedKey.expression; comments = _ }) = computed in
          this#visit_expression ~hint:Hint_None ~cond:NonConditionalContext expression;
          decompose_hint Decomp_ObjComputed object_hint
      in
      Base.List.iter properties ~f:(fun prop ->
          match prop with
          | Property p ->
            let open Ast.Expression.Object.Property in
            (match p with
            | (_, Init { key; value; shorthand = _ }) ->
              let hint = visit_object_key_and_compute_hint key in
              this#visit_expression ~hint ~cond:NonConditionalContext value;
              ()
            | (loc, Method { key; value = (_, fn) }) ->
              let func_hint = visit_object_key_and_compute_hint key in
              this#visit_function_expr ~func_hint loc fn;
              ()
            | (loc, Get { key; value = (_, fn); comments = _ }) ->
              let func_hint = visit_object_key_and_compute_hint key in
              this#visit_function_expr ~func_hint loc fn;
              ()
            | (loc, Set { key; value = (_, fn); comments = _ }) ->
              let func_hint = visit_object_key_and_compute_hint key in
              this#visit_function_expr ~func_hint loc fn;
              ())
          | SpreadProperty s ->
            let (_, { Ast.Expression.Object.SpreadProperty.argument; comments = _ }) = s in
            this#visit_expression
              ~hint:(decompose_hint Decomp_ObjSpread object_hint)
              ~cond:NonConditionalContext
              argument
      )
  end

let find_defs env_entries providers ast =
  let finder = new def_finder env_entries providers Module in
  finder#eval finder#program ast
