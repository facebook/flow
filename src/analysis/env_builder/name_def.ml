(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Hint_api
open Reason
open Flow_ast_mapper
open Loc_collections
module EnvMap = Env_api.EnvMap
module EnvSet = Env_api.EnvSet
include Name_def_types

module Destructure : sig
  val type_of_pattern :
    (ALoc.t, ALoc.t) Ast.Pattern.t -> (ALoc.t, ALoc.t) Ast.Type.annotation option

  val fold_pattern :
    record_identifier:(ALoc.t -> ALoc.t Reason.virtual_reason -> binding -> 'a) ->
    record_destructuring_intermediate:(ALoc.t -> binding -> unit) ->
    visit_default_expression:(hint:ast_hint -> (ALoc.t, ALoc.t) Ast.Expression.t -> unit) ->
    join:('a -> 'a -> 'a) ->
    default:'a ->
    binding ->
    (ALoc.t, ALoc.t) Flow_ast.Pattern.t ->
    'a

  val pattern :
    record_identifier:(ALoc.t -> ALoc.t Reason.virtual_reason -> binding -> unit) ->
    record_destructuring_intermediate:(ALoc.t -> binding -> unit) ->
    visit_default_expression:(hint:ast_hint -> (ALoc.t, ALoc.t) Ast.Expression.t -> unit) ->
    binding ->
    (ALoc.t, ALoc.t) Flow_ast.Pattern.t ->
    unit
end = struct
  open Ast.Pattern

  let type_of_pattern (_, p) =
    let open Ast.Pattern in
    match p with
    | Array { Array.annot = Ast.Type.Available t; _ }
    | Object { Object.annot = Ast.Type.Available t; _ }
    | Identifier { Identifier.annot = Ast.Type.Available t; _ } ->
      Some t
    | _ -> None

  let array_element (parent_loc, acc) index direct_default =
    let selector = Elem { index; has_default = direct_default <> None } in
    Select { selector; parent = (parent_loc, acc) }

  let array_rest_element (parent_loc, acc) i =
    let selector = ArrRest i in
    Select { selector; parent = (parent_loc, acc) }

  let object_named_property (parent_loc, acc) prop_loc x direct_default =
    let has_default = direct_default <> None in
    let selector = Prop { prop = x; prop_loc; has_default } in
    Select { selector; parent = (parent_loc, acc) }

  let object_computed_property (parent_loc, acc) e direct_default =
    let selector = Computed { expression = e; has_default = direct_default <> None } in
    Select { selector; parent = (parent_loc, acc) }

  let object_rest_property (parent_loc, acc) xs has_computed =
    let selector = ObjRest { used_props = xs; after_computed = has_computed } in
    Select { selector; parent = (parent_loc, acc) }

  let object_property (parent_loc, acc) xs key direct_default =
    let open Ast.Pattern.Object in
    match key with
    | Property.Identifier (loc, { Ast.Identifier.name = x; comments = _ }) ->
      let acc = object_named_property (parent_loc, acc) loc x direct_default in
      (acc, x :: xs, false)
    | Property.Literal (loc, { Ast.Literal.value = Ast.Literal.String x; _ }) ->
      let acc = object_named_property (parent_loc, acc) loc x direct_default in
      (acc, x :: xs, false)
    | Property.Computed (_, { Ast.ComputedKey.expression; comments = _ }) ->
      let acc = object_computed_property (parent_loc, acc) expression direct_default in
      (acc, xs, true)
    | Property.Literal (_, _) -> (acc, xs, false)

  let identifier ~record_identifier acc (name_loc, { Ast.Identifier.name; _ }) =
    record_identifier name_loc (mk_reason (RIdentifier (OrdinaryName name)) name_loc) acc

  let rec fold_pattern
      ~record_identifier
      ~record_destructuring_intermediate
      ~visit_default_expression
      ~join
      ~default
      acc
      (ploc, p) =
    match p with
    | Array { Array.elements; annot = _; comments = _ } ->
      record_destructuring_intermediate ploc acc;
      array_elements
        ~record_identifier
        ~record_destructuring_intermediate
        ~visit_default_expression
        ~join
        ~default
        (ploc, acc)
        elements
    | Object { Object.properties; annot = _; comments = _ } ->
      record_destructuring_intermediate ploc acc;
      object_properties
        ~record_identifier
        ~record_destructuring_intermediate
        ~visit_default_expression
        ~join
        ~default
        (ploc, acc)
        properties
    | Identifier { Identifier.name = id; optional = _; annot = _ } ->
      identifier ~record_identifier acc id
    | Expression _ -> default

  and pattern_hint = function
    | (loc, Ast.Pattern.Identifier _) -> Hint_t (WriteLocHint (Env_api.OrdinaryNameLoc, loc))
    | (loc, _) -> Hint_t (WriteLocHint (Env_api.PatternLoc, loc))

  and array_elements
      ~record_identifier
      ~record_destructuring_intermediate
      ~visit_default_expression
      ~join
      ~default
      (parent_loc, acc)
      elts =
    let open Ast.Pattern.Array in
    Base.List.fold
      ~init:(0, default)
      ~f:(fun (i, prev) elt ->
        let res =
          match elt with
          | Hole _ -> default
          | Element (_, { Element.argument = p; default = d }) ->
            let hint = pattern_hint p in
            Base.Option.iter d ~f:(visit_default_expression ~hint);
            let acc = array_element (parent_loc, acc) i d in
            fold_pattern
              ~record_identifier
              ~record_destructuring_intermediate
              ~visit_default_expression
              ~join
              ~default
              acc
              p
          | RestElement (_, { Ast.Pattern.RestElement.argument = p; comments = _ }) ->
            let acc = array_rest_element (parent_loc, acc) i in
            fold_pattern
              ~record_identifier
              ~record_destructuring_intermediate
              ~visit_default_expression
              ~join
              ~default
              acc
              p
        in
        (i + 1, join prev res))
      elts
    |> snd

  and object_properties =
    let open Ast.Pattern.Object in
    let prop
        ~record_identifier
        ~record_destructuring_intermediate
        ~visit_default_expression
        ~join
        ~default
        (parent_loc, acc)
        xs
        has_computed
        p =
      match p with
      | Property (_, { Property.key; pattern = p; default = d; shorthand = _ }) ->
        let hint = pattern_hint p in
        Base.Option.iter d ~f:(visit_default_expression ~hint);
        let (acc, xs, has_computed') = object_property (parent_loc, acc) xs key d in
        ( fold_pattern
            ~record_identifier
            ~record_destructuring_intermediate
            ~visit_default_expression
            ~join
            ~default
            acc
            p,
          xs,
          has_computed || has_computed'
        )
      | RestElement (_, { Ast.Pattern.RestElement.argument = p; comments = _ }) ->
        let acc = object_rest_property (parent_loc, acc) xs has_computed in
        ( fold_pattern
            ~record_identifier
            ~record_destructuring_intermediate
            ~visit_default_expression
            ~join
            ~default
            acc
            p,
          xs,
          false
        )
    in
    let rec loop
        ~record_identifier
        ~record_destructuring_intermediate
        ~visit_default_expression
        ~join
        ~default
        res_acc
        acc
        xs
        has_computed = function
      | [] -> res_acc
      | p :: ps ->
        let (res, xs, has_computed) =
          prop
            ~record_identifier
            ~record_destructuring_intermediate
            ~visit_default_expression
            ~join
            ~default
            acc
            xs
            has_computed
            p
        in
        loop
          ~record_identifier
          ~record_destructuring_intermediate
          ~visit_default_expression
          ~join
          ~default
          (join res_acc res)
          acc
          xs
          has_computed
          ps
    in
    fun ~record_identifier
        ~record_destructuring_intermediate
        ~visit_default_expression
        ~join
        ~default
        acc
        ps ->
      loop
        ~record_identifier
        ~record_destructuring_intermediate
        ~visit_default_expression
        ~join
        ~default
        default
        acc
        []
        false
        ps

  let pattern = fold_pattern ~default:() ~join:(fun _ _ -> ())
end

let predicate_synthesizable predicate body =
  match (predicate, body) with
  | ( Some _,
      Ast.Function.BodyBlock
        ( _,
          {
            Ast.Statement.Block.body =
              [
                ( ret_loc,
                  Ast.Statement.Return
                    { Ast.Statement.Return.argument = Some expr; comments = _; return_out = _ }
                );
              ];
            comments = _;
          }
        )
    )
  | (Some (ret_loc, { Ast.Type.Predicate.kind = Ast.Type.Predicate.Declared expr; comments = _ }), _)
    ->
    FunctionPredicateSynthesizable (ret_loc, expr)
  | _ -> (* Invalid predicate definition *) FunctionSynthesizable

let func_is_synthesizable_from_annotation
    ({ Ast.Function.predicate; return; generator; body; _ } as f) =
  match return with
  | Ast.Type.Available _ ->
    if Base.Option.is_some predicate then
      predicate_synthesizable predicate body
    else
      FunctionSynthesizable
  | Ast.Type.Missing loc ->
    if
      Base.Option.is_some predicate
      || Nonvoid_return.might_have_nonvoid_return ALoc.none f
      || generator
    then
      MissingReturn loc
    else
      FunctionSynthesizable

let obj_this_write_locs { Ast.Expression.Object.properties; _ } =
  let open Ast.Expression.Object in
  Base.List.fold properties ~init:EnvSet.empty ~f:(fun acc -> function
    | Property (_, Property.Method _) -> acc (* this-in-object is banned. *)
    | Property (_, Property.Init { value = (_, Ast.Expression.ArrowFunction _); _ }) ->
      acc (* Arrow functions don't bind `this`. *)
    | Property
        ( _,
          Property.Init
            {
              value =
                ( f_loc,
                  Ast.Expression.Function
                    { Ast.Function.params = (_, { Ast.Function.Params.this_ = None; _ }); _ }
                );
              _;
            }
        ) ->
      EnvSet.add (Env_api.FunctionThisLoc, f_loc) acc
    | _ ->
      (* Everything else is impossible due to obj_properties_synthesizable check. *)
      acc
  )

let rec obj_properties_synthesizable
    ~this_write_locs { Ast.Expression.Object.properties; comments = _ } =
  let open Ast.Expression.Object in
  let open Ast.Expression.Object.Property in
  let open Ast.Expression.Object.SpreadProperty in
  let handle_fun elem_loc this_write_locs acc = function
    | FunctionSynthesizable -> Ok (acc, this_write_locs)
    | MissingReturn loc -> Ok (FuncMissingAnnot loc :: acc, this_write_locs)
    | _ ->
      (match elem_loc with
      | Some elem_loc -> Ok (OtherMissingAnnot elem_loc :: acc, this_write_locs)
      | None -> Error ())
  in
  let rec synthesizable_expression (acc, this_write_locs) (elem_loc, expr) =
    match expr with
    | Ast.Expression.Literal _
    | Ast.Expression.Identifier _
    | Ast.Expression.TypeCast _
    | Ast.Expression.Member
        {
          Ast.Expression.Member._object =
            (_, (Ast.Expression.Identifier _ | Ast.Expression.TypeCast _));
          property = Ast.Expression.Member.PropertyIdentifier _;
          _;
        } ->
      Ok (acc, this_write_locs)
    | Ast.Expression.ArrowFunction fn
    | Ast.Expression.Function fn ->
      handle_fun (Some elem_loc) this_write_locs acc (func_is_synthesizable_from_annotation fn)
    | Ast.Expression.Object obj ->
      begin
        match obj_properties_synthesizable ~this_write_locs:(obj_this_write_locs obj) obj with
        | ObjectSynthesizable { this_write_locs = new_this_write_locs } ->
          Ok (acc, EnvSet.union this_write_locs new_this_write_locs)
        | MissingMemberAnnots { locs = (hd, tl) } -> Ok (hd :: tl @ acc, this_write_locs)
        | Unsynthesizable -> Ok (OtherMissingAnnot elem_loc :: acc, this_write_locs)
      end
    | Ast.Expression.Array { Ast.Expression.Array.elements; _ } ->
      begin
        match
          Base.List.fold_result
            elements
            ~init:(acc, this_write_locs)
            ~f:(fun (acc, this_write_locs) -> function
            | Ast.Expression.Array.Expression exp
            | Ast.Expression.Array.Spread (_, { Ast.Expression.SpreadElement.argument = exp; _ }) ->
              synthesizable_expression (acc, this_write_locs) exp
            | Ast.Expression.Array.Hole _ -> Error ()
          )
        with
        | Ok res -> Ok res
        | Error () -> Ok (OtherMissingAnnot elem_loc :: acc, this_write_locs)
      end
    | _ -> Ok (OtherMissingAnnot elem_loc :: acc, this_write_locs)
  in

  let missing =
    Base.List.fold_result
      ~init:([], this_write_locs)
      ~f:
        (fun (acc, this_write_locs) -> function
          | SpreadProperty (_, { argument = (_, Ast.Expression.Identifier _); _ }) ->
            Ok (acc, this_write_locs)
          | SpreadProperty _ -> Error ()
          | Property (_, Init { key = Identifier (_, { Ast.Identifier.name = "__proto__"; _ }); _ })
            ->
            Error ()
          | Property (_, Method { key = Identifier _; value = (_, fn); _ }) ->
            handle_fun None this_write_locs acc (func_is_synthesizable_from_annotation fn)
          | Property (_, Init { key = Identifier _; value = expr; _ }) ->
            synthesizable_expression (acc, this_write_locs) expr
          | Property _ -> Error ())
      properties
  in
  match missing with
  | Ok ([], this_write_locs) -> ObjectSynthesizable { this_write_locs }
  | Ok (hd :: tl, _) -> MissingMemberAnnots { locs = (hd, tl) }
  | Error () -> Unsynthesizable

class returned_expression_collector =
  object (this)
    inherit [(ALoc.t, ALoc.t) Ast.Expression.t list, ALoc.t] Flow_ast_visitor.visitor ~init:[]

    method! return _ ({ Ast.Statement.Return.argument; _ } as node) =
      Base.Option.iter argument ~f:(fun e -> this#update_acc (fun acc -> e :: acc));
      node
  end

let function_params_all_annotated
    ((_, { Ast.Function.Params.params = parameters; rest; this_; _ }) as params) body =
  Base.List.for_all parameters ~f:(fun (_, { Ast.Function.Param.argument; _ }) ->
      argument |> Destructure.type_of_pattern |> Base.Option.is_some
  )
  && Base.Option.value_map rest ~default:true ~f:(fun (_, { Ast.Function.RestParam.argument; _ }) ->
         argument |> Destructure.type_of_pattern |> Base.Option.is_some
     )
  && (Base.Option.is_some this_
     || not (Signature_utils.This_finder.found_this_in_body_or_params body params)
     )

let expression_is_definitely_synthesizable =
  let rec synthesizable (_, expr) =
    let func_is_synthesizable fn =
      let { Ast.Function.params; body; return; _ } = fn in
      if function_params_all_annotated params body then (
        match (return, body) with
        | (Ast.Type.Available _, _) -> true
        | (Ast.Type.Missing _, Ast.Function.BodyExpression e) -> synthesizable e
        | (Ast.Type.Missing _, Ast.Function.BodyBlock (loc, block)) ->
          let collector = new returned_expression_collector in
          ignore @@ collector#block loc block;
          collector#acc |> Base.List.for_all ~f:synthesizable
      ) else
        false
    in
    match expr with
    | Ast.Expression.ArrowFunction x
    | Ast.Expression.Function x ->
      func_is_synthesizable x
    | Ast.Expression.Array expr ->
      let { Ast.Expression.Array.elements; comments = _ } = expr in
      Base.List.for_all elements ~f:(function
          | Ast.Expression.Array.Expression expr -> synthesizable expr
          | Ast.Expression.Array.Spread (_, spread) ->
            synthesizable spread.Ast.Expression.SpreadElement.argument
          | Ast.Expression.Array.Hole _ -> true
          )
      && (not @@ Base.List.is_empty elements)
    | Ast.Expression.Object expr ->
      let open Ast.Expression.Object in
      let { properties; comments = _ } = expr in
      Base.List.for_all properties ~f:(function
          | Property p ->
            let open Ast.Expression.Object.Property in
            (match p with
            | (_, Init { key = _; value; shorthand = _ }) -> synthesizable value
            | (_, Get { key = _; value = (_, fn); comments = _ })
            | (_, Set { key = _; value = (_, fn); comments = _ })
            | (_, Method { key = _; value = (_, fn) }) ->
              func_is_synthesizable fn)
          | SpreadProperty s ->
            let (_, { Ast.Expression.Object.SpreadProperty.argument; comments = _ }) = s in
            synthesizable argument
          )
    | Ast.Expression.Logical expr ->
      synthesizable expr.Ast.Expression.Logical.left
      && synthesizable expr.Ast.Expression.Logical.right
    | Ast.Expression.Conditional expr ->
      synthesizable expr.Ast.Expression.Conditional.consequent
      && synthesizable expr.Ast.Expression.Conditional.alternate
    | Ast.Expression.Unary expr ->
      let open Flow_ast.Expression.Unary in
      let { argument; operator; comments = _ } = expr in
      (match operator with
      | Await -> synthesizable argument
      | _ -> true)
    | Ast.Expression.Call _
    | Ast.Expression.OptionalCall _
    | Ast.Expression.New _
    | Ast.Expression.JSXElement _ ->
      (* Implicit instantiation might happen in these nodes, and we might have underconstrained targs. *)
      false
    | Ast.Expression.Assignment _
    | Ast.Expression.Binary _
    | Ast.Expression.Class _
    | Ast.Expression.Comprehension _
    | Ast.Expression.Generator _
    | Ast.Expression.Identifier _
    | Ast.Expression.Import _
    | Ast.Expression.JSXFragment _
    | Ast.Expression.Literal _
    | Ast.Expression.Member _
    | Ast.Expression.MetaProperty _
    | Ast.Expression.OptionalMember _
    | Ast.Expression.Sequence _
    | Ast.Expression.Super _
    | Ast.Expression.TaggedTemplate _
    | Ast.Expression.TemplateLiteral _
    | Ast.Expression.This _
    | Ast.Expression.TypeCast _
    | Ast.Expression.Update _
    | Ast.Expression.Yield _ ->
      true
  in
  (fun e -> synthesizable e)

let def_of_function ~tparams_map ~hint ~has_this_def ~function_loc ~statics function_ =
  Function
    {
      hint;
      synthesizable_from_annotation = func_is_synthesizable_from_annotation function_;
      has_this_def;
      function_loc;
      function_;
      tparams_map;
      statics;
    }

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

(* Existing own properties on `Function` as defined in `lib/core.js`. We don't
   want to shadow these when creating function statics. *)
let func_own_props = SSet.of_list ["toString"; "arguments"; "caller"; "length"; "name"]

module Eq_test = Eq_test.Make (Scope_api.With_ALoc) (Ssa_api.With_ALoc) (Env_api.With_ALoc)

class def_finder env_entries providers toplevel_scope =
  let fail loc str = raise Env_api.(Env_invariant (Some loc, ASTStructureOverride str)) in

  object (this)
    inherit
      [env_entries_map * hint_map, ALoc.t] Flow_ast_visitor.visitor
        ~init:(EnvMap.empty, ALocMap.empty) as super

    val mutable tparams : tparams_map = ALocMap.empty

    val mutable scope_kind : scope_kind = toplevel_scope

    val mutable class_stack : class_stack = []

    val mutable return_hint_stack : ast_hint list = []

    method add_tparam loc name = tparams <- ALocMap.add loc name tparams

    method record_hint loc hint =
      this#update_acc (fun (env_map, hint_map) -> (env_map, ALocMap.add loc hint hint_map))

    method force_add_binding kind_and_loc reason src =
      this#update_acc (fun (env_map, hint_map) ->
          (EnvMap.add kind_and_loc (src, scope_kind, class_stack, reason) env_map, hint_map)
      )

    method add_binding kind_and_loc reason src =
      if Env_api.has_assigning_write kind_and_loc env_entries then
        this#force_add_binding kind_and_loc reason src

    method add_ordinary_binding loc = this#add_binding (Env_api.OrdinaryNameLoc, loc)

    method add_destructure_binding loc binding =
      this#add_binding (Env_api.PatternLoc, loc) (mk_reason RDestructuring loc) (Binding binding)

    method add_destructure_bindings root pattern =
      let record_identifier loc reason binding =
        this#add_ordinary_binding loc reason (Binding binding)
      in
      Destructure.pattern
        ~record_identifier
        ~record_destructuring_intermediate:this#add_destructure_binding
        ~visit_default_expression:(this#visit_expression ~cond:NonConditionalContext)
        (Root root)
        pattern

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

    method! statement_list stmts =
      (* Function statics *)
      let open Ast.Statement in
      let stmts = Flow_ast_utils.hoist_function_declarations stmts in
      let rec loop state stmts =
        match stmts with
        | [] ->
          SMap.iter
            (fun _ (statics, process_funcs) -> Base.List.iter ~f:(fun f -> f statics) process_funcs)
            state
        (* f.a = <e>; *)
        | ( _,
            Expression
              {
                Expression.expression =
                  ( assign_loc,
                    Ast.Expression.Assignment
                      ( {
                          Ast.Expression.Assignment.operator = None;
                          left =
                            ( _,
                              Ast.Pattern.Expression
                                ( member_loc,
                                  Ast.Expression.Member
                                    {
                                      Ast.Expression.Member._object =
                                        ( _,
                                          Ast.Expression.Identifier
                                            (_, { Ast.Identifier.name = obj_name; _ })
                                        );
                                      property =
                                        Ast.Expression.Member.PropertyIdentifier
                                          (_, { Ast.Identifier.name = prop_name; _ });
                                      _;
                                    }
                                )
                            ) as left;
                          right = init;
                          _;
                        } as assign
                      )
                  );
                _;
              }
          )
          :: xs
          when SMap.mem obj_name state && (not @@ SSet.mem prop_name func_own_props) ->
          this#visit_assignment_expression ~is_function_statics_assignment:true assign_loc assign;
          let state =
            SMap.update
              obj_name
              (function
                | None -> None
                | Some (statics, process_funcs) ->
                  let statics =
                    (* Only first assignment sets the type. *)
                    SMap.update
                      prop_name
                      (function
                        | None ->
                          let (entries, _) = this#acc in
                          let key =
                            match init with
                            | (_, Ast.Expression.Function { Ast.Function.id = Some (fun_loc, _); _ })
                            | (fun_loc, Ast.Expression.Function _) ->
                              (Env_api.OrdinaryNameLoc, fun_loc)
                            | _ ->
                              this#add_ordinary_binding
                                member_loc
                                (mk_pattern_reason left)
                                (ExpressionDef
                                   {
                                     cond_context = NonConditionalContext;
                                     hint = Hint_None;
                                     chain = false;
                                     expr = init;
                                   }
                                );
                              (Env_api.OrdinaryNameLoc, member_loc)
                          in
                          if EnvMap.mem key entries then
                            Some key
                          else
                            None
                        | x -> x)
                      statics
                  in
                  Some (statics, process_funcs))
              state
          in
          loop state xs
        (* function f() {}; export function f() {}; export default function f() {} *)
        | ( ( ( loc,
                FunctionDeclaration
                  ({ Ast.Function.id = Some (id_loc, { Ast.Identifier.name; _ }); _ } as func)
              )
            | ( _,
                ExportNamedDeclaration
                  {
                    ExportNamedDeclaration.declaration =
                      Some
                        ( loc,
                          FunctionDeclaration
                            ( { Ast.Function.id = Some (id_loc, { Ast.Identifier.name; _ }); _ } as
                            func
                            )
                        );
                    export_kind = ExportValue;
                    specifiers = None;
                    source = None;
                    _;
                  }
              )
            | ( _,
                ExportDefaultDeclaration
                  {
                    ExportDefaultDeclaration.declaration =
                      ExportDefaultDeclaration.Declaration
                        ( loc,
                          FunctionDeclaration
                            ( { Ast.Function.id = Some (id_loc, { Ast.Identifier.name; _ }); _ } as
                            func
                            )
                        );
                    _;
                  }
              ) ) as stmt
          )
          :: xs ->
          let state =
            if Env_api.has_assigning_write (Env_api.OrdinaryNameLoc, id_loc) env_entries then
              let visit statics = this#visit_function_declaration ~statics loc func in
              SMap.update
                name
                (function
                  | None -> Some (SMap.empty, [visit])
                  | Some (statics, process_funcs) -> Some (statics, visit :: process_funcs))
                state
            else (
              ignore @@ this#statement stmt;
              state
            )
          in
          loop state xs
        (* const f = () => {}; const f = function () {}; *)
        | ( ( _,
              ( VariableDeclaration
                  {
                    VariableDeclaration.declarations =
                      [
                        ( _,
                          {
                            VariableDeclaration.Declarator.id =
                              ( _,
                                Ast.Pattern.Identifier
                                  {
                                    Ast.Pattern.Identifier.name =
                                      (id_loc, { Ast.Identifier.name; _ }) as var_id;
                                    annot = Ast.Type.Missing _;
                                    optional = false;
                                  }
                              );
                            init =
                              Some
                                ( ( (loc, Ast.Expression.ArrowFunction func)
                                  | (loc, Ast.Expression.Function func) ) as init
                                );
                          }
                        );
                      ];
                    _;
                  }
              | ExportNamedDeclaration
                  {
                    ExportNamedDeclaration.declaration =
                      Some
                        ( _,
                          VariableDeclaration
                            {
                              VariableDeclaration.declarations =
                                [
                                  ( _,
                                    {
                                      VariableDeclaration.Declarator.id =
                                        ( _,
                                          Ast.Pattern.Identifier
                                            {
                                              Ast.Pattern.Identifier.name =
                                                (id_loc, { Ast.Identifier.name; _ }) as var_id;
                                              annot = Ast.Type.Missing _;
                                              optional = false;
                                            }
                                        );
                                      init =
                                        Some
                                          ( ( (loc, Ast.Expression.ArrowFunction func)
                                            | (loc, Ast.Expression.Function func) ) as init
                                          );
                                    }
                                  );
                                ];
                              _;
                            }
                        );
                    export_kind = ExportValue;
                    specifiers = None;
                    source = None;
                    _;
                  } )
            ) as stmt
          )
          :: xs ->
          let arrow =
            match init with
            | (_, Ast.Expression.ArrowFunction _) -> true
            | _ -> false
          in
          let state =
            if Env_api.has_assigning_write (Env_api.OrdinaryNameLoc, id_loc) env_entries then
              let visit statics =
                this#visit_function_expr
                  ~func_hint:Hint_None
                  ~has_this_def:(not arrow)
                  ~var_assigned_to:(Some var_id)
                  ~statics
                  ~arrow
                  loc
                  func
              in
              SMap.update
                name
                (function
                  | None -> Some (SMap.empty, [visit])
                  | Some (statics, process_funcs) -> Some (statics, visit :: process_funcs))
                state
            else (
              ignore @@ this#statement stmt;
              state
            )
          in
          loop state xs
        | x :: xs ->
          ignore @@ this#statement x;
          loop state xs
      in
      loop SMap.empty stmts;
      stmts

    method! variable_declarator ~kind decl =
      let open Ast.Statement.VariableDeclaration.Declarator in
      let (_, { id; init }) = decl in
      let (source, hint) =
        match (Destructure.type_of_pattern id, init, id) with
        | (Some annot, _, _) ->
          ( Some
              (Annotation
                 {
                   tparams_map = ALocMap.empty;
                   optional = false;
                   has_default_expression = false;
                   param_loc = None;
                   annot;
                 }
              ),
            Hint_t (AnnotationHint (ALocMap.empty, annot))
          )
        | ( None,
            Some (arr_loc, Ast.Expression.Array { Ast.Expression.Array.elements = []; _ }),
            (_, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name = (name_loc, _); _ })
          ) ->
          let { Provider_api.array_providers; _ } =
            Base.Option.value_exn (Provider_api.providers_of_def providers name_loc)
          in
          (Some (EmptyArray { array_providers; arr_loc }), Hint_None)
        | ( None,
            Some
              ( ( loc,
                  Ast.Expression.Object ({ Ast.Expression.Object.properties = _ :: _; _ } as obj)
                ) as init
              ),
            _
          ) ->
          let this_write_locs = obj_this_write_locs obj in
          begin
            match obj_properties_synthesizable ~this_write_locs obj with
            | Unsynthesizable -> (Some (Value { hint = Hint_None; expr = init }), Hint_None)
            | synthesizable -> (Some (ObjectValue { obj_loc = loc; obj; synthesizable }), Hint_None)
          end
        | (None, Some init, _) -> (Some (Value { hint = Hint_None; expr = init }), Hint_None)
        | (None, None, _) -> (None, Hint_None)
      in
      Base.Option.iter ~f:(fun acc -> this#add_destructure_bindings acc id) source;
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
                 {
                   tparams_map = ALocMap.empty;
                   optional = false;
                   has_default_expression = false;
                   param_loc = None;
                   annot;
                 }
              )
           )
        );
      super#declare_variable loc decl

    method! pattern_array_element ?kind elem =
      let open Ast.Pattern.Array.Element in
      let (loc, { argument; default = _ }) = elem in
      (* Default should already be visited during destructuring visit. *)
      super#pattern_array_element ?kind (loc, { argument; default = None })

    method! pattern_object_property ?kind prop =
      let open Ast.Pattern.Object.Property in
      let (loc, { key; pattern; default = _; shorthand }) = prop in
      (* Default should already be visited during destructuring visit. *)
      super#pattern_object_property ?kind (loc, { key; pattern; default = None; shorthand })

    method! function_param (loc, _) = fail loc "Should be visited by visit_function_param"

    method private visit_function_param ~hint (param : ('loc, 'loc) Ast.Function.Param.t) =
      let open Ast.Function.Param in
      let (loc, { argument; default = default_expression }) = param in
      let optional =
        match argument with
        | (_, Ast.Pattern.Identifier { Ast.Pattern.Identifier.optional; _ }) -> optional
        | _ -> false
      in
      let (param_loc, _) = argument in
      let annot = Destructure.type_of_pattern argument in
      let source =
        match annot with
        | Some annot ->
          Base.Option.iter
            default_expression
            ~f:
              (this#visit_expression
                 ~hint:(Hint_t (AnnotationHint (tparams, annot)))
                 ~cond:NonConditionalContext
              );
          Annotation
            {
              tparams_map = tparams;
              optional;
              has_default_expression = Base.Option.is_some default_expression;
              param_loc = Some param_loc;
              annot;
            }
        | None ->
          Base.Option.iter
            default_expression
            ~f:(this#visit_expression ~hint:Hint_None ~cond:NonConditionalContext);
          let reason =
            match argument with
            | ( _,
                Ast.Pattern.Identifier
                  { Ast.Pattern.Identifier.name = (_, { Ast.Identifier.name; _ }); _ }
              ) ->
              mk_reason (RParameter (Some name)) param_loc
            | _ -> mk_reason RDestructuring param_loc
          in
          this#record_hint param_loc hint;
          Contextual { reason; hint; optional; default_expression }
      in
      let record_identifier loc reason binding =
        this#add_ordinary_binding loc reason (Binding binding);
        true
      in
      if
        (not
           (Destructure.fold_pattern
              ~record_identifier
              ~record_destructuring_intermediate:this#add_destructure_binding
              ~visit_default_expression:(this#visit_expression ~cond:NonConditionalContext)
              ~default:false
              ~join:( || )
              (Root source)
              argument
           )
        )
        && Base.Option.is_none annot
      then
        this#add_binding
          (Env_api.FunctionParamLoc, loc)
          (mk_reason RDestructuring loc)
          NonBindingParam;
      ignore @@ super#function_param (loc, { argument; default = None })

    method private visit_function_rest_param ~hint (expr : ('loc, 'loc) Ast.Function.RestParam.t) =
      let open Ast.Function.RestParam in
      let (_, { argument; comments = _ }) = expr in
      let (param_loc, _) = argument in
      let source =
        match Destructure.type_of_pattern argument with
        | Some annot ->
          Annotation
            {
              tparams_map = tparams;
              optional = false;
              has_default_expression = false;
              param_loc = Some param_loc;
              annot;
            }
        | None ->
          let reason =
            match argument with
            | ( _,
                Ast.Pattern.Identifier
                  { Ast.Pattern.Identifier.name = (_, { Ast.Identifier.name; _ }); _ }
              ) ->
              mk_reason (RRestParameter (Some name)) param_loc
            | _ ->
              (* TODO: This should be a parse error, but we only produce an internal
                 error in statement.ml. *)
              mk_reason (RCustom "contextual variable") param_loc
          in
          this#record_hint param_loc hint;
          Contextual { reason; hint; optional = false; default_expression = None }
      in
      this#add_destructure_bindings source argument;
      ignore @@ super#function_rest_param expr

    method! function_this_param this_ =
      let (loc, { Ast.Function.ThisParam.annot; comments = _ }) = this_ in
      this#add_ordinary_binding
        loc
        (mk_reason RThis loc)
        (Binding
           (Root
              (Annotation
                 {
                   tparams_map = tparams;
                   optional = false;
                   has_default_expression = false;
                   param_loc = None;
                   annot;
                 }
              )
           )
        );
      super#function_this_param this_

    method! catch_clause_pattern pat =
      this#add_destructure_bindings Catch pat;
      super#catch_clause_pattern pat

    method visit_function_expr
        ~func_hint ~has_this_def ~var_assigned_to ~statics ~arrow function_loc expr =
      let {
        Ast.Function.id;
        async;
        generator;
        sig_loc;
        params = (_, { Ast.Function.Params.this_; _ }) as params;
        body;
        _;
      } =
        expr
      in
      let scope_kind = func_scope_kind expr in
      begin
        match this_ with
        | None
          when has_this_def && Signature_utils.This_finder.found_this_in_body_or_params body params
          ->
          this#add_binding
            (Env_api.FunctionThisLoc, function_loc)
            (mk_reason RThis function_loc)
            MissingThisAnnot
        | _ -> ()
      end;
      this#in_new_tparams_env (fun () ->
          this#visit_function ~scope_kind ~func_hint expr;
          (match var_assigned_to with
          | Some (name_loc, { Ast.Identifier.name; comments = _ }) ->
            this#add_ordinary_binding
              name_loc
              (mk_reason (RIdentifier (OrdinaryName name)) name_loc)
              (Binding
                 (Root
                    (FunctionValue
                       {
                         hint = func_hint;
                         synthesizable_from_annotation = func_is_synthesizable_from_annotation expr;
                         function_loc;
                         function_ = expr;
                         statics;
                         arrow;
                         tparams_map = tparams;
                       }
                    )
                 )
              )
          | None -> ());
          let add_def binding_loc =
            let reason =
              func_reason
                ~async
                ~generator
                ( if arrow then
                  function_loc
                else
                  sig_loc
                )
            in
            let def =
              def_of_function
                ~tparams_map:tparams
                ~hint:func_hint
                ~has_this_def
                ~function_loc
                ~statics
                expr
            in
            this#add_ordinary_binding binding_loc reason def
          in
          match id with
          | Some (id_loc, _) -> add_def id_loc
          | None when has_this_def -> add_def function_loc
          | None -> ()
      )

    method! function_expression loc expr =
      this#visit_function_expr
        ~func_hint:Hint_None
        ~has_this_def:true
        ~var_assigned_to:None
        ~statics:SMap.empty
        ~arrow:false
        loc
        expr;
      expr

    method! function_declaration loc expr =
      this#visit_function_declaration ~statics:SMap.empty loc expr;
      expr

    method visit_function_declaration ~statics loc expr =
      let {
        Ast.Function.id;
        async;
        generator;
        sig_loc;
        params = (_, { Ast.Function.Params.this_; _ }) as params;
        body;
        _;
      } =
        expr
      in
      let scope_kind = func_scope_kind expr in
      this#in_new_tparams_env (fun () ->
          begin
            match this_ with
            | None when Signature_utils.This_finder.found_this_in_body_or_params body params ->
              this#add_binding (Env_api.FunctionThisLoc, loc) (mk_reason RThis loc) MissingThisAnnot
            | _ -> ()
          end;
          this#visit_function ~func_hint:Hint_None ~scope_kind expr;
          let reason = func_reason ~async ~generator sig_loc in
          let def =
            def_of_function
              ~tparams_map:tparams
              ~hint:Hint_None
              ~has_this_def:true
              ~function_loc:loc
              ~statics
              expr
          in
          match id with
          | Some (id_loc, _) -> this#add_ordinary_binding id_loc reason def
          | None -> this#add_ordinary_binding loc reason def
      )

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
            params = (_, { Ast.Function.Params.params = params_list; rest; comments = _; this_ });
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
          ignore (Base.Option.map this_ ~f:this#function_this_param : _ option);
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

          let return_loc =
            match return with
            | Ast.Type.Available (loc, _)
            | Ast.Type.Missing loc ->
              loc
          in
          let return_hint =
            let base_hint =
              match return with
              | Ast.Type.Available annot -> Hint_t (AnnotationHint (ALocMap.empty, annot))
              | Ast.Type.Missing _ -> decompose_hint Decomp_FuncReturn func_hint
            in
            match scope_kind with
            | Async -> base_hint |> decompose_hint Decomp_Promise
            | _ -> base_hint
          in
          this#record_hint return_loc return_hint;
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
          let class_implicit_this_tparam =
            this#in_scope
              (fun () ->
                Flow_ast_visitor.run_opt this#class_identifier id;
                Flow_ast_visitor.run_opt this#type_params class_tparams;
                let this_tparam_loc = Base.Option.value_map ~default:loc ~f:fst id in
                let class_tparams_map = tparams in
                this#add_tparam this_tparam_loc "this";
                ignore @@ this#class_body body;
                Flow_ast_visitor.run_opt (Flow_ast_mapper.map_loc this#class_extends) extends;
                Flow_ast_visitor.run_opt this#class_implements implements;
                Flow_ast_visitor.run_list this#class_decorator class_decorators;
                {
                  tparams_map = class_tparams_map;
                  class_tparams_loc = Base.Option.map ~f:fst class_tparams;
                })
              Ordinary
              ()
          in
          class_stack <- old_stack;
          let this_super_write_locs =
            let (_, { Ast.Class.Body.body; _ }) = body in
            Base.List.fold body ~init:EnvSet.empty ~f:(fun acc -> function
              | Ast.Class.Body.Property
                  ( _,
                    {
                      Ast.Class.Property.value =
                        Ast.Class.Property.Initialized (f_loc, Ast.Expression.Function _);
                      _;
                    }
                  )
              | Ast.Class.Body.PrivateField
                  ( _,
                    {
                      Ast.Class.PrivateField.value =
                        Ast.Class.Property.Initialized
                          ( f_loc,
                            Ast.Expression.Function
                              {
                                Ast.Function.params = (_, { Ast.Function.Params.this_ = None; _ });
                                _;
                              }
                          );
                      _;
                    }
                  ) ->
                EnvSet.add (Env_api.FunctionThisLoc, f_loc) acc
              | _ -> acc
            )
          in
          let this_super_write_locs =
            this_super_write_locs
            |> EnvSet.add (Env_api.ClassSelfLoc, loc)
            |> EnvSet.add (Env_api.ClassInstanceThisLoc, loc)
            |> EnvSet.add (Env_api.ClassStaticThisLoc, loc)
            |> EnvSet.add (Env_api.ClassInstanceSuperLoc, loc)
            |> EnvSet.add (Env_api.ClassStaticSuperLoc, loc)
          in
          begin
            match id with
            | Some (id_loc, { Ast.Identifier.name; _ }) ->
              let name = OrdinaryName name in
              let reason = mk_reason (RType name) id_loc in
              this#add_ordinary_binding
                id_loc
                reason
                (Class
                   {
                     class_loc = loc;
                     class_implicit_this_tparam;
                     class_ = expr;
                     this_super_write_locs;
                   }
                )
            | None ->
              let reason = mk_reason (RType (OrdinaryName "<<anonymous class>>")) loc in
              this#add_ordinary_binding
                loc
                reason
                (Class
                   {
                     class_loc = loc;
                     class_implicit_this_tparam;
                     class_ = expr;
                     this_super_write_locs;
                   }
                )
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
                   {
                     tparams_map = ALocMap.empty;
                     optional = false;
                     has_default_expression = false;
                     param_loc = None;
                     annot;
                   }
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

    method! assignment loc _ = fail loc "Should be visited by visit_assignment_expression"

    method private visit_assignment_expression ~is_function_statics_assignment loc expr =
      let open Ast.Expression.Assignment in
      let { operator; left = (lhs_loc, lhs_node) as left; right; comments = _ } = expr in
      let is_provider =
        Flow_ast_utils.fold_bindings_of_pattern
          (fun acc (loc, _) -> acc || Env_api.Provider_api.is_provider providers loc)
          false
          left
      in
      let hint =
        if is_provider || is_function_statics_assignment then
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
            | Ast.Expression.Member.PropertyExpression expr ->
              decompose_hint
                (Decomp_ObjComputed (mk_expression_reason expr))
                (Hint_t (ValueHint _object)))
          | _ ->
            (* TODO create a hint based on the lhs pattern *)
            Hint_Placeholder
      in
      let () =
        match (operator, lhs_node) with
        | (None, Ast.Pattern.Expression (member_loc, Ast.Expression.Member member)) ->
          (* Use super member to visit sub-expressions to avoid record a read of the member. *)
          ignore @@ super#member member_loc member;
          this#add_ordinary_binding
            member_loc
            (mk_pattern_reason left)
            (MemberAssign { member_loc; member; rhs = right })
        | (None, _) ->
          let (_ : (_, _) Ast.Pattern.t) = this#assignment_pattern (lhs_loc, lhs_node) in
          this#add_destructure_bindings (Value { hint; expr = right }) left
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
        | _ ->
          let (_ : (_, _) Ast.Pattern.t) = this#assignment_pattern (lhs_loc, lhs_node) in
          ()
      in
      this#visit_expression ~hint ~cond:NonConditionalContext right

    method! update_expression loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Update.t) =
      let open Ast.Expression.Update in
      let { argument; operator; prefix = _; comments = _ } = expr in
      begin
        match argument with
        | (_, Ast.Expression.Identifier (id_loc, { Ast.Identifier.name; _ })) ->
          this#add_ordinary_binding
            id_loc
            (mk_reason (RIdentifier (OrdinaryName name)) id_loc)
            (Update { exp_loc = loc; op = operator })
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
                {
                  tparams_map = ALocMap.empty;
                  optional = false;
                  has_default_expression = false;
                  param_loc = None;
                  annot;
                }
            | None -> For (Of { await }, right)
          in
          this#add_destructure_bindings source id
        | LeftDeclaration _ ->
          raise Env_api.(Env_invariant (Some loc, Impossible "Invalid AST structure"))
        | LeftPattern pat -> this#add_destructure_bindings (For (Of { await }, right)) pat
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
                {
                  tparams_map = ALocMap.empty;
                  optional = false;
                  has_default_expression = false;
                  param_loc = None;
                  annot;
                }
            | None -> For (In, right)
          in
          this#add_destructure_bindings source id
        | LeftDeclaration _ ->
          raise Env_api.(Env_invariant (Some loc, Impossible "Invalid AST structure"))
        | LeftPattern pat -> this#add_destructure_bindings (For (In, right)) pat
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
      this#force_add_binding
        (Env_api.OrdinaryNameLoc, name_loc)
        (mk_reason (RType (OrdinaryName name)) name_loc)
        (TypeParam (tparams, tparam));
      this#add_tparam name_loc name;
      super#type_param tparam

    method! interface loc (interface : ('loc, 'loc) Ast.Statement.Interface.t) =
      let open Ast.Statement.Interface in
      let { id = (name_loc, _); _ } = interface in
      this#add_ordinary_binding name_loc (mk_reason RInterfaceType loc) (Interface (loc, interface));
      this#in_new_tparams_env (fun () -> super#interface loc interface)

    method! declare_module loc (m : ('loc, 'loc) Ast.Statement.DeclareModule.t) =
      let { Ast.Statement.DeclareModule.id; _ } = m in
      let (name_loc, name) =
        match id with
        | Ast.Statement.DeclareModule.Identifier
            (name_loc, { Ast.Identifier.name = value; comments = _ })
        | Ast.Statement.DeclareModule.Literal (name_loc, { Ast.StringLiteral.value; _ }) ->
          (name_loc, value)
      in
      let r = mk_reason (RModule (OrdinaryName name)) loc in
      this#add_ordinary_binding name_loc r (DeclaredModule (loc, m));
      super#declare_module loc m

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

    method! call loc _ = fail loc "Should be visited by visit_call_expression"

    method private visit_call_expression ~hint ~cond ~visit_callee loc expr =
      let {
        Ast.Expression.Call.callee;
        targs;
        arguments = (_, { Ast.Expression.ArgList.arguments; comments = _ }) as arg_list;
        comments = _;
      } =
        expr
      in
      (* Provide hint in the very special case of immediate function execution,
       * ie. `(function (){..})()`. *)
      let callee_hint =
        match (callee, arguments) with
        | ( ( _,
              ( Ast.Expression.ArrowFunction
                  {
                    Ast.Function.tparams = None;
                    params = (_, Ast.Function.Params.{ this_ = None; params = []; rest = None; _ });
                    _;
                  }
              | Ast.Expression.Function
                  {
                    Ast.Function.tparams = None;
                    params = (_, Ast.Function.Params.{ this_ = None; params = []; rest = None; _ });
                    _;
                  } )
            ),
            []
          ) ->
          decompose_hint Comp_ImmediateFuncCall hint
        | _ -> Hint_None
      in
      visit_callee ~hint:callee_hint callee;
      Base.Option.iter targs ~f:(fun targs -> ignore @@ this#call_type_args targs);
      if Flow_ast_utils.is_call_to_invariant callee then
        Base.List.iteri arguments ~f:(fun i -> function
          | Ast.Expression.Expression expr ->
            let cond =
              if i = 0 then
                OtherConditionalTest
              else
                NonConditionalContext
            in
            (* In invariant(...) call, the first argument is under conditional context. *)
            this#visit_expression ~hint:Hint_None ~cond expr
          | Ast.Expression.Spread (_, spread) ->
            this#visit_expression
              ~hint:Hint_None
              ~cond:NonConditionalContext
              spread.Ast.Expression.SpreadElement.argument
        )
      else
        match arguments with
        | [Ast.Expression.Expression expr] when Flow_ast_utils.is_call_to_is_array callee ->
          this#visit_expression ~hint:Hint_None ~cond expr
        | [Ast.Expression.Expression expr] when Flow_ast_utils.is_call_to_object_dot_freeze callee
          ->
          this#visit_expression ~hint ~cond:NonConditionalContext expr
        | _ when Flow_ast_utils.is_call_to_object_static_method callee ->
          Base.List.iter arguments ~f:(fun arg ->
              match arg with
              | Ast.Expression.Expression expr ->
                this#visit_expression ~hint:Hint_None ~cond:NonConditionalContext expr
              | Ast.Expression.Spread (_, spread) ->
                this#visit_expression
                  ~hint:Hint_None
                  ~cond:NonConditionalContext
                  spread.Ast.Expression.SpreadElement.argument
          )
        | _ ->
          let call_argumemts_hint =
            match callee with
            | (_, Ast.Expression.Member { Ast.Expression.Member._object; property; comments = _ })
            | ( _,
                Ast.Expression.OptionalMember
                  {
                    Ast.Expression.OptionalMember.member =
                      { Ast.Expression.Member._object; property; comments = _ };
                    filtered_out = _;
                    optional = _;
                  }
              ) ->
              let base_hint = Hint_t (ValueHint _object) in
              (match property with
              | Ast.Expression.Member.PropertyIdentifier (_, { Ast.Identifier.name; comments = _ })
                ->
                decompose_hint (Decomp_MethodName name) base_hint
              | Ast.Expression.Member.PropertyPrivateName (_, { Ast.PrivateName.name; comments = _ })
                ->
                decompose_hint (Decomp_MethodPrivateName (name, class_stack)) base_hint
              | Ast.Expression.Member.PropertyExpression _ ->
                decompose_hint Decomp_MethodElem base_hint)
            | _ -> Hint_t (ValueHint callee)
          in
          let call_reason = mk_expression_reason (loc, Ast.Expression.Call expr) in
          this#visit_call_arguments
            ~call_reason
            ~call_argumemts_hint
            ~return_hint:hint
            arg_list
            targs

    method private visit_call_arguments
        ~call_reason
        ~call_argumemts_hint
        ~return_hint
        ((_, { Ast.Expression.ArgList.arguments; comments = _ }) as arg_list)
        targs =
      Base.List.iteri arguments ~f:(fun i arg ->
          let hint =
            call_argumemts_hint
            |> decompose_hint
                 (Instantiate_Callee
                    {
                      Hint_api.reason = call_reason;
                      return_hint;
                      targs = lazy targs;
                      arg_list = lazy arg_list;
                      arg_index = i;
                    }
                 )
            |> decompose_hint (Decomp_FuncParam i)
          in
          match arg with
          | Ast.Expression.Expression expr ->
            this#visit_expression ~hint ~cond:NonConditionalContext expr
          | Ast.Expression.Spread (_, spread) ->
            this#visit_expression
              ~hint
              ~cond:NonConditionalContext
              spread.Ast.Expression.SpreadElement.argument
      )

    method! optional_call loc _ = fail loc "Should be visited by visit_optional_call_expression"

    method private visit_optional_call_expression ~hint ~cond loc expr =
      let open Ast.Expression.OptionalCall in
      let { call; optional = _; filtered_out = _ } = expr in
      this#visit_call_expression
        loc
        call
        ~hint
        ~cond
        ~visit_callee:(this#visit_expression ~cond:NonConditionalContext)

    method! new_ loc _ = fail loc "Should be visited by visit_new_expression"

    method visit_new_expression ~hint loc expr =
      let { Ast.Expression.New.callee; targs; arguments; comments = _ } = expr in
      this#visit_expression ~hint:Hint_None ~cond:NonConditionalContext callee;
      Base.Option.iter targs ~f:(fun targs -> ignore @@ this#call_type_args targs);
      let call_argumemts_hint = decompose_hint Decomp_CallNew (Hint_t (ValueHint callee)) in
      let arg_list =
        Base.Option.value
          arguments
          ~default:(fst callee, { Ast.Expression.ArgList.arguments = []; comments = None })
      in
      let call_reason = mk_expression_reason (loc, Ast.Expression.New expr) in
      this#visit_call_arguments ~call_reason ~call_argumemts_hint ~return_hint:hint arg_list targs

    method! member loc _ = fail loc "Should be visited by visit_member_expression"

    method private visit_member_expression ~cond ~hint loc mem =
      begin
        match EnvMap.find_opt_ordinary loc env_entries with
        | Some (Env_api.AssigningWrite reason) ->
          this#add_ordinary_binding
            loc
            reason
            (ExpressionDef
               { cond_context = cond; expr = (loc, Ast.Expression.Member mem); hint; chain = true }
            )
        | _ -> ()
      end;
      ignore @@ super#member loc mem

    method! optional_member loc _ = fail loc "Should be visited by visit_optional_member_expression"

    method private visit_optional_member_expression ~cond ~hint loc mem =
      begin
        match EnvMap.find_opt_ordinary loc env_entries with
        | Some (Env_api.AssigningWrite reason) ->
          this#add_ordinary_binding
            loc
            reason
            (ExpressionDef
               {
                 cond_context = cond;
                 expr = (loc, Ast.Expression.OptionalMember mem);
                 hint;
                 chain = true;
               }
            )
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

    method! unary_expression loc _ = fail loc "Should be visited by visit_unary_expression"

    method private visit_unary_expression ~hint expr =
      let open Flow_ast.Expression.Unary in
      let { argument; operator; comments = _ } = expr in
      let (hint, cond) =
        match operator with
        | Not -> (Hint_None, OtherConditionalTest)
        | Await -> (decompose_hint Decomp_Await hint, NonConditionalContext)
        | _ -> (Hint_None, NonConditionalContext)
      in
      this#visit_expression ~hint ~cond argument

    method! jsx_element loc_element expr =
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
          if name = "fbs" || name = "fbt" then
            Hint_None
          else if name = String.capitalize_ascii name then
            Hint_t
              (ValueHint (loc, Ast.Expression.Identifier (loc, { Ast.Identifier.name; comments })))
          else
            Hint_t (StringLiteralType name)
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
      let hint =
        let rec jsx_title_member_to_string (_, { Ast.JSX.MemberExpression._object; property; _ }) =
          let (_, { Ast.JSX.Identifier.name; comments = _ }) = property in
          match _object with
          | Ast.JSX.MemberExpression.MemberExpression member ->
            jsx_title_member_to_string member ^ "." ^ name
          | Ast.JSX.MemberExpression.Identifier (_, { Ast.JSX.Identifier.name = obj; comments = _ })
            ->
            obj ^ "." ^ name
        in
        let jsx_title_namespaced_name_to_string namespaced_name =
          let (_, { Ast.JSX.NamespacedName.namespace = (_, namespace); name = (_, name) }) =
            namespaced_name
          in
          namespace.Ast.JSX.Identifier.name ^ name.Ast.JSX.Identifier.name
        in
        let jsx_name =
          match opening_name with
          | Ast.JSX.Identifier (_, { Ast.JSX.Identifier.name; comments = _ }) -> name
          | Ast.JSX.MemberExpression member -> jsx_title_member_to_string member
          | Ast.JSX.NamespacedName namespace -> jsx_title_namespaced_name_to_string namespace
        in
        decompose_hint
          (Instantiate_Component
             {
               jsx_reason = mk_reason (RJSXElement (Some jsx_name)) loc_element;
               jsx_name;
               jsx_props = opening_attributes;
               jsx_children = children;
               (* TODO: thread hint *)
               jsx_hint = Hint_None;
             }
          )
          hint
      in
      let ref_hint = decompose_hint Decomp_JsxRef hint in
      let hint = decompose_hint Decomp_JsxProps hint in
      let hint =
        let checks = Eq_test.jsx_attributes_possible_sentinel_refinements opening_attributes in
        decompose_hint (Decomp_SentinelRefinement checks) hint
      in
      Base.List.iter opening_attributes ~f:(function
          | Opening.Attribute (_, { Attribute.name; value }) ->
            let hint =
              match name with
              | Ast.JSX.Attribute.Identifier (_, { Ast.JSX.Identifier.name = "ref"; comments = _ })
                ->
                ref_hint
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

    method! jsx_fragment _loc expr =
      let open Ast.JSX in
      let { frag_children; frag_opening_element = _; frag_closing_element = _; frag_comments = _ } =
        expr
      in
      let hint =
        decompose_hint
          (Decomp_ObjProp "children")
          (decompose_hint (Decomp_FuncParam 0) (Hint_t (BuiltinType "React$FragmentType")))
      in
      this#visit_jsx_children ~hint frag_children;
      expr

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
          this#record_hint loc hint;
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
      let hint =
        match EnvMap.find_opt (Env_api.ArrayProviderLoc, loc) env_entries with
        | Some (Env_api.AssigningWrite reason) ->
          this#add_binding
            (Env_api.ArrayProviderLoc, loc)
            reason
            (ExpressionDef { cond_context = cond; expr = exp; hint = Hint_None; chain = false });
          Hint_None
        | _ -> hint
      in
      let hint =
        if expression_is_definitely_synthesizable exp then
          Hint_None
        else
          hint
      in
      begin
        match EnvMap.find_opt (Env_api.ExpressionLoc, loc) env_entries with
        | Some (Env_api.AssigningWrite reason) ->
          this#add_binding
            (Env_api.ExpressionLoc, loc)
            reason
            (ExpressionDef { cond_context = cond; expr = exp; hint; chain = false })
        | _ -> ()
      end;
      this#record_hint loc hint;
      match expr with
      | Ast.Expression.Array expr -> this#visit_array_expression ~array_hint:hint expr
      | Ast.Expression.ArrowFunction x ->
        let scope_kind = func_scope_kind x in
        this#in_new_tparams_env (fun () -> this#visit_function ~func_hint:hint ~scope_kind x)
      | Ast.Expression.Assignment expr ->
        this#visit_assignment_expression ~is_function_statics_assignment:false loc expr
      | Ast.Expression.Function x ->
        this#visit_function_expr
          ~func_hint:hint
          ~has_this_def:true
          ~var_assigned_to:None
          ~statics:SMap.empty
          ~arrow:false
          loc
          x
      | Ast.Expression.Object expr -> this#visit_object_expression ~object_hint:hint expr
      | Ast.Expression.Member m -> this#visit_member_expression ~cond ~hint loc m
      | Ast.Expression.OptionalMember m -> this#visit_optional_member_expression ~cond ~hint loc m
      | Ast.Expression.Binary expr -> this#visit_binary_expression ~cond expr
      | Ast.Expression.Logical expr -> this#visit_logical_expression ~hint ~cond expr
      | Ast.Expression.Call expr ->
        let visit_callee = this#visit_expression ~cond:NonConditionalContext in
        this#visit_call_expression ~hint ~cond ~visit_callee loc expr
      | Ast.Expression.OptionalCall expr -> this#visit_optional_call_expression ~hint ~cond loc expr
      | Ast.Expression.New expr -> this#visit_new_expression ~hint loc expr
      | Ast.Expression.Unary expr -> this#visit_unary_expression ~hint expr
      | Ast.Expression.Conditional expr -> this#visit_conditional ~hint expr
      | Ast.Expression.Class _
      | Ast.Expression.Comprehension _
      | Ast.Expression.Generator _
      | Ast.Expression.Identifier _
      | Ast.Expression.Import _
      | Ast.Expression.JSXElement _
      | Ast.Expression.JSXFragment _
      | Ast.Expression.Literal _
      | Ast.Expression.MetaProperty _
      | Ast.Expression.Sequence _
      | Ast.Expression.Super _
      | Ast.Expression.TaggedTemplate _
      | Ast.Expression.TemplateLiteral _
      | Ast.Expression.This _
      | Ast.Expression.TypeCast _
      | Ast.Expression.Update _
      | Ast.Expression.Yield _ ->
        ignore @@ super#expression exp

    method! array loc _ = fail loc "Should be visited by visit_array_expression"

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

    method! conditional loc _ = fail loc "Should be visited by visit_conditional"

    method visit_conditional ~hint expr =
      let open Ast.Expression.Conditional in
      let { test; consequent; alternate; comments = _ } = expr in
      this#visit_expression ~hint:Hint_None ~cond:OtherConditionalTest test;
      this#visit_expression ~hint ~cond:NonConditionalContext consequent;
      this#visit_expression
        ~hint:(Hint_api.merge_hints hint (Hint_t (ValueHint consequent)))
        ~cond:NonConditionalContext
        alternate

    method! binary loc _ = fail loc "Should be visited by visit_binary_expression"

    method private visit_binary_expression ~cond expr =
      let open Ast.Expression.Binary in
      let { operator; left; right; comments = _ } = expr in
      match (operator, cond) with
      | (Instanceof, OtherConditionalTest) ->
        this#visit_expression ~hint:Hint_None ~cond left;
        ignore @@ this#expression right
      | ((Equal | NotEqual | StrictEqual | StrictNotEqual), OtherConditionalTest) ->
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
          ~is_switch_cond_context:false
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

    method! logical loc _ = fail loc "Should be visited by visit_logical_expression"

    method private visit_logical_expression ~hint ~cond expr =
      let open Ast.Expression.Logical in
      let { operator; left; right; comments = _ } = expr in
      let (left_cond, right_hint) =
        match operator with
        | And -> (OtherConditionalTest, hint)
        | Or -> (OtherConditionalTest, Hint_api.merge_hints hint (Hint_t (ValueHint left)))
        | NullishCoalesce -> (cond, Hint_api.merge_hints hint (Hint_t (ValueHint left)))
      in
      this#visit_expression ~hint ~cond:left_cond left;
      this#visit_expression ~hint:right_hint ~cond right

    method! object_ loc _ = fail loc "Should be visited by visit_object_expression"

    method private visit_object_expression ~object_hint expr =
      let open Ast.Expression.Object in
      let { properties; comments = _ } = expr in
      let object_hint =
        let checks = Eq_test.object_properties_possible_sentinel_refinements properties in
        decompose_hint (Decomp_SentinelRefinement checks) object_hint
      in
      let visit_object_key_and_compute_hint = function
        | Ast.Expression.Object.Property.Literal
            (_, { Ast.Literal.value = Ast.Literal.String name; _ }) ->
          decompose_hint (Decomp_ObjProp name) object_hint
        | Ast.Expression.Object.Property.Literal _ -> Hint_None
        | Ast.Expression.Object.Property.Identifier
            (_, { Ast.Identifier.name = "__proto__"; comments = _ }) ->
          Hint_None
        | Ast.Expression.Object.Property.Identifier (_, { Ast.Identifier.name; comments = _ }) ->
          decompose_hint (Decomp_ObjProp name) object_hint
        | Ast.Expression.Object.Property.PrivateName _ -> Hint_None (* Illegal syntax *)
        | Ast.Expression.Object.Property.Computed computed ->
          let (_, { Ast.ComputedKey.expression; comments = _ }) = computed in
          this#visit_expression ~hint:Hint_None ~cond:NonConditionalContext expression;
          decompose_hint (Decomp_ObjComputed (mk_expression_reason expression)) object_hint
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
            | (loc, Method { key; value = (_, fn) })
            | (loc, Get { key; value = (_, fn); comments = _ })
            | (loc, Set { key; value = (_, fn); comments = _ }) ->
              let func_hint = visit_object_key_and_compute_hint key in
              this#visit_function_expr
                ~func_hint
                ~has_this_def:false
                ~var_assigned_to:None
                ~statics:SMap.empty
                ~arrow:false
                loc
                fn;
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
