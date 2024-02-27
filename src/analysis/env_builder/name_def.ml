(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Hint
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
    record_identifier:(ALoc.t -> string -> binding -> 'a) ->
    record_destructuring_intermediate:(ALoc.t -> binding -> unit) ->
    visit_default_expression:(hints:ast_hints -> (ALoc.t, ALoc.t) Ast.Expression.t -> unit) ->
    join:('a -> 'a -> 'a) ->
    default:'a ->
    binding ->
    (ALoc.t, ALoc.t) Flow_ast.Pattern.t ->
    'a

  val pattern :
    record_identifier:(ALoc.t -> string -> binding -> unit) ->
    record_destructuring_intermediate:(ALoc.t -> binding -> unit) ->
    visit_default_expression:(hints:ast_hints -> (ALoc.t, ALoc.t) Ast.Expression.t -> unit) ->
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
    | Property.StringLiteral (loc, { Ast.StringLiteral.value = x; _ }) ->
      let acc = object_named_property (parent_loc, acc) loc x direct_default in
      (acc, x :: xs, false)
    | Property.Computed (_, { Ast.ComputedKey.expression; comments = _ }) ->
      let acc = object_computed_property (parent_loc, acc) expression direct_default in
      (acc, xs, true)
    | Property.NumberLiteral _ -> (acc, xs, false)
    | Property.BigIntLiteral _ -> (acc, xs, false)

  let identifier ~record_identifier acc (name_loc, { Ast.Identifier.name; _ }) =
    record_identifier name_loc name acc

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
    | (loc, Ast.Pattern.Identifier _) ->
      [Hint_t (WriteLocHint (Env_api.OrdinaryNameLoc, loc), ExpectedTypeHint)]
    | (loc, _) -> [Hint_t (WriteLocHint (Env_api.PatternLoc, loc), ExpectedTypeHint)]

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
            let hints = pattern_hint p in
            Base.Option.iter d ~f:(visit_default_expression ~hints);
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
        let hints = pattern_hint p in
        Base.Option.iter d ~f:(visit_default_expression ~hints);
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

let pattern_has_annot p = p |> Destructure.type_of_pattern |> Base.Option.is_some

let predicate_function_invalid_param_reasons params =
  let open Flow_ast in
  let open Reason in
  let (_, { Function.Params.params; rest; this_ = _; comments = _ }) = params in
  let reasons =
    List.filter_map
      (fun (_, param) ->
        let open Function.Param in
        match param.argument with
        | (ploc, Pattern.Object _)
        | (ploc, Pattern.Array _)
        | (ploc, Pattern.Expression _) ->
          Some (mk_reason RDestructuring ploc)
        | (_, Pattern.Identifier _) -> None)
      params
  in
  match rest with
  | Some (rloc, { Function.RestParam.argument; comments = _ }) ->
    let desc = Reason.code_desc_of_pattern argument in
    let reason = mk_reason (RRestParameter (Some desc)) rloc in
    reason :: reasons
  | None -> reasons

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
  | (Some _, Ast.Function.BodyExpression ((ret_loc, _) as expr))
  | (Some (ret_loc, { Ast.Type.Predicate.kind = Ast.Type.Predicate.Declared expr; comments = _ }), _)
    ->
    FunctionPredicateSynthesizable (ret_loc, expr)
  | _ -> (* Invalid predicate definition *) FunctionSynthesizable

let func_is_synthesizable_from_annotation
    ({ Ast.Function.predicate; return; generator; body; params; _ } as f) =
  match return with
  | Ast.Function.ReturnAnnot.Available _
  | Ast.Function.ReturnAnnot.TypeGuard _ ->
    if
      Base.Option.is_some predicate
      && Base.List.is_empty (predicate_function_invalid_param_reasons params)
    then
      predicate_synthesizable predicate body
    else
      FunctionSynthesizable
  | Ast.Function.ReturnAnnot.Missing loc ->
    if
      Base.Option.is_some predicate
      && Base.List.is_empty (predicate_function_invalid_param_reasons params)
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
    | Ast.Expression.StringLiteral _
    | Ast.Expression.NumberLiteral _
    | Ast.Expression.BooleanLiteral _
    | Ast.Expression.NullLiteral _
    | Ast.Expression.BigIntLiteral _
    | Ast.Expression.RegExpLiteral _
    | Ast.Expression.ModuleRefLiteral _
    | Ast.Expression.Identifier _
    | Ast.Expression.TypeCast _
    | Ast.Expression.AsExpression _
    | Ast.Expression.Member
        {
          Ast.Expression.Member._object =
            ( _,
              ( Ast.Expression.Identifier _ | Ast.Expression.TypeCast _
              | Ast.Expression.AsExpression _ )
            );
          property = Ast.Expression.Member.PropertyIdentifier _;
          _;
        } ->
      Ok (acc, this_write_locs)
    | Ast.Expression.ArrowFunction fn
    | Ast.Expression.Function fn ->
      handle_fun (Some elem_loc) this_write_locs acc (func_is_synthesizable_from_annotation fn)
    | Ast.Expression.Object obj -> begin
      match obj_properties_synthesizable ~this_write_locs:(obj_this_write_locs obj) obj with
      | ObjectSynthesizable { this_write_locs = new_this_write_locs } ->
        Ok (acc, EnvSet.union this_write_locs new_this_write_locs)
      | MissingMemberAnnots { locs = (hd, tl) } ->
        Ok (Base.List.append (hd :: tl) acc, this_write_locs)
      | Unsynthesizable -> Ok (OtherMissingAnnot elem_loc :: acc, this_write_locs)
    end
    | Ast.Expression.Array { Ast.Expression.Array.elements; _ } -> begin
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
    ~allow_unannotated_this
    ((_, { Ast.Function.Params.params = parameters; rest; this_; _ }) as params)
    body =
  Base.List.for_all parameters ~f:(fun (_, { Ast.Function.Param.argument; _ }) ->
      argument |> Destructure.type_of_pattern |> Base.Option.is_some
  )
  && Base.Option.value_map rest ~default:true ~f:(fun (_, { Ast.Function.RestParam.argument; _ }) ->
         argument |> Destructure.type_of_pattern |> Base.Option.is_some
     )
  && (allow_unannotated_this
     || Base.Option.is_some this_
     || not (Signature_utils.This_finder.found_this_in_body_or_params body params)
     )

let identifier_has_autocomplete ~autocomplete_hooks (loc, { Ast.Identifier.name; _ }) =
  autocomplete_hooks.Env_api.With_ALoc.id_hook name loc

let literal_has_autocomplete ~autocomplete_hooks loc =
  autocomplete_hooks.Env_api.With_ALoc.literal_hook loc

let expression_has_autocomplete ~autocomplete_hooks = function
  | (_, Ast.Expression.Identifier id) -> identifier_has_autocomplete ~autocomplete_hooks id
  | (loc, Ast.Expression.StringLiteral _) -> literal_has_autocomplete ~autocomplete_hooks loc
  | _ -> false

let expression_is_definitely_synthesizable ~autocomplete_hooks =
  let rec synthesizable (loc, expr) =
    let func_is_synthesizable ~allow_unannotated_this fn =
      let { Ast.Function.params; body; return; _ } = fn in
      if function_params_all_annotated ~allow_unannotated_this params body then (
        match (return, body) with
        | (Ast.Function.ReturnAnnot.Available _, _)
        | (Ast.Function.ReturnAnnot.TypeGuard _, _) ->
          true
        | (Ast.Function.ReturnAnnot.Missing _, Ast.Function.BodyExpression e) -> synthesizable e
        | (Ast.Function.ReturnAnnot.Missing _, Ast.Function.BodyBlock (loc, block)) ->
          let collector = new returned_expression_collector in
          ignore @@ collector#block loc block;
          collector#acc |> Base.List.for_all ~f:synthesizable
      ) else
        false
    in
    match expr with
    | Ast.Expression.ArrowFunction x -> func_is_synthesizable ~allow_unannotated_this:true x
    | Ast.Expression.Function x -> func_is_synthesizable ~allow_unannotated_this:false x
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
            | (_, Init { key; value; shorthand = _ }) ->
              (match key with
              | Property.Identifier (loc, { Ast.Identifier.name; _ })
              | Property.StringLiteral (loc, { Ast.StringLiteral.value = name; _ })
                when autocomplete_hooks.Env_api.With_ALoc.obj_prop_decl_hook name loc ->
                (* Autocompletion in LTI will use hints to find the expected type of the object
                   we are completing. There are similar case for identifiers and literals below. *)
                false
              | _ -> synthesizable value)
            | (_, Get { key = _; value = (_, fn); comments = _ })
            | (_, Set { key = _; value = (_, fn); comments = _ })
            | (_, Method { key = _; value = (_, fn) }) ->
              func_is_synthesizable ~allow_unannotated_this:true fn)
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
    | Ast.Expression.Call { Flow_ast.Expression.Call.targs; _ }
    | Ast.Expression.OptionalCall
        { Flow_ast.Expression.OptionalCall.call = { Flow_ast.Expression.Call.targs; _ }; _ }
    | Ast.Expression.New { Flow_ast.Expression.New.targs; _ } ->
      (match targs with
      | Some (_, { Flow_ast.Expression.CallTypeArgs.arguments; _ }) ->
        Base.List.for_all arguments ~f:(function
            | Flow_ast.Expression.CallTypeArg.Explicit _ -> true
            | Flow_ast.Expression.CallTypeArg.Implicit _ -> false
            )
      | None -> false)
    | Ast.Expression.JSXElement _ ->
      (* Implicit instantiation might happen in these nodes, and we might have underconstrained targs. *)
      false
    (* TaggedTemplates are function calls! They are not automatically synthesizable *)
    | Ast.Expression.TaggedTemplate _ -> false
    | Ast.Expression.Identifier id -> not (identifier_has_autocomplete ~autocomplete_hooks id)
    | Ast.Expression.StringLiteral _ -> not (literal_has_autocomplete ~autocomplete_hooks loc)
    | Ast.Expression.NumberLiteral _
    | Ast.Expression.BooleanLiteral _
    | Ast.Expression.NullLiteral _
    | Ast.Expression.RegExpLiteral _
    | Ast.Expression.BigIntLiteral _
    | Ast.Expression.ModuleRefLiteral _
    | Ast.Expression.Assignment _
    | Ast.Expression.Binary _
    | Ast.Expression.Class _
    | Ast.Expression.Import _
    | Ast.Expression.JSXFragment _
    | Ast.Expression.Member _
    | Ast.Expression.MetaProperty _
    | Ast.Expression.OptionalMember _
    | Ast.Expression.Sequence _
    | Ast.Expression.Super _
    | Ast.Expression.TemplateLiteral _
    | Ast.Expression.This _
    | Ast.Expression.TypeCast _
    | Ast.Expression.AsConstExpression _
    | Ast.Expression.AsExpression _
    | Ast.Expression.TSSatisfies _
    | Ast.Expression.Update _
    | Ast.Expression.Yield _ ->
      true
  in
  (fun e -> synthesizable e)

let def_of_function ~tparams_map ~hints ~has_this_def ~function_loc ~statics ~arrow function_ =
  Function
    {
      hints;
      synthesizable_from_annotation = func_is_synthesizable_from_annotation function_;
      arrow;
      has_this_def;
      function_loc;
      function_;
      tparams_map;
      statics;
    }

let def_of_component ~tparams_map ~component_loc component =
  Component { tparams_map; component_loc; component }

let func_scope_kind ?key { Ast.Function.async; generator; _ } =
  match (async, generator, key) with
  | ( false,
      false,
      Some
        (Ast.Expression.Object.Property.Identifier (_, { Ast.Identifier.name = "constructor"; _ }))
    ) ->
    Ctor
  | (true, true, _) -> AsyncGenerator
  | (true, false, _) -> Async
  | (false, true, _) -> Generator
  | (false, false, _) -> Ordinary

(* Existing own properties on `Function` as defined in `lib/core.js`. We don't
   want to shadow these when creating function statics. *)
let func_own_props = SSet.of_list ["toString"; "arguments"; "caller"; "length"; "name"]

module Eq_test = Eq_test.Make (Scope_api.With_ALoc) (Ssa_api.With_ALoc) (Env_api.With_ALoc)

class def_finder ~autocomplete_hooks env_info toplevel_scope =
  let fail loc str = raise Env_api.(Env_invariant (Some loc, ASTStructureOverride str)) in

  object (this)
    inherit
      [env_entries_map * hint_map, ALoc.t] Flow_ast_visitor.visitor
        ~init:(EnvMap.empty, ALocMap.empty) as super

    val mutable tparams : tparams_map = ALocMap.empty

    val mutable scope_kind : scope_kind = toplevel_scope

    val mutable class_stack : class_stack = []

    val mutable return_hint_stack : ast_hints list = []

    method add_tparam loc name = tparams <- ALocMap.add loc name tparams

    method record_hint loc hint =
      this#update_acc (fun (env_map, hint_map) -> (env_map, ALocMap.add loc hint hint_map))

    method force_add_binding kind_and_loc reason src =
      this#update_acc (fun (env_map, hint_map) ->
          (EnvMap.add kind_and_loc (src, scope_kind, class_stack, reason) env_map, hint_map)
      )

    method add_binding kind_and_loc reason src =
      if Env_api.has_assigning_write kind_and_loc env_info.Env_api.env_entries then
        this#force_add_binding kind_and_loc reason src

    method add_ordinary_binding loc = this#add_binding (Env_api.OrdinaryNameLoc, loc)

    method add_destructure_binding loc binding =
      this#add_binding (Env_api.PatternLoc, loc) (mk_reason RDestructuring loc) (Binding binding)

    method add_destructure_bindings root pattern =
      let record_identifier loc name binding =
        let binding = this#mk_hooklike_if_necessary (Flow_ast_utils.hook_name name) binding in
        this#add_ordinary_binding
          loc
          (mk_reason (RIdentifier (OrdinaryName name)) loc)
          (Binding binding)
      in
      Destructure.pattern
        ~record_identifier
        ~record_destructuring_intermediate:this#add_destructure_binding
        ~visit_default_expression:(this#visit_expression ~cond:NonConditionalContext)
        (Root root)
        pattern

    method mk_hooklike_if_necessary hooklike binding =
      if hooklike then begin
        Hooklike binding
      end else
        binding

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
      let stmts = Flow_ast_utils.hoist_function_and_component_declarations stmts in
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
                            | (fun_loc, (Ast.Expression.Function _ | Ast.Expression.ArrowFunction _))
                              ->
                              (Env_api.OrdinaryNameLoc, fun_loc)
                            | _ ->
                              this#add_ordinary_binding
                                member_loc
                                (mk_pattern_reason left)
                                (ExpressionDef
                                   {
                                     cond_context = NonConditionalContext;
                                     hints = [];
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
            if
              Env_api.has_assigning_write
                (Env_api.OrdinaryNameLoc, id_loc)
                env_info.Env_api.env_entries
            then
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
            if
              Env_api.has_assigning_write
                (Env_api.OrdinaryNameLoc, id_loc)
                env_info.Env_api.env_entries
            then
              let visit statics =
                this#visit_function_expr
                  ~func_hints:[]
                  ~has_this_def:(not arrow)
                  ~var_assigned_to:(Some var_id)
                  ~statics
                  ~arrow
                  ~hooklike:(Flow_ast_utils.hook_name name)
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
      let concrete =
        match (init, id) with
        | ( Some (arr_loc, Ast.Expression.Array { Ast.Expression.Array.elements = []; _ }),
            (_, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name = (name_loc, _); _ })
          ) ->
          let { Provider_api.array_providers; _ } =
            Base.Option.value_exn (Provider_api.providers_of_def env_info.Env_api.providers name_loc)
          in
          Some (EmptyArray { array_providers; arr_loc })
        | ( Some
              ( ( loc,
                  Ast.Expression.Object ({ Ast.Expression.Object.properties = _ :: _; _ } as obj)
                ) as init
              ),
            _
          ) ->
          let this_write_locs = obj_this_write_locs obj in
          begin
            match obj_properties_synthesizable ~this_write_locs obj with
            | Unsynthesizable -> Some (Value { hints = []; expr = init })
            | synthesizable -> Some (ObjectValue { obj_loc = loc; obj; synthesizable })
          end
        | (Some init, _) -> Some (Value { hints = []; expr = init })
        | (None, _) -> None
      in
      let (source, hints) =
        match Destructure.type_of_pattern id with
        | Some annot ->
          ( Some
              (Annotation
                 {
                   tparams_map = ALocMap.empty;
                   optional = false;
                   has_default_expression = false;
                   react_deep_read_only = None;
                   param_loc = None;
                   annot;
                   concrete;
                 }
              ),
            [Hint_t (AnnotationHint (ALocMap.empty, annot), ExpectedTypeHint)]
          )
        | None -> (concrete, [])
      in
      Base.Option.iter ~f:(fun acc -> this#add_destructure_bindings acc id) source;
      ignore @@ this#variable_declarator_pattern ~kind id;
      Base.Option.iter init ~f:(fun init ->
          this#visit_expression ~hints ~cond:NonConditionalContext init
      );
      decl

    method! declare_variable loc (decl : ('loc, 'loc) Ast.Statement.DeclareVariable.t) =
      let open Ast.Statement.DeclareVariable in
      let { id = (id_loc, { Ast.Identifier.name; _ }); annot; kind = _; comments = _ } = decl in
      let hook_like = Flow_ast_utils.hook_name name in
      this#add_ordinary_binding
        id_loc
        (mk_reason (RIdentifier (OrdinaryName name)) id_loc)
        (Binding
           (this#mk_hooklike_if_necessary
              hook_like
              (Root
                 (Annotation
                    {
                      tparams_map = ALocMap.empty;
                      optional = false;
                      has_default_expression = false;
                      react_deep_read_only = None;
                      param_loc = None;
                      annot;
                      concrete = None;
                    }
                 )
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

    method private visit_function_param ~hints ~is_hook (param : ('loc, 'loc) Ast.Function.Param.t)
        =
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
                 ~hints:[Hint_t (AnnotationHint (tparams, annot), ExpectedTypeHint)]
                 ~cond:NonConditionalContext
              );
          Annotation
            {
              tparams_map = tparams;
              optional;
              has_default_expression = Base.Option.is_some default_expression;
              react_deep_read_only =
                ( if is_hook then
                  Some Hook
                else
                  None
                );
              param_loc = Some param_loc;
              annot;
              concrete = None;
            }
        | None ->
          Base.Option.iter
            default_expression
            ~f:(this#visit_expression ~hints:[] ~cond:NonConditionalContext);
          let reason =
            match argument with
            | ( _,
                Ast.Pattern.Identifier
                  { Ast.Pattern.Identifier.name = (_, { Ast.Identifier.name; _ }); _ }
              ) ->
              mk_reason (RParameter (Some name)) param_loc
            | _ -> mk_reason RDestructuring param_loc
          in
          this#record_hint param_loc hints;
          Contextual { reason; hints; optional; default_expression }
      in
      let record_identifier loc name binding =
        let binding = this#mk_hooklike_if_necessary (Flow_ast_utils.hook_name name) binding in
        this#add_ordinary_binding
          loc
          (mk_reason (RIdentifier (OrdinaryName name)) loc)
          (Binding binding);
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
          (Binding (Root source));
      ignore @@ super#function_param (loc, { argument; default = None })

    method private visit_function_rest_param
        ~hints ~is_hook (expr : ('loc, 'loc) Ast.Function.RestParam.t) =
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
              react_deep_read_only =
                ( if is_hook then
                  Some Hook
                else
                  None
                );
              param_loc = Some param_loc;
              annot;
              concrete = None;
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
          this#record_hint param_loc hints;
          Contextual { reason; hints; optional = false; default_expression = None }
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
                   react_deep_read_only = None;
                   param_loc = None;
                   annot;
                   concrete = None;
                 }
              )
           )
        );
      super#function_this_param this_

    method! catch_clause_pattern pat =
      let source =
        match pat with
        | ( _,
            Ast.Pattern.Identifier
              {
                Ast.Pattern.Identifier.annot =
                  Ast.Type.Available (_, (_, (Ast.Type.Any _ | Ast.Type.Mixed _)));
                _;
              }
          ) ->
          (match Destructure.type_of_pattern pat with
          | Some annot ->
            Annotation
              {
                tparams_map = ALocMap.empty;
                optional = false;
                has_default_expression = false;
                react_deep_read_only = None;
                param_loc = None;
                annot;
                concrete = None;
              }
          | None -> CatchUnannotated)
        | _ -> CatchUnannotated
      in
      this#add_destructure_bindings source pat;
      super#catch_clause_pattern pat

    method! component_declaration loc stmt =
      this#visit_component_declaration loc stmt;
      stmt

    method visit_component_declaration loc stmt =
      let { Ast.Statement.ComponentDeclaration.id; sig_loc; params = _; body = _; _ } = stmt in
      this#in_new_tparams_env (fun () ->
          this#visit_component stmt;
          let (_, { Ast.Identifier.name; _ }) = id in
          let reason = mk_reason (RComponent (OrdinaryName name)) sig_loc in
          let def = def_of_component ~tparams_map:tparams ~component_loc:loc stmt in
          let (id_loc, _) = id in
          this#add_ordinary_binding id_loc reason def
      )

    method private visit_component stmt =
      this#in_scope
        (fun () ->
          let {
            Ast.Statement.ComponentDeclaration.id = _;
            params =
              ( _,
                {
                  Ast.Statement.ComponentDeclaration.Params.params = params_list;
                  rest;
                  comments = _;
                }
              );
            body;
            renders;
            tparams = component_tparams;
            sig_loc = _;
            comments = _;
          } =
            stmt
          in
          Base.Option.iter component_tparams ~f:(fun tparams -> ignore @@ this#type_params tparams);
          Base.List.iter ~f:this#visit_component_param params_list;
          Base.Option.iter ~f:this#visit_component_rest_param rest;
          ignore @@ this#component_renders_annotation renders;

          let renders_loc =
            match renders with
            | Ast.Type.AvailableRenders (loc, _)
            | Ast.Type.MissingRenders loc ->
              loc
          in
          let renders_hint =
            match renders with
            | Ast.Type.AvailableRenders (loc, renders) ->
              let annot = (loc, (loc, Ast.Type.Renders renders)) in
              [Hint_t (AnnotationHint (tparams, annot), ExpectedTypeHint)]
            | Ast.Type.MissingRenders _ -> [Hint_t (BuiltinType "React$Node", ExpectedTypeHint)]
          in
          this#record_hint renders_loc renders_hint;
          let old_stack = return_hint_stack in
          return_hint_stack <- renders_hint :: return_hint_stack;
          let (body_loc, block) = body in
          ignore @@ this#block body_loc block;
          return_hint_stack <- old_stack)
        ComponentBody
        ()

    method private visit_component_param
        (param : ('loc, 'loc) Ast.Statement.ComponentDeclaration.Param.t) =
      let open Ast.Statement.ComponentDeclaration.Param in
      let (loc, { local; default = default_expression; shorthand; name }) = param in
      let optional =
        match local with
        | (_, Ast.Pattern.Identifier { Ast.Pattern.Identifier.optional; _ }) -> optional
        | _ -> false
      in
      let (param_loc, _) = local in
      let annot = Destructure.type_of_pattern local in
      let source =
        match annot with
        | Some annot ->
          Base.Option.iter
            default_expression
            ~f:
              (this#visit_expression
                 ~hints:[Hint_t (AnnotationHint (tparams, annot), ExpectedTypeHint)]
                 ~cond:NonConditionalContext
              );
          Annotation
            {
              tparams_map = tparams;
              optional;
              has_default_expression = Base.Option.is_some default_expression;
              react_deep_read_only = Some Comp;
              param_loc = Some param_loc;
              annot;
              concrete = None;
            }
        | None ->
          Base.Option.iter
            default_expression
            ~f:(this#visit_expression ~hints:[] ~cond:NonConditionalContext);
          let reason =
            match local with
            | ( _,
                Ast.Pattern.Identifier
                  { Ast.Pattern.Identifier.name = (_, { Ast.Identifier.name; _ }); _ }
              ) ->
              mk_reason (RParameter (Some name)) param_loc
            | _ -> mk_reason RDestructuring param_loc
          in
          UnannotatedParameter reason
      in
      let record_identifier loc name binding =
        let binding = this#mk_hooklike_if_necessary (Flow_ast_utils.hook_name name) binding in
        this#add_ordinary_binding
          loc
          (mk_reason (RIdentifier (OrdinaryName name)) loc)
          (Binding binding);
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
              local
           )
        )
        && Base.Option.is_none annot
      then
        this#add_binding
          (Env_api.FunctionParamLoc, loc)
          (mk_reason RDestructuring loc)
          (Binding (Root source));
      ignore @@ super#component_param (loc, { local; default = None; shorthand; name })

    method private visit_component_rest_param
        (param : ('loc, 'loc) Ast.Statement.ComponentDeclaration.RestParam.t) =
      let open Ast.Statement.ComponentDeclaration.RestParam in
      let (loc, { argument; comments = _ }) = param in
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
          Annotation
            {
              tparams_map = tparams;
              optional;
              has_default_expression = false;
              react_deep_read_only = Some Comp;
              param_loc = Some param_loc;
              annot;
              concrete = None;
            }
        | None ->
          let reason =
            match argument with
            | ( _,
                Ast.Pattern.Identifier
                  { Ast.Pattern.Identifier.name = (_, { Ast.Identifier.name; _ }); _ }
              ) ->
              mk_reason (RParameter (Some name)) param_loc
            | _ -> mk_reason RDestructuring param_loc
          in
          UnannotatedParameter reason
      in
      let record_identifier loc name binding =
        let binding = this#mk_hooklike_if_necessary (Flow_ast_utils.hook_name name) binding in
        this#add_ordinary_binding
          loc
          (mk_reason (RIdentifier (OrdinaryName name)) loc)
          (Binding binding);
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
          (Binding (Root source));
      ignore @@ super#component_rest_param param

    method visit_function_expr
        ~func_hints ~has_this_def ~var_assigned_to ~statics ~arrow ~hooklike function_loc expr =
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
          this#visit_function ~scope_kind ~func_hints expr;
          (match var_assigned_to with
          | Some (name_loc, { Ast.Identifier.name; comments = _ }) ->
            let binding =
              Root
                (FunctionValue
                   {
                     hints = func_hints;
                     synthesizable_from_annotation = func_is_synthesizable_from_annotation expr;
                     function_loc;
                     function_ = expr;
                     statics;
                     arrow;
                     tparams_map = tparams;
                   }
                )
            in
            let binding = this#mk_hooklike_if_necessary hooklike binding in
            this#add_ordinary_binding
              name_loc
              (mk_reason (RIdentifier (OrdinaryName name)) name_loc)
              (Binding binding)
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
                ~hints:func_hints
                ~has_this_def
                ~arrow:false
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
        ~func_hints:[]
        ~has_this_def:true
        ~var_assigned_to:None
        ~statics:SMap.empty
        ~arrow:false
        ~hooklike:false
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
          this#visit_function ~func_hints:[] ~scope_kind expr;
          let reason = func_reason ~async ~generator sig_loc in
          let def =
            def_of_function
              ~tparams_map:tparams
              ~hints:[]
              ~has_this_def:true
              ~function_loc:loc
              ~arrow:false
              ~statics
              expr
          in
          match id with
          | Some (id_loc, _) -> this#add_ordinary_binding id_loc reason def
          | None -> this#add_ordinary_binding loc reason def
      )

    method! function_type loc ft =
      this#in_new_tparams_env ~keep:true (fun () -> super#function_type loc ft)

    method! object_mapped_type_property mt =
      this#in_new_tparams_env ~keep:true (fun () -> super#object_mapped_type_property mt)

    method! function_ _ expr =
      let scope_kind = func_scope_kind expr in
      this#in_new_tparams_env (fun () -> this#visit_function ~scope_kind ~func_hints:[] expr);
      expr

    method hint_pred_kind predicate return =
      match (predicate, return) with
      | (Some _, _) -> Some PredKind
      | ( _,
          Ast.Function.ReturnAnnot.TypeGuard
            (_, (_, { Ast.Type.TypeGuard.guard = ((name_loc, { Ast.Identifier.name; _ }), _); _ }))
        ) ->
        Some (TypeGuardKind (name_loc, name))
      | (_, _) -> None

    method private name_of_param param =
      let module P = Ast.Pattern in
      let (_, { Ast.Function.Param.argument; _ }) = param in
      match argument with
      | (_, P.Identifier { P.Identifier.name = (_, { Ast.Identifier.name; _ }); _ }) -> Some name
      | _ -> None

    method private params_list_to_str_opt params = Base.List.map params ~f:this#name_of_param

    method private visit_function ~scope_kind ~func_hints expr =
      this#in_scope
        (fun () ->
          let {
            Ast.Function.id = _;
            params =
              (_, { Ast.Function.Params.params = params_list; rest; comments = _; this_ }) as params;
            body;
            async;
            generator;
            hook = is_hook;
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
          let param_str_list = this#params_list_to_str_opt params_list in
          let pred = this#hint_pred_kind predicate return in
          Base.List.iteri
            ~f:(fun i ->
              this#visit_function_param
                ~is_hook
                ~hints:(decompose_hints (Decomp_FuncParam (param_str_list, i, pred)) func_hints))
            params_list;
          Base.Option.iter
            ~f:
              (this#visit_function_rest_param
                 ~is_hook
                 ~hints:(decompose_hints (Decomp_FuncRest (param_str_list, pred)) func_hints)
              )
            rest;
          ignore @@ this#function_return_annotation return;

          let return_loc =
            match return with
            | Ast.Function.ReturnAnnot.Available (loc, _)
            | Ast.Function.ReturnAnnot.TypeGuard (loc, _)
            | Ast.Function.ReturnAnnot.Missing loc ->
              loc
          in
          let return_hint =
            let base_hint =
              match return with
              | Ast.Function.ReturnAnnot.Available annot ->
                [Hint_t (AnnotationHint (tparams, annot), ExpectedTypeHint)]
              | Ast.Function.ReturnAnnot.TypeGuard _ -> []
              | Ast.Function.ReturnAnnot.Missing _ -> decompose_hints Decomp_FuncReturn func_hints
            in
            match scope_kind with
            | Async -> base_hint |> decompose_hints Decomp_Promise
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
              this#visit_expression ~hints:return_hint ~cond:NonConditionalContext expr;
              loc
          in
          return_hint_stack <- old_stack;

          begin
            if generator then
              let (loc, gen) =
                match return with
                | Ast.Function.ReturnAnnot.Missing loc
                | Ast.Function.ReturnAnnot.TypeGuard (loc, _) ->
                  (loc, None)
                | Ast.Function.ReturnAnnot.Available ((loc, _) as return_annot) ->
                  (loc, Some { tparams_map = tparams; return_annot; async })
              in

              this#add_ordinary_binding loc (mk_reason (RCustom "next") body_loc) (GeneratorNext gen)
          end;

          Base.Option.iter predicate ~f:(fun (_, { Ast.Type.Predicate.kind; comments = _ }) ->
              match kind with
              | Ast.Type.Predicate.Declared expr
                when Base.List.is_empty (predicate_function_invalid_param_reasons params) ->
                this#visit_expression ~hints:[] ~cond:NonConditionalContext expr
              | _ -> ()
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
          let () =
            this#in_scope
              (fun () ->
                Flow_ast_visitor.run_opt this#class_identifier id;
                Flow_ast_visitor.run_opt this#type_params class_tparams;
                let this_tparam_loc = Base.Option.value_map ~default:loc ~f:fst id in
                this#add_tparam this_tparam_loc "this";
                ignore @@ this#class_body body;
                Flow_ast_visitor.run_opt (Flow_ast_mapper.map_loc this#class_extends) extends;
                Flow_ast_visitor.run_opt this#class_implements implements;
                Flow_ast_visitor.run_list this#class_decorator class_decorators;
                ())
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
                (Class { class_loc = loc; class_ = expr; this_super_write_locs })
            | None ->
              let reason = mk_reason (RType (OrdinaryName "<<anonymous class>>")) loc in
              this#add_ordinary_binding
                loc
                reason
                (Class { class_loc = loc; class_ = expr; this_super_write_locs })
          end;
          expr
      )

    method! class_property _loc (prop : ('loc, 'loc) Ast.Class.Property.t') =
      let open Ast.Class.Property in
      let { key; value; annot; static = _; variance; decorators; comments = _ } = prop in
      let (_ : _ list) = map_list this#class_decorator decorators in
      ignore @@ this#object_key key;
      ignore @@ this#type_annotation_hint annot;
      let hints =
        match annot with
        | Ast.Type.Available annot ->
          [Hint_t (AnnotationHint (ALocMap.empty, annot), ExpectedTypeHint)]
        | Ast.Type.Missing _ -> []
      in
      this#visit_class_property_value ~hints value;
      ignore @@ this#variance_opt variance;
      prop

    method! class_private_field _loc (prop : ('loc, 'loc) Ast.Class.PrivateField.t') =
      let open Ast.Class.PrivateField in
      let { key; value; annot; static = _; variance; decorators; comments = _ } = prop in
      let (_ : _ list) = map_list this#class_decorator decorators in
      ignore @@ this#private_name key;
      ignore @@ this#type_annotation_hint annot;
      let hints =
        match annot with
        | Ast.Type.Available annot ->
          [Hint_t (AnnotationHint (ALocMap.empty, annot), ExpectedTypeHint)]
        | Ast.Type.Missing _ -> []
      in
      this#visit_class_property_value ~hints value;
      ignore @@ this#variance_opt variance;
      prop

    method private visit_class_property_value ~hints value =
      let open Ast.Class.Property in
      match value with
      | Declared -> ()
      | Uninitialized -> ()
      | Initialized x -> this#visit_expression ~cond:NonConditionalContext ~hints x

    method! class_method _loc (meth : ('loc, 'loc) Ast.Class.Method.t') =
      let open Ast.Class.Method in
      let { kind = _; key; value = (_, value); static = _; decorators; comments = _ } = meth in
      let _ = this#object_key key in
      let scope_kind = func_scope_kind ~key value in
      let () =
        this#in_new_tparams_env ~keep:true (fun () ->
            this#visit_function ~scope_kind ~func_hints:[] value
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
        let { id = (id_loc, { Ast.Identifier.name; _ }); annot; predicate = _; comments = _ } =
          decl
        in
        this#add_ordinary_binding
          id_loc
          (func_reason ~async:false ~generator:false loc)
          (Binding
             (this#mk_hooklike_if_necessary
                (Flow_ast_utils.hook_name name)
                (Root
                   (Annotation
                      {
                        tparams_map = ALocMap.empty;
                        optional = false;
                        has_default_expression = false;
                        react_deep_read_only = None;
                        param_loc = None;
                        annot;
                        concrete = None;
                      }
                   )
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

    method! declare_component loc (decl : ('loc, 'loc) Ast.Statement.DeclareComponent.t) =
      let open Ast.Statement.DeclareComponent in
      let { id = (id_loc, { Ast.Identifier.name; _ }); _ } = decl in
      this#add_ordinary_binding
        id_loc
        (mk_reason (RComponent (OrdinaryName name)) loc)
        (DeclaredComponent (loc, decl));
      super#declare_component loc decl

    method! assignment loc _ = fail loc "Should be visited by visit_assignment_expression"

    method private visit_assignment_expression ~is_function_statics_assignment loc expr =
      let open Ast.Expression.Assignment in
      let { operator; left = (lhs_loc, lhs_node) as left; right; comments = _ } = expr in
      let expression_pattern_hints = function
        | (_, Ast.Expression.Member { Ast.Expression.Member._object; property; comments = _ })
          when not is_function_statics_assignment ->
          (match property with
          | Ast.Expression.Member.PropertyIdentifier (_, { Ast.Identifier.name; comments = _ }) ->
            decompose_hints (Decomp_ObjProp name) [Hint_t (ValueHint _object, ExpectedTypeHint)]
          | Ast.Expression.Member.PropertyPrivateName (_, { Ast.PrivateName.name; _ }) ->
            decompose_hints
              (Decomp_PrivateProp (name, class_stack))
              [Hint_t (ValueHint _object, ExpectedTypeHint)]
          | Ast.Expression.Member.PropertyExpression expr ->
            decompose_hints
              (Decomp_ObjComputed (mk_expression_reason expr))
              [Hint_t (ValueHint _object, ExpectedTypeHint)])
        | (_, Ast.Expression.Member _) -> []
        | _ -> [Hint_t (AnyErrorHint (mk_reason RDestructuring lhs_loc), ExpectedTypeHint)]
      in
      let rec other_pattern_hint_opt = function
        | (_, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name = (id_loc, _); _ })
          when not @@ Env_api.Provider_api.is_provider env_info.Env_api.providers id_loc ->
          Env_api.Provider_api.providers_of_def env_info.Env_api.providers id_loc
          |> Base.Option.value_map ~f:(fun x -> x.Env_api.Provider_api.providers) ~default:[]
          |> Base.List.map ~f:(fun { Env_api.Provider_api.reason; _ } -> Reason.loc_of_reason reason)
          |> Nel.of_list
          |> Base.Option.map ~f:(fun providers -> ProvidersHint providers)
        | (_, Ast.Pattern.Identifier _) -> None
        | (_, Ast.Pattern.Expression (l, _)) ->
          (* Unsupported syntax like [foo.bar] = expr *)
          Some (AnyErrorHint (mk_reason (RCustom "unsupported syntax") l))
        | (loc, Ast.Pattern.Array { Ast.Pattern.Array.elements; _ }) ->
          Base.List.fold_until
            elements
            ~init:[]
            ~finish:(fun elements -> Some (ComposedArrayPatternHint (loc, List.rev elements)))
            ~f:
              (fun acc -> function
                | Ast.Pattern.Array.Element (_, { Ast.Pattern.Array.Element.argument; _ }) ->
                  other_pattern_hint_opt argument
                  |> Base.Option.value_map ~default:(Base.Continue_or_stop.Stop None) ~f:(fun h ->
                         Base.Continue_or_stop.Continue (ArrayElementPatternHint h :: acc)
                     )
                | Ast.Pattern.Array.RestElement (_, { Ast.Pattern.RestElement.argument; _ }) ->
                  other_pattern_hint_opt argument
                  |> Base.Option.value_map ~default:(Base.Continue_or_stop.Stop None) ~f:(fun h ->
                         Base.Continue_or_stop.Continue (ArrayRestElementPatternHint h :: acc)
                     )
                | Ast.Pattern.Array.Hole _ -> Base.Continue_or_stop.Stop None)
        | (loc, Ast.Pattern.Object { Ast.Pattern.Object.properties; _ }) ->
          Base.List.fold_until
            properties
            ~init:[]
            ~finish:(fun props -> Some (ComposedObjectPatternHint (loc, List.rev props)))
            ~f:
              (fun acc -> function
                | Ast.Pattern.Object.Property (_, { Ast.Pattern.Object.Property.key; pattern; _ })
                  ->
                  (match (key, other_pattern_hint_opt pattern) with
                  | (Ast.Pattern.Object.Property.Identifier (l, { Ast.Identifier.name; _ }), Some h)
                  | ( Ast.Pattern.Object.Property.StringLiteral
                        (l, { Ast.StringLiteral.value = name; _ }),
                      Some h
                    ) ->
                    Base.Continue_or_stop.Continue (ObjectPropPatternHint (name, l, h) :: acc)
                  | _ -> Base.Continue_or_stop.Stop None)
                | Ast.Pattern.Object.RestElement (_, { Ast.Pattern.RestElement.argument; _ }) ->
                  other_pattern_hint_opt argument
                  |> Base.Option.value_map ~default:(Base.Continue_or_stop.Stop None) ~f:(fun h ->
                         Base.Continue_or_stop.Continue (ObjectSpreadPropPatternHint h :: acc)
                     ))
      in
      let other_pattern_hints p =
        other_pattern_hint_opt p
        |> Base.Option.value_map ~default:[] ~f:(fun h -> [Hint_t (h, ExpectedTypeHint)])
      in
      match (operator, lhs_node) with
      | (None, Ast.Pattern.Expression ((member_loc, Ast.Expression.Member member) as e)) ->
        (* Use super member to visit sub-expressions to avoid record a read of the member. *)
        ignore @@ super#member member_loc member;
        this#add_ordinary_binding
          member_loc
          (mk_pattern_reason left)
          (MemberAssign { member_loc; member; rhs = right });
        this#visit_expression ~hints:(expression_pattern_hints e) ~cond:NonConditionalContext right
      | (None, Ast.Pattern.Expression e) ->
        let (_ : (_, _) Ast.Pattern.t) = this#assignment_pattern (lhs_loc, lhs_node) in
        this#add_destructure_bindings (Value { hints = []; expr = right }) left;
        this#visit_expression ~hints:(expression_pattern_hints e) ~cond:NonConditionalContext right
      | (None, _) ->
        let (_ : (_, _) Ast.Pattern.t) = this#assignment_pattern (lhs_loc, lhs_node) in
        this#add_destructure_bindings (Value { hints = []; expr = right }) left;
        this#visit_expression ~hints:(other_pattern_hints left) ~cond:NonConditionalContext right
      | ( Some operator,
          Ast.Pattern.Identifier
            { Ast.Pattern.Identifier.name = (id_loc, { Ast.Identifier.name; _ }); _ }
        ) ->
        this#add_ordinary_binding
          id_loc
          (mk_reason (RIdentifier (OrdinaryName name)) id_loc)
          (OpAssign { exp_loc = loc; lhs = left; op = operator; rhs = right });
        this#visit_expression ~hints:(other_pattern_hints left) ~cond:NonConditionalContext right
      | (Some operator, Ast.Pattern.Expression ((def_loc, _) as e)) ->
        (* In op_assign, the LHS will also be read. *)
        let cond =
          match operator with
          | AndAssign
          | OrAssign ->
            OtherConditionalTest
          | _ -> NonConditionalContext
        in
        this#visit_expression ~cond ~hints:[] e;
        this#add_ordinary_binding
          def_loc
          (mk_pattern_reason left)
          (OpAssign { exp_loc = loc; lhs = left; op = operator; rhs = right });
        this#visit_expression ~hints:(expression_pattern_hints e) ~cond:NonConditionalContext right
      | (Some _operator, (Ast.Pattern.Array _ | Ast.Pattern.Object _)) ->
        (* [a] += 1;
           ({b} += 1);
           will have invalid-lhs errors, we shouldn't visit the LHS pattern.
        *)
        this#visit_expression ~hints:(other_pattern_hints left) ~cond:NonConditionalContext right

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
            ~hints:(Base.Option.value ~default:[] (Base.List.hd return_hint_stack))
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
            let forof = For (Of { await }, right) in
            match Destructure.type_of_pattern id with
            | Some annot ->
              Annotation
                {
                  tparams_map = ALocMap.empty;
                  optional = false;
                  has_default_expression = false;
                  react_deep_read_only = None;
                  param_loc = None;
                  annot;
                  concrete = Some forof;
                }
            | None -> forof
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
            let forin = For (In, right) in
            match Destructure.type_of_pattern id with
            | Some annot ->
              Annotation
                {
                  tparams_map = ALocMap.empty;
                  optional = false;
                  has_default_expression = false;
                  react_deep_read_only = None;
                  param_loc = None;
                  annot;
                  concrete = Some forin;
                }
            | None -> forin
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
      Base.Option.iter test ~f:(this#visit_expression ~hints:[] ~cond:OtherConditionalTest);
      Base.Option.iter update ~f:(this#visit_expression ~hints:[] ~cond:OtherConditionalTest);
      ignore @@ this#statement body;
      stmt

    method! while_ _loc (stmt : ('loc, 'loc) Ast.Statement.While.t) =
      let open Ast.Statement.While in
      let { test; body; comments = _ } = stmt in
      this#visit_expression ~hints:[] ~cond:OtherConditionalTest test;
      ignore @@ this#statement body;
      stmt

    method! do_while _loc (stmt : ('loc, 'loc) Ast.Statement.DoWhile.t) =
      let open Ast.Statement.DoWhile in
      let { body; test; comments = _ } = stmt in
      ignore @@ this#statement body;
      this#visit_expression ~hints:[] ~cond:OtherConditionalTest test;
      stmt

    method! if_statement _ (stmt : ('loc, 'loc) Ast.Statement.If.t) =
      let open Ast.Statement.If in
      let { test; consequent; alternate; comments = _ } = stmt in
      this#visit_expression ~hints:[] ~cond:OtherConditionalTest test;
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

    method private visit_type_param ~from_infer_type (tparam : ('loc, 'loc) Ast.Type.TypeParam.t) =
      let open Ast.Type.TypeParam in
      let (_, { name = (name_loc, { Ast.Identifier.name; _ }); _ }) = tparam in
      this#force_add_binding
        (Env_api.OrdinaryNameLoc, name_loc)
        (mk_reason (RType (OrdinaryName name)) name_loc)
        (TypeParam { tparams_map = tparams; from_infer_type; tparam });
      this#add_tparam name_loc name;
      super#type_param tparam

    method! infer_type t =
      let open Ast.Type.Infer in
      let { tparam; comments } = t in
      { tparam = this#visit_type_param ~from_infer_type:true tparam; comments }

    method! type_param = this#visit_type_param ~from_infer_type:false

    method! interface loc (interface : ('loc, 'loc) Ast.Statement.Interface.t) =
      let open Ast.Statement.Interface in
      let { id = (name_loc, _); _ } = interface in
      this#add_ordinary_binding name_loc (mk_reason RInterfaceType loc) (Interface (loc, interface));
      this#in_new_tparams_env (fun () -> super#interface loc interface)

    method! declare_module loc (m : ('loc, 'loc) Ast.Statement.DeclareModule.t) =
      this#in_scope (super#declare_module loc) DeclareModule m

    method! declare_namespace loc (n : ('loc, 'loc) Ast.Statement.DeclareNamespace.t) =
      let {
        Ast.Statement.DeclareNamespace.id = (name_loc, { Ast.Identifier.name; comments = _ });
        _;
      } =
        n
      in
      this#add_ordinary_binding
        name_loc
        (mk_reason (RNamespace name) name_loc)
        (DeclaredNamespace (loc, n));
      this#in_scope (super#declare_namespace loc) DeclareNamespace n

    method! enum_declaration loc (enum : ('loc, 'loc) Ast.Statement.EnumDeclaration.t) =
      let open Ast.Statement.EnumDeclaration in
      let { id = (name_loc, { Ast.Identifier.name; _ }); body; _ } = enum in
      this#add_ordinary_binding name_loc (mk_reason (REnum name) name_loc) (Enum (loc, name, body));
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
            ~f:
              (fun {
                     kind;
                     local;
                     remote = (rem_id_loc, { Ast.Identifier.name = remote; _ });
                     remote_name_def_loc = _;
                   } ->
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
                     import = Named { kind; remote; local = name };
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
            (Import { import_kind; source; source_loc; import = Namespace name })
        | None -> ()
      end;
      Base.Option.iter
        ~f:
          (fun {
                 identifier = (id_loc, { Ast.Identifier.name; _ });
                 remote_default_name_def_loc = _;
               } ->
          let import_reason = mk_reason (RDefaultImportedType (name, source)) id_loc in
          this#add_ordinary_binding
            id_loc
            import_reason
            (Import { import_kind; source; source_loc; import = Default name }))
        default;
      super#import_declaration loc decl

    method! call loc _ = fail loc "Should be visited by visit_call_expression"

    method private visit_call_expression ~hints ~cond ~visit_callee loc expr =
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
      let callee_hints =
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
          decompose_hints Comp_ImmediateFuncCall hints
        | _ -> []
      in
      visit_callee ~hints:callee_hints callee;
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
            this#visit_expression ~hints:[] ~cond expr
          | Ast.Expression.Spread (_, spread) ->
            this#visit_expression
              ~hints:[]
              ~cond:NonConditionalContext
              spread.Ast.Expression.SpreadElement.argument
        )
      else
        match arguments with
        | [Ast.Expression.Expression expr] when Flow_ast_utils.is_call_to_is_array callee ->
          this#visit_expression ~hints:[] ~cond expr
        | [Ast.Expression.Expression expr] when Flow_ast_utils.is_call_to_object_dot_freeze callee
          ->
          this#visit_expression ~hints ~cond:NonConditionalContext expr
        | _ when Flow_ast_utils.is_call_to_object_static_method callee ->
          Base.List.iter arguments ~f:(fun arg ->
              match arg with
              | Ast.Expression.Expression expr ->
                this#visit_expression ~hints:[] ~cond:NonConditionalContext expr
              | Ast.Expression.Spread (_, spread) ->
                this#visit_expression
                  ~hints:[]
                  ~cond:NonConditionalContext
                  spread.Ast.Expression.SpreadElement.argument
          )
        | _ ->
          let call_argumemts_hints =
            match callee with
            | (loc, Ast.Expression.Member { Ast.Expression.Member._object; property; comments = _ })
            | ( loc,
                Ast.Expression.OptionalMember
                  {
                    Ast.Expression.OptionalMember.member =
                      { Ast.Expression.Member._object; property; comments = _ };
                    filtered_out = _;
                    optional = _;
                  }
              )
              when (* Use the type of the callee directly as hint if the member access is refined *)
                   not (Loc_sig.ALocS.LMap.mem loc env_info.Env_api.env_values) ->
              let base_hint = [Hint_t (ValueHint _object, ExpectedTypeHint)] in
              (match property with
              | Ast.Expression.Member.PropertyIdentifier (_, { Ast.Identifier.name; comments = _ })
                ->
                decompose_hints (Decomp_MethodName name) base_hint
              | Ast.Expression.Member.PropertyPrivateName (_, { Ast.PrivateName.name; comments = _ })
                ->
                decompose_hints (Decomp_MethodPrivateName (name, class_stack)) base_hint
              | Ast.Expression.Member.PropertyExpression _ ->
                decompose_hints Decomp_MethodElem base_hint)
            | (_, Ast.Expression.Super _) ->
              decompose_hints Decomp_CallSuper [Hint_t (ValueHint callee, ExpectedTypeHint)]
            | _ -> [Hint_t (ValueHint callee, ExpectedTypeHint)]
          in
          let call_reason = mk_expression_reason (loc, Ast.Expression.Call expr) in
          this#visit_call_arguments
            ~call_reason
            ~call_argumemts_hints
            ~return_hints:hints
            arg_list
            targs

    method private visit_call_arguments
        ~call_reason
        ~call_argumemts_hints
        ~return_hints
        ((_, { Ast.Expression.ArgList.arguments; comments = _ }) as arg_list)
        targs =
      let call_argumemts_hints =
        decompose_hints (Simplify_Callee call_reason) call_argumemts_hints
      in
      let param_str_list = Base.List.init (List.length arguments) ~f:(fun _ -> None) in
      Base.List.iteri arguments ~f:(fun i arg ->
          match arg with
          | Ast.Expression.Expression expr ->
            let hints =
              call_argumemts_hints
              |> decompose_hints
                   (Instantiate_Callee
                      {
                        Hint.reason = call_reason;
                        return_hints = lazy return_hints;
                        targs = lazy targs;
                        arg_list = lazy arg_list;
                        arg_index = i;
                      }
                   )
              |> decompose_hints (Decomp_FuncParam (param_str_list, i, None))
            in
            this#visit_expression ~hints ~cond:NonConditionalContext expr
          | Ast.Expression.Spread (_, spread) ->
            this#visit_expression
              ~hints:[]
              ~cond:NonConditionalContext
              spread.Ast.Expression.SpreadElement.argument
      )

    method! optional_call loc _ = fail loc "Should be visited by visit_optional_call_expression"

    method private visit_optional_call_expression ~hints ~cond loc expr =
      let open Ast.Expression.OptionalCall in
      let { call; optional = _; filtered_out = _ } = expr in
      this#visit_call_expression
        loc
        call
        ~hints
        ~cond
        ~visit_callee:(this#visit_expression ~cond:NonConditionalContext)

    method! new_ loc _ = fail loc "Should be visited by visit_new_expression"

    method visit_new_expression ~hints loc expr =
      let { Ast.Expression.New.callee; targs; arguments; comments = _ } = expr in
      this#visit_expression ~hints:[] ~cond:NonConditionalContext callee;
      Base.Option.iter targs ~f:(fun targs -> ignore @@ this#call_type_args targs);
      let call_argumemts_hints =
        decompose_hints Decomp_CallNew [Hint_t (ValueHint callee, ExpectedTypeHint)]
      in
      let arg_list =
        Base.Option.value
          arguments
          ~default:(fst callee, { Ast.Expression.ArgList.arguments = []; comments = None })
      in
      let call_reason = mk_expression_reason (loc, Ast.Expression.New expr) in
      this#visit_call_arguments
        ~call_reason
        ~call_argumemts_hints
        ~return_hints:hints
        arg_list
        targs

    method! member loc _ = fail loc "Should be visited by visit_member_expression"

    method private visit_member_expression ~cond ~hints loc mem =
      begin
        match EnvMap.find_opt_ordinary loc env_info.Env_api.env_entries with
        | Some (Env_api.AssigningWrite reason) ->
          this#add_ordinary_binding
            loc
            reason
            (ExpressionDef
               { cond_context = cond; expr = (loc, Ast.Expression.Member mem); hints; chain = true }
            )
        | _ -> ()
      end;
      ignore @@ super#member loc mem

    method! optional_member loc _ = fail loc "Should be visited by visit_optional_member_expression"

    method private visit_optional_member_expression ~cond ~hints loc mem =
      begin
        match EnvMap.find_opt_ordinary loc env_info.Env_api.env_entries with
        | Some (Env_api.AssigningWrite reason) ->
          this#add_ordinary_binding
            loc
            reason
            (ExpressionDef
               {
                 cond_context = cond;
                 expr = (loc, Ast.Expression.OptionalMember mem);
                 hints;
                 chain = true;
               }
            )
        | _ -> ()
      end;
      let open Ast.Expression.OptionalMember in
      let { member; optional = _; filtered_out = _ } = mem in
      ignore @@ super#member loc member

    method private cast annot expression =
      this#visit_expression
        ~hints:[Hint_t (AnnotationHint (ALocMap.empty, annot), ExpectedTypeHint)]
        ~cond:NonConditionalContext
        expression;
      ignore @@ this#type_annotation annot

    method! type_cast _ expr =
      let { Ast.Expression.TypeCast.annot; expression; comments = _ } = expr in
      this#cast annot expression;
      expr

    method! as_expression _ expr =
      let { Ast.Expression.AsExpression.annot; expression; comments = _ } = expr in
      this#cast annot expression;
      expr

    method! unary_expression loc _ = fail loc "Should be visited by visit_unary_expression"

    method private visit_unary_expression ~hints expr =
      let open Flow_ast.Expression.Unary in
      let { argument; operator; comments = _ } = expr in
      let (hints, cond) =
        match operator with
        | Not -> ([], OtherConditionalTest)
        | Await -> (decompose_hints Decomp_Await hints, NonConditionalContext)
        | _ -> ([], NonConditionalContext)
      in
      this#visit_expression ~hints ~cond argument

    method! jsx_element loc_element expr =
      let open Ast.JSX in
      let {
        opening_element =
          ( _,
            {
              Opening.name = opening_name;
              targs = _;
              self_closing = _;
              attributes = opening_attributes;
            }
          );
        closing_element = _;
        children;
        comments = _;
      } =
        expr
      in
      let hints =
        match opening_name with
        | Ast.JSX.Identifier (loc, { Ast.JSX.Identifier.name; comments }) ->
          if name = "fbs" || name = "fbt" then
            []
          else if name = String.capitalize_ascii name then
            [
              Hint_t
                ( ValueHint (loc, Ast.Expression.Identifier (loc, { Ast.Identifier.name; comments })),
                  ExpectedTypeHint
                );
            ]
          else
            [Hint_t (StringLiteralType name, ExpectedTypeHint)]
        | Ast.JSX.NamespacedName _ -> []
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
          [Hint_t (ValueHint (jsx_title_member_to_expression member), ExpectedTypeHint)]
      in
      let hints =
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
        decompose_hints
          (Instantiate_Component
             {
               jsx_reason = mk_reason (RJSXElement (Some jsx_name)) loc_element;
               jsx_name;
               jsx_props = opening_attributes;
               jsx_children = children;
               (* TODO: thread hint *)
               jsx_hints = lazy [];
             }
          )
          hints
      in
      let ref_hints = decompose_hints Decomp_JsxRef hints in
      let hints = decompose_hints Decomp_JsxProps hints in
      let hints =
        let has_autocomplete =
          Base.List.exists opening_attributes ~f:(function
              | Opening.Attribute (_, { Attribute.name = _; value }) ->
                Base.Option.value_map value ~default:false ~f:(fun value ->
                    match value with
                    | Attribute.StringLiteral (loc, _) ->
                      literal_has_autocomplete ~autocomplete_hooks loc
                    | Attribute.ExpressionContainer
                        (_, { Ast.JSX.ExpressionContainer.expression; comments = _ }) ->
                      (match expression with
                      | Ast.JSX.ExpressionContainer.EmptyExpression -> false
                      | Ast.JSX.ExpressionContainer.Expression e ->
                        expression_has_autocomplete ~autocomplete_hooks e)
                )
              | Opening.SpreadAttribute (_, { SpreadAttribute.argument; comments = _ }) ->
                expression_has_autocomplete ~autocomplete_hooks argument
              )
        in
        if has_autocomplete then
          (* During autocomplete, we are working with ASTs with placeholder values,
             which can make sentinel refinements refine to empty. In these cases, it's better to
             have a coarser set of results instead of nothing. *)
          hints
        else
          let checks = Eq_test.jsx_attributes_possible_sentinel_refinements opening_attributes in
          decompose_hints (Decomp_SentinelRefinement checks) hints
      in
      Base.List.iter opening_attributes ~f:(function
          | Opening.Attribute (_, { Attribute.name; value }) ->
            let (hints, name_loc) =
              match name with
              | Ast.JSX.Attribute.Identifier (loc, { Ast.JSX.Identifier.name = "ref"; comments = _ })
                ->
                (ref_hints, Some loc)
              | Ast.JSX.Attribute.Identifier (loc, { Ast.JSX.Identifier.name; comments = _ }) ->
                (decompose_hints (Decomp_ObjProp name) hints, Some loc)
              | Ast.JSX.Attribute.NamespacedName _ -> ([], None)
            in
            Base.Option.iter name_loc ~f:(fun name_loc -> this#record_hint name_loc hints);
            Base.Option.iter value ~f:(fun value ->
                match value with
                | Attribute.StringLiteral (loc, _) -> this#record_hint loc hints
                | Attribute.ExpressionContainer (_, expr) -> this#visit_jsx_expression ~hints expr
            )
          | Opening.SpreadAttribute (_, { SpreadAttribute.argument; comments = _ }) ->
            this#visit_expression
              ~hints:(decompose_hints Decomp_ObjSpread hints)
              ~cond:NonConditionalContext
              argument
          );
      this#visit_jsx_children ~hints:(decompose_hints (Decomp_ObjProp "children") hints) children;
      expr

    method private visit_jsx_expression ~hints expr =
      let open Ast.JSX.ExpressionContainer in
      let { expression; comments = _ } = expr in
      match expression with
      | Expression expr -> this#visit_expression ~hints ~cond:NonConditionalContext expr
      | EmptyExpression -> ()

    method! jsx_fragment _loc expr =
      let open Ast.JSX in
      let { frag_children; frag_opening_element = _; frag_closing_element = _; frag_comments = _ } =
        expr
      in
      let hints =
        decompose_hints
          (Decomp_ObjProp "children")
          (decompose_hints
             (Decomp_FuncParam ([None], 0, None))
             [Hint_t (BuiltinType "React$FragmentType", ExpectedTypeHint)]
          )
      in
      this#visit_jsx_children ~hints frag_children;
      expr

    method private visit_jsx_children ~hints (_, children) =
      let children =
        Base.List.filter children ~f:(function
            | (loc, Ast.JSX.Text { Ast.JSX.Text.value; _ }) ->
              Base.Option.is_some (Utils_jsx.trim_jsx_text (ALoc.to_loc_exn loc) value)
            | _ -> true
            )
      in
      let single_child =
        match children with
        | [_] -> true
        | _ -> false
      in
      Base.List.iteri children ~f:(fun i (loc, child) ->
          let hints =
            if single_child then
              hints
            else
              decompose_hints (Decomp_ArrElement (Some i)) hints
          in
          this#record_hint loc hints;
          match child with
          | Ast.JSX.Element elem -> ignore @@ this#jsx_element loc elem
          | Ast.JSX.Fragment frag -> ignore @@ this#jsx_fragment loc frag
          | Ast.JSX.ExpressionContainer expr -> this#visit_jsx_expression ~hints expr
          | Ast.JSX.SpreadChild _ -> () (* Unsupported syntax *)
          | Ast.JSX.Text _ -> ()
      )

    method! expression expr =
      this#visit_expression ~hints:[] ~cond:NonConditionalContext expr;
      expr

    method private visit_expression ~hints ~cond ((loc, expr) as exp) =
      let hints =
        match EnvMap.find_opt (Env_api.ArrayProviderLoc, loc) env_info.Env_api.env_entries with
        | Some (Env_api.AssigningWrite reason) ->
          this#add_binding
            (Env_api.ArrayProviderLoc, loc)
            reason
            (ExpressionDef { cond_context = cond; expr = exp; hints = []; chain = false });
          []
        | _ -> hints
      in
      let hints =
        if expression_is_definitely_synthesizable ~autocomplete_hooks exp then
          []
        else
          hints
      in
      begin
        match EnvMap.find_opt (Env_api.ExpressionLoc, loc) env_info.Env_api.env_entries with
        | Some (Env_api.AssigningWrite reason) ->
          this#add_binding
            (Env_api.ExpressionLoc, loc)
            reason
            (ExpressionDef { cond_context = cond; expr = exp; hints; chain = false })
        | _ -> ()
      end;
      this#record_hint loc hints;
      match expr with
      | Ast.Expression.Array expr -> this#visit_array_expression ~array_hints:hints loc expr
      | Ast.Expression.ArrowFunction x ->
        let scope_kind = func_scope_kind x in
        this#in_new_tparams_env (fun () ->
            this#visit_function ~func_hints:hints ~scope_kind x;
            match EnvMap.find_opt_ordinary loc env_info.Env_api.env_entries with
            | Some (Env_api.AssigningWrite reason) ->
              let def =
                def_of_function
                  ~tparams_map:tparams
                  ~hints
                  ~has_this_def:false
                  ~arrow:true
                  ~function_loc:loc
                  ~statics:SMap.empty
                  x
              in
              this#add_ordinary_binding loc reason def
            | _ -> ()
        )
      | Ast.Expression.Assignment expr ->
        this#visit_assignment_expression ~is_function_statics_assignment:false loc expr
      | Ast.Expression.Function x ->
        this#visit_function_expr
          ~func_hints:hints
          ~has_this_def:true
          ~var_assigned_to:None
          ~statics:SMap.empty
          ~arrow:false
          ~hooklike:false
          loc
          x
      | Ast.Expression.Object expr -> this#visit_object_expression ~object_hints:hints expr
      | Ast.Expression.Member m -> this#visit_member_expression ~cond ~hints loc m
      | Ast.Expression.OptionalMember m -> this#visit_optional_member_expression ~cond ~hints loc m
      | Ast.Expression.Binary expr -> this#visit_binary_expression ~cond expr
      | Ast.Expression.Logical expr -> this#visit_logical_expression ~hints ~cond expr
      | Ast.Expression.Call expr ->
        let visit_callee = this#visit_expression ~cond:NonConditionalContext in
        this#visit_call_expression ~hints ~cond ~visit_callee loc expr
      | Ast.Expression.OptionalCall expr ->
        this#visit_optional_call_expression ~hints ~cond loc expr
      | Ast.Expression.New expr -> this#visit_new_expression ~hints loc expr
      | Ast.Expression.Unary expr -> this#visit_unary_expression ~hints expr
      | Ast.Expression.Conditional expr -> this#visit_conditional ~hints expr
      | Ast.Expression.Class _
      | Ast.Expression.Identifier _
      | Ast.Expression.Import _
      | Ast.Expression.JSXElement _
      | Ast.Expression.JSXFragment _
      | Ast.Expression.StringLiteral _
      | Ast.Expression.NumberLiteral _
      | Ast.Expression.BooleanLiteral _
      | Ast.Expression.NullLiteral _
      | Ast.Expression.RegExpLiteral _
      | Ast.Expression.BigIntLiteral _
      | Ast.Expression.ModuleRefLiteral _
      | Ast.Expression.MetaProperty _
      | Ast.Expression.Sequence _
      | Ast.Expression.Super _
      | Ast.Expression.TaggedTemplate _
      | Ast.Expression.TemplateLiteral _
      | Ast.Expression.This _
      | Ast.Expression.TypeCast _
      | Ast.Expression.AsExpression _
      | Ast.Expression.AsConstExpression _
      | Ast.Expression.TSSatisfies _
      | Ast.Expression.Update _
      | Ast.Expression.Yield _ ->
        ignore @@ super#expression exp

    method! array loc _ = fail loc "Should be visited by visit_array_expression"

    method private visit_array_expression ~array_hints loc expr =
      let { Ast.Expression.Array.elements; comments = _ } = expr in
      let (_ : bool) =
        Base.List.foldi ~init:false elements ~f:(fun i seen_spread element ->
            let mk_hints decomp =
              if seen_spread then
                []
              else
                decompose_hints decomp array_hints
            in
            match element with
            | Ast.Expression.Array.Expression expr ->
              let index =
                if seen_spread then
                  None
                else
                  Some i
              in
              this#visit_expression
                ~hints:(mk_hints (Decomp_ArrElement index))
                ~cond:NonConditionalContext
                expr;
              seen_spread
            | Ast.Expression.Array.Spread (_, spread) ->
              let hints =
                if seen_spread then
                  []
                else
                  decompose_hints (Decomp_ArrSpread i) array_hints
              in
              this#visit_expression
                ~hints
                ~cond:NonConditionalContext
                spread.Ast.Expression.SpreadElement.argument;
              true
            | Ast.Expression.Array.Hole _ -> seen_spread
        )
      in
      if Base.List.is_empty elements then
        (* We overwrite the hint on empty array with the hint on empty array element.
           Since only the hint on empty array element will be read, this is safe. *)
        this#record_hint loc (decompose_hints Decomp_EmptyArrayElement array_hints)

    method! conditional loc _ = fail loc "Should be visited by visit_conditional"

    method visit_conditional ~hints expr =
      let open Ast.Expression.Conditional in
      let { test; consequent; alternate; comments = _ } = expr in
      this#visit_expression ~hints:[] ~cond:OtherConditionalTest test;
      this#visit_expression ~hints ~cond:NonConditionalContext consequent;
      this#visit_expression
        ~hints:(Base.List.append hints [Hint_t (ValueHint consequent, BestEffortHint)])
        ~cond:NonConditionalContext
        alternate

    method! binary loc _ = fail loc "Should be visited by visit_binary_expression"

    method private visit_binary_expression ~cond expr =
      let open Ast.Expression.Binary in
      let { operator; left; right; comments = _ } = expr in
      match (operator, cond) with
      | (Instanceof, OtherConditionalTest) ->
        this#visit_expression ~hints:[] ~cond left;
        ignore @@ this#expression right
      | ((Equal | NotEqual | StrictEqual | StrictNotEqual), OtherConditionalTest) ->
        Eq_test.visit_eq_test
          ~on_type_of_test:(fun _ expr value _ _ ->
            this#visit_expression ~hints:[] ~cond expr;
            ignore @@ this#expression value)
          ~on_literal_test:(fun ~strict:_ ~sense:_ _ expr _ value ->
            this#visit_expression ~hints:[] ~cond expr;
            ignore @@ this#expression value)
          ~on_null_test:(fun ~sense:_ ~strict:_ _ expr value ->
            this#visit_expression ~hints:[] ~cond expr;
            ignore @@ this#expression value)
          ~on_void_test:(fun ~sense:_ ~strict:_ ~check_for_bound_undefined:_ _ expr value ->
            this#visit_expression ~hints:[] ~cond expr;
            ignore @@ this#expression value)
          ~on_member_eq_other:(fun expr value ->
            this#visit_expression ~hints:[] ~cond expr;
            ignore @@ this#expression value)
          ~on_other_eq_member:(fun value expr ->
            this#visit_expression ~hints:[] ~cond expr;
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

    method private visit_logical_expression ~hints ~cond expr =
      let open Ast.Expression.Logical in
      let { operator; left; right; comments = _ } = expr in
      let (left_cond, left_hints, right_hints) =
        match operator with
        | And -> (OtherConditionalTest, hints, hints)
        | Or ->
          ( OtherConditionalTest,
            decompose_hints Comp_MaybeT hints,
            Base.List.append hints [Hint_t (ValueHint left, BestEffortHint)]
          )
        | NullishCoalesce ->
          ( cond,
            decompose_hints Comp_MaybeT hints,
            Base.List.append hints [Hint_t (ValueHint left, BestEffortHint)]
          )
      in
      this#visit_expression ~hints:left_hints ~cond:left_cond left;
      this#visit_expression ~hints:right_hints ~cond right

    method! object_ loc _ = fail loc "Should be visited by visit_object_expression"

    method private visit_object_expression ~object_hints expr =
      let open Ast.Expression.Object in
      let { properties; comments = _ } = expr in
      let object_hints =
        let has_autocomplete =
          Base.List.exists properties ~f:(function
              | Property p ->
                let open Ast.Expression.Object.Property in
                (match p with
                | ( _,
                    Init
                      {
                        key =
                          ( Property.Identifier (loc, { Ast.Identifier.name; _ })
                          | Property.StringLiteral (loc, { Ast.StringLiteral.value = name; _ }) );
                        _;
                      }
                  )
                  when autocomplete_hooks.Env_api.With_ALoc.obj_prop_decl_hook name loc ->
                  true
                | (_, Init { value; _ }) -> expression_has_autocomplete ~autocomplete_hooks value
                | _ -> false)
              | SpreadProperty _ -> false
              )
        in
        if has_autocomplete then
          (* During autocomplete, we are working with ASTs with placeholder values,
             which can make sentinel refinements refine to empty. In these cases, it's better to
             have a coarser set of results instead of nothing. *)
          object_hints
        else
          let checks = Eq_test.object_properties_possible_sentinel_refinements properties in
          decompose_hints (Decomp_SentinelRefinement checks) object_hints
      in
      let visit_object_key_and_compute_hint = function
        | Ast.Expression.Object.Property.StringLiteral (_, { Ast.StringLiteral.value = name; _ }) ->
          decompose_hints (Decomp_ObjProp name) object_hints
        | Ast.Expression.Object.Property.NumberLiteral _
        | Ast.Expression.Object.Property.BigIntLiteral _ ->
          []
        | Ast.Expression.Object.Property.Identifier
            (_, { Ast.Identifier.name = "__proto__"; comments = _ }) ->
          []
        | Ast.Expression.Object.Property.Identifier (_, { Ast.Identifier.name; comments = _ }) ->
          decompose_hints (Decomp_ObjProp name) object_hints
        | Ast.Expression.Object.Property.PrivateName _ -> [] (* Illegal syntax *)
        | Ast.Expression.Object.Property.Computed computed ->
          let (_, { Ast.ComputedKey.expression; comments = _ }) = computed in
          this#visit_expression ~hints:[] ~cond:NonConditionalContext expression;
          decompose_hints (Decomp_ObjComputed (mk_expression_reason expression)) object_hints
      in
      Base.List.iter properties ~f:(fun prop ->
          match prop with
          | Property p ->
            let open Ast.Expression.Object.Property in
            (match p with
            | (_, Init { key; value; shorthand = _ }) ->
              let hints = visit_object_key_and_compute_hint key in
              this#visit_expression ~hints ~cond:NonConditionalContext value;
              ()
            | (loc, Method { key; value = (_, fn) })
            | (loc, Get { key; value = (_, fn); comments = _ })
            | (loc, Set { key; value = (_, fn); comments = _ }) ->
              let func_hints = visit_object_key_and_compute_hint key in
              this#visit_function_expr
                ~func_hints
                ~has_this_def:false
                ~var_assigned_to:None
                ~statics:SMap.empty
                ~arrow:false
                ~hooklike:false
                loc
                fn;
              ())
          | SpreadProperty s ->
            let (_, { Ast.Expression.Object.SpreadProperty.argument; comments = _ }) = s in
            this#visit_expression
              ~hints:(decompose_hints Decomp_ObjSpread object_hints)
              ~cond:NonConditionalContext
              argument
      )

    method! switch loc switch =
      let open Ast.Statement.Switch in
      let { discriminant; cases; _ } = switch in
      let res = super#switch loc switch in
      (* Overwrite the (probably empty) hints on the case expressions recorded by super#switch *)
      Base.List.iter cases ~f:(fun case ->
          match case with
          | (_, { Case.test = Some (test_loc, _); _ }) ->
            this#record_hint test_loc [Hint_t (ValueHint discriminant, ExpectedTypeHint)]
          | _ -> ()
      );
      res
  end

let find_defs ~autocomplete_hooks env_info toplevel_scope_kind ast =
  let finder = new def_finder ~autocomplete_hooks env_info toplevel_scope_kind in
  finder#eval finder#program ast
