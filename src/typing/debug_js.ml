(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type
open TypeUtil
open Utils_js
open Loc_collections

let string_of_polarity = function
  | Polarity.Negative -> "Negative"
  | Polarity.Neutral -> "Neutral"
  | Polarity.Positive -> "Positive"

let string_of_union_enum = function
  | UnionEnum.Str x -> spf "string %s" (display_string_of_name x)
  | UnionEnum.Num (_, x) -> spf "number %s" x
  | UnionEnum.Bool x -> spf "boolean %b" x
  | UnionEnum.Null -> "null"
  | UnionEnum.Void -> "void"

let string_of_sentinel = function
  | UnionEnum.One enum -> string_of_union_enum enum
  | UnionEnum.Many enums ->
    ListUtils.to_string " | " string_of_union_enum @@ UnionEnumSet.elements enums

let string_of_selector = function
  | Elem _ -> "Elem _" (* TODO print info about the key *)
  | Prop (x, _) -> spf "Prop %s" x
  | ArrRest i -> spf "ArrRest %i" i
  | ObjRest xs -> spf "ObjRest [%s]" (String.concat "; " xs)
  | Default -> "Default"

let string_of_destructor = function
  | NonMaybeType -> "NonMaybeType"
  | PropertyType x -> spf "PropertyType %s" (display_string_of_name x)
  | ElementType _ -> "ElementType"
  | OptionalIndexedAccessNonMaybeType _ -> "OptionalIndexedAccessNonMaybeType"
  | OptionalIndexedAccessResultType _ -> "OptionalIndexedAccessResultType"
  | Bind _ -> "Bind"
  | ReadOnlyType -> "ReadOnly"
  | SpreadType _ -> "Spread"
  | RestType _ -> "Rest"
  | ValuesType -> "Values"
  | CallType _ -> "CallType"
  | TypeMap (TupleMap _) -> "TupleMap"
  | TypeMap (ObjectMap _) -> "ObjectMap"
  | TypeMap (ObjectMapi _) -> "ObjectMapi"
  | ReactElementPropsType -> "ReactElementProps"
  | ReactElementConfigType -> "ReactElementConfig"
  | ReactElementRefType -> "ReactElementRef"
  | ReactConfigType _ -> "ReactConfig"

let string_of_destruct_kind = function
  | DestructAnnot -> "Annot"
  | DestructInfer -> "Infer"

let bool_of_sealtype = function
  | Object.Spread.Sealed -> true
  | _ -> false

(*****************************************************************)

(* debug printer *)

let lookup_trust cx id =
  Trust_constraint.(
    match Context.find_trust_graph cx id with
    | TrustResolved trust -> trust
    | TrustUnresolved b -> get_trust b)

let dump_reason cx reason =
  let strip_root =
    if Context.should_strip_root cx then
      Some (Context.root cx)
    else
      None
  in
  Reason.dump_reason ~strip_root reason

let rec dump_t_ (depth, tvars) cx t =
  let p ?(reason = true) ?(extra = "") ?(trust = None) t =
    spf
      "%s %s(%s%s%s)"
      (string_of_ctor t)
      ( if not (Context.trust_tracking cx) then
        ""
      else
        Base.Option.value_map ~default:"" ~f:(lookup_trust cx |> string_of_trust_rep) trust )
      ( if reason then
        spf "%S" (dump_reason cx (reason_of_t t))
      else
        "" )
      ( if reason && extra <> "" then
        ", "
      else
        "" )
      extra
  in
  let kid = dump_t_ (depth - 1, tvars) cx in
  let tvar id = dump_tvar_ (depth - 1, tvars) cx id in
  let defer_use expr t =
    match expr with
    | LatentPredT (_, p) -> spf "LatentPred %s on %s" (string_of_predicate p) t
    | TypeDestructorT (use_op, _, destructor) ->
      spf "%s, TypeDestruct %s on %s" (string_of_use_op use_op) (string_of_destructor destructor) t
  in
  let string_of_mixed_flavor = function
    | Mixed_everything -> "Mixed_everything"
    | Mixed_function -> "Mixed_function"
    | Mixed_truthy -> "Mixed_truthy"
    | Mixed_non_maybe -> "Mixed_non_maybe"
    | Mixed_non_null -> "Mixed_non_null"
    | Mixed_non_void -> "Mixed_non_void"
  in
  let string_of_any_source = function
    | AnnotatedAny -> "AnnotatedAny"
    | AnyError _ -> "Error"
    | Unsound _ -> "Unsound"
    | Untyped -> "Untyped"
  in
  let custom_fun =
    let react_prop_type =
      React.PropType.(
        let complex = function
          | ArrayOf -> "ArrayOf"
          | InstanceOf -> "InstanceOf"
          | ObjectOf -> "ObjectOf"
          | OneOf -> "OneOf"
          | OneOfType -> "OneOfType"
          | Shape -> "Shape"
        in
        function
        | Primitive (is_required, t) -> spf "Primitive (%b, %s)" is_required (kid t)
        | Complex kind -> complex kind)
    in
    function
    | ObjectAssign -> "ObjectAssign"
    | ObjectGetPrototypeOf -> "ObjectGetPrototypeOf"
    | ObjectSetPrototypeOf -> "ObjectSetPrototypeOf"
    | Compose false -> "Compose"
    | Compose true -> "ComposeReverse"
    | ReactPropType p -> spf "ReactPropType (%s)" (react_prop_type p)
    | ReactCreateElement -> "ReactCreateElement"
    | ReactCloneElement -> "ReactCloneElement"
    | ReactElementFactory _ -> "ReactElementFactory"
    | ReactCreateClass -> "ReactCreateClass"
    | Idx -> "Idx"
    | TypeAssertIs -> "TypeAssert.is"
    | TypeAssertThrows -> "TypeAssert.throws"
    | TypeAssertWraps -> "TypeAssert.wraps"
    | DebugPrint -> "DebugPrint"
    | DebugThrow -> "DebugThrow"
    | DebugSleep -> "DebugSleep"
  in
  if depth = 0 then
    string_of_ctor t
  else
    match t with
    | OpenT (_, id) -> p ~extra:(tvar id) t
    | DefT (_, trust, NumT lit) ->
      p
        ~trust:(Some trust)
        ~extra:
          (match lit with
          | Literal (_, (_, raw)) -> raw
          | Truthy -> "truthy"
          | AnyLiteral -> "")
        t
    | DefT (_, trust, StrT c) ->
      p
        ~trust:(Some trust)
        ~extra:
          (match c with
          | Literal (_, s) -> spf "%S" (display_string_of_name s)
          | Truthy -> "truthy"
          | AnyLiteral -> "")
        t
    | DefT (_, trust, BoolT c) ->
      p
        ~trust:(Some trust)
        ~extra:
          (match c with
          | Some b -> spf "%B" b
          | None -> "")
        t
    | DefT (_, trust, FunT (_, _, { params; return_t; this_t; _ })) ->
      p
        ~trust:(Some trust)
        ~extra:
          (spf
             "<this: %s>(%s) => %s"
             (kid (fst this_t))
             (String.concat "; " (Base.List.map ~f:(fun (_, t) -> kid t) params))
             (kid return_t))
        t
    | AnyT (_, src) -> p ~extra:(string_of_any_source src) t
    | DefT (_, trust, MixedT flavor) ->
      p ~trust:(Some trust) ~extra:(string_of_mixed_flavor flavor) t
    | DefT (_, trust, EmptyT)
    | DefT (_, trust, SymbolT)
    | DefT (_, trust, NullT)
    | DefT (_, trust, VoidT) ->
      p ~trust:(Some trust) t
    | NullProtoT _
    | ObjProtoT _
    | FunProtoT _
    | FunProtoApplyT _
    | FunProtoBindT _
    | FunProtoCallT _ ->
      p t
    | DefT (_, trust, PolyT { tparams = tps; t_out = c; id; _ }) ->
      p
        ~trust:(Some trust)
        ~extra:
          (spf
             "%s [%s] #%s"
             (kid c)
             (String.concat "; " (Base.List.map ~f:(fun tp -> tp.name) (Nel.to_list tps)))
             (Poly.string_of_id id))
        t
    | ThisClassT (_, inst, _) -> p ~extra:(kid inst) t
    | BoundT (_, name) -> p ~extra:name t
    | GenericT { name; bound; _ } -> p ~extra:(spf "%s: %s" name (kid bound)) t
    | ExistsT _ -> p t
    | DefT (_, trust, ObjT { props_tmap; _ }) ->
      p ~trust:(Some trust) t ~extra:(Properties.string_of_id props_tmap)
    | DefT (_, trust, ArrT (ArrayAT (elemt, None))) ->
      p ~trust:(Some trust) ~extra:(spf "Array %s" (kid elemt)) t
    | DefT (_, trust, ArrT (ArrayAT (elemt, Some tup))) ->
      p
        ~trust:(Some trust)
        ~extra:
          (spf
             "Array %s, %s"
             (kid elemt)
             (spf "[%s]" (String.concat "; " (Base.List.map ~f:kid tup))))
        t
    | DefT (_, trust, ArrT (TupleAT (_, tup))) ->
      p
        ~trust:(Some trust)
        ~extra:(spf "Tuple [%s]" (String.concat ", " (Base.List.map ~f:kid tup)))
        t
    | DefT (_, trust, ArrT (ROArrayAT elemt)) ->
      p ~trust:(Some trust) ~extra:(spf "ReadOnlyArray %s" (kid elemt)) t
    | DefT (_, trust, CharSetT chars) ->
      p ~trust:(Some trust) ~extra:(spf "<%S>" (String_utils.CharSet.to_string chars)) t
    | DefT (_, trust, ClassT inst) -> p ~trust:(Some trust) ~extra:(kid inst) t
    | DefT (_, trust, InstanceT (_, _, _, { class_id; _ })) ->
      p ~trust:(Some trust) ~extra:(spf "#%s" (ALoc.debug_to_string (class_id :> ALoc.t))) t
    | DefT (_, trust, TypeT (kind, arg)) ->
      p ~trust:(Some trust) ~extra:(spf "%s, %s" (string_of_type_t_kind kind) (kid arg)) t
    | DefT (_, trust, EnumT { enum_id; members = _; representation_t = _; has_unknown_members = _ })
    | DefT
        ( _,
          trust,
          EnumObjectT { enum_id; members = _; representation_t = _; has_unknown_members = _ } ) ->
      p ~trust:(Some trust) ~extra:(spf "enum #%s" (ALoc.debug_to_string (enum_id :> ALoc.t))) t
    | AnnotT (_, arg, use_desc) -> p ~extra:(spf "use_desc=%b, %s" use_desc (kid arg)) t
    | OpaqueT (_, { underlying_t = Some arg; _ }) -> p ~extra:(spf "%s" (kid arg)) t
    | OpaqueT _ -> p t
    | OptionalT { reason = _; type_ = arg; use_desc = _ } -> p ~extra:(kid arg) t
    | EvalT (arg, expr, id) ->
      p ~extra:(spf "%s, %s" (defer_use expr (kid arg)) (Eval.string_of_id id)) t
    | TypeAppT (_, _, base, args) ->
      p ~extra:(spf "%s, [%s]" (kid base) (String.concat "; " (Base.List.map ~f:kid args))) t
    | ThisTypeAppT (_, base, this, args_opt) ->
      p
        ~reason:false
        ~extra:
          begin
            match args_opt with
            | Some args ->
              spf
                "%s, %s, [%s]"
                (kid base)
                (kid this)
                (String.concat "; " (Base.List.map ~f:kid args))
            | None -> spf "%s, %s" (kid base) (kid this)
          end
        t
    | ExactT (_, arg) -> p ~extra:(kid arg) t
    | MaybeT (_, arg) -> p ~extra:(kid arg) t
    | IntersectionT (_, rep) ->
      p ~extra:(spf "[%s]" (String.concat "; " (Base.List.map ~f:kid (InterRep.members rep)))) t
    | UnionT (_, rep) ->
      p
        ~extra:
          (spf
             "[%s]%s"
             (String.concat "; " (Base.List.map ~f:kid (UnionRep.members rep)))
             (UnionRep.string_of_specialization rep))
        t
    | DefT (_, trust, IdxWrapper inner_obj) -> p ~trust:(Some trust) ~extra:(kid inner_obj) t
    | DefT (_, trust, ReactAbstractComponentT _) -> p ~trust:(Some trust) t
    | ShapeT (_, arg) -> p ~extra:(kid arg) t
    | MatchingPropT (_, _, arg) -> p ~extra:(kid arg) t
    | KeysT (_, arg) -> p ~extra:(kid arg) t
    | DefT (_, trust, SingletonStrT s) ->
      p ~trust:(Some trust) ~extra:(spf "%S" (display_string_of_name s)) t
    | DefT (_, trust, SingletonNumT (_, s)) -> p ~trust:(Some trust) ~extra:s t
    | DefT (_, trust, SingletonBoolT b) -> p ~trust:(Some trust) ~extra:(spf "%B" b) t
    | ModuleT (_, { exports_tmap; _ }, _) ->
      p
        t
        ~extra:
          ( Context.find_exports cx exports_tmap
          |> NameUtils.Map.bindings
          |> Base.List.map ~f:(fun (name, (_, t)) ->
                 kid t |> spf "%s: %s" (display_string_of_name name))
          |> String.concat ", "
          |> spf "[%s]" )
    | InternalT (ExtendsT (_, l, u)) -> p ~extra:(spf "%s, %s" (kid l) (kid u)) t
    | CustomFunT (_, kind) -> p ~extra:(custom_fun kind) t
    | InternalT (ChoiceKitT _) -> p t
    | TypeDestructorTriggerT (_, _, _, s, (r, x)) ->
      p
        ~extra:(spf "%s on upper, (%s, %s)" (string_of_destructor s) (string_of_reason r) (tvar x))
        t
    | OpenPredT { base_t = arg; m_pos = p_pos; m_neg = p_neg; reason = _ } ->
      p
        t
        ~extra:
          (spf
             "%s, {%s}, {%s}"
             (kid arg)
             (String.concat
                "; "
                (Base.List.map
                   ~f:(fun (k, p) -> spf "%s: %s" (Key.string_of_key k) (string_of_predicate p))
                   (Key_map.elements p_pos)))
             (String.concat
                "; "
                (Base.List.map
                   ~f:(fun (k, p) -> spf "%s: %s" (Key.string_of_key k) (string_of_predicate p))
                   (Key_map.elements p_neg))))
    | ReposT (_, arg)
    | InternalT (ReposUpperT (_, arg)) ->
      p ~extra:(kid arg) t

and dump_use_t_ (depth, tvars) cx t =
  let p ?(reason = true) ?(extra = "") use_t =
    spf
      "%s (%s%s%s)"
      (string_of_use_ctor use_t)
      ( if reason then
        spf "%S" (dump_reason cx (reason_of_use_t use_t))
      else
        "" )
      ( if reason && extra <> "" then
        ", "
      else
        "" )
      extra
  in
  let kid t = dump_t_ (depth - 1, tvars) cx t in
  let use_kid use_t = dump_use_t_ (depth - 1, tvars) cx use_t in
  let tvar id = dump_tvar_ (depth - 1, tvars) cx id in
  let tout (reason, id) = spf "(%s, %s)" (string_of_reason reason) (tvar id) in
  let prop p = dump_prop_ (depth - 1, tvars) cx p in
  let string_of_use_op = string_of_use_op_rec in
  let call_arg_kid = function
    | Arg t -> kid t
    | SpreadArg t -> spf "...%s" (kid t)
  in
  let tlist ts = spf "[%s]" (String.concat "; " (Base.List.map ~f:kid ts)) in
  let props map =
    spf
      "{%s}"
      (String.concat
         "; "
         (NameUtils.Map.fold
            (fun k p acc -> spf "%s = %s" (display_string_of_name k) (prop p) :: acc)
            map
            []))
  in
  let propref = function
    | Named (r, x) -> spf "%S %s" (dump_reason cx r) (display_string_of_name x)
    | Computed t -> kid t
  in
  let lookup_kind = function
    | NonstrictReturning (default_opt, testid_opt) ->
      spf
        "Nonstrict%s%s"
        (Base.Option.value_map default_opt ~default:"" ~f:(fun (t, _) ->
             spf " returning %s" (kid t)))
        (Base.Option.value_map testid_opt ~default:"" ~f:(fun (id, _) -> spf " for test id %d" id))
    | Strict r -> spf "Strict %S" (dump_reason cx r)
    | ShadowRead (_, ids) ->
      spf
        "ShadowRead [%s]"
        (String.concat "; " (Nel.to_list ids |> Base.List.map ~f:Properties.string_of_id))
    | ShadowWrite ids ->
      spf
        "ShadowWrite [%s]"
        (String.concat "; " (Nel.to_list ids |> Base.List.map ~f:Properties.string_of_id))
  in
  let lookup_action = function
    | ReadProp { tout = (reason, tout); _ } ->
      spf "Read (%s, %s)" (string_of_reason reason) (tvar tout)
    | WriteProp { tin; _ } -> spf "Write %s" (kid tin)
    | LookupProp (op, p) -> spf "Lookup (%s, %s)" (string_of_use_op op) (prop p)
    | SuperProp (_, p) -> spf "Super %s" (prop p)
    | MatchProp (_, tin) -> spf "Match %s" (kid tin)
  in
  let specialize_cache = function
    | None -> "None"
    | Some rs -> spf "Some [%s]" (String.concat "; " @@ Base.List.map ~f:(dump_reason cx) rs)
  in
  let try_flow = function
    | UnionCases (use_op, t, _rep, ts) ->
      spf
        "(%s, %s, [%s])"
        (string_of_use_op use_op)
        (kid t)
        (String.concat "; " (Base.List.map ~f:kid ts))
    | IntersectionCases (ts, use_t) ->
      spf "([%s], %s)" (String.concat "; " (Base.List.map ~f:kid ts)) (use_kid use_t)
  in
  let react_kit =
    React.(
      let resolved_object (_, pmap, _) = props pmap in
      let resolve_array = function
        | ResolveArray -> "ResolveArray"
        | ResolveElem (todo, done_rev) -> spf "ResolveElem (%s, %s)" (tlist todo) (tlist done_rev)
      in
      let resolve_object = function
        | ResolveObject -> "ResolveObject"
        | ResolveDict (_, todo, acc) ->
          spf "ResolveDict (%s, %s)" (props todo) (resolved_object acc)
        | ResolveProp (k, todo, acc) ->
          spf
            "ResolveProp (%s, %s, %s)"
            (display_string_of_name k)
            (props todo)
            (resolved_object acc)
      in
      let simplify_prop_type =
        SimplifyPropType.(
          function
          | ArrayOf -> "ArrayOf"
          | InstanceOf -> "InstanceOf"
          | ObjectOf -> "ObjectOf"
          | OneOf tool -> spf "OneOf (%s)" (resolve_array tool)
          | OneOfType tool -> spf "OneOfType (%s)" (resolve_array tool)
          | Shape tool -> spf "Shape (%s)" (resolve_object tool))
      in
      let create_class =
        CreateClass.(
          let tool = function
            | Spec _ -> "Spec"
            | Mixins _ -> "Mixins"
            | Statics _ -> "Statics"
            | PropTypes (_, tool) -> spf "PropTypes (%s)" (resolve_object tool)
            | DefaultProps _ -> "DefaultProps"
            | InitialState _ -> "InitialState"
          in
          let knot { this; static; state_t; default_t } =
            spf
              "{this = %s; static = %s; state = %s; default = %s}"
              (kid this)
              (kid static)
              (kid state_t)
              (kid default_t)
          in
          (fun t k -> spf "%s, %s" (tool t) (knot k)))
      in
      function
      | CreateElement0 (_, config, (children, children_spread), tout)
      | CreateElement (_, _, config, (children, children_spread), tout) ->
        p
          ~extra:
            (spf
               "CreateElement (%s; %s%s) => %s"
               (kid config)
               (String.concat "; " (Base.List.map ~f:kid children))
               (match children_spread with
               | Some children_spread -> spf "; ...%s" (kid children_spread)
               | None -> "")
               (kid tout))
          t
      | ConfigCheck config -> spf "ConfigCheck (%s)" (kid config)
      | GetProps tout -> spf "GetProps (%s)" (kid tout)
      | GetConfig tout -> spf "GetConfig (%s)" (kid tout)
      | GetConfigType (default_props, tout) ->
        spf "GetConfigType (%s, %s)" (kid default_props) (kid tout)
      | GetRef tout -> spf "GetRef (%s)" (kid tout)
      | SimplifyPropType (tool, tout) ->
        spf "SimplifyPropType (%s, %s)" (simplify_prop_type tool) (kid tout)
      | CreateClass (tool, knot, tout) ->
        spf "CreateClass (%s, %s)" (create_class tool knot) (kid tout))
  in
  let slice { Object.reason = _; props; flags = { obj_kind; _ }; generics = _ } =
    let xs =
      match obj_kind with
      | Indexed { dict_polarity = p; _ } -> [Polarity.sigil p ^ "[]"]
      | Exact
      | Inexact
      | UnsealedInFile _ ->
        []
    in
    let xs =
      NameUtils.Map.fold
        (fun k (t, _, _) xs ->
          let opt =
            match t with
            | OptionalT _ -> "?"
            | _ -> ""
          in
          (display_string_of_name k ^ opt) :: xs)
        props
        xs
    in
    let xs = String.concat "; " xs in
    match obj_kind with
    | Exact -> spf "{|%s|}" xs
    | _ -> spf "{%s}" xs
  in
  let operand_slice reason prop_map dict =
    let props =
      NameUtils.Map.fold
        (fun k p acc ->
          match (Type.Property.read_t p, Type.Property.write_t p) with
          | (Some t, _)
          | (_, Some t) ->
            NameUtils.Map.add k (t, true, false) acc
          | _ -> acc)
        prop_map
        NameUtils.Map.empty
    in
    let obj_kind =
      match dict with
      | None -> Exact
      | Some d -> Indexed d
    in
    let flags = { obj_kind; frozen = false } in
    slice { Object.reason; props; flags; generics = Generic.spread_empty }
  in
  let object_kit =
    Object.(
      let join (_loc, op) =
        match op with
        | And -> "And"
        | Or -> "Or"
      in
      let resolved xs = spf "[%s]" (String.concat "; " (Base.List.map ~f:slice (Nel.to_list xs))) in
      let resolve = function
        | Next -> "Next"
        | List0 (todo, j) ->
          spf
            "List0 ([%s], %s)"
            (String.concat "; " (Base.List.map ~f:kid (Nel.to_list todo)))
            (join j)
        | List (todo, done_rev, j) ->
          spf
            "List ([%s], [%s], %s)"
            (String.concat "; " (Base.List.map ~f:kid todo))
            (String.concat "; " (Base.List.map ~f:resolved (Nel.to_list done_rev)))
            (join j)
      in
      let resolve_tool = function
        | Resolve tool -> spf "Resolve %s" (resolve tool)
        | Super (s, tool) -> spf "Super (%s, %s)" (slice s) (resolve tool)
      in
      let acc_element = function
        | Spread.InlineSlice { Spread.reason; prop_map; dict; generics = _ } ->
          operand_slice reason prop_map dict
        | Spread.ResolvedSlice xs -> resolved xs
      in
      let spread target state =
        Object.Spread.(
          let target =
            match target with
            | Annot { make_exact } -> spf "Annot { make_exact=%b }" make_exact
            | Value { make_seal } -> spf "Value {make_seal=%b" (bool_of_sealtype make_seal)
          in
          let spread_operand = function
            | Slice { Spread.reason; prop_map; dict; generics = _ } ->
              operand_slice reason prop_map dict
            | Type t -> kid t
          in
          let state =
            let { todo_rev; acc; spread_id; union_reason; curr_resolve_idx } = state in
            spf
              "{todo_rev=[%s]; acc=[%s]; spread_id=%s; curr_resolve_idx=%s; union_reason=%s}"
              (String.concat "; " (Base.List.map ~f:spread_operand todo_rev))
              (String.concat "; " (Base.List.map ~f:acc_element acc))
              (string_of_int spread_id)
              (string_of_int curr_resolve_idx)
              (Base.Option.value_map union_reason ~default:"None" ~f:(dump_reason cx))
          in
          spf "Spread (%s, %s)" target state)
      in
      let rest merge_mode state =
        Object.Rest.(
          spf
            "Rest ({merge_mode=%s}, %s)"
            (match merge_mode with
            | Sound -> "Sound"
            | IgnoreExactAndOwn -> "IgnoreExactAndOwn"
            | ReactConfigMerge _ -> "ReactConfigMerge")
            (match state with
            | One t -> spf "One (%s)" (kid t)
            | Done o -> spf "Done (%s)" (resolved o)))
      in
      let react_props state =
        Object.ReactConfig.(
          spf
            "(%s)"
            (match state with
            | Config _ -> "Config"
            | Defaults _ -> "Defaults"))
      in
      let tool = function
        | ReadOnly -> "ReadOnly"
        | ObjectRep -> "ObjectRep"
        | ObjectWiden id -> spf "ObjectWiden (%s)" (string_of_int id)
        | Spread (options, state) -> spread options state
        | Rest (options, state) -> rest options state
        | ReactConfig state -> react_props state
      in
      (fun a b -> spf "(%s, %s)" (resolve_tool a) (tool b)))
  in
  let method_action = function
    | CallM { meth_args_tlist; meth_tout = (call_r, call_tvar); meth_generic_this; _ }
    | ChainM (_, _, _, { meth_args_tlist; meth_tout = (call_r, call_tvar); meth_generic_this; _ }, _)
      ->
      spf
        "<this: %s>(%s) => (%s, %s)"
        (Base.Option.value_map ~f:kid ~default:"None" meth_generic_this)
        (String.concat "; " (Base.List.map ~f:call_arg_kid meth_args_tlist))
        (string_of_reason call_r)
        (tvar call_tvar)
  in
  if depth = 0 then
    string_of_use_ctor t
  else
    match t with
    | UseT (use_op, OpenT (r, id)) ->
      spf "UseT (%s, OpenT (%S, %d))" (string_of_use_op use_op) (dump_reason cx r) id
    | UseT (use_op, (DefT (_, trust, _) as t)) ->
      spf
        "UseT (%s, %s%s)"
        (string_of_use_op use_op)
        ( if Context.trust_tracking cx then
          string_of_trust_rep (lookup_trust cx) trust
        else
          "" )
        (kid t)
    | UseT (use_op, t) -> spf "UseT (%s, %s)" (string_of_use_op use_op) (kid t)
    | AdderT (use_op, _, _, x, y) ->
      p ~extra:(spf "%s, %s, %s" (string_of_use_op use_op) (kid x) (kid y)) t
    | AndT (_, x, y) -> p ~extra:(spf "%s, %s" (kid x) (tout y)) t
    | ArrRestT (use_op, _, _, _) -> p ~extra:(string_of_use_op use_op) t
    | AssertArithmeticOperandT _ -> p t
    | AssertBinaryInLHST _ -> p t
    | AssertBinaryInRHST _ -> p t
    | AssertForInRHST _ -> p t
    | AssertImportIsValueT _ -> p t
    | AssertInstanceofRHST _ -> p t
    | AssertIterableT _ -> p t
    | BecomeT { reason = _; t = arg; empty_success = _ } -> p ~extra:(kid arg) t
    | BindT (use_op, _, _, _) -> p t ~extra:(string_of_use_op use_op)
    | CallElemT (_, _, _, _) -> p t
    | CallT (use_op, _, { call_args_tlist; call_tout = (call_r, call_tvar); call_this_t; _ }) ->
      p
        ~extra:
          (spf
             "%s, <this: %s>(%s) => (%s, %s)"
             (string_of_use_op use_op)
             (kid call_this_t)
             (String.concat "; " (Base.List.map ~f:call_arg_kid call_args_tlist))
             (string_of_reason call_r)
             (tvar call_tvar))
        t
    | CallLatentPredT _ -> p t
    | CallOpenPredT _ -> p t
    | ChoiceKitUseT (_, TryFlow (_, spec)) -> p ~extra:(try_flow spec) t
    | ChoiceKitUseT (_, FullyResolveType id) -> p ~extra:(tvar id) t
    | CJSExtractNamedExportsT _ -> p t
    | CJSRequireT _ -> p t
    | ComparatorT { arg; _ } -> p ~extra:(kid arg) t
    | ConstructorT _ -> p t
    | CopyNamedExportsT _ -> p t
    | CopyTypeExportsT _ -> p t
    | DebugPrintT _ -> p t
    | DebugSleepT _ -> p t
    | ElemT _ -> p t
    | ExportNamedT (_, tmap, _export_kind, arg) ->
      p
        t
        ~extra:
          (spf
             "%s, {%s}"
             (kid arg)
             (String.concat
                "; "
                (Base.List.map
                   ~f:(fun (x, _) -> display_string_of_name x)
                   (NameUtils.Map.bindings tmap))))
    | ExportTypeT _ -> p t
    | FunImplicitVoidReturnT _ -> p t
    | AssertExportIsTypeT _ -> p t
    | GetElemT (_, _, ix, (preason, ptvar)) ->
      p ~extra:(spf "%s, (%s, %s)" (kid ix) (string_of_reason preason) (tvar ptvar)) t
    | GetKeysT _ -> p t
    | GetValuesT _ -> p t
    | MatchPropT (use_op, _, prop, (preason, ptvar))
    | GetPropT (use_op, _, prop, (preason, ptvar)) ->
      p
        ~extra:
          (spf
             "%s, (%s), (%s, %s)"
             (string_of_use_op use_op)
             (propref prop)
             (string_of_reason preason)
             (tvar ptvar))
        t
    | GetPrivatePropT (_, _, prop, _, _, (preason, ptvar)) ->
      p ~extra:(spf "(%s), (%s, %s)" prop (string_of_reason preason) (tvar ptvar)) t
    | GetProtoT (_, (_, arg)) -> p ~extra:(tvar arg) t
    | GetStaticsT (_, arg) -> p ~extra:(tvar arg) t
    | GuardT (pred, result, sink) ->
      p
        ~reason:false
        ~extra:(spf "%s, %s, %s" (string_of_predicate pred) (kid result) (tout sink))
        t
    | HasOwnPropT _ -> p t
    | IdxUnMaybeifyT _ -> p t
    | IdxUnwrap _ -> p t
    | ImportDefaultT _ -> p t
    | ImportModuleNsT _ -> p t
    | ImportNamedT _ -> p t
    | ImportTypeofT _ -> p t
    | ImportTypeT _ -> p t
    | IntersectionPreprocessKitT _ -> p t
    | InvariantT _ -> p t
    | LookupT { lookup_kind = kind; propref = prop; lookup_action = action; ids; _ } ->
      p
        ~extra:
          (spf
             "%S, %s, %s, [%s]"
             (propref prop)
             (lookup_kind kind)
             (lookup_action action)
             (match ids with
             | None -> "None"
             | Some ids ->
               spf
                 "Some %s"
                 (String.concat
                    "; "
                    (Properties.Set.elements ids |> Base.List.map ~f:Properties.string_of_id))))
        t
    | MakeExactT _ -> p t
    | MapTypeT _ -> p t
    | MethodT (_, _, _, prop, action, _) ->
      p ~extra:(spf "(%s, %s)" (propref prop) (method_action action)) t
    | MixinT (_, arg) -> p ~extra:(kid arg) t
    | NotT (_, arg) -> p ~extra:(tout arg) t
    | NullishCoalesceT (_, x, y) -> p ~extra:(spf "%s, %s" (kid x) (tout y)) t
    | ObjAssignToT (_, _, arg1, arg2, _) -> p t ~extra:(spf "%s, %s" (kid arg1) (kid arg2))
    | ObjAssignFromT (_, _, arg1, arg2, _) -> p t ~extra:(spf "%s, %s" (kid arg1) (kid arg2))
    | ObjRestT (_, xs, arg, _) -> p t ~extra:(spf "[%s], %s" (String.concat "; " xs) (kid arg))
    | ObjSealT _ -> p t
    | ObjTestProtoT _ -> p t
    | ObjTestT _ -> p t
    | OptionalChainT { t_out; voided_out; _ } ->
      p ~extra:(spf "%s, %s" (use_kid t_out) (kid voided_out)) t
    | OptionalIndexedAccessT { index_type; _ } -> p ~extra:(kid index_type) t
    | OrT (_, x, y) -> p ~extra:(spf "%s, %s" (kid x) (tout y)) t
    | PredicateT (pred, arg) ->
      p ~reason:false ~extra:(spf "%s, %s" (string_of_predicate pred) (tout arg)) t
    | ReactKitT (use_op, _, tool) ->
      p t ~extra:(spf "%s, %s" (string_of_use_op use_op) (react_kit tool))
    | RefineT _ -> p t
    | ReactPropsToOut (_, props)
    | ReactInToProps (_, props) ->
      p ~extra:(kid props |> spf "%s") t
    | ReposLowerT (_, use_desc, arg) -> p t ~extra:(spf "use_desc=%b, %s" use_desc (use_kid arg))
    | ReposUseT (_, use_desc, use_op, arg) ->
      p t ~extra:(spf "use_desc=%b, %s" use_desc (use_kid (UseT (use_op, arg))))
    | ResolveSpreadT (use_op, _, { rrt_resolve_to; _ }) ->
      (match rrt_resolve_to with
      | ResolveSpreadsToArrayLiteral (_, elem_t, tout)
      | ResolveSpreadsToArray (elem_t, tout) ->
        p ~extra:(spf "%s, %s, %s" (string_of_use_op use_op) (kid elem_t) (kid tout)) t
      | ResolveSpreadsToMultiflowPartial (_, _, _, tout) ->
        p ~extra:(spf "%s, %s" (string_of_use_op use_op) (kid tout)) t
      | ResolveSpreadsToCallT (_, tin) ->
        p ~extra:(spf "%s, %s" (string_of_use_op use_op) (kid tin)) t
      | ResolveSpreadsToMultiflowCallFull _
      | ResolveSpreadsToMultiflowSubtypeFull _
      | ResolveSpreadsToCustomFunCall _ ->
        p ~extra:(string_of_use_op use_op) t)
    | SentinelPropTestT (_, l, _key, sense, sentinel, result) ->
      p
        ~reason:false
        ~extra:(spf "%s, %b, %s, %s" (kid l) sense (string_of_sentinel sentinel) (tout result))
        t
    | SubstOnPredT (_, _, subst, arg) ->
      let subst =
        SMap.bindings subst
        |> List.map (fun (k, v) -> spf "%s -> %s" k (Key.string_of_key v))
        |> String.concat ", "
      in
      p t ~extra:(spf "[subst: {%s}] %s" subst (kid arg))
    | SuperT _ -> p t
    | ImplementsT (_, arg) -> p ~reason:false ~extra:(kid arg) t
    | SetElemT (_, _, ix, _, etype, _) -> p ~extra:(spf "%s, %s" (kid ix) (kid etype)) t
    | SetPropT (use_op, _, prop, _, _, ptype, _) ->
      p ~extra:(spf "%s, (%s), %s" (string_of_use_op use_op) (propref prop) (kid ptype)) t
    | SetPrivatePropT (_, _, prop, _, _, _, ptype, _) ->
      p ~extra:(spf "(%s), %s" prop (kid ptype)) t
    | SetProtoT (_, arg) -> p ~extra:(kid arg) t
    | SpecializeT (_, _, _, cache, args_opt, ret) ->
      p
        ~extra:
          begin
            match args_opt with
            | Some args ->
              spf
                "%s, [%s], %s"
                (specialize_cache cache)
                (String.concat "; " (Base.List.map ~f:kid args))
                (kid ret)
            | None -> spf "%s, %s" (specialize_cache cache) (kid ret)
          end
        t
    | StrictEqT { arg; _ }
    | EqT { arg; _ } ->
      p ~extra:(kid arg) t
    | ObjKitT (use_op, _, resolve_tool, tool, tout) ->
      p
        ~extra:
          (spf "%s, %s, %s" (string_of_use_op use_op) (object_kit resolve_tool tool) (kid tout))
        t
    | TestPropT (_, _, prop, (preason, ptvar)) ->
      p ~extra:(spf "(%s), (%s, %s)" (propref prop) (string_of_reason preason) (tvar ptvar)) t
    | ThisSpecializeT (_, this, _) -> p ~extra:(spf "%s" (kid this)) t
    | ToStringT (_, arg) -> p ~extra:(use_kid arg) t
    | UnaryMinusT _ -> p t
    | UnifyT (x, y) -> p ~reason:false ~extra:(spf "%s, %s" (kid x) (kid y)) t
    | VarianceCheckT (_, _, args, pol) ->
      p
        ~extra:
          (spf "[%s], %s" (String.concat "; " (Base.List.map ~f:kid args)) (Polarity.string pol))
        t
    | ConcretizeTypeAppsT _ -> p t
    | TypeAppVarianceCheckT _ -> p t
    | TypeCastT (_, arg) -> p ~reason:false ~extra:(kid arg) t
    | EnumCastT { use_op = _; enum = (reason, trust, enum) } ->
      p ~reason:false ~extra:(kid (DefT (reason, trust, EnumT enum))) t
    | EnumExhaustiveCheckT { check; _ } ->
      let check_str =
        match check with
        | EnumExhaustiveCheckPossiblyValid _ -> "EnumExhaustiveCheckPossiblyValid"
        | EnumExhaustiveCheckInvalid _ -> "EnumExhaustiveCheckInvalid"
      in
      p ~extra:check_str t
    | FilterOptionalT (_, arg) -> p ~reason:false ~extra:(kid arg) t
    | FilterMaybeT (_, arg) -> p ~reason:false ~extra:(kid arg) t
    | SealGenericT { name; cont = Lower (_, l); _ } -> p ~extra:(spf "%s <~ %s" name (kid l)) t
    | SealGenericT { name; cont = Upper u; _ } -> p ~extra:(spf "%s ~> %s" name (use_kid u)) t
    | CondT (_, then_t, else_t, tout) ->
      p
        t
        ~extra:
          (spf
             "%s, %s, %s"
             (match then_t with
             | None -> "None"
             | Some t -> spf "Some (%s)" (kid t))
             (kid else_t)
             (kid tout))
    | ExtendsUseT (_, _, nexts, l, u) ->
      p
        ~extra:
          (spf "[%s], %s, %s" (String.concat "; " (Base.List.map ~f:kid nexts)) (kid l) (kid u))
        t
    | DestructuringT (_, k, s, (r, tout), _) ->
      p
        t
        ~extra:
          (spf
             "%s, %s, (%s, %s)"
             (string_of_destruct_kind k)
             (string_of_selector s)
             (string_of_reason r)
             (tvar tout))
    | CreateObjWithComputedPropT { reason = _; value; tout_tvar } ->
      p t ~extra:(spf "%s %s" (kid value) (kid (OpenT tout_tvar)))
    | ResolveUnionT { resolved; unresolved; upper; id; _ } ->
      p
        t
        ~extra:
          (spf
             "%d [%s], [%s], %s"
             id
             (String.concat "; " (Base.List.map ~f:kid resolved))
             (String.concat "; " (Base.List.map ~f:kid unresolved))
             (use_kid upper))
    | ModuleExportsAssignT (_, _, _) -> p t

and dump_tvar_ (depth, tvars) cx id =
  if ISet.mem id tvars then
    spf "%d, ^" id
  else
    let stack = ISet.add id tvars in
    Constraint.(
      try
        match Context.find_tvar cx id with
        | Goto g -> spf "%d, Goto %d" id g
        | Root { constraints = (lazy (Resolved (_, t) | FullyResolved (_, (lazy t)))); _ } ->
          spf "%d, Resolved %s" id (dump_t_ (depth - 1, stack) cx t)
        | Root { constraints = (lazy (Unresolved { lower; upper; _ })); _ } ->
          if lower = TypeMap.empty && upper = UseTypeMap.empty then
            spf "%d" id
          else
            spf
              "%d, [%s], [%s]"
              id
              (String.concat
                 "; "
                 (List.rev
                    (TypeMap.fold (fun t _ acc -> dump_t_ (depth - 1, stack) cx t :: acc) lower [])))
              (String.concat
                 "; "
                 (List.rev
                    (UseTypeMap.fold
                       (fun (use_t, _) _ acc -> dump_use_t_ (depth - 1, stack) cx use_t :: acc)
                       upper
                       [])))
      with Context.Tvar_not_found _ -> spf "Not Found: %d" id)

and dump_prop_ (depth, tvars) cx p =
  let kid t = dump_t_ (depth, tvars) cx t in
  match p with
  | Field (_loc, t, polarity) -> spf "Field (%s) %s" (string_of_polarity polarity) (kid t)
  | Get (_loc, t) -> spf "Get %s" (kid t)
  | Set (_loc, t) -> spf "Set %s" (kid t)
  | GetSet (_loc1, t1, _loc2, t2) -> spf "Get %s Set %s" (kid t1) (kid t2)
  | Method (_loc, t) -> spf "Method %s" (kid t)

(* This is the type-dump debugging API.
   We should make sure these are not called recursively to avoid circumventing
   one of the termination mechanisms: depth or tvar-set.
*)
let dump_t ?(depth = 3) cx t = dump_t_ (depth, ISet.empty) cx t

let dump_use_t ?(depth = 3) cx t = dump_use_t_ (depth, ISet.empty) cx t

let dump_prop ?(depth = 3) cx p = dump_prop_ (depth, ISet.empty) cx p

let dump_tvar ?(depth = 3) cx id = dump_tvar_ (depth, ISet.empty) cx id

let dump_flow ?(depth = 3) cx (l, u) =
  spf "Lower: %s ~>\n Upper: %s" (dump_t ~depth cx l) (dump_use_t ~depth cx u)

(*****************************************************)

(* scopes and types *)

let string_of_scope_entry =
  Scope.(
    let string_of_value_binding
        cx
        {
          Entry.kind;
          value_state;
          value_declare_loc;
          value_assign_loc;
          specific;
          general;
          closure_writes;
        } =
      let general_str =
        match general with
        | Annotated t -> spf "Annotated %s" (dump_t cx t)
        | Inferred t -> spf "Inferred %s" (dump_t cx t)
      in
      spf
        "{ kind: %s; value_state: %s; value_declare_loc: %S; value_assign_loc: %s; specific: %s; general: %s%s }"
        (Entry.string_of_value_kind kind)
        (State.to_string value_state)
        (string_of_aloc value_declare_loc)
        (string_of_aloc value_assign_loc)
        (dump_t cx specific)
        general_str
        (Base.Option.value_map closure_writes ~default:"" ~f:(fun (locs, t) ->
             spf
               "; closure_writes: { locs: { %s }; t: %s }"
               (ListUtils.to_string ", " string_of_aloc @@ ALocSet.elements locs)
               (dump_t cx t)))
    in
    let string_of_type_binding cx { Entry.type_state; type_loc; type_; type_binding_kind = _ } =
      spf
        "{ type_state: %s; type_loc: %S; type_: %s }"
        (State.to_string type_state)
        (string_of_aloc type_loc)
        (dump_t cx type_)
    in
    fun cx ->
      Entry.(
        function
        | Value r -> spf "Value %s" (string_of_value_binding cx r)
        | Type r -> spf "Type %s" (string_of_type_binding cx r)
        | Class r -> spf "Class %s" (ALoc.debug_to_string (r.class_binding_id :> ALoc.t))))

let string_of_scope_entries cx entries =
  let strings =
    NameUtils.Map.fold
      (fun name entry acc ->
        spf "%s: %s" (Reason.display_string_of_name name) (string_of_scope_entry cx entry) :: acc)
      entries
      []
    |> String.concat "; \n"
  in
  spf "[ %s ]" strings

let string_of_scope_refi cx { Scope.refi_loc; refined; original } =
  spf
    "{ refi_loc: %S; refined: %s; original: %s }"
    (string_of_aloc refi_loc)
    (dump_t cx refined)
    (dump_t cx original)

let string_of_scope_refis cx refis =
  let strings =
    Key_map.fold
      (fun key refi acc ->
        spf "%s: %s" (Key.string_of_key key) (string_of_scope_refi cx refi) :: acc)
      refis
      []
    |> String.concat ";\n"
  in
  spf "[ %s ]" strings

let string_of_scope cx scope =
  Scope.(
    spf
      "{ kind: %s;\nentries:\n%s\nrefis:\n%s\n}"
      (string_of_kind scope.kind)
      (string_of_scope_entries cx scope.entries)
      (string_of_scope_refis cx scope.refis))

let string_of_reason cx reason =
  let strip_root =
    if Context.should_strip_root cx then
      Some (Context.root cx)
    else
      None
  in
  Reason.string_of_reason ~strip_root reason

let string_of_file cx =
  let filename = File_key.to_string (Context.file cx) in
  match Context.is_verbose cx with
  | false -> filename
  | true ->
    let root_str = Path.to_string (Context.root cx) ^ Filename.dir_sep in
    if String_utils.string_starts_with filename root_str then
      Files.relative_path root_str filename
    else
      filename

let string_of_default =
  Default.fold
    ~expr:(fun (loc, _) -> spf "Expr %s" (string_of_loc loc))
    ~selector:(fun _ str sel -> spf "Selector (%s) (%s)" str (string_of_selector sel))
    ~cons:(fun str default -> spf "Cons (%s) (%s)" str default)

let string_of_signature_error pp_loc err =
  let open Signature_error in
  match err with
  | ExpectedAnnotation (loc, sort) ->
    spf "Expected annotation at %s @ %s" (Expected_annotation_sort.to_string sort) (pp_loc loc)
  | UnexpectedObjectKey (_loc, key_loc) -> spf "Expected simple object key @ %s" (pp_loc key_loc)
  | UnexpectedArraySpread (_loc, spread_loc) ->
    spf "Unexpected array spread @ %s" (pp_loc spread_loc)
  | UnexpectedArrayHole loc -> spf "Unexpected array hole @ %s" (pp_loc loc)
  | EmptyArray loc -> spf "Cannot determine the element type of an empty array @ %s" (pp_loc loc)
  | EmptyObject loc ->
    spf "Cannot determine types of initialized properties of an empty object @ %s" (pp_loc loc)
  | UnexpectedExpression (loc, esort) ->
    spf
      "Cannot determine the type of this %s @ %s"
      (Flow_ast_utils.ExpressionSort.to_string esort)
      (pp_loc loc)

let dump_error_message =
  let open Error_message in
  let string_of_use_op = string_of_use_op_rec in
  let dump_internal_error = function
    | PackageHeapNotFound _ -> "PackageHeapNotFound"
    | AbnormalControlFlow -> "AbnormalControlFlow"
    | MethodNotAFunction -> "MethodNotAFunction"
    | OptionalMethod -> "OptionalMethod"
    | PredFunWithoutParamNames -> "PredFunWithoutParamNames"
    | UnsupportedGuardPredicate _ -> "UnsupportedGuardPredicate"
    | BreakEnvMissingForCase -> "BreakEnvMissingForCase"
    | PropertyDescriptorPropertyCannotBeRead -> "PropertyDescriptorPropertyCannotBeRead"
    | ForInLHS -> "ForInLHS"
    | ForOfLHS -> "ForOfLHS"
    | InstanceLookupComputed -> "InstanceLookupComputed"
    | PropRefComputedOpen -> "PropRefComputedOpen"
    | PropRefComputedLiteral -> "PropRefComputedLiteral"
    | ShadowReadComputed -> "ShadowReadComputed"
    | ShadowWriteComputed -> "ShadowWriteComputed"
    | RestParameterNotIdentifierPattern -> "RestParameterNotIdentifierPattern"
    | InterfaceTypeSpread -> "InterfaceTypeSpread"
    | Error_message.DebugThrow -> "DebugThrow"
    | MergeTimeout _ -> "MergeTimeout"
    | MergeJobException _ -> "MergeJobException"
    | CheckTimeout _ -> "CheckTimeout"
    | CheckJobException _ -> "CheckJobException"
    | UnexpectedTypeapp _ -> "UnexpectedTypeapp"
  in
  let dump_upper_kind = function
    | IncompatibleGetPropT _ -> "IncompatibleGetPropT"
    | IncompatibleSetPropT _ -> "IncompatibleSetPropT"
    | IncompatibleMatchPropT _ -> "IncompatibleSetPropT"
    | IncompatibleGetPrivatePropT -> "IncompatibleGetPrivatePropT"
    | IncompatibleSetPrivatePropT -> "IncompatibleSetPrivatePropT"
    | IncompatibleMethodT _ -> "IncompatibleMethodT"
    | IncompatibleCallT -> "IncompatibleCallT"
    | IncompatibleMixedCallT -> "IncompatibleMixedCallT"
    | IncompatibleConstructorT -> "IncompatibleConstructorT"
    | IncompatibleGetElemT _ -> "IncompatibleGetElemT"
    | IncompatibleSetElemT _ -> "IncompatibleSetElemT"
    | IncompatibleCallElemT _ -> "IncompatibleCallElemT"
    | IncompatibleElemTOfArrT -> "IncompatibleElemTOfArrT"
    | IncompatibleObjAssignFromTSpread -> "IncompatibleObjAssignFromTSpread"
    | IncompatibleObjAssignFromT -> "IncompatibleObjAssignFromT"
    | IncompatibleObjRestT -> "IncompatibleObjRestT"
    | IncompatibleObjSealT -> "IncompatibleObjSealT"
    | IncompatibleArrRestT -> "IncompatibleArrRestT"
    | IncompatibleSuperT -> "IncompatibleSuperT"
    | IncompatibleMixinT -> "IncompatibleMixinT"
    | IncompatibleSpecializeT -> "IncompatibleSpecializeT"
    | IncompatibleThisSpecializeT -> "IncompatibleThisSpecializeT"
    | IncompatibleVarianceCheckT -> "IncompatibleVarianceCheckT"
    | IncompatibleGetKeysT -> "IncompatibleGetKeysT"
    | IncompatibleHasOwnPropT _ -> "IncompatibleHasOwnPropT"
    | IncompatibleGetValuesT -> "IncompatibleGetValuesT"
    | IncompatibleUnaryMinusT -> "IncompatibleUnaryMinusT"
    | IncompatibleMapTypeTObject -> "IncompatibleMapTypeTObject"
    | IncompatibleTypeAppVarianceCheckT -> "IncompatibleTypeAppVarianceCheckT"
    | IncompatibleGetStaticsT -> "IncompatibleGetStaticsT"
    | IncompatibleBindT -> "IncompatibleBindT"
    | IncompatibleUnclassified ctor -> spf "IncompatibleUnclassified %S" ctor
  in
  fun cx err ->
    match err with
    | EIncompatible
        {
          lower = (reason_lower, _lower_kind);
          upper = (reason_upper, upper_kind);
          use_op;
          branches = _;
        } ->
      spf
        "EIncompatible { lower = (%s, _); upper = (%s, %s); use_op = %s; branches = _ }"
        (dump_reason cx reason_lower)
        (dump_reason cx reason_upper)
        (dump_upper_kind upper_kind)
        (match use_op with
        | None -> "None"
        | Some use_op -> spf "Some(%s)" (string_of_use_op use_op))
    | EIncompatibleDefs { use_op; reason_lower; reason_upper; branches = _ } ->
      spf
        "EIncompatibleDefs { reason_lower = %s; reason_upper = %s; use_op = %s; branches = _ }"
        (dump_reason cx reason_lower)
        (dump_reason cx reason_upper)
        (string_of_use_op use_op)
    | EIncompatibleProp { reason_prop; reason_obj; special = _; prop = _; use_op = _ } ->
      spf
        "EIncompatibleProp { reason_prop = %s; reason_obj = %s; special = _; prop = _; use_op = _ }"
        (dump_reason cx reason_prop)
        (dump_reason cx reason_obj)
    | EDebugPrint (reason, _) -> spf "EDebugPrint (%s, _)" (dump_reason cx reason)
    | EExportValueAsType (reason, name) ->
      spf "EExportValueAsType (%s, %s)" (dump_reason cx reason) (display_string_of_name name)
    | EImportValueAsType (reason, str) ->
      spf "EImportValueAsType (%s, %s)" (dump_reason cx reason) str
    | EImportTypeAsTypeof (reason, str) ->
      spf "EImportTypeAsTypeof (%s, %s)" (dump_reason cx reason) str
    | EImportTypeAsValue (reason, str) ->
      spf "EImportTypeAsValue (%s, %s)" (dump_reason cx reason) str
    | ERefineAsValue (reason, name) ->
      spf "ERefineAsValue (%s, %s)" (dump_reason cx reason) (display_string_of_name name)
    | ENoDefaultExport (reason, module_name, _) ->
      spf "ENoDefaultExport (%s, %s)" (dump_reason cx reason) module_name
    | EOnlyDefaultExport (reason, module_name, export_name) ->
      spf "EOnlyDefaultExport (%s, %s, %s)" (dump_reason cx reason) module_name export_name
    | ENoNamedExport (reason, module_name, export_name, _) ->
      spf "ENoNamedExport (%s, %s, %s)" (dump_reason cx reason) module_name export_name
    | EMissingTypeArgs { reason_tapp; reason_arity; min_arity; max_arity } ->
      spf
        "EMissingTypeArgs { reason_tapp=%s; reason_arity=%s; min_arity=%d; max_arity=%d }"
        (dump_reason cx reason_tapp)
        (dump_reason cx reason_arity)
        min_arity
        max_arity
    | EAnyValueUsedAsType { reason_use } ->
      spf "EAnyValueUsedAsType { use = %s }" (dump_reason cx reason_use)
    | EValueUsedAsType { reason_use } ->
      spf "EValueUsedAsType { use = %s }" (dump_reason cx reason_use)
    | EExpectedStringLit { reason_lower; reason_upper; use_op } ->
      spf
        "EExpectedStringLit { reason_lower = %s; reason_upper = %s; use_op = %s }"
        (dump_reason cx reason_lower)
        (dump_reason cx reason_upper)
        (string_of_use_op use_op)
    | EExpectedNumberLit { reason_lower; reason_upper; use_op } ->
      spf
        "EExpectedNumberLit { reason_lower = %s; reason_upper = %s; use_op = %s }"
        (dump_reason cx reason_lower)
        (dump_reason cx reason_upper)
        (string_of_use_op use_op)
    | EExpectedBooleanLit { reason_lower; reason_upper; use_op } ->
      spf
        "EExpectedBooleanLit { reason_lower = %s; reason_upper = %s; use_op = %s }"
        (dump_reason cx reason_lower)
        (dump_reason cx reason_upper)
        (string_of_use_op use_op)
    | EPropNotFound { prop_name = prop; reason_prop; reason_obj; use_op; suggestion } ->
      spf
        "EPropNotFound (%s, %s, %s, %s, %s)"
        (match prop with
        | Some prop -> spf "Some %s" (display_string_of_name prop)
        | None -> "None")
        (dump_reason cx reason_prop)
        (dump_reason cx reason_obj)
        (string_of_use_op use_op)
        (match suggestion with
        | Some prop -> spf "Some %s" prop
        | None -> "None")
    | EPropNotReadable { reason_prop; prop_name; use_op } ->
      spf
        "EPropNotReadable { reason_prop = %s; prop_name = %s; use_op = %s }"
        (dump_reason cx reason_prop)
        (match prop_name with
        | Some x -> spf "%S" (display_string_of_name x)
        | None -> "(computed)")
        (string_of_use_op use_op)
    | EPropNotWritable { reason_prop; prop_name; use_op } ->
      spf
        "EPropNotWritable { reason_prop = %s; prop_name = %s; use_op = %s }"
        (dump_reason cx reason_prop)
        (match prop_name with
        | Some x -> spf "%S" (display_string_of_name x)
        | None -> "(computed)")
        (string_of_use_op use_op)
    | EPropPolarityMismatch ((reason1, reason2), x, _, _) ->
      spf
        "EPropPolarityMismatch ((%s, %s), %s, _, _)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        (match x with
        | Some x -> spf "%S" (display_string_of_name x)
        | None -> "(computed)")
    | EPolarityMismatch { reason; name; expected_polarity; actual_polarity } ->
      spf
        "EPolarityMismatch { reason=%s; name=%S; expected_polarity=%s; actual_polarity=%s }"
        (dump_reason cx reason)
        name
        (Polarity.string expected_polarity)
        (Polarity.string actual_polarity)
    | EBuiltinLookupFailed { reason; name } ->
      spf
        "EBuiltinLookupFailed { reason = %s; name = %S }"
        (dump_reason cx reason)
        (match name with
        | Some x -> spf "Some(%S)" (Reason.display_string_of_name x)
        | None -> "None")
    | EStrictLookupFailed { reason_prop; reason_obj; name; suggestion; use_op } ->
      spf
        "EStrictLookupFailed { reason_prop = %s; reason_obj = %s; name = %S; suggestion = %S; use_op = %s }"
        (dump_reason cx reason_prop)
        (dump_reason cx reason_obj)
        (match name with
        | Some x -> spf "Some(%S)" (display_string_of_name x)
        | None -> "None")
        (match suggestion with
        | Some x -> spf "Some(%S)" x
        | None -> "None")
        (match use_op with
        | Some use_op -> spf "Some(%s)" (string_of_use_op use_op)
        | None -> "None")
    | EPrivateLookupFailed ((reason1, reason2), x, use_op) ->
      spf
        "EPrivateLookupFailed ((%s, %s), %s, %s)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        (display_string_of_name x)
        (string_of_use_op use_op)
    | EAdditionMixed (reason, use_op) ->
      spf "EAdditionMixed (%s, %s)" (dump_reason cx reason) (string_of_use_op use_op)
    | EComparison (reason1, reason2) ->
      spf "EComparison (%s, %s)" (dump_reason cx reason1) (dump_reason cx reason2)
    | ENonStrictEqualityComparison (reason1, reason2) ->
      spf "ENonStrictEqualityComparison (%s, %s)" (dump_reason cx reason1) (dump_reason cx reason2)
    | ETupleArityMismatch ((reason1, reason2), arity1, arity2, use_op) ->
      spf
        "ETupleArityMismatch (%s, %s, %d, %d, %s)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        arity1
        arity2
        (string_of_use_op use_op)
    | ENonLitArrayToTuple ((reason1, reason2), use_op) ->
      spf
        "ENonLitArrayToTuple ((%s, %s), %s)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        (string_of_use_op use_op)
    | ETupleOutOfBounds { use_op; reason; reason_op; length; index } ->
      spf
        "ETupleOutOfBounds { use_op = %s; reason = %s; reason_op = %s; length = %d; index = %s }"
        (string_of_use_op use_op)
        (dump_reason cx reason)
        (dump_reason cx reason_op)
        length
        index
    | ETupleNonIntegerIndex { use_op; reason; index } ->
      spf
        "ETupleNonIntegerIndex { use_op = %s; reason = %s; index = %s }"
        (string_of_use_op use_op)
        (dump_reason cx reason)
        index
    | ETupleUnsafeWrite { reason; use_op } ->
      spf
        "ETupleUnsafeWrite { reason = %s; use_op = %s }"
        (dump_reason cx reason)
        (string_of_use_op use_op)
    | EROArrayWrite ((reason1, reason2), use_op) ->
      spf
        "EROArrayWrite (%s, %s, %s)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        (string_of_use_op use_op)
    | EUnionSpeculationFailed { use_op; reason; reason_op; branches = _ } ->
      spf
        "EUnionSpeculationFailed { use_op = %s; reason = %s; reason_op = %s; branches = _ }"
        (string_of_use_op use_op)
        (dump_reason cx reason)
        (dump_reason cx reason_op)
    | ESpeculationAmbiguous { reason; _ } ->
      spf "ESpeculationAmbiguous { reason = %s; _ }" (dump_reason cx reason)
    | EIncompatibleWithExact ((reason1, reason2), use_op, _) ->
      spf
        "EIncompatibleWithExact ((%s, %s), %s)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        (string_of_use_op use_op)
    | EFunctionIncompatibleWithIndexer ((reason1, reason2), use_op) ->
      spf
        "EFunctionIncompatibleWithIndexer((%s, %s), %s)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        (string_of_use_op use_op)
    | EUnsupportedExact (reason1, reason2) ->
      spf "EUnsupportedExact (%s, %s)" (dump_reason cx reason1) (dump_reason cx reason2)
    | EIdxArity reason -> spf "EIdxArity (%s)" (dump_reason cx reason)
    | EIdxUse1 reason -> spf "EIdxUse1 (%s)" (dump_reason cx reason)
    | EIdxUse2 reason -> spf "EIdxUse2 (%s)" (dump_reason cx reason)
    | EUnexpectedThisType loc -> spf "EUnexpectedThisType (%s)" (string_of_aloc loc)
    | ETypeParamArity (loc, expected) ->
      spf "ETypeParamArity (%s, %d)" (string_of_aloc loc) expected
    | ETypeParamMinArity (loc, expected) ->
      spf "ETypeParamMinArity (%s, %d)" (string_of_aloc loc) expected
    | ECallTypeArity { call_loc; is_new; reason_arity; expected_arity } ->
      spf
        "ECallTypeArity { call_loc=%s; is_new=%b; reason_arity=%s; expected_arity=%d; }"
        (string_of_aloc call_loc)
        is_new
        (dump_reason cx reason_arity)
        expected_arity
    | ETooManyTypeArgs (reason_tapp, reason_arity, maximum_arity) ->
      spf
        "ETooManyTypeArgs (%s, %s, %d)"
        (dump_reason cx reason_tapp)
        (dump_reason cx reason_arity)
        maximum_arity
    | ETooFewTypeArgs (reason_tapp, reason_arity, minimum_arity) ->
      spf
        "ETooFewTypeArgs (%s, %s, %d)"
        (dump_reason cx reason_tapp)
        (dump_reason cx reason_arity)
        minimum_arity
    | EInvalidTypeArgs (reason_tapp, reason_arity) ->
      spf "EInvalidTypeArgs (%s, %s)" (dump_reason cx reason_tapp) (dump_reason cx reason_arity)
    | EPropertyTypeAnnot loc -> spf "EPropertyTypeAnnot (%s)" (string_of_aloc loc)
    | EExportsAnnot loc -> spf "EExportsAnnot (%s)" (string_of_aloc loc)
    | ECharSetAnnot loc -> spf "ECharSetAnnot (%s)" (string_of_aloc loc)
    | EInvalidCharSet { invalid = (reason, _); valid; use_op } ->
      spf
        "EInvalidCharSet { invalid = (%s, _); valid = %s; use_op = %s }"
        (dump_reason cx reason)
        (dump_reason cx valid)
        (string_of_use_op use_op)
    | EUnsupportedKeyInObjectType loc -> spf "EUnsupportedKeyInObjectType (%s)" (string_of_aloc loc)
    | EPredAnnot loc -> spf "EPredAnnot (%s)" (string_of_aloc loc)
    | ERefineAnnot loc -> spf "ERefineAnnot (%s)" (string_of_aloc loc)
    | ETrustedAnnot loc -> spf "ETrustedAnnot (%s)" (string_of_aloc loc)
    | EPrivateAnnot loc -> spf "EPrivateAnnot (%s)" (string_of_aloc loc)
    | EUnexpectedTypeof loc -> spf "EUnexpectedTypeof (%s)" (string_of_aloc loc)
    | EFunPredCustom ((reason1, reason2), msg) ->
      spf "EFunPredCustom (%s, %s, %S)" (dump_reason cx reason1) (dump_reason cx reason2) msg
    | EIncompatibleWithShape (lower, upper, use_op) ->
      spf
        "EIncompatibleWithShape (%s, %s, %s)"
        (dump_reason cx lower)
        (dump_reason cx upper)
        (string_of_use_op use_op)
    | EInternal (loc, err) ->
      spf "EInternal (%s, %s)" (string_of_aloc loc) (dump_internal_error err)
    | EUnsupportedSyntax (loc, _) -> spf "EUnsupportedSyntax (%s, _)" (string_of_aloc loc)
    | EUseArrayLiteral loc -> spf "EUseArrayLiteral (%s)" (string_of_aloc loc)
    | EMissingAnnotation (reason, _) -> spf "EMissingAnnotation (%s)" (dump_reason cx reason)
    | EMissingLocalAnnotation reason -> spf "EMissingLocalAnnotation (%s)" (dump_reason cx reason)
    | EBindingError (_binding_error, loc, x, entry) ->
      spf
        "EBindingError (_, %s, %s, %s)"
        (string_of_aloc loc)
        (Reason.display_string_of_name x)
        (Scope.Entry.string_of_kind entry)
    | ERecursionLimit (reason1, reason2) ->
      spf "ERecursionLimit (%s, %s)" (dump_reason cx reason1) (dump_reason cx reason2)
    | EModuleOutsideRoot (loc, name) -> spf "EModuleOutsideRoot (%s, %S)" (string_of_aloc loc) name
    | EMalformedPackageJson (loc, error) ->
      spf "EMalformedPackageJson (%s, %S)" (string_of_aloc loc) error
    | EUnsafeGetSet loc -> spf "EUnsafeGetSet (%s)" (string_of_aloc loc)
    | EUninitializedInstanceProperty (loc, err) ->
      spf
        "EUninitializedInstanceProperty (%s, %s)"
        (string_of_aloc loc)
        Lints.(
          match err with
          | PropertyNotDefinitelyInitialized -> "PropertyNotDefinitelyInitialized"
          | ReadFromUninitializedProperty -> "ReadFromUninitializedProperty"
          | MethodCallBeforeEverythingInitialized -> "MethodCallBeforeEverythingInitialized"
          | PropertyFunctionCallBeforeEverythingInitialized ->
            "PropertyFunctionCallBeforeEverythingInitialized"
          | ThisBeforeEverythingInitialized -> "ThisBeforeEverythingInitialized")
    | EExperimentalEnums loc -> spf "EExperimentalEnums (%s)" (string_of_aloc loc)
    | EExperimentalEnumsWithUnknownMembers loc ->
      spf "EExperimentalEnumsWithUnknownMembers (%s)" (string_of_aloc loc)
    | EExperimentalThisAnnot loc -> spf "EExperimentalThisAnnot (%s)" (string_of_aloc loc)
    | EIndexedAccessNotEnabled loc -> spf "EIndexedAccessNotEnabled (%s)" (string_of_aloc loc)
    | EIndeterminateModuleType loc -> spf "EIndeterminateModuleType (%s)" (string_of_aloc loc)
    | EBadExportPosition loc -> spf "EBadExportPosition (%s)" (string_of_aloc loc)
    | EBadExportContext (name, loc) -> spf "EBadExportContext (%s, %s)" name (string_of_aloc loc)
    | EBadDefaultImportAccess (loc, reason) ->
      spf "EBadDefaultImportAccess (%s, %s)" (string_of_aloc loc) (dump_reason cx reason)
    | EBadDefaultImportDestructuring loc ->
      spf "EBadDefaultImportDestructuring (%s)" (string_of_aloc loc)
    | EInvalidImportStarUse (loc, reason) ->
      spf "EInvalidImportStarUse (%s, %s)" (string_of_aloc loc) (dump_reason cx reason)
    | ENonConstVarExport (loc, reason) ->
      spf
        "ENonConstVarExport (%s, %s)"
        (string_of_aloc loc)
        (Base.Option.value_map ~f:(dump_reason cx) ~default:"None" reason)
    | EThisInExportedFunction loc -> spf "EThisInExportedFunction (%s)" (string_of_aloc loc)
    | EMixedImportAndRequire (loc, reason) ->
      spf "EMixedImportAndRequire (%s, %s)" (string_of_aloc loc) (dump_reason cx reason)
    | EToplevelLibraryImport loc -> spf "EToplevelLibraryImport (%s)" (string_of_aloc loc)
    | EExportRenamedDefault { loc; name; is_reexport } ->
      spf
        "EExportRenamedDefault { loc = %s; name = %s; is_reexport = %B }"
        (string_of_aloc loc)
        (Base.Option.value ~default:"None" name)
        is_reexport
    | EUnreachable loc -> spf "EUnreachable (%s)" (string_of_aloc loc)
    | EInvalidObjectKit { reason; reason_op; use_op } ->
      spf
        "EInvalidObjectKit { reason = %s; reason_op = %s; use_op = %s }"
        (dump_reason cx reason)
        (dump_reason cx reason_op)
        (string_of_use_op use_op)
    | EInvalidTypeof (loc, name) -> spf "EInvalidTypeof (%s, %S)" (string_of_aloc loc) name
    | EBinaryInLHS reason -> spf "EBinaryInLHS (%s)" (dump_reason cx reason)
    | EBinaryInRHS reason -> spf "EBinaryInRHS (%s)" (dump_reason cx reason)
    | EArithmeticOperand reason -> spf "EArithmeticOperand (%s)" (dump_reason cx reason)
    | EForInRHS reason -> spf "EForInRHS (%s)" (dump_reason cx reason)
    | EInstanceofRHS reason -> spf "EInstanceofRHS (%s)" (dump_reason cx reason)
    | EObjectComputedPropertyAccess (reason1, reason2) ->
      spf "EObjectComputedPropertyAccess (%s, %s)" (dump_reason cx reason1) (dump_reason cx reason2)
    | EObjectComputedPropertyAssign (reason1, reason2) ->
      spf "EObjectComputedPropertyAssign (%s, %s)" (dump_reason cx reason1) (dump_reason cx reason2)
    | EInvalidLHSInAssignment loc -> spf "EInvalidLHSInAssignment (%s)" (string_of_aloc loc)
    | EIncompatibleWithUseOp { reason_lower; reason_upper; use_op } ->
      spf
        "EIncompatibleWithUseOp { reason_lower = %s; reason_upper = %s; use_op = %s }"
        (dump_reason cx reason_lower)
        (dump_reason cx reason_upper)
        (string_of_use_op use_op)
    | ETrustIncompatibleWithUseOp (reason1, reason2, use_op) ->
      spf
        "ETrustIncompatibleWithUseOp (%s, %s, %s)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        (string_of_use_op use_op)
    | EUnsupportedImplements reason -> spf "EUnsupportedImplements (%s)" (dump_reason cx reason)
    | ENotAReactComponent { reason; use_op } ->
      spf
        "ENotAReactComponent { reason = %s; use_op = %s }"
        (dump_reason cx reason)
        (string_of_use_op use_op)
    | EInvalidReactConfigType { reason; use_op } ->
      spf
        "EInvalidReactConfigType { reason = %s; use_op = %s }"
        (dump_reason cx reason)
        (string_of_use_op use_op)
    | EInvalidReactPropType { reason; use_op; tool = _ } ->
      spf
        "EInvalidReactPropType { reason = %s; use_op = %s; _ }"
        (dump_reason cx reason)
        (string_of_use_op use_op)
    | EInvalidReactCreateClass { reason; use_op; tool = _ } ->
      spf
        "EInvalidReactCreateClass { reason = %s; use_op = %s; _ }"
        (dump_reason cx reason)
        (string_of_use_op use_op)
    | EReactElementFunArity (reason, _, _) ->
      spf "EReactElementFunArity (%s)" (dump_reason cx reason)
    | EFunctionCallExtraArg (unused_reason, def_reason, param_count, use_op) ->
      spf
        "EFunctionCallExtraArg (%s, %s, %d, %s)"
        (dump_reason cx unused_reason)
        (dump_reason cx def_reason)
        param_count
        (string_of_use_op use_op)
    | EUnsupportedSetProto reason -> spf "EUnsupportedSetProto (%s)" (dump_reason cx reason)
    | EEscapedGeneric { reason; use_op; bound_name; _ } ->
      spf
        "EEscapedGeneric { reason = %s; use_op = %s; bound_name = %s; _ }"
        (dump_reason cx reason)
        (string_of_use_op use_op)
        bound_name
    | EDuplicateModuleProvider { module_name; provider; conflict } ->
      spf
        "EDuplicateModuleProvider (%S, %s, %s)"
        module_name
        (string_of_aloc provider)
        (string_of_aloc conflict)
    | EParseError (loc, _parse_error) -> spf "EParseError (%s, _)" (string_of_aloc loc)
    (* TODO: string of parse error constructor *)
    | EDocblockError (loc, err) ->
      spf
        "EDocblockError (%s, %s)"
        (string_of_aloc loc)
        (match err with
        | MultipleFlowAttributes -> "MultipleFlowAttributes"
        | MultipleProvidesModuleAttributes -> "MultipleProvidesModuleAttributes"
        | MultipleJSXAttributes -> "MultipleJSXAttributes"
        | InvalidJSXAttribute _ -> "InvalidJSXAttribute")
    | EImplicitInexactObject loc -> spf "EImplicitInexactObject (%s)" (string_of_aloc loc)
    | EAmbiguousObjectType loc -> spf "EAmbiguousObjectType (%s)" (string_of_aloc loc)
    | EUntypedTypeImport (loc, module_name) ->
      spf "EUntypedTypeImport (%s, %s)" (string_of_aloc loc) module_name
    | EUntypedImport (loc, module_name) ->
      spf "EUntypedImport (%s, %s)" (string_of_aloc loc) module_name
    | ENonstrictImport loc -> spf "ENonstrictImport (%s)" (string_of_aloc loc)
    | EUnclearType loc -> spf "EUnclearType (%s)" (string_of_aloc loc)
    | EDeprecatedUtility (loc, name) -> spf "EDeprecatedUtility (%s, %s)" (string_of_aloc loc) name
    | EDeprecatedType loc -> spf "EDeprecatedType (%s)" (string_of_aloc loc)
    | EUnsafeGettersSetters loc -> spf "EUnclearGettersSetters (%s)" (string_of_aloc loc)
    | EUnusedSuppression loc -> spf "EUnusedSuppression (%s)" (string_of_aloc loc)
    | ECodelessSuppression (loc, c) -> spf "ECodelessSuppression (%s, %s)" (string_of_aloc loc) c
    | ELintSetting (loc, kind) ->
      LintSettings.(
        let kind_str =
          match kind with
          | Invalid_setting -> "Invalid_setting"
          | Malformed_argument -> "Malformed_argument"
          | Naked_comment -> "Naked_comment"
          | Nonexistent_rule -> "Nonexistent_rule"
          | Overwritten_argument -> "Overwritten_argument"
          | Redundant_argument -> "Redundant_argument"
        in
        spf "ELintSetting (%s, %s)" (string_of_aloc loc) kind_str)
    | ESketchyNullLint { kind; loc; null_loc; falsy_loc } ->
      Lints.(
        let kind_str =
          match kind with
          | SketchyNullBool -> "SketchyNullBool"
          | SketchyNullString -> "SketchyNullString"
          | SketchyNullNumber -> "SketchyNullNumber"
          | SketchyNullMixed -> "SketchyNullMixed"
          | SketchyNullEnumBool -> "SketchyNullEnumBool"
          | SketchyNullEnumString -> "SketchyNullEnumString"
          | SketchyNullEnumNumber -> "SketchyNullEnumNumber"
        in
        spf
          "ESketchyNullLint {kind=%s; loc=%s; null_loc=%s; falsy_loc=%s}"
          kind_str
          (string_of_aloc loc)
          (string_of_aloc null_loc)
          (string_of_aloc falsy_loc))
    | ESketchyNumberLint (kind, reason) ->
      Lints.(
        let kind_str =
          match kind with
          | SketchyNumberAnd -> "SketchyNumberAnd"
        in
        spf "ESketchyNumberLint (%s) (%s)" kind_str (dump_reason cx reason))
    | EInvalidPrototype (loc, reason) ->
      spf "EInvalidPrototype (%s) (%s)" (string_of_aloc loc) (dump_reason cx reason)
    | EUnnecessaryOptionalChain (loc, _) ->
      spf "EUnnecessaryOptionalChain (%s)" (string_of_aloc loc)
    | EUnnecessaryInvariant (loc, _) -> spf "EUnnecessaryInvariant (%s)" (string_of_aloc loc)
    | EUnexpectedTemporaryBaseType loc ->
      spf "EUnexpectedTemporaryBaseType (%s)" (string_of_aloc loc)
    | ECannotDelete (l1, r1) -> spf "ECannotDelete (%s, %s)" (string_of_aloc l1) (dump_reason cx r1)
    | ESignatureVerification sve ->
      let msg = string_of_signature_error ALoc.debug_to_string sve in
      spf "ESignatureVerification (%s)" msg
    | EBigIntNotYetSupported reason -> spf "EBigIntNotYetSupported (%s)" (dump_reason cx reason)
    | ECannotSpreadInterface { spread_reason; interface_reason; use_op } ->
      spf
        "ECannotSpreadInterface (%s) (%s) (%s)"
        (dump_reason cx spread_reason)
        (dump_reason cx interface_reason)
        (string_of_use_op use_op)
    | ECannotSpreadIndexerOnRight { spread_reason; object_reason; key_reason; use_op } ->
      spf
        "ECannotSpreadIndexerOnRight (%s) (%s) (%s) (%s)"
        (dump_reason cx spread_reason)
        (dump_reason cx object_reason)
        (dump_reason cx key_reason)
        (string_of_use_op use_op)
    | EUnableToSpread
        { spread_reason; object1_reason; object2_reason; propname; error_kind = _; use_op } ->
      spf
        "EUnableToSpread (%s) (%s) (%s) (%s) (%s)"
        (dump_reason cx spread_reason)
        (dump_reason cx object1_reason)
        (dump_reason cx object2_reason)
        (display_string_of_name propname)
        (string_of_use_op use_op)
    | EInexactMayOverwriteIndexer
        { spread_reason; key_reason; value_reason; object2_reason; use_op } ->
      spf
        "EInexactMayOverwriteIndexer (%s) (%s) (%s) (%s) (%s)"
        (dump_reason cx spread_reason)
        (dump_reason cx key_reason)
        (dump_reason cx value_reason)
        (dump_reason cx object2_reason)
        (string_of_use_op use_op)
    | EExponentialSpread { reason; reasons_for_operand1; reasons_for_operand2 } ->
      let format_reason_group { first_reason; second_reason } =
        spf
          "[%s; %s]"
          (dump_reason cx first_reason)
          (Base.Option.value_map ~default:"None" ~f:(dump_reason cx) second_reason)
      in
      spf
        "EExponentialSpread %s ([%s]) ([%s])"
        (dump_reason cx reason)
        (format_reason_group reasons_for_operand1)
        (format_reason_group reasons_for_operand2)
    | EComputedPropertyWithMultipleLowerBounds
        { computed_property_reason; new_lower_bound_reason; existing_lower_bound_reason } ->
      spf
        "EComputedPropertyWithMultipleLowerBounds (%s) (%s) (%s)"
        (dump_reason cx computed_property_reason)
        (dump_reason cx new_lower_bound_reason)
        (dump_reason cx existing_lower_bound_reason)
    | EComputedPropertyWithUnion { computed_property_reason; union_reason } ->
      spf
        "EComputedPropertyWithUnion (%s) (%s)"
        (dump_reason cx computed_property_reason)
        (dump_reason cx union_reason)
    | EEnumInvalidMemberAccess { member_name; suggestion; reason; enum_reason } ->
      spf
        "EEnumInvalidMemberAccess (%s) (%s) (%s) (%s)"
        (Base.Option.value_map ~default:"<None>" ~f:display_string_of_name member_name)
        (Base.Option.value ~default:"<None>" suggestion)
        (dump_reason cx reason)
        (dump_reason cx enum_reason)
    | EEnumModification { loc; enum_reason } ->
      spf "EEnumModification (%s) (%s)" (string_of_aloc loc) (dump_reason cx enum_reason)
    | EEnumMemberDuplicateValue { loc; prev_use_loc; enum_reason } ->
      spf
        "EEnumMemberDuplicateValue (%s) (%s) (%s)"
        (string_of_aloc loc)
        (string_of_aloc prev_use_loc)
        (dump_reason cx enum_reason)
    | EEnumInvalidObjectUtil { reason; enum_reason } ->
      spf "EEnumInvalidObjectUtil (%s) (%s)" (dump_reason cx reason) (dump_reason cx enum_reason)
    | EEnumNotIterable { reason; for_in } ->
      spf "EEnumNotIterable (%s) (%s)" (dump_reason cx reason) (spf "for_in = %B" for_in)
    | EEnumMemberAlreadyChecked { reason; prev_check_reason; enum_reason; member_name } ->
      spf
        "EEnumMemberAlreadyChecked (%s) (%s) (%s) (%s)"
        (dump_reason cx reason)
        (dump_reason cx prev_check_reason)
        (dump_reason cx enum_reason)
        member_name
    | EEnumAllMembersAlreadyChecked { reason; enum_reason } ->
      spf
        "EEnumAllMembersAlreadyChecked (%s) (%s)"
        (dump_reason cx reason)
        (dump_reason cx enum_reason)
    | EEnumNotAllChecked { reason; enum_reason; left_to_check; default_case } ->
      spf
        "EEnumNotAllChecked (%s) (%s) (%s) (%s)"
        (dump_reason cx reason)
        (dump_reason cx enum_reason)
        (String.concat ", " left_to_check)
        (Base.Option.value_map ~default:"<None>" ~f:(dump_reason cx) default_case)
    | EEnumUnknownNotChecked { reason; enum_reason } ->
      spf "EEnumUnknownNotChecked (%s) (%s)" (dump_reason cx reason) (dump_reason cx enum_reason)
    | EEnumInvalidCheck { reason; enum_reason; example_member } ->
      spf
        "EEnumInvalidCheck (%s) (%s) (%s)"
        (dump_reason cx reason)
        (dump_reason cx enum_reason)
        (Base.Option.value ~default:"<None>" example_member)
    | EEnumMemberUsedAsType { reason; enum_reason } ->
      spf "EEnumMemberUsedAsType (%s) (%s)" (dump_reason cx reason) (dump_reason cx enum_reason)
    | EEnumIncompatible { reason_lower; reason_upper; use_op; representation_type } ->
      spf
        "EEnumIncompatible { reason_lower = %s; reason_upper = %s; use_op = %s; representation_type = %s }"
        (dump_reason cx reason_lower)
        (dump_reason cx reason_upper)
        (string_of_use_op use_op)
        (Base.Option.value ~default:"<None>" representation_type)
    | EAssignExportedConstLikeBinding { loc; definition; binding_kind } ->
      spf
        "EAssignExportedConstLikeBinding (%s) (%s) (%s)"
        (string_of_aloc loc)
        (dump_reason cx definition)
        (Scope.Entry.string_of_let_binding_kind binding_kind)
    | ECannotResolveOpenTvar { use_op; reason; blame_reasons } ->
      spf
        "ECannotResolveOpenTvar (%s) (%s) (%s)"
        (string_of_use_op use_op)
        (dump_reason cx reason)
        (ListUtils.to_string ", " (dump_reason cx) blame_reasons)
    | EMalformedCode loc -> spf "EMalformedCode (%s)" (string_of_aloc loc)
    | EImplicitInstantiationTemporaryError _ -> "EImplicitInstantiationTemporaryError"
    | EImportInternalReactServerModule loc ->
      spf "EImportInternalReactServerModule (%s)" (string_of_aloc loc)
    | EImplicitInstantiationUnderconstrainedError _ -> "EImplicitInstantiationUnderconstrainedError"
    | EClassToObject _ -> "EClassToObject"
    | EMethodUnbinding { use_op; reason_prop; reason_op } ->
      spf
        "EMethodUnbinding (%s) (%s) (%s)"
        (string_of_use_op use_op)
        (dump_reason cx reason_op)
        (dump_reason cx reason_prop)

module Verbose = struct
  let print_if_verbose_lazy cx trace ?(delim = "") ?(indent = 0) (lines : string list Lazy.t) =
    match Context.verbose cx with
    | Some { Verbose.indent = num_spaces; _ } ->
      let indent = max (indent + Trace.trace_depth trace - 1) 0 in
      let prefix = String.make (indent * num_spaces) ' ' in
      let pid = Context.pid_prefix cx in
      let add_prefix line = spf "\n%s%s%s" prefix pid line in
      let lines = Base.List.map ~f:add_prefix (Lazy.force lines) in
      prerr_endline (String.concat delim lines)
    | None -> ()

  let print_if_verbose cx trace ?(delim = "") ?(indent = 0) (lines : string list) =
    match Context.verbose cx with
    | Some _ -> print_if_verbose_lazy cx trace ~delim ~indent (lazy lines)
    | None -> ()

  let print_types_if_verbose cx trace ?(note : string option) ((l : Type.t), (u : Type.use_t)) =
    match Context.verbose cx with
    | Some { Verbose.depth; _ } ->
      let delim =
        match note with
        | Some x -> spf " ~> %s" x
        | None -> " ~>"
      in
      print_if_verbose cx trace ~delim [dump_t ~depth cx l; dump_use_t ~depth cx u]
    | None -> ()
end
