(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type
open TypeUtil
open Utils_js

let string_of_union_enum = function
  | UnionEnum.Str x -> spf "string %s" (display_string_of_name x)
  | UnionEnum.Num x -> spf "number %f" x
  | UnionEnum.Bool x -> spf "boolean %b" x
  | UnionEnum.BigInt (_, x) -> spf "bigint %s" x
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
  | ReactDRO _ -> "ReactDRO"
  | MakeHooklike -> "MakeHooklike"
  | NonMaybeType -> "NonMaybeType"
  | PropertyType { name; _ } -> spf "PropertyType %s" (display_string_of_name name)
  | ElementType _ -> "ElementType"
  | OptionalIndexedAccessNonMaybeType _ -> "OptionalIndexedAccessNonMaybeType"
  | OptionalIndexedAccessResultType _ -> "OptionalIndexedAccessResultType"
  | ReadOnlyType -> "ReadOnly"
  | ReactCheckComponentConfig _ -> "ReactCheckComponentConfig"
  | ReactCheckComponentRef -> "ReactCheckComponentRef"
  | PartialType -> "PartialType"
  | RequiredType -> "RequiredType"
  | SpreadType _ -> "Spread"
  | SpreadTupleType _ -> "SpreadTupleType"
  | RestType _ -> "Rest"
  | ValuesType -> "Values"
  | CallType _ -> "CallType"
  | ConditionalType _ -> "ConditionalType"
  | TypeMap (TupleMap _) -> "TupleMap"
  | TypeMap (ObjectMap _) -> "ObjectMap"
  | TypeMap (ObjectMapi _) -> "ObjectMapi"
  | TypeMap ObjectKeyMirror -> "ObjectKeyMirror"
  | TypeMap (ObjectMapConst _) -> "ObjectMapConst"
  | ReactElementPropsType -> "ReactElementProps"
  | ReactElementConfigType -> "ReactElementConfig"
  | ReactPromoteRendersRepresentation _ -> "ReactPromoteRendersRepresentation"
  | ReactElementRefType -> "ReactElementRef"
  | ReactConfigType _ -> "ReactConfig"
  | MappedType _ -> "MappedType"

let string_of_destruct_kind = function
  | DestructAnnot -> "Annot"
  | DestructInfer -> "Infer"

let bool_of_sealtype = function
  | Object.Spread.Sealed -> true
  | _ -> false

(*****************************************************************)

(* debug printer *)

let dump_reason cx reason =
  let strip_root =
    if Context.should_strip_root cx then
      Some (Context.root cx)
    else
      None
  in
  Reason.dump_reason ~strip_root reason

let rec dump_t_ (depth, tvars) cx t =
  let p ?(reason = true) ?(extra = "") t =
    spf
      "%s (%s%s%s)"
      (string_of_ctor t)
      ( if reason then
        spf "%S" (dump_reason cx (reason_of_t t))
      else
        ""
      )
      ( if reason && extra <> "" then
        ", "
      else
        ""
      )
      extra
  in
  let kid = dump_t_ (depth - 1, tvars) cx in
  let tvar id = dump_tvar_ (depth - 1, tvars) cx id in
  let defer_use expr t =
    match expr with
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
    | CatchAny -> "CatchAny"
    | AnyError _ -> "Error"
    | Unsound _ -> "Unsound"
    | Untyped -> "Untyped"
    | Placeholder -> "Placeholder"
  in
  let custom_fun = function
    | ObjectAssign -> "ObjectAssign"
    | ObjectGetPrototypeOf -> "ObjectGetPrototypeOf"
    | ObjectSetPrototypeOf -> "ObjectSetPrototypeOf"
    | Compose false -> "Compose"
    | Compose true -> "ComposeReverse"
    | ReactCreateElement -> "ReactCreateElement"
    | ReactCloneElement -> "ReactCloneElement"
    | ReactElementFactory _ -> "ReactElementFactory"
    | DebugPrint -> "DebugPrint"
    | DebugThrow -> "DebugThrow"
    | DebugSleep -> "DebugSleep"
  in
  let instance_t { static = _; super = _; implements = _; inst = { class_id; type_args; _ } } =
    spf
      "[%s] #%s"
      (String.concat
         ", "
         (Base.List.map
            ~f:(fun (n, _, t, _) -> spf "%s=%s" (Subst_name.string_of_subst_name n) (kid t))
            type_args
         )
      )
      (ALoc.debug_to_string (class_id :> ALoc.t))
  in
  if depth = 0 then
    string_of_ctor t
  else
    match t with
    | OpenT (_, id) -> p ~extra:(tvar id) t
    | DefT (_, NumT lit) ->
      p
        ~extra:
          (match lit with
          | Literal (_, (_, raw)) -> raw
          | Truthy -> "truthy"
          | AnyLiteral -> "")
        t
    | DefT (_, StrT c) ->
      p
        ~extra:
          (match c with
          | Literal (_, s) -> spf "%S" (display_string_of_name s)
          | Truthy -> "truthy"
          | AnyLiteral -> "")
        t
    | DefT (_, BoolT c) ->
      p
        ~extra:
          (match c with
          | Some b -> spf "%B" b
          | None -> "")
        t
    | DefT (_, BigIntT lit) ->
      p
        ~extra:
          (match lit with
          | Literal (_, (_, raw)) -> raw
          | Truthy -> "truthy"
          | AnyLiteral -> "")
        t
    | DefT (_, FunT (_, { params; rest_param; return_t; this_t; predicate; _ })) ->
      p
        ~extra:
          (spf
             "<this: %s>(%s%s) => %s%s"
             (kid (fst this_t))
             (String.concat "; " (Base.List.map ~f:(fun (_, t) -> kid t) params))
             (Base.Option.value_map rest_param ~default:"" ~f:(fun (_, _, t) -> "..." ^ kid t))
             (kid return_t)
             (match predicate with
             | Some (PredBased _) -> " %checks"
             | Some (TypeGuardBased { param_name = (_, name); type_guard }) ->
               spf " %s is %s" name (kid type_guard)
             | None -> "")
          )
        t
    | AnyT (_, src) -> p ~extra:(string_of_any_source src) t
    | DefT (_, MixedT flavor) -> p ~extra:(string_of_mixed_flavor flavor) t
    | DefT (_, EmptyT)
    | DefT (_, SymbolT)
    | DefT (_, NullT)
    | DefT (_, VoidT) ->
      p t
    | NullProtoT _
    | ObjProtoT _
    | FunProtoT _
    | FunProtoApplyT _
    | FunProtoBindT _
    | FunProtoCallT _ ->
      p t
    | DefT (_, PolyT { tparams = tps; t_out = c; id; _ }) ->
      p
        ~extra:
          (spf
             "%s [%s] #%s"
             (kid c)
             (String.concat
                "; "
                (Base.List.map
                   ~f:(fun tp -> Subst_name.string_of_subst_name tp.name)
                   (Nel.to_list tps)
                )
             )
             (Poly.string_of_id id)
          )
        t
    | ThisInstanceT (_, inst_t, _, _) -> p ~extra:(instance_t inst_t) t
    | GenericT { name; bound; _ } ->
      p ~extra:(spf "%s: %s" (Subst_name.string_of_subst_name name) (kid bound)) t
    | DefT (_, ObjT { props_tmap; flags; _ }) ->
      let obj_kind =
        match flags.obj_kind with
        | Exact -> "Exact"
        | Inexact -> "Inexact"
        | Indexed { key; value; _ } ->
          spf "Indexed {[%s]: %s}" (p ~reason:false key) (p ~reason:false value)
      in
      p t ~extra:(spf "%s, %s" (Properties.string_of_id props_tmap) obj_kind)
    | DefT (_, ArrT (ArrayAT { elem_t; tuple_view = None; react_dro = _ })) ->
      p ~extra:(spf "Array %s" (kid elem_t)) t
    | DefT (_, ArrT (ArrayAT { elem_t; tuple_view = Some (elements, _arity); react_dro = _ })) ->
      p
        ~extra:
          (spf
             "Array %s, %s"
             (kid elem_t)
             (spf
                "[%s]"
                (String.concat
                   ", "
                   (Base.List.map ~f:(fun (TupleElement { t; _ }) -> kid t) elements)
                )
             )
          )
        t
    | DefT (_, ArrT (TupleAT { elements; _ })) ->
      p
        ~extra:
          (spf
             "Tuple [%s]"
             (String.concat ", " (Base.List.map ~f:(fun (TupleElement { t; _ }) -> kid t) elements))
          )
        t
    | DefT (_, ArrT (ROArrayAT (elemt, _))) -> p ~extra:(spf "ReadOnlyArray %s" (kid elemt)) t
    | DefT (_, CharSetT chars) -> p ~extra:(spf "<%S>" (String_utils.CharSet.to_string chars)) t
    | DefT (_, ClassT inst) -> p ~extra:(kid inst) t
    | DefT (_, InstanceT inst_t) -> p ~extra:(instance_t inst_t) t
    | DefT (_, TypeT (kind, arg)) ->
      p ~extra:(spf "%s, %s" (string_of_type_t_kind kind) (kid arg)) t
    | DefT
        (_, EnumT { enum_name; enum_id; members = _; representation_t = _; has_unknown_members = _ })
    | DefT
        ( _,
          EnumObjectT
            { enum_name; enum_id; members = _; representation_t = _; has_unknown_members = _ }
        ) ->
      p ~extra:(spf "enum %s #%s" enum_name (ALoc.debug_to_string (enum_id :> ALoc.t))) t
    | AnnotT (_, arg, use_desc) -> p ~extra:(spf "use_desc=%b, %s" use_desc (kid arg)) t
    | OpaqueT (_, { underlying_t; opaque_type_args; _ }) ->
      p
        ~extra:
          (spf
             "[%s]%s"
             (String.concat
                "; "
                (Base.List.map opaque_type_args ~f:(fun (n, _, t, _) ->
                     spf "%s=%s" (Subst_name.show n) (kid t)
                 )
                )
             )
             (Base.Option.value_map underlying_t ~default:"" ~f:(fun t -> spf " (%s)" (kid t)))
          )
        t
    | OptionalT { reason = _; type_ = arg; use_desc = _ } -> p ~extra:(kid arg) t
    | EvalT (arg, expr, id) ->
      p ~extra:(spf "%s, %s" (defer_use expr (kid arg)) (Eval.string_of_id id)) t
    | TypeAppT { reason = _; use_op = _; type_; targs; from_value = _; use_desc = _ } ->
      p ~extra:(spf "%s, [%s]" (kid type_) (String.concat "; " (Base.List.map ~f:kid targs))) t
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
             (UnionRep.string_of_specialization rep)
          )
        t
    | DefT (_, ReactAbstractComponentT _) -> p t
    | DefT (_, RendersT _) -> p t
    | MatchingPropT (_, _, arg) -> p ~extra:(kid arg) t
    | KeysT (_, arg) -> p ~extra:(kid arg) t
    | DefT (_, NumericStrKeyT (_, s)) -> p ~extra:s t
    | DefT (_, SingletonStrT s) -> p ~extra:(spf "%S" (display_string_of_name s)) t
    | DefT (_, SingletonNumT (_, s)) -> p ~extra:s t
    | DefT (_, SingletonBoolT b) -> p ~extra:(spf "%B" b) t
    | DefT (_, SingletonBigIntT (_, s)) -> p ~extra:s t
    | ModuleT
        {
          module_reason = _;
          module_export_types = { value_exports_tmap; type_exports_tmap; _ };
          module_is_strict = _;
          module_available_platforms = _;
        } ->
      let exports_tmap_to_string exports_tmap =
        Context.find_exports cx exports_tmap
        |> NameUtils.Map.bindings
        |> Base.List.map ~f:(fun (name, { preferred_def_locs = _; name_loc = _; type_ }) ->
               kid type_ |> spf "%s: %s" (display_string_of_name name)
           )
        |> String.concat ", "
      in
      p
        t
        ~extra:
          (spf
             "[%s] [%s]"
             (exports_tmap_to_string value_exports_tmap)
             (exports_tmap_to_string type_exports_tmap)
          )
    | NamespaceT { values_type; types_tmap } ->
      p t ~extra:(spf "values=%s, types=%s" (kid values_type) (Properties.string_of_id types_tmap))
    | InternalT (ExtendsT (_, l, u)) -> p ~extra:(spf "%s, %s" (kid l) (kid u)) t
    | CustomFunT (_, kind) -> p ~extra:(custom_fun kind) t
    | InternalT (ChoiceKitT _) -> p t
    | InternalT (EnforceUnionOptimized _) -> p t

and dump_use_t_ (depth, tvars) cx t =
  let p ?(reason = true) ?(extra = "") use_t =
    spf
      "%s (%s%s%s)"
      (string_of_use_ctor use_t)
      ( if reason then
        spf "%S" (dump_reason cx (reason_of_use_t use_t))
      else
        ""
      )
      ( if reason && extra <> "" then
        ", "
      else
        ""
      )
      extra
  in
  let kid t = dump_t_ (depth - 1, tvars) cx t in
  let use_kid use_t = dump_use_t_ (depth - 1, tvars) cx use_t in
  let tvar id = dump_tvar_ (depth - 1, tvars) cx id in
  let tout (reason, id) = spf "(%s, %s)" (string_of_reason reason) (tvar id) in
  let normalized_prop p = dump_normalized_prop_ (depth - 1, tvars) cx p in
  let string_of_use_op = string_of_use_op_rec in
  let call_arg_kid = function
    | Arg t -> kid t
    | SpreadArg t -> spf "...%s" (kid t)
  in
  let propref = function
    | Named { reason; name; from_indexed_access = _ } ->
      spf "%S %s" (dump_reason cx reason) (display_string_of_name name)
    | Computed t -> kid t
  in
  let lookup_kind = function
    | NonstrictReturning (default_opt, testid_opt) ->
      spf
        "Nonstrict%s%s"
        (Base.Option.value_map default_opt ~default:"" ~f:(fun (t, _) -> spf " returning %s" (kid t))
        )
        (Base.Option.value_map testid_opt ~default:"" ~f:(fun (id, _) -> spf " for test id %d" id))
    | Strict r -> spf "Strict %S" (dump_reason cx r)
  in
  let lookup_action = function
    | ReadProp { tout = (reason, tout); _ } ->
      spf "Read (%s, %s)" (string_of_reason reason) (tvar tout)
    | WriteProp { tin; _ } -> spf "Write %s" (kid tin)
    | LookupProp (op, p) -> spf "Lookup (%s, %s)" (string_of_use_op op) (normalized_prop p)
    | SuperProp (_, p) -> spf "Super %s" (normalized_prop p)
    | MatchProp { prop_t = tin; _ } -> spf "Match %s" (kid tin)
  in
  let specialize_cache cache =
    if cache then
      "Some cache"
    else
      "None"
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
    | SingletonCase (t, use_t) -> spf "(%s, %s)" (kid t) (use_kid use_t)
  in
  let react_kit =
    React.(
      function
      | CreateElement0
          { clone = _; config; children = (children, children_spread); tout; return_hint = _ }
      | CreateElement
          {
            clone = _;
            component = _;
            config;
            children = (children, children_spread);
            tout;
            targs = _;
            return_hint = _;
            record_monomorphized_result = _;
          } ->
        p
          ~extra:
            (spf
               "CreateElement (%s; %s%s) => %s"
               (kid config)
               (String.concat "; " (Base.List.map ~f:kid children))
               (match children_spread with
               | Some children_spread -> spf "; ...%s" (kid children_spread)
               | None -> "")
               (kid tout)
            )
          t
      | ConfigCheck config -> spf "ConfigCheck (%s)" (kid config)
      | GetProps tout -> spf "GetProps (%s)" (kid tout)
      | GetConfig tout -> spf "GetConfig (%s)" (kid tout)
      | GetConfigType (default_props, tout) ->
        spf "GetConfigType (%s, %s)" (kid default_props) (kid tout)
      | GetRef tout -> spf "GetRef (%s)" (kid tout)
    )
  in
  let slice
      {
        Object.reason = _;
        props;
        flags = { obj_kind; _ };
        generics = _;
        interface = _;
        reachable_targs = _;
      } =
    let xs =
      match obj_kind with
      | Indexed { dict_polarity = p; _ } -> [Polarity.sigil p ^ "[]"]
      | Exact
      | Inexact ->
        []
    in
    let xs =
      NameUtils.Map.fold
        (fun k { Object.prop_t; _ } xs ->
          let opt =
            match prop_t with
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
            NameUtils.Map.add
              k
              {
                Object.prop_t = t;
                is_own = true;
                is_method = false;
                polarity = Polarity.Neutral;
                key_loc = Type.Property.first_loc p;
              }
              acc
          | _ -> acc)
        prop_map
        NameUtils.Map.empty
    in
    let obj_kind =
      match dict with
      | None -> Exact
      | Some d -> Indexed d
    in
    let flags = { obj_kind; frozen = false; react_dro = None } in
    slice
      {
        Object.reason;
        props;
        flags;
        generics = Generic.spread_empty;
        interface = None;
        reachable_targs = [];
      }
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
        | Spread.InlineSlice { Spread.reason; prop_map; dict; generics = _; reachable_targs = _ } ->
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
            | Slice { Spread.reason; prop_map; dict; generics = _; reachable_targs = _ } ->
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
          spf "Spread (%s, %s)" target state
        )
      in
      let rest merge_mode state =
        Object.Rest.(
          spf
            "Rest ({merge_mode=%s}, %s)"
            (match merge_mode with
            | Sound -> "Sound"
            | IgnoreExactAndOwn -> "IgnoreExactAndOwn"
            | Omit -> "Omit"
            | ReactConfigMerge _ -> "ReactConfigMerge")
            (match state with
            | One t -> spf "One (%s)" (kid t)
            | Done o -> spf "Done (%s)" (resolved o))
        )
      in
      let react_props state =
        Object.ReactConfig.(
          spf
            "(%s)"
            (match state with
            | Config _ -> "Config"
            | Defaults _ -> "Defaults")
        )
      in
      let object_map prop_type = spf "ObjectMap {prop_type: %s}" (kid prop_type) in
      let tool = function
        | ReadOnly -> "ReadOnly"
        | Object.ReactCheckComponentConfig _ -> "ReactCheckComponentConfig"
        | Partial -> "Partial"
        | Required -> "Required"
        | ObjectRep -> "ObjectRep"
        | Spread (options, state) -> spread options state
        | Rest (options, state) -> rest options state
        | ReactConfig state -> react_props state
        | Object.ObjectMap { prop_type; mapped_type_flags = _; selected_keys_opt = _ } ->
          object_map prop_type
      in
      (fun a b -> spf "(%s, %s)" (resolve_tool a) (tool b))
    )
  in
  let method_action = function
    | CallM
        {
          methodcalltype =
            { meth_args_tlist; meth_tout = (call_r, call_tvar); meth_generic_this; _ };
          _;
        }
    | ChainM
        {
          methodcalltype =
            { meth_args_tlist; meth_tout = (call_r, call_tvar); meth_generic_this; _ };
          _;
        } ->
      spf
        "<this: %s>(%s) => (%s, %s)"
        (Base.Option.value_map ~f:kid ~default:"None" meth_generic_this)
        (String.concat "; " (Base.List.map ~f:call_arg_kid meth_args_tlist))
        (string_of_reason call_r)
        (tvar call_tvar)
    | NoMethodAction _ -> "NoMethodAction"
  in
  if depth = 0 then
    string_of_use_ctor t
  else
    match t with
    | UseT (use_op, OpenT (r, id)) ->
      spf "UseT (%s, OpenT (%S, %d))" (string_of_use_op use_op) (dump_reason cx r) id
    | UseT (use_op, (DefT (_, _) as t)) -> spf "UseT (%s, %s)" (string_of_use_op use_op) (kid t)
    | UseT (use_op, t) -> spf "UseT (%s, %s)" (string_of_use_op use_op) (kid t)
    | ArithT { use_op; rhs_t; result_t; _ } ->
      p ~extra:(spf "%s, %s, %s" (string_of_use_op use_op) (kid rhs_t) (kid result_t)) t
    | AndT (_, x, y) -> p ~extra:(spf "%s, %s" (kid x) (tout y)) t
    | ArrRestT (use_op, _, _, _) -> p ~extra:(string_of_use_op use_op) t
    | AssertBinaryInLHST _ -> p t
    | AssertBinaryInRHST _ -> p t
    | AssertForInRHST _ -> p t
    | AssertImportIsValueT _ -> p t
    | AssertInstanceofRHST _ -> p t
    | AssertNonComponentLikeT _ -> p t
    | AssertIterableT _ -> p t
    | BindT (use_op, _, _) -> p t ~extra:(string_of_use_op use_op)
    | CallElemT (_, _, _, _, _) -> p t
    | CallT
        {
          use_op;
          reason = _;
          call_action =
            Funcalltype { call_args_tlist; call_tout = (call_r, call_tvar); call_this_t; _ };
          return_hint = _;
        } ->
      p
        ~extra:
          (spf
             "%s, <this: %s>(%s) => (%s, %s)"
             (string_of_use_op use_op)
             (kid call_this_t)
             (String.concat "; " (Base.List.map ~f:call_arg_kid call_args_tlist))
             (string_of_reason call_r)
             (tvar call_tvar)
          )
        t
    | CallT { use_op; reason = _; call_action = ConcretizeCallee _; return_hint = _ } ->
      p ~extra:(spf "%s ConcretizeCallee" (string_of_use_op use_op)) t
    | CallLatentPredT { sense; _ } -> p t ~extra:(spf "sense=%b" sense)
    | ChoiceKitUseT (_, TryFlow (_, spec)) -> p ~extra:(try_flow spec) t
    | ChoiceKitUseT (_, FullyResolveType id) -> p ~extra:(tvar id) t
    | CJSExtractNamedExportsT _ -> p t
    | ComparatorT { arg; _ } -> p ~extra:(kid arg) t
    | ConstructorT _ -> p t
    | CopyNamedExportsT _ -> p t
    | CopyTypeExportsT _ -> p t
    | CheckUntypedImportT _ -> p t
    | DebugPrintT _ -> p t
    | DebugSleepT _ -> p t
    | ElemT (_use_op, _reason, obj, _access) -> p ~extra:(spf "obj: %s" (kid obj)) t
    | ConditionalT
        {
          distributive_tparam_name;
          infer_tparams;
          extends_t;
          true_t;
          false_t;
          tout = (_, tout_id);
          _;
        } ->
      p
        ~extra:
          (spf
             "%s[%s] extends %s ? %s : %s => %s"
             (Base.Option.value_map distributive_tparam_name ~default:"" ~f:(fun name ->
                  spf "distributive over %s: " (Subst_name.string_of_subst_name name)
              )
             )
             (String.concat
                "; "
                (Base.List.map infer_tparams ~f:(fun tp -> Subst_name.string_of_subst_name tp.name))
             )
             (kid extends_t)
             (kid true_t)
             (kid false_t)
             (tvar tout_id)
          )
        t
    | ExportNamedT
        { reason = _; value_exports_tmap; type_exports_tmap; export_kind = _; tout = arg } ->
      let tmap_to_string tmap =
        String.concat
          "; "
          (Base.List.map ~f:(fun (x, _) -> display_string_of_name x) (NameUtils.Map.bindings tmap))
      in
      p
        t
        ~extra:
          (spf
             "%s, {%s}, {%s}"
             (kid arg)
             (tmap_to_string value_exports_tmap)
             (tmap_to_string type_exports_tmap)
          )
    | ExportTypeT _ -> p t
    | ImplicitVoidReturnT _ -> p t
    | AssertExportIsTypeT _ -> p t
    | GetElemT
        {
          use_op = _;
          reason = _;
          id = _;
          from_annot;
          access_iterables;
          key_t;
          tout = (preason, ptvar);
        } ->
      p
        ~extra:
          (spf
             "%s, (%s, %s), from_annot=%b, access_iterables=%b"
             (kid key_t)
             (string_of_reason preason)
             (tvar ptvar)
             from_annot
             access_iterables
          )
        t
    | GetKeysT _ -> p t
    | GetValuesT _ -> p t
    | GetDictValuesT _ -> p t
    | GetPropT
        {
          use_op;
          reason = _;
          id = _;
          from_annot = _;
          propref = prop;
          tout = (preason, ptvar);
          hint = _;
        } ->
      p
        ~extra:
          (spf
             "%s, (%s), (%s, %s)"
             (string_of_use_op use_op)
             (propref prop)
             (string_of_reason preason)
             (tvar ptvar)
          )
        t
    | GetPrivatePropT (_, _, prop, _, _, (preason, ptvar)) ->
      p ~extra:(spf "(%s), (%s, %s)" prop (string_of_reason preason) (tvar ptvar)) t
    | GetProtoT (_, (_, arg)) -> p ~extra:(tvar arg) t
    | GetStaticsT (_, arg) -> p ~extra:(tvar arg) t
    | GetTypeFromNamespaceT { use_op; reason = _; prop_ref = (_, name); tout = (preason, ptvar) } ->
      p
        ~extra:
          (spf
             "%s, (%s), (%s, %s)"
             (string_of_use_op use_op)
             (display_string_of_name name)
             (string_of_reason preason)
             (tvar ptvar)
          )
        t
    | GuardT (pred, result, sink) ->
      p
        ~reason:false
        ~extra:(spf "%s, %s, %s" (string_of_predicate pred) (kid result) (tout sink))
        t
    | HasOwnPropT _ -> p t
    | PreprocessKitT _ -> p t
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
                    (Properties.Set.elements ids |> Base.List.map ~f:Properties.string_of_id)
                 ))
          )
        t
    | MakeExactT _ -> p t
    | MapTypeT _ -> p t
    | MethodT (_, _, _, prop, action) ->
      p ~extra:(spf "(%s, %s)" (propref prop) (method_action action)) t
    | PrivateMethodT (_, _, _, prop, _, _, action) ->
      p ~extra:(spf "(%s), (%s)" prop (method_action action)) t
    | MixinT (_, arg) -> p ~extra:(kid arg) t
    | NotT (_, arg) -> p ~extra:(tout arg) t
    | NullishCoalesceT (_, x, y) -> p ~extra:(spf "%s, %s" (kid x) (tout y)) t
    | ObjAssignToT (_, _, arg1, arg2, _) -> p t ~extra:(spf "%s, %s" (kid arg1) (kid arg2))
    | ObjAssignFromT (_, _, arg1, arg2, _) -> p t ~extra:(spf "%s, %s" (kid arg1) (kid arg2))
    | ObjRestT (_, xs, arg, _) -> p t ~extra:(spf "[%s], %s" (String.concat "; " xs) (kid arg))
    | ObjTestProtoT _ -> p t
    | ObjTestT _ -> p t
    | OptionalChainT { t_out; voided_out; _ } ->
      p ~extra:(spf "%s, %s" (use_kid t_out) (kid voided_out)) t
    | OptionalIndexedAccessT { index = OptionalIndexedAccessTypeIndex index_type; _ } ->
      p ~extra:(kid index_type) t
    | OptionalIndexedAccessT { index = OptionalIndexedAccessStrLitIndex name; _ } ->
      p ~extra:(display_string_of_name name) t
    | OrT (_, x, y) -> p ~extra:(spf "%s, %s" (kid x) (tout y)) t
    | PredicateT (pred, arg) ->
      p ~reason:false ~extra:(spf "%s, %s" (string_of_predicate pred) (tout arg)) t
    | ReactKitT (use_op, _, tool) ->
      p t ~extra:(spf "%s, %s" (string_of_use_op use_op) (react_kit tool))
    | ReactPropsToOut (_, props)
    | ReactInToProps (_, props) ->
      p ~extra:(kid props |> spf "%s") t
    | ReposLowerT (_, use_desc, arg) -> p t ~extra:(spf "use_desc=%b, %s" use_desc (use_kid arg))
    | ReposUseT (_, use_desc, use_op, arg) ->
      p t ~extra:(spf "use_desc=%b, %s" use_desc (use_kid (UseT (use_op, arg))))
    | ResolveSpreadT (use_op, _, { rrt_resolve_to; _ }) ->
      (match rrt_resolve_to with
      | ResolveSpreadsToTupleType (_, elem_t, tout)
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
    | SentinelPropTestT (_, l, sense, sentinel, result) ->
      p
        ~reason:false
        ~extra:(spf "%s, %b, %s, %s" (kid l) sense (string_of_sentinel sentinel) (tout result))
        t
    | SuperT _ -> p t
    | ImplementsT (_, arg) -> p ~reason:false ~extra:(kid arg) t
    | SetElemT (_, _, ix, _, etype, _) -> p ~extra:(spf "%s, %s" (kid ix) (kid etype)) t
    | SetPropT (use_op, _, prop, _, _, ptype, _) ->
      p ~extra:(spf "%s, (%s), %s" (string_of_use_op use_op) (propref prop) (kid ptype)) t
    | SetPrivatePropT (_, _, prop, _, _, _, _, ptype, _) ->
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
        ~extra:(spf "%s, %s, %s" (string_of_use_op use_op) (object_kit resolve_tool tool) (kid tout))
        t
    | TestPropT { use_op; reason = _; id = _; propref = prop; tout = (preason, ptvar); hint = _ } ->
      p
        ~extra:
          (spf
             "%s, (%s), (%s, %s)"
             (string_of_use_op use_op)
             (propref prop)
             (string_of_reason preason)
             (tvar ptvar)
          )
        t
    | ThisSpecializeT (_, this, _) -> p ~extra:(spf "%s" (kid this)) t
    | ToStringT { t_out; _ } -> p ~extra:(use_kid t_out) t
    | UnaryArithT _ -> p t
    | ValueToTypeReferenceT (use_op, reason, kind, tout) ->
      p
        ~extra:
          (spf
             "%s, %s, %s, %s"
             (string_of_use_op use_op)
             (string_of_reason reason)
             (string_of_type_t_kind kind)
             (kid tout)
          )
        t
    | VarianceCheckT (_, _, args, pol) ->
      p
        ~extra:
          (spf "[%s], %s" (String.concat "; " (Base.List.map ~f:kid args)) (Polarity.string pol))
        t
    | ConcretizeTypeAppsT _ -> p t
    | TypeCastT (_, arg) -> p ~reason:false ~extra:(kid arg) t
    | EnumCastT { use_op = _; enum = (reason, enum) } ->
      p ~reason:false ~extra:(kid (DefT (reason, EnumT enum))) t
    | EnumExhaustiveCheckT { check; _ } ->
      let check_str =
        match check with
        | EnumExhaustiveCheckPossiblyValid _ -> "EnumExhaustiveCheckPossiblyValid"
        | EnumExhaustiveCheckInvalid _ -> "EnumExhaustiveCheckInvalid"
      in
      p ~extra:check_str t
    | FilterOptionalT (_, arg) -> p ~reason:false ~extra:(kid arg) t
    | FilterMaybeT (_, arg) -> p ~reason:false ~extra:(kid arg) t
    | DeepReadOnlyT ((_, tv), _, _) -> p ~extra:(tvar tv) t
    | HooklikeT (_, tv) -> p ~extra:(tvar tv) t
    | ExtractReactRefT (_, arg) -> p ~reason:false ~extra:(kid arg) t
    | SealGenericT { name; cont = Lower (_, l); _ } ->
      p ~extra:(spf "%s <~ %s" (Subst_name.string_of_subst_name name) (kid l)) t
    | SealGenericT { name; cont = Upper u; _ } ->
      p ~extra:(spf "%s ~> %s" (Subst_name.string_of_subst_name name) (use_kid u)) t
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
             (kid tout)
          )
    | ExtendsUseT (_, _, nexts, l, u) ->
      p
        ~extra:(spf "[%s], %s, %s" (String.concat "; " (Base.List.map ~f:kid nexts)) (kid l) (kid u))
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
             (tvar tout)
          )
    | ResolveUnionT { resolved; unresolved; upper; id; _ } ->
      p
        t
        ~extra:
          (spf
             "%d [%s], [%s], %s"
             id
             (String.concat "; " (Base.List.map ~f:kid resolved))
             (String.concat "; " (Base.List.map ~f:kid unresolved))
             (use_kid upper)
          )
    | CheckUnusedPromiseT { reason; _ } -> spf "CheckUnusedPromiseT (%s)" (string_of_reason reason)
    | WriteComputedObjPropCheckT { reason; _ } ->
      spf "WriteComputedObjPropCheckT (%s)" (string_of_reason reason)
    | PromoteRendersRepresentationT _ -> "PromoteRendersRepresentationT"
    | ConvertEmptyPropsToMixedT _ -> "ConvertEmptyPropsToMixedT"
    | TryRenderTypePromotionT _ -> "TryRenderTypePromotionT"
    | ExitRendersT _ -> "ExitRendersT"
    | EvalTypeDestructorT { destructor = s; tout = (r, x); _ } ->
      p
        ~extra:(spf "%s on upper, (%s, %s)" (string_of_destructor s) (string_of_reason r) (tvar x))
        t

and dump_tvar_ (depth, tvars) cx id =
  if ISet.mem id tvars then
    spf "%d, ^" id
  else
    let stack = ISet.add id tvars in
    Constraint.(
      try
        match !(Context.find_tvar cx id) with
        | Goto goto -> spf "%d, Goto %d" id goto.parent
        | Root { constraints = Resolved t; _ } ->
          spf "%d, Resolved %s" id (dump_t_ (depth - 1, stack) cx t)
        | Root { constraints = FullyResolved (lazy t); _ } ->
          spf "%d, FullyResolved %s" id (dump_t_ (depth - 1, stack) cx t)
        | Root { constraints = Unresolved { lower; upper; _ }; _ } ->
          if lower = TypeMap.empty && upper = UseTypeMap.empty then
            spf "%d" id
          else
            spf
              "%d, [%s], [%s]"
              id
              (String.concat
                 "; "
                 (List.rev
                    (TypeMap.fold (fun t _ acc -> dump_t_ (depth - 1, stack) cx t :: acc) lower [])
                 )
              )
              (String.concat
                 "; "
                 (List.rev
                    (UseTypeMap.fold
                       (fun (use_t, _) _ acc -> dump_use_t_ (depth - 1, stack) cx use_t :: acc)
                       upper
                       []
                    )
                 )
              )
      with
      | Union_find.Tvar_not_found _ -> spf "Not Found: %d" id
    )

and dump_prop_ (depth, tvars) cx p =
  let kid t = dump_t_ (depth, tvars) cx t in
  match p with
  | Field { preferred_def_locs = _; key_loc = _; type_; polarity } ->
    spf "Field (%s) %s" (Polarity.string polarity) (kid type_)
  | Get { key_loc = _; type_ } -> spf "Get %s" (kid type_)
  | Set { key_loc = _; type_ } -> spf "Set %s" (kid type_)
  | GetSet { get_key_loc = _; get_type; set_key_loc = _; set_type } ->
    spf "Get %s Set %s" (kid get_type) (kid set_type)
  | Method { key_loc = _; type_ } -> spf "Method %s" (kid type_)

and dump_normalized_prop_ (depth, tvars) cx p =
  let kid t = dump_t_ (depth, tvars) cx t in
  let kid_opt = Base.Option.value_map ~default:"None" ~f:kid in
  match p with
  | OrdinaryField { type_; polarity } ->
    spf "OrdinaryField (%s) %s" (Polarity.string polarity) (kid type_)
  | SyntheticField { get_type; set_type } ->
    spf "SyntheticField(%s, %s)" (kid_opt get_type) (kid_opt set_type)

(* This is the type-dump debugging API.
   We should make sure these are not called recursively to avoid circumventing
   one of the termination mechanisms: depth or tvar-set.
*)
let dump_t ?(depth = 3) cx t = dump_t_ (depth, ISet.empty) cx t

let dump_use_t ?(depth = 3) cx t = dump_use_t_ (depth, ISet.empty) cx t

let dump_prop ?(depth = 3) cx p = dump_prop_ (depth, ISet.empty) cx p

let dump_normalized_prop ?(depth = 3) cx p = dump_normalized_prop_ (depth, ISet.empty) cx p

let dump_tvar ?(depth = 3) cx id = dump_tvar_ (depth, ISet.empty) cx id

let dump_flow ?(depth = 3) cx (l, u) =
  spf "Lower: %s ~>\n Upper: %s" (dump_t ~depth cx l) (dump_use_t ~depth cx u)

(*****************************************************)

(* types *)

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
    let root_str = File_path.to_string (Context.root cx) ^ Filename.dir_sep in
    if String.starts_with ~prefix:root_str filename then
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
    | UnexpectedModuleT _ -> "UnexpectedModuleT"
    | ReadOfUnreachedTvar _ -> "ReadOfUnreachedTvar"
    | ReadOfUnresolvedTvar _ -> "ReadOfUnresolvedTvar"
    | MethodNotAFunction -> "MethodNotAFunction"
    | OptionalMethod -> "OptionalMethod"
    | UnsupportedGuardPredicate _ -> "UnsupportedGuardPredicate"
    | PropertyDescriptorPropertyCannotBeRead -> "PropertyDescriptorPropertyCannotBeRead"
    | ForInLHS -> "ForInLHS"
    | ForOfLHS -> "ForOfLHS"
    | PropRefComputedOpen -> "PropRefComputedOpen"
    | PropRefComputedLiteral -> "PropRefComputedLiteral"
    | RestParameterNotIdentifierPattern -> "RestParameterNotIdentifierPattern"
    | InterfaceTypeSpread -> "InterfaceTypeSpread"
    | Error_message.DebugThrow -> "DebugThrow"
    | ParseJobException _ -> "ParseJobException"
    | CheckTimeout _ -> "CheckTimeout"
    | CheckJobException _ -> "CheckJobException"
    | UnexpectedAnnotationInference _ -> "UnexpectedAnnotationInference"
    | MissingEnvRead _ -> "MissingEnvRead"
    | MissingEnvWrite _ -> "MissingEnvWrite"
    | EnvInvariant _ -> "EnvInvariant"
    | ImplicitInstantiationInvariant _ -> "ImplicitInstantiationInvariant"
  in
  let dump_upper_kind = function
    | IncompatibleGetPropT _ -> "IncompatibleGetPropT"
    | IncompatibleSetPropT _ -> "IncompatibleSetPropT"
    | IncompatibleGetPrivatePropT -> "IncompatibleGetPrivatePropT"
    | IncompatibleSetPrivatePropT -> "IncompatibleSetPrivatePropT"
    | IncompatibleMethodT _ -> "IncompatibleMethodT"
    | IncompatibleCallT -> "IncompatibleCallT"
    | IncompatibleMixedCallT -> "IncompatibleMixedCallT"
    | IncompatibleGetElemT _ -> "IncompatibleGetElemT"
    | IncompatibleSetElemT _ -> "IncompatibleSetElemT"
    | IncompatibleCallElemT _ -> "IncompatibleCallElemT"
    | IncompatibleElemTOfArrT -> "IncompatibleElemTOfArrT"
    | IncompatibleObjAssignFromTSpread -> "IncompatibleObjAssignFromTSpread"
    | IncompatibleObjAssignFromT -> "IncompatibleObjAssignFromT"
    | IncompatibleObjRestT -> "IncompatibleObjRestT"
    | IncompatibleArrRestT -> "IncompatibleArrRestT"
    | IncompatibleSuperT -> "IncompatibleSuperT"
    | IncompatibleMixinT -> "IncompatibleMixinT"
    | IncompatibleSpecializeT -> "IncompatibleSpecializeT"
    | IncompatibleThisSpecializeT -> "IncompatibleThisSpecializeT"
    | IncompatibleVarianceCheckT -> "IncompatibleVarianceCheckT"
    | IncompatibleGetKeysT -> "IncompatibleGetKeysT"
    | IncompatibleHasOwnPropT _ -> "IncompatibleHasOwnPropT"
    | IncompatibleGetValuesT -> "IncompatibleGetValuesT"
    | IncompatibleUnaryArithT -> "IncompatibleUnaryArithT"
    | IncompatibleMapTypeTObject -> "IncompatibleMapTypeTObject"
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
    | ENoDefaultExport (reason, module_name, _) ->
      spf "ENoDefaultExport (%s, %s)" (dump_reason cx reason) module_name
    | EOnlyDefaultExport (reason, module_name, export_name) ->
      spf "EOnlyDefaultExport (%s, %s, %s)" (dump_reason cx reason) module_name export_name
    | ENoNamedExport (reason, module_name, export_name, _) ->
      spf "ENoNamedExport (%s, %s, %s)" (dump_reason cx reason) module_name export_name
    | EMissingTypeArgs { reason_op; reason_tapp; reason_arity; min_arity; max_arity } ->
      spf
        "EMissingTypeArgs { reason_op=%s; reason_tapp=%s; reason_arity=%s; min_arity=%d; max_arity=%d }"
        (dump_reason cx reason_op)
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
    | EExpectedBigIntLit { reason_lower; reason_upper; use_op } ->
      spf
        "EExpectedBigIntLit { reason_lower = %s; reason_upper = %s; use_op = %s }"
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
    | EBuiltinLookupFailed { reason; name; potential_generator } ->
      spf
        "EBuiltinLookupFailed { reason = %s; name = %S; potential_generator = %s }"
        (dump_reason cx reason)
        (Reason.display_string_of_name name)
        (match potential_generator with
        | Some generator -> spf "Some(%s)" generator
        | None -> "None")
    | EPrivateLookupFailed ((reason1, reason2), x, use_op) ->
      spf
        "EPrivateLookupFailed ((%s, %s), %s, %s)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        (display_string_of_name x)
        (string_of_use_op use_op)
    | EPlatformSpecificImplementationModuleLookupFailed { loc = _; name } ->
      spf "EPlatformSpecificImplementationModuleLookupFailed(%s)" name
    | EAdditionMixed (reason, use_op) ->
      spf "EAdditionMixed (%s, %s)" (dump_reason cx reason) (string_of_use_op use_op)
    | EComparison (reason1, reason2) ->
      spf "EComparison (%s, %s)" (dump_reason cx reason1) (dump_reason cx reason2)
    | ENonStrictEqualityComparison (reason1, reason2) ->
      spf "ENonStrictEqualityComparison (%s, %s)" (dump_reason cx reason1) (dump_reason cx reason2)
    | ETupleArityMismatch
        ((reason1, reason2), (num_req1, num_total1), (num_req2, num_total2), use_op) ->
      spf
        "ETupleArityMismatch (%s, %s, %d-%d, %d-%d, %s)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        num_req1
        num_total1
        num_req2
        num_total2
        (string_of_use_op use_op)
    | ENonLitArrayToTuple ((reason1, reason2), use_op) ->
      spf
        "ENonLitArrayToTuple ((%s, %s), %s)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        (string_of_use_op use_op)
    | ETupleRequiredAfterOptional { reason_tuple; reason_required; reason_optional } ->
      spf
        "ETupleRequiredAfterOptional {reason_tuple = %s; reason_required = %s; reason_optional = %s}"
        (dump_reason cx reason_tuple)
        (dump_reason cx reason_required)
        (dump_reason cx reason_optional)
    | ETupleInvalidTypeSpread { reason_spread; reason_arg } ->
      spf
        "ETupleInvalidTypeSpread {reason_spread = %s; reason_arg = %s}"
        (dump_reason cx reason_spread)
        (dump_reason cx reason_arg)
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
    | ETupleElementNotReadable { reason; index; name = _; use_op } ->
      spf
        "ETupleElementNotReadable { reason = %s; index = %s; use_op = %s }"
        (dump_reason cx reason)
        (string_of_int index)
        (string_of_use_op use_op)
    | ETupleElementNotWritable { reason; index; name = _; use_op } ->
      spf
        "ETupleElementNotWritable { reason = %s; index = %s; use_op = %s }"
        (dump_reason cx reason)
        (string_of_int index)
        (string_of_use_op use_op)
    | ETupleElementPolarityMismatch
        { index; reason_lower; polarity_lower; reason_upper; polarity_upper; use_op } ->
      spf
        "ETupleElementPolarityMismatch { index = %s; reason_lower = %s; polarity_lower = %s; reason_upper = %s; polarity_upper = %s; use_op = %s }"
        (string_of_int index)
        (dump_reason cx reason_lower)
        (Polarity.string polarity_lower)
        (dump_reason cx reason_upper)
        (Polarity.string polarity_upper)
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
    | EInvalidInfer loc -> spf "EInvalidInfer (%s)" (string_of_aloc loc)
    | EInvalidExtends reason -> spf "EInvalidExtends (%s)" (dump_reason cx reason)
    | EPropertyTypeAnnot loc -> spf "EPropertyTypeAnnot (%s)" (string_of_aloc loc)
    | EExportsAnnot loc -> spf "EExportsAnnot (%s)" (string_of_aloc loc)
    | ECharSetAnnot loc -> spf "ECharSetAnnot (%s)" (string_of_aloc loc)
    | EInvalidCharSet { invalid = (reason, _); valid; use_op } ->
      spf
        "EInvalidCharSet { invalid = (%s, _); valid = %s; use_op = %s }"
        (dump_reason cx reason)
        (dump_reason cx valid)
        (string_of_use_op use_op)
    | EInvalidRendersTypeArgument
        { loc; renders_variant; invalid_render_type_kind; invalid_type_reasons } ->
      spf
        "EInvalidRendersTypeArgument { loc = %s; renders_variant = %s, invalid_render_type_kind = %s; invalid_type_reasons = [%s] }"
        (string_of_aloc loc)
        (match renders_variant with
        | Flow_ast.Type.Renders.Normal -> "renders"
        | Flow_ast.Type.Renders.Maybe -> "renders?"
        | Flow_ast.Type.Renders.Star -> "renders*")
        (string_of_invalid_render_type_kind invalid_render_type_kind)
        (invalid_type_reasons
        |> Nel.to_list
        |> Base.List.map ~f:(dump_reason cx)
        |> Base.String.concat ~sep:", "
        )
    | EUnsupportedKeyInObject { loc; obj_kind; key_error_kind } ->
      let key_error_kind = Error_message.InvalidObjKey.str_of_kind key_error_kind in
      let obj_kind =
        match obj_kind with
        | `Type -> "type"
        | `Literal -> "literal"
      in
      spf "EUnsupportedKeyInObject (%s, %s, %s)" (string_of_aloc loc) obj_kind key_error_kind
    | EAmbiguousNumericKeyWithVariance loc ->
      spf "EAmbiguousNumericKeyWithVariance (%s)" (string_of_aloc loc)
    | EPredicateFuncTooShort { loc; _ } -> spf "EPredicateFuncTooShort (%s)" (string_of_aloc loc)
    | EFunPredInvalidIndex loc -> spf "EFunPredInvalidIndex (%s)" (string_of_aloc loc)
    | EPredicateFuncArityMismatch { use_op; _ } ->
      spf "EPredicateFuncArityMismatch (%s)" (string_of_use_op use_op)
    | EPredicateFuncIncompatibility { use_op; _ } ->
      spf "EPredicateFuncIncompatibility (%s)" (string_of_use_op use_op)
    | EPredicateInvalidParameter { pred_reason = r; _ } ->
      spf "EPredicateInvalidParameter (%s)" (dump_reason cx r)
    | ETypeGuardIndexMismatch { use_op; _ } ->
      spf "ETypeGuardIndexMismatch (%s)" (string_of_use_op use_op)
    | ETypeGuardParamUnbound _ -> "ETypeGuardParamUnbound"
    | ETypeGuardFunctionInvalidWrites _ -> "ETypeGuardFunctionInvalidWrites"
    | ETypeGuardFunctionParamHavoced _ -> "ETypeGuardFunctionParamHavoced"
    | ETypeGuardIncompatibleWithFunctionKind _ -> "ETypeGuardIncompatibleWithFunctionKind"
    | EInternal (loc, err) -> spf "EInternal (%s, %s)" (string_of_aloc loc) (dump_internal_error err)
    | EUnsupportedSyntax (loc, _) -> spf "EUnsupportedSyntax (%s, _)" (string_of_aloc loc)
    | EUseArrayLiteral loc -> spf "EUseArrayLiteral (%s)" (string_of_aloc loc)
    | EMissingAnnotation (reason, _) -> spf "EMissingAnnotation (%s)" (dump_reason cx reason)
    | EMissingLocalAnnotation { reason; _ } ->
      spf "EMissingLocalAnnotation (%s)" (dump_reason cx reason)
    | EBindingError (_binding_error, loc, x, entry_loc) ->
      spf
        "EBindingError (_, %s, %s, %s)"
        (string_of_aloc loc)
        (Reason.display_string_of_name x)
        (string_of_aloc entry_loc)
    | ERecursionLimit (reason1, reason2) ->
      spf "ERecursionLimit (%s, %s)" (dump_reason cx reason1) (dump_reason cx reason2)
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
          | ThisBeforeEverythingInitialized -> "ThisBeforeEverythingInitialized"
        )
    | EEnumsNotEnabled loc -> spf "EEnumsNotEnabled (%s)" (string_of_aloc loc)
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
    | EUnsupportedVarianceAnnotation (loc, s) ->
      spf "EUnsupportedVarianceAnnotation (%s, %s)" (string_of_aloc loc) s
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
    | EInvalidRef (loc, name) -> spf "EInvalidRef (%s, %S)" (string_of_aloc loc) name
    | EBinaryInLHS reason -> spf "EBinaryInLHS (%s)" (dump_reason cx reason)
    | EBinaryInRHS reason -> spf "EBinaryInRHS (%s)" (dump_reason cx reason)
    | EArithmeticOperand reason -> spf "EArithmeticOperand (%s)" (dump_reason cx reason)
    | EForInRHS reason -> spf "EForInRHS (%s)" (dump_reason cx reason)
    | EInstanceofRHS reason -> spf "EInstanceofRHS (%s)" (dump_reason cx reason)
    | EObjectComputedPropertyAccess (reason1, reason2, kind) ->
      let kind = Error_message.InvalidObjKey.str_of_kind kind in
      spf
        "EObjectComputedPropertyAccess (%s, %s, %s)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        kind
    | EObjectComputedPropertyAssign (reason1, reason2, kind) ->
      let kind = Error_message.InvalidObjKey.str_of_kind kind in
      spf
        "EObjectComputedPropertyAssign (%s, %s, %s)"
        (dump_reason cx reason1)
        (Base.Option.value_map ~f:(dump_reason cx) ~default:"none" reason2)
        kind
    | EInvalidLHSInAssignment loc -> spf "EInvalidLHSInAssignment (%s)" (string_of_aloc loc)
    | EIncompatibleWithUseOp { reason_lower; reason_upper; use_op } ->
      spf
        "EIncompatibleWithUseOp { reason_lower = %s; reason_upper = %s; use_op = %s }"
        (dump_reason cx reason_lower)
        (dump_reason cx reason_upper)
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
        | InvalidFlowMode _ -> "InvalidFlowMode"
        | MultipleJSXAttributes -> "MultipleJSXAttributes"
        | InvalidJSXAttribute _ -> "InvalidJSXAttribute"
        | MultipleJSXRuntimeAttributes -> "MultipleJSXRuntimeAttributes"
        | InvalidJSXRuntimeAttribute -> "InvalidJSXRuntimeAttribute"
        | InvalidSupportsPlatform _ -> "InvalidSupportsPlatform"
        | DisallowedSupportsPlatform -> "DisallowedSupportsPlatform")
    | EImplicitInexactObject loc -> spf "EImplicitInexactObject (%s)" (string_of_aloc loc)
    | EAmbiguousObjectType loc -> spf "EAmbiguousObjectType (%s)" (string_of_aloc loc)
    | EUntypedTypeImport (loc, module_name) ->
      spf "EUntypedTypeImport (%s, %s)" (string_of_aloc loc) module_name
    | EUntypedImport (loc, module_name) ->
      spf "EUntypedImport (%s, %s)" (string_of_aloc loc) module_name
    | ENonstrictImport loc -> spf "ENonstrictImport (%s)" (string_of_aloc loc)
    | EUnclearType loc -> spf "EUnclearType (%s)" (string_of_aloc loc)
    | EDeprecatedBool loc -> spf "EDeprecatedBool (%s)" (string_of_aloc loc)
    | EDeprecatedDollarCall loc -> spf "EDeprecatedDollarCall (%s)" (string_of_aloc loc)
    | EDeprecatedDollarObjMap loc -> spf "EDeprecatedDollarObjMap (%s)" (string_of_aloc loc)
    | EDeprecatedPredicate loc -> spf "EDeprecatedPredicate (%s)" (string_of_aloc loc)
    | EIncorrectTypeWithReplacement { loc; kind } ->
      let deprecated_name = Error_message.IncorrectType.incorrect_of_kind kind in
      spf "EIncorrectTypeWithReplacement (%s) (%s)" (string_of_aloc loc) deprecated_name
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
        spf "ELintSetting (%s, %s)" (string_of_aloc loc) kind_str
      )
    | ESketchyNullLint { kind; loc; null_loc; falsy_loc } ->
      Lints.(
        let kind_str =
          match kind with
          | SketchyNullBool -> "SketchyNullBool"
          | SketchyNullString -> "SketchyNullString"
          | SketchyNullNumber -> "SketchyNullNumber"
          | SketchyNullBigInt -> "SketchyNullBigInt"
          | SketchyNullMixed -> "SketchyNullMixed"
          | SketchyNullEnumBool -> "SketchyNullEnumBool"
          | SketchyNullEnumString -> "SketchyNullEnumString"
          | SketchyNullEnumNumber -> "SketchyNullEnumNumber"
          | SketchyNullEnumBigInt -> "SketchyNullEnumBigInt"
        in
        spf
          "ESketchyNullLint {kind=%s; loc=%s; null_loc=%s; falsy_loc=%s}"
          kind_str
          (string_of_aloc loc)
          (string_of_aloc null_loc)
          (string_of_aloc falsy_loc)
      )
    | ESketchyNumberLint (kind, reason) ->
      Lints.(
        let kind_str =
          match kind with
          | SketchyNumberAnd -> "SketchyNumberAnd"
        in
        spf "ESketchyNumberLint (%s) (%s)" kind_str (dump_reason cx reason)
      )
    | EInvalidConstructor reason -> spf "EInvalidConstructor (%s)" (dump_reason cx reason)
    | EInvalidPrototype (loc, reason) ->
      spf "EInvalidPrototype (%s) (%s)" (string_of_aloc loc) (dump_reason cx reason)
    | EUnnecessaryOptionalChain (loc, _) -> spf "EUnnecessaryOptionalChain (%s)" (string_of_aloc loc)
    | EUnnecessaryInvariant (loc, _) -> spf "EUnnecessaryInvariant (%s)" (string_of_aloc loc)
    | EUnnecessaryDeclareTypeOnlyExport loc ->
      spf "EUnnecessaryDeclareTypeOnlyExport (%s)" (string_of_aloc loc)
    | EUnexpectedTemporaryBaseType loc ->
      spf "EUnexpectedTemporaryBaseType (%s)" (string_of_aloc loc)
    | ECannotDelete (l1, r1) -> spf "ECannotDelete (%s, %s)" (string_of_aloc l1) (dump_reason cx r1)
    | ESignatureVerification sve ->
      let msg = string_of_signature_error ALoc.debug_to_string sve in
      spf "ESignatureVerification (%s)" msg
    | EPrimitiveAsInterface { use_op; reason; interface_reason; kind = _ } ->
      spf
        "EPrimitiveAsInterface (%s) (%s) (%s)"
        (string_of_use_op use_op)
        (dump_reason cx reason)
        (dump_reason cx interface_reason)
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
    | EComputedPropertyWithUnion reason ->
      spf "EComputedPropertyWithUnion (%s)" (dump_reason cx reason)
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
    | EEnumInvalidObjectUtilType { reason; enum_reason } ->
      spf "EEnumInvalidObjectUtilType (%s) (%s)" (dump_reason cx reason) (dump_reason cx enum_reason)
    | EEnumInvalidObjectFunction { reason; enum_reason } ->
      spf "EEnumInvalidObjectFunction (%s) (%s)" (dump_reason cx reason) (dump_reason cx enum_reason)
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
    | EEnumIncompatible
        { reason_lower; reason_upper; use_op; representation_type; casting_syntax = _ } ->
      spf
        "EEnumIncompatible { reason_lower = %s; reason_upper = %s; use_op = %s; representation_type = %s }"
        (dump_reason cx reason_lower)
        (dump_reason cx reason_upper)
        (string_of_use_op use_op)
        (Base.Option.value ~default:"<None>" representation_type)
    | EAssignConstLikeBinding { loc; definition; binding_kind } ->
      spf
        "EAssignConstLikeBinding (%s) (%s) (%s)"
        (string_of_aloc loc)
        (dump_reason cx definition)
        (string_of_assigned_const_like_binding_type binding_kind)
    | ECannotResolveOpenTvar { use_op; reason; blame_reasons } ->
      spf
        "ECannotResolveOpenTvar (%s) (%s) (%s)"
        (string_of_use_op use_op)
        (dump_reason cx reason)
        (ListUtils.to_string ", " (dump_reason cx) blame_reasons)
    | EMalformedCode loc -> spf "EMalformedCode (%s)" (string_of_aloc loc)
    | EObjectThisReference (loc, r) ->
      spf "EObjectThisReference (%s, %s)" (string_of_aloc loc) (dump_reason cx r)
    | EComponentThisReference { this_loc; component_loc } ->
      spf
        "EComponentThisReference (this=%s, component=%s)"
        (string_of_aloc this_loc)
        (string_of_aloc component_loc)
    | EComponentCase loc -> spf "EComponentCase (%s)" (string_of_aloc loc)
    | EComponentMissingReturn r -> spf "EComponentMissingReturn (%s)" (dump_reason cx r)
    | ENestedComponent r -> spf "ENestedComponent (%s)" (dump_reason cx r)
    | EInvalidDeclaration { declaration = r; _ } -> spf "EInvalidDeclaration %s" (dump_reason cx r)
    | EImplicitInstantiationUnderconstrainedError { use_op; _ } ->
      spf "EImplicitInstantiationUnderconstrainedError (%s)" (string_of_use_op use_op)
    | EClassToObject _ -> "EClassToObject"
    | EMethodUnbinding { use_op; reason_prop; reason_op } ->
      spf
        "EMethodUnbinding (%s) (%s) (%s)"
        (string_of_use_op use_op)
        (dump_reason cx reason_op)
        (dump_reason cx reason_prop)
    | EHookIncompatible { use_op; lower; upper; _ } ->
      spf
        "EHookIncompatible (%s) (%s) (%s)"
        (string_of_use_op use_op)
        (dump_reason cx lower)
        (dump_reason cx upper)
    | EHookUniqueIncompatible { use_op; lower; upper } ->
      spf
        "EHookUniqueIncompatible (%s) (%s) (%s)"
        (string_of_use_op use_op)
        (dump_reason cx lower)
        (dump_reason cx upper)
    | EHookRuleViolation _ -> "EHookRuleViolation ( )"
    | EHookNaming _ -> "EHookNaming ( )"
    | EInvalidGraphQL (loc, err) ->
      let err_str =
        match err with
        | Graphql.InvalidTaggedTemplate -> "invalid tagged template"
        | Graphql.InvalidGraphQL -> "invalid graphql"
      in
      spf "EInvalidGraphQL (%s) (%s)" (string_of_aloc loc) err_str
    | EAnnotationInference (loc, reason_op, reason, _) ->
      spf
        "EAnnotationInference (%s) (%s) (%s)"
        (string_of_aloc loc)
        (dump_reason cx reason_op)
        (dump_reason cx reason)
    | ETrivialRecursiveDefinition (loc, reason) ->
      spf "ETrivialRecursiveDefinition (%s) (%s)" (string_of_aloc loc) (dump_reason cx reason)
    | EDefinitionCycle _ -> "EDefinitionCycle"
    | ERecursiveDefinition _ -> "ERecursiveDefinition"
    | EDuplicateClassMember { loc; name; _ } ->
      spf "EDuplicateClassMember (%s) (%s)" (string_of_aloc loc) name
    | EReferenceInAnnotation _ -> "EReferenceInAnnotation"
    | EEmptyArrayNoProvider { loc } -> spf "EEmptyArrayNoProvider (%s)" (string_of_aloc loc)
    | EUnusedPromise { loc; _ } -> spf "EUnusedPromise (%s)" (string_of_aloc loc)
    | EReactIntrinsicOverlap _ -> "EReactIntrinsicOverlap (_, _, _)"
    | EReactRefInRender _ -> "EReactRefInRender _"
    | EBigIntRShift3 reason -> spf "EBigIntRShift3 (%s)" (dump_reason cx reason)
    | EBigIntNumCoerce reason -> spf "EBigIntNumCoerce (%s)" (dump_reason cx reason)
    | EInvalidCatchParameterAnnotation loc ->
      spf "EInvalidCatchParameterAnnotation (%s)" (string_of_aloc loc)
    | ETSSyntax { loc; _ } -> spf "ETSSyntax (%s)" (string_of_aloc loc)
    | EInvalidBinaryArith { reason_out; reason_l; reason_r; kind } ->
      spf
        "EInvalidBinaryArith (%s, %s, %s, %s)"
        (dump_reason cx reason_out)
        (dump_reason cx reason_l)
        (dump_reason cx reason_r)
        (Type.ArithKind.string_of_arith_kind kind)
    | EInvalidMappedType { loc; kind } ->
      spf
        "EInvalidMappedType (%s, %s)"
        (string_of_aloc loc)
        Error_message.(
          match kind with
          | InterfaceOrDeclaredClass -> "InterfaceOrDeclaredClass"
          | ExtraProperties -> "ExtraProperties"
          | RequiredInlineKeyof -> "RequiredInlineKeyof"
          | ExplicitExactOrInexact -> "ExplicitExactOrInexact"
          | RemoveOptionality -> "RemoveOptionality"
        )
    | EDuplicateComponentProp { first; _ } ->
      spf "EDuplicateComponentProp (%s)" (dump_reason cx first)
    | ERefComponentProp { loc; _ } -> spf "ERefComponentProp (%s)" (string_of_aloc loc)
    | EInvalidComponentRestParam loc -> spf "EInvalidComponentRestParam (%s)" (string_of_aloc loc)
    | EInvalidTypeCastSyntax { loc; _ } -> spf "EInvalidTypeCastSyntax (%s)" (string_of_aloc loc)
    | EMissingPlatformSupport { loc; available_platforms; required_platforms } ->
      spf
        "EMissingPlatformSupport(%s, %s, %s)"
        (string_of_aloc loc)
        (SSet.to_string available_platforms)
        (SSet.to_string required_platforms)
    | EUnionOptimization { loc; _ } -> spf "EUnionOptimization (%s)" (string_of_aloc loc)
    | EUnionOptimizationOnNonUnion { loc; _ } ->
      spf "EUnionOptimizationOnNonUnion (%s)" (string_of_aloc loc)

module Verbose = struct
  let print_if_verbose_lazy
      cx ?(trace = Trace.dummy_trace) ?(delim = "") ?(indent = 0) (lines : string list Lazy.t) =
    match Context.verbose cx with
    | Some { Verbose.indent = num_spaces; _ } when Context.is_verbose cx ->
      let indent = max (indent + Trace.trace_depth trace - 1) 0 in
      let prefix = String.make (indent * num_spaces) ' ' in
      let pid = Context.pid_prefix cx in
      let add_prefix line = spf "\n%s%s%s" prefix pid line in
      let lines = Base.List.map ~f:add_prefix (Lazy.force lines) in
      prerr_endline (String.concat delim lines)
    | _ -> ()

  let print_if_verbose
      cx ?(trace = Trace.dummy_trace) ?(delim = "") ?(indent = 0) (lines : string list) =
    if Context.is_verbose cx then print_if_verbose_lazy cx ~trace ~delim ~indent (lazy lines)

  let print_types_if_verbose cx trace ?(note : string option) ((l : Type.t), (u : Type.use_t)) =
    match Context.verbose cx with
    | Some { Verbose.depth; _ } when Context.is_verbose cx ->
      let delim =
        match note with
        | Some x -> spf " ~> %s" x
        | None -> " ~>"
      in
      print_if_verbose cx ~trace ~delim [dump_t ~depth cx l; dump_use_t ~depth cx u]
    | _ -> ()

  let print_unify_types_if_verbose cx trace ?(note : string option) ((l : Type.t), (u : Type.t)) =
    match Context.verbose cx with
    | Some { Verbose.depth; _ } when Context.is_verbose cx ->
      let delim =
        match note with
        | Some x -> spf " = %s" x
        | None -> " ="
      in
      print_if_verbose cx ~trace ~delim [dump_t ~depth cx l; dump_t ~depth cx u]
    | _ -> ()
end
