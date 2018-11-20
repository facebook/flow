(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(**
 * This file is a general-purpose utility for generating code. It is
 * Context.t-aware, which allows it to resolve and codegen type syntax from
 * types themselves.
 *
 * Example usage:
 *
 *   let code_str =
 *     Codegen.mk_env cx
 *       |> Codegen.add_str "/* Before printed type */\n"
 *       |> Codegen.gen_type NullT.t
 *       |> Codegen.add_str "\n/* After printed type */\n"
 *       |> Codegen.to_string
 *   in
 *   print_endline code_str
 *)

let spf = Printf.sprintf

type codegen_env = {
  buf: Buffer.t;
  class_names: string IMap.t;
  mutable next_class_name: int;
  flow_cx: Context.t;
  tparams: Type.typeparam list;
  applied_tparams: Type.t list;
}

let add_applied_tparams applied_tparams env = {env with applied_tparams;}
let add_str str env = Buffer.add_string env.buf str; env
let add_tparams tparams env = {env with tparams=(Nel.to_list tparams);}
let find_props tmap_id env = Context.find_props env.flow_cx tmap_id
let has_class_name class_id env = IMap.mem class_id env.class_names
let next_class_name env =
  let id = env.next_class_name in
  env.next_class_name <- id + 1;
  spf "Class%d" id
let resolve_type t env = Flow_js.resolve_type env.flow_cx t
let resolve_tvar tvar env = Flow_js.resolve_tvar env.flow_cx tvar
let set_class_name class_id name env =
  {env with class_names = IMap.add class_id name env.class_names;}
let to_string env = Buffer.contents env.buf

let mk_env merged_flow_cx = {
  applied_tparams = [];
  buf = Buffer.create 320;
  class_names = IMap.empty;
  flow_cx = merged_flow_cx;
  next_class_name = 0;
  tparams = [];
}

(**
 * Just a helper function to simplify this:
 *
 *   let env =
 *     add_str "first" env
 *       |> add_str "second"
 *       |> add_str "third"
 *   in
 *   let env =
 *     if conditional
 *     then add_str "maybe fourth" env
 *     else env
 *   in
 *   add_str "fifth" env
 *     |> add_str "sixth"
 *     |> add_str "seventh"
 *
 * into this:
 *
 *   add_str "first" env
 *     |> add_str "second"
 *     |> add_str "third"
 *     |> gen_if conditional (add_str "maybe fourth")
 *     |> add_str "fifth"
 *     |> add_str "sixth"
 *     |> add_str "seventh"
 *)
let gen_if conditional gen_fn env =
  if conditional then gen_fn env else env

(**
 * Given a type which must be a built-in class instance type, trace out the
 * built-in's name and codegen it.
 *
 * NOTE: It would be good to come back to this and find a more general
 *       (less fragile) way of preserving class names alongside instance types.
 *)
let gen_builtin_class_type t env = Type.(
  (* AVERT YOUR EYES *)
  let reason = reason_of_t t in
  let builtin_name = DescFormat.name_of_instance_reason reason in

  (**
   * Assert that the builtin name we found does match with the class_id we're
   * backtracking. This is super defensive just because our method of getting
   * a builtin's name is so hacky. Once we make that better, we can be less
   * defensive here.
   *)
  let classid =
    match t with
    | DefT (_, InstanceT (_, _, _, {class_id; _;})) -> class_id
    | t -> failwith (
      spf
        ("Internal error: Expected an InstanceT while looking up a builtin " ^^
         "class name, but got a %s!")
        (string_of_ctor t)
    )
  in

  let builtin_t = Flow_js.get_builtin env.flow_cx builtin_name reason in
  let builtin_classid =
    match resolve_type builtin_t env with
    | ThisClassT(_, DefT (_, InstanceT (_, _, _, {class_id; _;}))) ->
      class_id
    | DefT (_, PolyT(_, _, ThisClassT(_, DefT (_, InstanceT(_, _, _, {class_id; _;}))), _)) ->
      class_id
    | builtin_t -> failwith (spf "Unexpected global type: %s" (string_of_ctor builtin_t))
  in

  if builtin_classid = classid
  then add_str builtin_name env
  else failwith (
    "Internal error: Encountered an instance type for a class that " ^
    "has not been defined!"
  )
)

(* Helper to generate a list of items with some separator between. *)
let gen_separated_list list sep gen_fn env =
  let count = List.length list in
  let (env, _) = List.fold_left (fun (env, idx) item ->
    let idx = idx + 1 in
    let env = gen_fn item env in
    ((if idx < count then add_str sep env else env), idx)
  ) (env, 0) list in
  env

(* Generate type syntax for a given type *)
let rec gen_type t env = Type.(
  match t with
  | AnnotT (_, source_t, _) -> gen_type (resolve_type source_t env) env
  | OpaqueT (_, {underlying_t = Some t; _}) -> gen_type t env
  | OpaqueT (_, {super_t = Some t; _}) -> gen_type t env
  | DefT (_, AnyFunT) -> add_str "Function" env
  | DefT (_, AnyObjT) -> add_str "Object" env
  | DefT (_, AnyT _)
  | AnyWithLowerBoundT _
  | AnyWithUpperBoundT _
  | MergedT _
    -> add_str "any" env
  | DefT (_, ArrT arrtype) ->
    (match arrtype with
    | ArrayAT (elemt, None) ->
      add_str "Array<" env
        |> gen_type elemt
        |> add_str ">"
    | ROArrayAT (elemt) ->
      add_str "$ReadOnlyArray<" env
        |> gen_type elemt
        |> add_str ">"
    | ArrayAT (_, Some tuple_types)
    | TupleAT (_, tuple_types) ->
      env
      |> add_str "["
      |> gen_separated_list tuple_types ", " gen_type
      |> add_str "]"
    )

  | DefT (_, BoolT (Some _)) ->
    (* TODO: Consider polarity and print the literal type when appropriate *)
    add_str "boolean" env
  | DefT (_, BoolT None) ->
    add_str "boolean" env
  | BoundT (_, name, _) -> add_str name env
  | DefT (_, ClassT t) ->
    add_str "Class<" env
      |> gen_type t
      |> add_str ">"
  | DefT (_, CharSetT chars) ->
    add_str "$CharSet<\"" env
      |> add_str (String_utils.CharSet.to_string chars)
      |> add_str "\">"
  | CustomFunT (_, ObjectAssign) -> add_str "Object$Assign" env
  | CustomFunT (_, ObjectGetPrototypeOf) -> add_str "Object$GetPrototypeOf" env
  | CustomFunT (_, ObjectSetPrototypeOf) -> add_str "Object$SetPrototypeOf" env
  | CustomFunT (_, Compose false) -> add_str "$Compose" env
  | CustomFunT (_, Compose true) -> add_str "$ComposeReverse" env
  | CustomFunT (_, ReactPropType (React.PropType.Primitive (_, t))) ->
    add_str "React$PropType$Primitive<" env
      |> gen_type t
      |> add_str ">"
  | CustomFunT (_, ReactPropType (React.PropType.Complex kind)) ->
    add_str React.PropType.(match kind with
    | ArrayOf -> "React$PropType$ArrayOf"
    | InstanceOf -> "React$PropType$InstanceOf"
    | ObjectOf -> "React$PropType$ObjectOf"
    | OneOf -> "React$PropType$OneOf"
    | OneOfType -> "React$PropType$OneOfType"
    | Shape -> "React$PropType$Shape"
    ) env
  | CustomFunT (_, ReactCreateClass) -> add_str "React$CreateClass" env
  | CustomFunT (_, ReactCreateElement) -> add_str "React$CreateElement" env
  | CustomFunT (_, ReactCloneElement) -> add_str "React$CloneElement" env
  | CustomFunT (_, ReactElementFactory _) -> add_str "React$ElementFactory" env
  | CustomFunT (_, Idx) -> add_str "$Facebookism$Idx" env
  | CustomFunT (_, TypeAssertIs) -> add_str "$Facebookism$TypeAssertIs" env
  | CustomFunT (_, TypeAssertThrows) -> add_str "$Facebookism$TypeAssertThrows" env
  | CustomFunT (_, TypeAssertWraps) -> add_str "$Facebookism$TypeAssertWraps" env
  | CustomFunT (_, DebugPrint) -> add_str "$Flow$DebugPrint" env
  | CustomFunT (_, DebugThrow) -> add_str "$Flow$DebugThrow" env
  | CustomFunT (_, DebugSleep) -> add_str "$Flow$DebugSleep" env
  (* TODO: Once predicate types are a little more fleshed out, fill out this
   *       codegen.
   *)
  | OpenPredT (_, _, _, _) -> add_str "mixed /* TODO: OpenPredT */" env
  | ExactT (_, t) -> add_str "$Exact<" env |> gen_type t |> add_str ">"
  | ObjProtoT _ -> add_str "typeof Object.prototype" env
  | FunProtoT _ -> add_str "typeof Function.prototype" env
  | FunProtoApplyT _ -> add_str "typeof Function.prototype.apply" env
  | FunProtoBindT _ -> add_str "typeof Function.prototype.bind" env
  | FunProtoCallT _ -> add_str "typeof Function.prototype.call" env
  | DefT (_, FunT (_static, _prototype, ft)) ->
    let {params; rest_param; return_t; _;} = ft in
    gen_tparams_list env
      |> add_str "("
      |> gen_func_params params rest_param
      |> add_str ") => "
      |> gen_type return_t
  | DefT (_, InstanceT (_static, _super, _, {class_id; _;})) -> (
    (* TODO: See if we can preserve class names *)
    let env =
      match IMap.get class_id env.class_names with
      | Some name -> add_str name env
      | None -> gen_builtin_class_type t env
    in
    gen_tparams_list env
  )
  | DefT (_, IntersectionT intersection) -> gen_intersection_list intersection env
  | KeysT (_, t) -> add_str "$Keys<" env |> gen_type t |> add_str ">"
  | DefT (_, MaybeT t) -> add_str "?" env |> gen_type t
  | DefT (_, MixedT _) -> add_str "mixed" env
  | DefT (_, NumT (Literal _)) ->
    (* TODO: Consider polarity and print the literal type when appropriate *)
    add_str "number" env
  | DefT (_, NumT (Truthy|AnyLiteral)) -> add_str "number" env
  | DefT (_, NullT) | NullProtoT _ -> add_str "null" env
  | DefT (_, ObjT {flags = _; dict_t; call_t = _; props_tmap; proto_t = _;}) -> (
    let env = add_str "{" env in

    (* Generate prop entries *)
    let props = find_props props_tmap env in
    let props = SMap.elements props |> List.sort (fun (k1, _) (k2, _) ->
      Pervasives.compare k1 k2
    ) in
    let env = gen_separated_list props ", " (fun (k, p) env ->
      gen_prop k p env
    ) env in

    (* Generate potential dict entry *)
    let env =
      match dict_t with
      | Some {dict_name; key; value; dict_polarity} ->
        let key_name = (
          match dict_name with
          | Some n -> n
          | None -> "_"
        ) in
        let sigil = Type.Polarity.sigil dict_polarity in
        let key = resolve_type key env in
        let value = resolve_type value env in
        gen_if (List.length props > 0) (add_str ", ") env
          |> add_str sigil
          |> add_str "["
          |> add_str key_name
          |> add_str ": "
          |> gen_type key
          |> add_str "]: "
          |> gen_type value
      | None -> env
    in

    add_str "}" env
  )
  | DefT (_, OptionalT t) -> add_str "void | " env |> gen_type t
  | OpenT tvar -> gen_type (resolve_tvar tvar env) env
  | DefT (_, PolyT (_, tparams, t, _)) -> gen_type t (add_tparams tparams env)
  | ReposT (_, t) -> gen_type t env
  | InternalT (ReposUpperT (_, t)) -> gen_type t env
  | ShapeT t -> add_str "$Shape<" env |> gen_type t |> add_str ">"
  | DefT (_, SingletonBoolT v) -> add_str (spf "%b" v) env
  | DefT (_, SingletonNumT (_, v)) -> add_str (spf "%s" v) env
  | DefT (_, SingletonStrT v) -> add_str (spf "%S" v) env
  | DefT (_, StrT (Literal _)) ->
    (* TODO: Consider polarity and print the literal type when appropriate *)
    add_str "string" env
  | DefT (_, StrT (Truthy|AnyLiteral)) -> add_str "string" env
  | ThisClassT (_, t) -> gen_type t env
  | ThisTypeAppT (_, t, _, Some ts) -> add_applied_tparams ts env |> gen_type t
  | ThisTypeAppT (_, t, _, None) -> gen_type t env
  | DefT (_, TypeAppT (_, t, ts)) -> add_applied_tparams ts env |> gen_type t
  | DefT (_, TypeT (_, t)) -> gen_type t env
  | DefT (_, UnionT union) -> gen_union_list union env
  | DefT (_, VoidT) -> add_str "void" env
  | InternalT (OptionalChainVoidT _) -> add_str "void" env
  | DefT (_, ReactAbstractComponentT {props; default_props; instance}) ->
      add_str "React$AbstractComponent<" env |> gen_type props
        |> add_str ", " |> gen_type default_props
        |> add_str ", " |> gen_type instance
        |> add_str ">"

  (**
   * These types can't be expressed in code well so we fail back to `mixed`.
   *
   * TODO: This handling is a little low-fidelity which may not work for all
   *       cases. It works for current needs (best-effort codegen of shadow
   *       files), but at some point it might make sense to offer other kinds of
   *       handling for these types depening on the needs of the API user
   *       (i.e. raise, etc).
   *)
  | InternalT (ChoiceKitT _)
  | TypeDestructorTriggerT _
  | DefT (_, EmptyT)
  | EvalT _
  | ExistsT _
  | InternalT (ExtendsT _)
  | DefT (_, IdxWrapper _)
  | ModuleT _
  | OpaqueT _
  | MatchingPropT _
    -> add_str (spf "mixed /* UNEXPECTED TYPE: %s */" (string_of_ctor t)) env
)

and gen_prop k p env =
  let open Type in

  let gen_getter k t env =
    add_str "get " env
      |> add_str k
      |> add_str "(): "
      |> gen_type t
  in

  let gen_setter k t env =
    add_str "set " env
      |> add_str k
      |> add_str "("
      |> gen_func_param (Some "value") t
      |> add_str "): void"
  in

  let rec gen_method k t env =
    match t with
    | DefT (_, FunT (_static, _prototype, ft)) ->
      let {params; rest_param; return_t; _;} = ft in
      add_str k env
        |> gen_tparams_list
        |> add_str "("
        |> gen_func_params params rest_param
        |> add_str "): "
        |> gen_type return_t
    | DefT (_, PolyT (_, tparams, t, _)) -> gen_method k t (add_tparams tparams env)
    | _ -> add_str (spf "mixed /* UNEXPECTED TYPE: %s */" (string_of_ctor t)) env
  in

  match p with
  | Field (_, t, polarity) ->
    let sigil = Polarity.sigil polarity in
    let (sep, t) =
      match resolve_type t env with
      | DefT (_, OptionalT t) -> ("?: ", resolve_type t env)
      | t -> (": ", t)
    in
    add_str sigil env
      |> add_str k
      |> add_str sep
      |> gen_type t
  | Get (_, t) -> gen_getter k t env
  | Set (_, t) -> gen_setter k t env
  | GetSet (_, t1, _, t2) ->
    gen_getter k t1 env |> gen_setter k t2
  | Method (_, t) -> gen_method k t env

and gen_func_params params rest_param env =
  let params_rev = List.fold_left (fun acc (name, t) ->
    (name, t, false) :: acc
  ) [] params in
  let params_rev = match rest_param with
  | None -> params_rev
  | Some (name, _, t) -> (name, t, true) :: params_rev
  in
  let params = List.rev params_rev in
  gen_separated_list params ", " (fun (name, t, is_rest) env ->
    if is_rest
    then gen_func_rest_param name t env
    else gen_func_param name t env
  ) env

and gen_func_rest_param name t env =
  let name = Option.value name ~default:"_" in
  add_str "..." env
  |> add_str name
  |> add_str ": "
  |> gen_type t

and gen_func_param name t env =
  let open Type in
  let name = Option.value name ~default:"_" in
  match t with
  | DefT (_, OptionalT t) ->
    add_str name env
    |> add_str "?: "
    |> gen_type t
  | t ->
    add_str name env
    |> add_str ": "
    |> gen_type t

and gen_intersection_list intersection env =
  let members = Type.InterRep.members intersection in
  gen_separated_list members " & " gen_type env

and gen_tparams_list = Type.(
  let gen_tparam {reason = _; name; bound; polarity; default;} env =
    let bound = resolve_type bound env in
    let env = add_str (Polarity.sigil polarity) env in
    let env = add_str name env in
    let env = (
      match bound with
      | DefT (_, MixedT _) -> env
      | bound -> add_str ": " env |> gen_type bound
    ) in
    let env = (
      match default with
      | Some default -> add_str " = " env |> gen_type default
      | None -> env
    ) in
    env
  in

  fun env ->
    let tparams = env.tparams in
    let params_count = List.length tparams in
    let applied_tparams = env.applied_tparams in
    let applied_tparams_count = List.length applied_tparams in
    match (params_count, applied_tparams_count) with
    | (0, 0) -> env
    | (_, 0) ->
      {env with tparams = []; }
        |> add_str "<"
        |> gen_separated_list tparams ", " gen_tparam
        |> add_str ">"
    | _ ->
      {env with tparams = []; applied_tparams = []; }
        |> add_str "<"
        |> gen_separated_list applied_tparams ", " gen_type
        |> add_str ">"
)

and gen_union_list union env =
  let members = Type.UnionRep.members union in
  gen_separated_list members " | " gen_type env
