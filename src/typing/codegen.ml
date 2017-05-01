(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
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
let add_tparams tparams env = {env with tparams;}
let find_props tmap_id env = Context.find_props env.flow_cx tmap_id
let has_class_name class_id env = IMap.mem class_id env.class_names
let next_class_name env =
  let id = env.next_class_name in
  env.next_class_name <- id + 1;
  spf "Class%d" id
let resolve_type t env = Flow_js.resolve_type env.flow_cx t
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
  let builtin_name = Reason.(string_of_desc (desc_of_reason reason)) in

  (**
   * Assert that the builtin name we found does match with the class_id we're
   * backtracking. This is super defensive just because our method of getting
   * a builtin's name is so hacky. Once we make that better, we can be less
   * defensive here.
   *)
  let classid =
    match t with
    | InstanceT (_, _, _, _, {class_id; _;}) -> class_id
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
    | ThisClassT(_, InstanceT(_, _, _, _, {class_id; _;})) ->
      class_id
    | PolyT(_, _, ThisClassT(_, InstanceT(_, _, _, _, {class_id; _;}))) ->
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
  | AbstractT (_, t) -> add_str "$Abstract<" env |> gen_type t |> add_str ">"
  | AnnotT t -> gen_type t env
  | AnyFunT _ -> add_str "Function" env
  | AnyObjT _ -> add_str "Object" env
  | AnyT _
  | AnyWithLowerBoundT _
  | AnyWithUpperBoundT _
    -> add_str "any" env
  | ArrT (_, arrtype) ->
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
    | EmptyAT ->
      (* There isn't any real way to write this type at the moment *)
      add_str "Array<empty>" env
    )

  | BoolT (_, Some _) ->
    (* TODO: Consider polarity and print the literal type when appropriate *)
    add_str "boolean" env
  | BoolT (_, None) ->
    add_str "boolean" env
  | BoundT {name; _;} -> add_str name env
  | ClassT (_, t) ->
    add_str "Class<" env
      |> gen_type t
      |> add_str ">"
  | CustomFunT (_, ObjectAssign) -> add_str "Object$Assign" env
  | CustomFunT (_, ObjectGetPrototypeOf) -> add_str "Object$GetPrototypeOf" env
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
  | CustomFunT (_, Merge) -> add_str "$Facebookism$Merge" env
  | CustomFunT (_, MergeDeepInto) -> add_str "$Facebookism$MergeDeepInto" env
  | CustomFunT (_, MergeInto) -> add_str "$Facebookism$MergeInto" env
  | CustomFunT (_, Mixin) -> add_str "$Facebookism$Mixin" env
  | CustomFunT (_, Idx) -> add_str "$Facebookism$Idx" env
  | CustomFunT (_, DebugPrint) -> add_str "$Flow$DebugPrint" env
  (* TODO: Once predicate types are a little more fleshed out, fill out this
   *       codegen.
   *)
  | OpenPredT (_, _, _, _) -> add_str "mixed /* TODO: OpenPredT */" env
  | DiffT (t1, t2) ->
    add_str "$Diff<" env
      |> gen_type t1
      |> add_str ", "
      |> gen_type t2
      |> add_str ">"
  | ExactT (_, t) -> add_str "$Exact<" env |> gen_type t |> add_str ">"
  | ObjProtoT _ -> add_str "typeof Object.prototype" env
  | FunProtoT _ -> add_str "typeof Function.prototype" env
  | FunProtoApplyT _ -> add_str "typeof Function.prototype.apply" env
  | FunProtoBindT _ -> add_str "typeof Function.prototype.bind" env
  | FunProtoCallT _ -> add_str "typeof Function.prototype.call" env
  | FunT (_, _static, _prototype, ft) ->
    let {params_tlist; params_names; rest_param; return_t; _;} = ft in
    gen_tparams_list env
      |> add_str "("
      |> gen_func_params params_names params_tlist rest_param
      |> add_str ") => "
      |> gen_type return_t
  | InstanceT (_, _static, _super, _, {class_id; _;}) -> (
    (* TODO: See if we can preserve class names *)
    let env =
      match IMap.get class_id env.class_names with
      | Some name -> add_str name env
      | None -> gen_builtin_class_type t env
    in
    gen_tparams_list env
  )
  | IntersectionT (_, intersection) -> gen_intersection_list intersection env
  | KeysT (_, t) -> add_str "$Keys<" env |> gen_type t |> add_str ">"
  | MaybeT (_, t) -> add_str "?" env |> gen_type t
  | MixedT _ -> add_str "mixed" env
  | NumT (_, Literal _) ->
    (* TODO: Consider polarity and print the literal type when appropriate *)
    add_str "number" env
  | NumT (_, (Truthy|AnyLiteral)) -> add_str "number" env
  | NullT _ -> add_str "null" env
  | ObjT (_, {flags = _; dict_t; props_tmap; proto_t = _;}) -> (
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
  | OptionalT (_, t) -> add_str "void | " env |> gen_type t
  | OpenT _ -> gen_type (resolve_type t env) env
  | PolyT (_, tparams, t) -> gen_type t (add_tparams tparams env)
  | ReposT (_, t) -> gen_type t env
  | ReposUpperT (_, t) -> gen_type t env
  | ShapeT t -> add_str "$Shape<" env |> gen_type t |> add_str ">"
  | SingletonBoolT (_, v) -> add_str (spf "%b" v) env
  | SingletonNumT (_, (_, v)) -> add_str (spf "%s" v) env
  | SingletonStrT (_, v) -> add_str (spf "%S" v) env
  | StrT (_, Literal _) ->
    (* TODO: Consider polarity and print the literal type when appropriate *)
    add_str "string" env
  | StrT (_, (Truthy|AnyLiteral)) -> add_str "string" env
  | ThisClassT (_, t) -> gen_type t env
  | ThisTypeAppT (_, t, _, ts) -> add_applied_tparams ts env |> gen_type t
  | TypeAppT (_, t, ts) -> add_applied_tparams ts env |> gen_type t
  | TypeT (_, t) -> gen_type t env
  | UnionT (_, union) -> gen_union_list union env
  | VoidT _ -> add_str "void" env

  | TypeMapT (_, TupleMap, t1, t2) ->
    add_str "$TupleMap<" env
    |> gen_type t1
    |> add_str ", "
    |> gen_type t2
    |> add_str ">"

  | TypeMapT (_, ObjectMap, t1, t2) ->
    add_str "$ObjMap<" env
    |> gen_type t1
    |> add_str ", "
    |> gen_type t2
    |> add_str ">"

  | TypeMapT (_, ObjectMapi, t1, t2) ->
    add_str "$ObjMapi<" env
    |> gen_type t1
    |> add_str ", "
    |> gen_type t2
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
  | ChoiceKitT _
  | EmptyT _
  | EvalT _
  | ExistsT _
  | ExtendsT _
  | IdxWrapper _
  | ModuleT _
  | TaintT _
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
      |> gen_func_param "value" t
      |> add_str "): void"
  in

  let rec gen_method k t env =
    match t with
    | FunT (_, _static, _prototype, ft) ->
      let {params_tlist; params_names; rest_param; return_t; _;} = ft in
      add_str k env
        |> gen_tparams_list
        |> add_str "("
        |> gen_func_params params_names params_tlist rest_param
        |> add_str "): "
        |> gen_type return_t
    | PolyT (_, tparams, t) -> gen_method k t (add_tparams tparams env)
    | _ -> add_str (spf "mixed /* UNEXPECTED TYPE: %s */" (string_of_ctor t)) env
  in

  match p with
  | Field (t, polarity) ->
    let sigil = Polarity.sigil polarity in
    let (sep, t) =
      match resolve_type t env with
      | OptionalT (_, t) -> ("?: ", resolve_type t env)
      | t -> (": ", t)
    in
    add_str sigil env
      |> add_str k
      |> add_str sep
      |> gen_type t
  | Get t -> gen_getter k t env
  | Set t -> gen_setter k t env
  | GetSet (t1, t2) ->
    gen_getter k t1 env |> gen_setter k t2
  | Method t -> gen_method k t env

and gen_func_params params_names params_tlist rest_param env =
  let params =
    match params_names with
    | Some params_names ->
      List.rev (List.fold_left2 (fun params name t ->
        (name, t, false):: params
      ) [] params_names params_tlist)
    | None ->
      List.mapi (fun idx t -> (spf "p%d" idx, t, false)) params_tlist
  in
  let params = match rest_param with
  | None -> params
  | Some (name, _, t) ->
    params @ [Option.value ~default:"rest" name, t, true] in
  gen_separated_list params ", " (fun (name, t, is_rest) env ->
    if is_rest
    then gen_func_rest_param name t env
    else gen_func_param name t env
  ) env

and gen_func_rest_param name t env =
  add_str "..." env
  |> add_str name
  |> add_str ": "
  |> gen_type t

and gen_func_param name t env =
  let open Type in
  match t with
  | OptionalT (_, t) ->
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
      | MixedT _ -> env
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
