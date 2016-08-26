(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let spf = Printf.sprintf

type codegen_env = {
  buf: Buffer.t;
  declared_classes: string IMap.t;
  mutable next_declared_class_name: int;
  flow_cx: Context.t;
  indent: string;
  tparams: Type.typeparam list;
  applied_tparams: Type.t list;
}

let add_str str env = Buffer.add_string env.buf str; env
let add_tparams tparams env =
  {env with tparams;}
let add_applied_tparams applied_tparams env =
  {env with applied_tparams;}
let find_props tmap_id env = Flow_js.find_props env.flow_cx tmap_id
let next_declared_class_name env =
  let id = env.next_declared_class_name in
  env.next_declared_class_name <- id + 1;
  id
let resolve_type t env = Flow_js.resolve_type env.flow_cx t
let to_string env = Buffer.contents env.buf
let with_indent indent fn env =
  let orig_indent = env.indent in
  let env = {env with indent = indent ^ orig_indent;} in
  let env = fn env in
  {env with indent = orig_indent;}

let mk_env flow_cx buf = {
  applied_tparams = [];
  buf;
  declared_classes = IMap.empty;
  flow_cx;
  indent = "";
  next_declared_class_name = 0;
  tparams = [];
}

let add_declared_class class_id name env =
  {env with declared_classes = IMap.add class_id name env.declared_classes;}

let has_declared_class class_id env =
  IMap.mem class_id env.declared_classes

let gen_separated_list list sep fn env =
  let count = List.length list in
  let (env, _) = List.fold_left (fun (env, idx) item ->
    let idx = idx + 1 in
    let env = fn item env in
    ((if idx < count then add_str sep env else env), idx)
  ) (env, 0) list in
  env

let rec gen_type t env = Type.(
  match t with
  | AnnotT (_, t) -> gen_type t env
  | AnyFunT _ -> add_str "Function" env
  | AnyObjT _ -> add_str "Object" env
  | AnyT _
  | AnyWithLowerBoundT _
  | AnyWithUpperBoundT _
    -> add_str "any" env
  | ArrT (_, tparam, ts) ->
    (match ts with
    | [] ->
      add_str "Array<" env
        |> gen_type tparam
        |> add_str ">"
    | _ ->
      let t_count = List.length ts in
      let env = add_str "[" env in
      let (env, _) = List.fold_left (fun (env, idx) t ->
        let env = gen_type t env in
        let idx = idx + 1 in
        let env =
          if idx < t_count then add_str ", " env else env
        in
        (env, idx)
      ) (env, 0) ts in
      add_str "]" env
    )
  | BoolT (_, Some v) -> add_str (spf "%b" v) env
  | BoolT (_, None) -> add_str "boolean" env
  | BoundT {name; _;} -> add_str name env
  | ClassT t ->
    add_str "Class<" env
      |> gen_type t
      |> add_str ">"
  | CustomFunT (_, ObjectAssign) -> add_str "Object$Assign" env
  | CustomFunT (_, ObjectGetPrototypeOf) -> add_str "Object$GetPrototypeOf" env
  | CustomFunT (_, PromiseAll) -> add_str "Promise$All" env
  | CustomFunT (_, ReactCreateElement) -> add_str "React$CreateElement" env
  | CustomFunT (_, Merge) -> add_str "$Facebookism$Merge" env
  | CustomFunT (_, MergeDeepInto) -> add_str "$Facebookism$MergeDeepInto" env
  | CustomFunT (_, MergeInto) -> add_str "$Facebookism$MergeInto" env
  | CustomFunT (_, Mixin) -> add_str "$Facebookism$Mixin" env
  | CustomFunT (_, Idx) -> add_str "$Facebookism$Idx" env
  (* TODO: Once predicate types are a little more fleshed out, fill out this
   *       codegen.
   *)
  | DepPredT (_, _) -> add_str "mixed /* TODO: Predicate type */" env
  | DiffT (t1, t2) ->
    add_str "$Diff<" env
      |> gen_type t1
      |> add_str ", "
      |> gen_type t2
      |> add_str ">"
  | ExactT (_, t) -> add_str "$Exact<" env |> gen_type t |> add_str ">"
  | FunProtoT _ -> add_str "typeof Function.prototype" env
  | FunProtoApplyT _ -> add_str "typeof Function.prototype.apply" env
  | FunProtoBindT _ -> add_str "typeof Function.prototype.bind" env
  | FunProtoCallT _ -> add_str "typeof Function.prototype.call" env
  | FunT (_, _static, _prototype, {params_tlist; params_names; return_t; _;}) ->
    gen_tparams_list env
      |> add_str "("
      |> gen_fun_params params_names params_tlist
      |> add_str ") => "
      |> gen_type return_t
  | InstanceT (_, _static, _super, {class_id; _;}) -> (
    (* TODO: See if we can preserve class names *)
    let env =
      match IMap.get class_id env.declared_classes with
      | Some name -> add_str name env
      | None -> failwith (
        "Encountered an instance type for a class that has not been defined!"
      )
    in
    gen_tparams_list env
  )
  | IntersectionT (_, intersection) -> gen_intersect_list intersection env
  | KeysT (_, t) -> add_str "$Keys<" env |> gen_type t |> add_str ">"
  | MaybeT t -> add_str "?" env |> gen_type t
  | MixedT _ -> add_str "mixed" env
  | NumT (_, Literal (_, v)) -> add_str (spf "%s" v) env
  | NumT (_, (Truthy|AnyLiteral)) -> add_str "number" env
  | NullT _ -> add_str "null" env
  | ObjT (_, {flags = _; dict_t; props_tmap; proto_t = _;}) -> (
    match dict_t with
    | Some {dict_name; key; value;} ->
      let key_name = (
        match dict_name with
        | Some n -> n
        | None -> "_"
      ) in
      let key = resolve_type key env in
      let value = resolve_type value env in
      add_str (spf "{[%s: " key_name) env
        |> gen_type key
        |> add_str "]: "
        |> gen_type value
        |> add_str "}"
    | None ->
      let props = find_props props_tmap env in
      let num_props = SMap.cardinal props in
      let env = add_str "{" env in
      let (env, _) = SMap.fold (fun k t (env, idx) ->
        let t = resolve_type t env in
        let (sep, t) = (
          match t with
          | OptionalT t -> ("?: ", t)
          | _ -> (": ", t)
        ) in
        let idx = idx + 1 in

        let env =
          add_str k env
          |> add_str sep
          |> gen_type t
        in
        ((if idx < num_props then (add_str ", " env) else env), idx)
      ) props (env, 0) in
      add_str "}" env
  )
  | OptionalT t -> add_str "void | " env |> gen_type t
  | OpenT _ -> gen_type (resolve_type t env) env
  | PolyT (tparams, t) -> gen_type t (add_tparams tparams env)
  | RestT rest -> gen_type rest env
  | ShapeT t -> add_str "$Shape<" env |> gen_type t |> add_str ">"
  | SingletonBoolT (_, v) -> add_str (spf "%b" v) env
  | SingletonNumT (_, (_, v)) -> add_str (spf "%s" v) env
  | SingletonStrT (_, v) -> add_str (spf "%S" v) env
  | StrT (_, Literal v) -> add_str (spf "%S" v) env
  | StrT (_, (Truthy|AnyLiteral)) -> add_str "string" env
  | ThisClassT t -> gen_type t env
  | ThisTypeAppT (t, _, ts) -> add_applied_tparams ts env |> gen_type t
  | TypeAppT (t, ts) -> add_applied_tparams ts env |> gen_type t
  | TypeT (_, t) -> gen_type t env
  | UnionT (_, union) -> gen_union_list union env
  | VoidT _ -> add_str "void" env

  (**
   * These types can't be expressed in code well so we fail back to `mixed`.
   *
   * TODO: This handling is a little low-fidelity which may not work for all
   *       cases. It works for current needs (best-effort codegen of shadow
   *       files), but at some point it might make sense to offer other kinds of
   *       handling for these types depening on the needs of the API user
   *       (i.e. raise, etc).
   *)
  | AbstractT _
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

and gen_fun_params params_names params_tlist env =
  let params =
    match params_names with
    | Some params_names ->
      List.rev (List.fold_left2 (fun params name t ->
        (name, t):: params
      ) [] params_names params_tlist)
    | None ->
      List.mapi (fun idx t -> (spf "p%d" idx, t)) params_tlist
  in
  gen_separated_list params ", " Type.(fun (name, t) env ->
    match t with
    | RestT t ->
      add_str "..." env
      |> add_str name
      |> add_str ": Array<"
      |> gen_type t
      |> add_str ">"
    | OptionalT t ->
      add_str name env
      |> add_str "?: "
      |> gen_type t
    | t ->
      add_str name env
      |> add_str ": "
      |> gen_type t
  ) env

and gen_intersect_list intersection env =
  let members = Type.InterRep.members intersection in
  gen_separated_list members " & " gen_type env

and gen_tparams_list = Type.(
  let gen_tparam {reason = _; name; bound; polarity; default;} env =
    let bound = resolve_type bound env in
    let env = (
      match polarity with
      | Negative -> add_str "-" env
      | Neutral -> env
      | Positive -> add_str "+" env
    ) in
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
        |> add_str "<" |> gen_separated_list applied_tparams ", " gen_type |> add_str ">"
)

and gen_union_list union env =
  let members = Type.UnionRep.members union in
  gen_separated_list members " | " gen_type env
