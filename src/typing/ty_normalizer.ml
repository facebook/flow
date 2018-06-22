(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Pervasives
open Utils_js
open Reason

module T = Type
module VSet = ISet

(* The type normalizer converts infered types (of type `Type.t`) under a context
   cx to the simplified form of type `Ty.t`. It is called by various modules,
   e.g. type-at-pos, coverage, dump-types, and so is parameterized by a
   configuration struct, instantiated by the client.

   The type normalizer should only be used on types arising from "fully merged"
   contexts -- that is, contexts which have all dependencies copied in and
   constraints evaluated.
*)


(************)
(* Config   *)
(************)

module type Config = sig

  (* MergedT is somewhat unconventional. It introduces UseT's that the
     normalizer is not intended to handle. If this flag is set to true, all
     instances of MergedT will fall through and return Top. Otherwise, we
     attempt to convert the use_t's under the MergedT. This operation only
     succeeds if the use is a UseT and the underlying type is successfully
     normalized.

     Pick `true` if the result does not need to be "parseable", e.g. coverage.
  *)
  val opt_fall_through_merged: bool

  (* Expand the signatures of built-in functions, such as:
      Function.prototype.apply: (thisArg: any, argArray?: any): any
  *)
  val opt_expand_internal_types: bool

  (* AnnotT is used to hide information of the lower bound flowing in and
     provides instead a type interface that then flows to the upper bounds.
     For the normalizer this is good point to cut down on the recursion to
     AnnotT's lower bounds and instead return a type constructed from the name
     associated with the annotation. Typically this coincides with types used as
     annotations, so this is a natural type type to return for type queries.
  *)
  val opt_expand_type_aliases: bool

  (* The normalizer keeps a stack of type parameters that are in scope. This stack
     may contain the same name twice (but with different associated locations).
     This is a case of shadowing. For certain uses of normalized types (e.g. suggest)
     we do not wish to allow the generation of type parameters that are shadowed by
     another definition. For example the inferred type for `z` in:

     function outer<T>(y: T) {
       function inner<T>(x: T, z) { inner(x, y); }
     }

    is the _outer_ T. Adding the annotation ": T" for `z` would not be correct.
    This flags toggles this behavior.
  *)
  val opt_flag_shadowed_type_params: bool

end


(* Errors *)

type error_kind =
  | BadMethodType
  | BadBoundT
  | BadCallProp
  | BadClassT
  | BadPoly
  | BadTypeAlias
  | BadTypeApp
  | BadInternalT
  | BadInstanceT
  | BadEvalT
  | BadUse
  | ShadowTypeParam
  | UnsupportedTypeCtor
  | UnsupportedUseCtor

type error = error_kind * string

let error_kind_to_string = function
  | BadMethodType -> "Bad method type"
  | BadBoundT -> "Unbound type parameter"
  | BadCallProp -> "Bad call property"
  | BadClassT -> "Bad class"
  | BadPoly -> "Bad polymorphic type"
  | BadTypeAlias -> "Bad type alias"
  | BadTypeApp -> "Bad type application"
  | BadInternalT -> "Bad internal type"
  | BadInstanceT -> "Bad instance type"
  | BadEvalT -> "Bad eval"
  | BadUse -> "Bad use"
  | ShadowTypeParam -> "Shadowed type parameters"
  | UnsupportedTypeCtor -> "Unsupported type constructor"
  | UnsupportedUseCtor -> "Unsupported use constructor"

let error_to_string (kind, msg) =
  spf "[%s] %s" (error_kind_to_string kind) msg

(***************)
(* Normalizer  *)
(***************)

module Make(C: Config) : sig

  (* Takes a context and a single type argument, and returns a Ty.t or an error.
     This assumes the type parameter environment at that point is empty. *)
  val from_type: cx:Context.t -> Type.t -> (Ty.t, error) result

  (* Takes a context and a type scheme and returns a Ty.t or an error. *)
  val from_scheme: cx:Context.t -> Type_table.type_scheme -> (Ty.t, error) result

  (* Takes a context and a list of types as input and returns a list of a choice
     of a normalized type or an error. It differs from mapping `from_type` on
     each input as it folds over the input elements of the input propagating the
     state (that crucially includes the type cache) after each transformation to
     the next element. Again this assumes that there are no type parameters in
     scope.
  *)
  val from_types:
    cx:Context.t -> ('a * Type.t) list -> ('a * (Ty.t, error) result) list

  val from_schemes:
    cx:Context.t -> ('a * Type_table.type_scheme) list -> ('a * (Ty.t, error) result) list

  val fold_hashtbl:
    cx:Context.t ->
    f:('a -> (Loc.t * (Ty.t, error) result) -> 'a) ->
    g:('b -> Type_table.type_scheme) ->
    htbl: (Loc.t, 'b) Hashtbl.t ->
    'a -> 'a

end = struct

  (****************)
  (* State monad  *)
  (****************)

  module State = struct

    type t = {

      (* Source of fresh ints for creating new Ty.tvar's *)
      counter: int;

      (* A cache for resolved type variables.

         We cache the result even when the output is an error, to avoid
         recomputing tvar resolution. This is mostly useful for the batch call
         (`from_types`).

         The key to this map is the Type.tvar `ident`.
      *)
      tvar_cache: (Ty.t, error) result IMap.t;

      (* Hide the context in State to avoid clutter *)
      cx: Context.t;

      (* This set is useful for synthesizing recursive types. It holds the set
         of type variables that are encountered "free". We say that a type
         variable is free when it appears in the body of its own definition.
         The process of calculating free variables in a type could be
         implemented post-fact. The reason we prefer to keep this in the state
         instead is performance, since it trivializes the "check if variable
         appears free".
       *)
      free_tvars: VSet.t;

  }

    let empty ~cx = {
      counter = 0;
      tvar_cache = IMap.empty;
      cx;
      free_tvars = VSet.empty;
    }

  end

  (* Monad definition *)

  include StateResult.Make(State)


  (* Monadic helper functions *)

  let mapM f xs = all (List.map f xs)

  let concat_fold_m f xs = mapM f xs >>| List.concat

  let get_cx = get >>| fun st -> State.(st.cx)

  let find_cons i =
    get_cx >>| fun cx -> Context.find_constraints cx i

  let fresh_num =
    let open State in
    get >>= fun st ->
    let n = st.counter in
    put { st with counter = n + 1 } >>| fun _ -> n



  (* Error reporting *)

  let terr ~kind ?msg t =
    let t_str = Option.map t
      ~f:(fun t -> spf "Raised on type: %s" (Type.string_of_ctor t))
    in
    let msg = ListUtils.cat_maybes [msg; t_str] |> String.concat "\n" in
    error (kind, msg)



  (***************)
  (* Type cache  *)
  (***************)

  let update_tvar_cache i t =
    let open State in
    get >>= fun st ->
    let tvar_cache = IMap.add i t st.tvar_cache in
    put { st with tvar_cache }

  let find_tvar root_id =
    let open State in
    get >>| fun st ->
    IMap.get root_id st.tvar_cache


  (****************)
  (* Environment  *)
  (****************)

  (* The environment is passed in a top-down manner during normalization. *)

  module Env = struct
    type t = {
      (* For debugging purposes mostly *)
      depth: int;
      (* Type parameters in scope *)
      tparams: (string * Loc.t) list;
      (* File the query originated from *)
      file: File_key.t;

      (* In determining whether a symbol is Local, Imported, Remote, etc, it is
         useful to keep the list of imported names and the corresponding
         location available. We can then make this decision by comparing the
         source file with the current context's file information. *)
      imported_names: Loc.t SMap.t;

      (* The default behavior with type aliases is to return the name of the alias
         instead of the expansion of the type. When normalizing type aliases `TypeT t`,
         however, we proceed by recovering the name of the alias (say A) and then
         normalizing the body `T` to recover the type "type A = T". So for this case
         only it is useful to allow a one-off expansion of the contents of TypeT.
         We do this by setting this property to `Some A`.

         The reason we need the alias name (instead of just the fact that we're expanding
         an alias) is that the RTypeAlias reason (used to discover aliases) may be
         repeated across nested types (ExactT and MaybeT -- see statement.ml) and so we
         need to skip over duplicates until we either encounter another type constructor
         or a different type alias.

         NOTE: The use of the name might not be robust against aliases with the same name
         (coming from different scopes for example). Ideally we would use the location
         or a unique ID of the type alias to make this distinction, but at the moment
         keeping this information around introduces a small space regression. *)
      under_type_alias: string option;
    }

    let init cx tparams imported_names = {
      depth = 0;
      tparams;
      file = Context.file cx;
      imported_names;
      under_type_alias = None;
    }

    let descend e = { e with depth = e.depth + 1 }

    (* Lookup a type parameter T in the current environment. There are three outcomes:
       1. T appears in env and for its first occurence locations match. This means it
          is not shadowed by another parameter with the same name. In this case
          return the type parameter.
       2. T appears in env but is not the first occurence. This means that some other
          type parameter shadows it. We split cases depending on the value of
          Config.opt_flag_shadowed_type_params:
          - true: flag a warning, since the type is not well-formed in this context.
          - false: return the type normally ignoring the warning.
       3. The type parameter is not in env. Do the default action.
    *)
    let lookup_tparam ~default env t name loc =
      let pred (tp_name, _) = (name = tp_name) in
      match List.find_opt pred env.tparams with
      | Some (_, tp_loc) ->
        if loc = tp_loc || not C.opt_flag_shadowed_type_params
        then return Ty.(Bound (Symbol (Local loc, name)))
        (* If we care about shadowing of type params, then flag an error *)
        else terr ~kind:ShadowTypeParam (Some t)
      | None -> default t

    let add_typeparam env typeparam = Type.(
      let loc = Reason.loc_of_reason typeparam.reason in
      let name = typeparam.name in
      { env with tparams = (name, loc) :: env.tparams }
    )
  end


  (**************)
  (* Type ops   *)
  (**************)

  (* Simplify a list of types by:
     - returning the "zero" element if such exists, or
     - filtering out "one" elements
  *)
  let simplify_zero_one ~zero ~one =
    let rec simplify_aux acc = function
    | [] -> acc
    | t::ts ->
      if t = zero then [t]
      else if t = one then simplify_aux acc ts
      else simplify_aux (t::acc) ts
    in
    simplify_aux []


  (* Simplify union/intersection types

     This visitor:
     - removes identical nodes from union and intersection types. (At the moment
       the comparison used is `Pervasives.compare`, but perhaps something more
       clever can replace this.)
     - removes the neutral element for union (resp. intersection) types, which
       is the bottom (resp. top) type.

     The Any state of this visitor is used to capture any change to the type
     structure.
  *)
  let simplify_unions_inters_visitor =
    let open Ty_visitor.AnyVisitor in
    object(self) inherit c
    method private simplify env ~break ~zero ~one ~make ts =
      mapM (self#type_ env) ts >>= fun ts' ->
      let ts' = List.concat (List.map break ts') in
      let ts' = List.sort Pervasives.compare ts' in
      let ts' = ListUtils.uniq ts' in
      let ts' = simplify_zero_one ~zero ~one ts' in
      tell (List.length ts <> List.length ts') >>| fun _ ->
      make ts'
    method! type_ env = function
    | Ty.Union (t1, t2, ts) ->
        let break = Ty.bk_union in
        let make = Ty.mk_union in
        self#simplify env ~break ~zero:Ty.Top ~one:Ty.Bot ~make (t1::t2::ts)
    | Ty.Inter (t1, t2, ts) ->
        let break = Ty.bk_inter in
        let make = Ty.mk_union in
        self#simplify env ~break ~zero:Ty.Bot ~one:Ty.Top ~make (t1::t2::ts)
    | t ->
        (* WARNING: do not descend to other constructors or this will
           get slow *)
        return t
    end

  let rec simplify_unions_inters t =
    let t', changed = simplify_unions_inters_visitor#type_ () t in
    if changed then simplify_unions_inters t' else t


  (* We wrap the union and intersection constructors with the following
     functions that keep types as small as possible.
  *)
  let uniq_union ts = ts |> Ty.mk_union |> simplify_unions_inters
  let uniq_inter ts = ts |> Ty.mk_inter |> simplify_unions_inters



  (*********************)
  (* Recursive types   *)
  (*********************)

  (* There are three phases in resolving a type variable:

     A. UNSEEN: Type variable has not been seen yet. It does not appear in the
        type cache.

     B. UNDER RESOLUTION: Variable has been seen at least once and it is set to
        "under resolution". It appears in the type_cache as a mapping from
        the original `Type.tvar` to a fresh `Ty.tvar`. This binding is important
        for termination purposes as well as determining if the variable appears
        free in a type context.

     C. RESOLVED: All lower bounds of the variable have been normalized and so
        the variable is considered resolved. The cache is updated with the final
        type or an error message.

  *)

  (* Helper functions *)

  (* We shouldn't really create bare Mu types for two reasons.

     First, substitutions and type applications may replace the recursive variables with
     a non-recursive type part, rendering the entire type non-recursive. When
     `check_recursive` is set to true, we first perform the check.

     Second, TypeAliases and Mu types can both imply recursion, so we have little
     to gain from nesting them. Therefore, before creating a Mu type, we should check if
     the type under the Mu is a TypeAlias and if it is then use that structure to
     express the recursion. To do this we replace 'i' for 'ta_name' in the body of
     the alias.

     PV: The second one is a seemingly awkward fix. However, I am not very concerned as
     `TypeAlias` is abusively treated as a type. Eventually, the goal is to include it
     in a "module element" category rather than the Ty.t structure.
  *)
  let mk_mu ~check_recursive i t =
    let is_rec =
      if check_recursive
      then Ty_utils.FreeVars.is_free_in ~is_top:true i t
      else true in
    if is_rec then
      Ty.(match t with
      | TypeAlias {ta_name;_} -> Ty_utils.subst (RVar i) (true, ta_name) t
      | _ -> Mu (i, t))
    else t

  (* When inferring recursive types, the top-level appearances of the recursive
     variable should be eliminated. This visitor performs the following
     transformations:

       (recursive var: X , type: X           ) ==> Bot
       (recursive var: X , type: X | t       ) ==> Bot | t
       (recursive var: X , type: X & t       ) ==> Top & t
       (recursive var: X , type: mu Y . X | t) ==> mu Y . Bot | t

     The visitor only descends down to the first concrete constructor
     (e.g. Function, Class) and is applied to the subparts of unions,
     intersections and recursive types.

     It is expected to followed by type minimization, so that the introduced
     Bot and Top can be eliminated.
  *)

  module RemoveTopLevelTvarVisitor = struct
    module Env = struct
      type t = U (* union context *)
             | I (* intersection context *)
      let descend _ e = e
      (* The starting state is that of a union. This allows for the behavior
         outlined above.
      *)
      let init = U
      (* The "zero" element is Bot (resp. Top) if we're in a union (resp.
         intersection) context, so that a subsequent minimization pass
         eliminates it.
      *)
      let zero = function
        | I -> Ty.Top
        | U -> Ty.Bot
    end

    module M = Ty_visitor.MakeAny(Env)
    open M

    let run v =
      let visitor = object(self) inherit c
        method! type_ env = function
        | Ty.Union (t0,t1,ts) ->
            mapM (self#type_ Env.U) (t0::t1::ts) >>| Ty.mk_union
        | Ty.Inter (t0,t1,ts) ->
            mapM (self#type_ Env.I) (t0::t1::ts) >>| Ty.mk_inter
        | Ty.TVar (Ty.RVar v', _) when v = v' ->
            tell true >>| fun _ -> Env.zero env
        | Ty.Mu (v, t) ->
            self#type_ env t >>| mk_mu ~check_recursive:true v
        | t ->
            return t
      end
      in
      visitor#type_ Env.init
  end


  (* Constructing recursive types.

     This function is expected to be called after fully normalizing the lower
     bounds of a type variable `v` and constructing a type `t`. `free_vars` is
     the set of free variables appearing in `t`. This information is available
     from the state of the monad.

     To determine if we truly have a recursive type we take the following into
     account:

      - If `v` does NOT appear in `free_vars`, then `t` is NOT recursive, so
        we return it as-is.

      - If `v` appears in `free_vars`, we may be dealing with a recursive type
        but we also might have a degenerate case like this one:

          Mu (v, v | string)

        which is not a recursive type. (It is equivalent to string.)
        So, first we simplify the type by performing the "remove_top_level_tvar"
        transformation and some subsequent simplifications. Then if the type
        changed we check again if `v` is in the free variables.

        NOTE that we need to recompute free vars since the simplifications might
        have eliminated some of them. Here we use the FreeVars module. This is
        an expensive pass, which is why we avoid doing it if it's definitely
        not a recursive type.
  *)
  let to_recursive free_vars i t =
    if VSet.mem i free_vars then
      (* Maybe recursive (might be a degenerate) *)
      let t, changed = RemoveTopLevelTvarVisitor.run i t in
      let t = if changed then simplify_unions_inters t else t in
      (* If not changed then all free_vars are still in, o.w. recompute free vars *)
      mk_mu ~check_recursive:changed i t
    else
      (* Definitely not recursive *)
      t


  (***********************)
  (* Construct built-ins *)
  (***********************)

  let opt_param = Ty.({ prm_optional = true })
  let non_opt_param = Ty.({ prm_optional = false })

  let mk_fun ?(params=[]) ?rest ?tparams ret = Ty.(
    Fun {
      fun_params = params;
      fun_rest_param = rest;
      fun_return = ret;
      fun_type_params = tparams;
    }
  )

  let mk_tparam ?bound ?(pol=Ty.Neutral) ?default name = Ty.({
    tp_name = name;
    tp_bound = bound;
    tp_polarity = pol;
    tp_default = default;
  })

  let symbol_from_loc env loc name =
    let open File_key in
    let symbol_source = Loc.source loc in
    let provenance =
      match symbol_source with
      | Some LibFile _ -> Ty.Library loc
      | Some (SourceFile _) ->
        let current_source = env.Env.file in
        (* Locally defined name *)
        if Some current_source = symbol_source
        then Ty.Local loc
        (* Otherwise it is one of:
           - Imported, or
           - Remote (defined in a different file but not imported in this one) *)
        else (match SMap.get name env.Env.imported_names with
          | Some loc' when loc = loc' -> Ty.Imported loc
          | _ -> Ty.Remote loc)
      | Some (JsonFile _)
      | Some (ResourceFile _) -> Ty.Local loc
      | Some Builtins -> Ty.Builtin
      | None -> Ty.Local loc
    in
    Ty.Symbol (provenance, name)

  (* TODO due to repositioninig `reason_loc` may not point to the actual
     location where `name` was defined. *)
  let symbol_from_reason env reason name =
    let def_loc = Reason.def_loc_of_reason reason in
    symbol_from_loc env def_loc name


  (*************************)
  (* Main transformation   *)
  (*************************)

  let rec type__ ~env t = type_poly ~env t

  (* Before we start pattern-matching on the structure of the input type, we can
     reconstruct some types based on attached reasons. Two cases are of interest here:
     - Type parameters: we use RPolyTest reasons for these
     - Type aliases: we use RTypeAlias reasons for these *)

  and type_poly ~env t =
    (* The RPolyTest description is used for types that represent type parameters.
       When normalizing, we want such types to be replaced by the type parameter,
       whose name is part of the description, but only in the case that the parameter
       is in scope. The reason we need to make this distinction is that bound tests
       may exit the scope of the structure that introduced them, in which case we
       do not perform the substitution. There instead we unfold the underlying type. *)
    let reason = Type.reason_of_t t in
    match desc_of_reason ~unwrap:false reason with
    | RPolyTest (name, _) ->
      let loc = Reason.def_loc_of_reason reason in
      let default t = type_with_alias_reason ~env t in
      Env.lookup_tparam ~default env t name loc
    | _ ->
      type_with_alias_reason ~env t

  and type_with_alias_reason ~env t =
    if C.opt_expand_type_aliases then type_after_reason ~env t else
    let reason = Type.reason_of_t t in
    match desc_of_reason ~unwrap:false reason with
    | RTypeAlias (name, _) ->
      (* The default action is to avoid expansion by using the type alias name.
         The one case where we want to skip this process is when recovering the
         body of a type alias A. In that case the environment field under_type_alias
         will be 'Some A'. If the type alias name in the reason is also A, then we
         are still at the top-level of the type-alias, so we proceed by expanding
         one level preserving the same environment. *)
      let continue =
        match env.Env.under_type_alias with
        | Some name' -> name = name'
        | None -> false
      in
      if continue then type_after_reason ~env t else
      let symbol = symbol_from_reason env reason name in
      return (Ty.named_t symbol)
    | _ ->
      (* We are now beyond the point of the one-off expansion. Reset the environment
         assigning None to under_type_alias, so that aliases are used in subsequent
         invocations. *)
      let env = Env.{ env with under_type_alias = None } in
      type_after_reason ~env t

  and type_after_reason ~env t =
    let open Type in
    let env = Env.descend env in
    match t with
    | OpenT (_, id) -> type_variable ~env id
    | BoundT tparam -> bound_t ~env t tparam
    | AnnotT (t, _) -> type__ ~env t
    | EvalT (t, d, id) -> eval_t ~env t id d
    | ExactT (_, t) -> exact_t ~env t
    | CustomFunT (_, f) -> custom_fun ~env f
    | InternalT i -> internal_t ~env t i
    | MatchingPropT _ -> return Ty.Bot
    | AnyWithUpperBoundT t ->
      type__ ~env t >>| fun ty ->
      Ty.generic_builtin_t "$SubType" [ty]
    | AnyWithLowerBoundT t ->
      type__ ~env t >>| fun ty ->
      Ty.generic_builtin_t "$SuperType" [ty]
    | DefT (_, MixedT _) -> return Ty.Top
    | DefT (_, AnyT) -> return Ty.Any
    | DefT (_, AnyObjT) -> return Ty.AnyObj
    | DefT (_, AnyFunT) -> return Ty.AnyFun
    | DefT (_, VoidT) -> return Ty.Void
    | DefT (_, NumT _) -> return Ty.Num
    | DefT (_, StrT _) -> return Ty.Str
    | DefT (_, BoolT _) -> return Ty.Bool
    | DefT (_, EmptyT) -> return Ty.Bot
    | DefT (_, NullT) -> return Ty.Null
    | DefT (_, SingletonNumT (_, lit)) -> return (Ty.NumLit lit)
    | DefT (_, SingletonStrT lit) -> return (Ty.StrLit lit)
    | DefT (_, SingletonBoolT lit) -> return (Ty.BoolLit lit)
    | DefT (_, MaybeT t) ->
      type__ ~env t >>| fun t -> uniq_union [Ty.Void; Ty.Null; t]
    | DefT (_, OptionalT t) ->
      type__ ~env t >>| fun t -> uniq_union [Ty.Void; t]
    | DefT (_, FunT (_, _, f)) ->
      fun_ty ~env f None >>| fun t -> Ty.Fun t
    | DefT (_, ObjT o) ->
      obj_ty ~env o >>| fun t -> Ty.Obj t
    | DefT (_, ArrT a) -> arr_ty ~env a
    | DefT (_, UnionT rep) ->
      let t0, (t1, ts) = UnionRep.members_nel rep in
      type__ ~env t0 >>= fun t0 ->
      type__ ~env t1 >>= fun t1 ->
      mapM (type__ ~env) ts >>| fun ts ->
      uniq_union (t0::t1::ts)
    | DefT (_, IntersectionT rep) ->
      let t0, (t1, ts) = InterRep.members_nel rep in
      type__ ~env t0 >>= fun t0 ->
      type__ ~env t1 >>= fun t1 ->
      mapM (type__ ~env) ts >>| fun ts ->
      uniq_inter (t0::t1::ts)
    | DefT (_, PolyT (ps, t, _)) -> poly_ty ~env t ps
    | DefT (r, TypeT (kind, t)) -> type_t ~env r kind t None
    | DefT (_, TypeAppT (_, t, ts)) -> type_app ~env t ts
    | DefT (r, InstanceT (_, _, _, t)) -> instance_t ~env r t
    | DefT (_, ClassT t) -> class_t ~env t None
    | DefT (_, IdxWrapper t) -> type__ ~env t
    | ThisClassT (_, t) -> this_class_t ~env t None
    (* NOTE For now we are ignoring the "this" type here. *)
    | ThisTypeAppT (_, c, _, None) -> type__ ~env c
    | ThisTypeAppT (_, c, _, Some ts) -> type_app ~env c ts
    | KeysT (_, t) ->
      type__ ~env t >>| fun ty ->
      Ty.generic_builtin_t "$Keys" [ty]
    | OpaqueT (r, o) -> opaque_t ~env r o
    | ReposT (_, t) -> type__ ~env t
    | ShapeT t -> type__ ~env t
    | TypeDestructorTriggerT _ -> return Ty.Bot
    | MergedT (_, uses) -> merged_t ~env uses
    | ExistsT _ -> return Ty.Exists
    | ObjProtoT _ -> return (Ty.builtin_t "Object.prototype")
    | FunProtoT _ -> return (Ty.builtin_t "Function.prototype")
    | OpenPredT (_, t, _, _) -> type__ ~env t

    | FunProtoApplyT _ ->
      if C.opt_expand_internal_types then
        (* Function.prototype.apply: (thisArg: any, argArray?: any): any *)
        return Ty.(mk_fun
          ~params:[
            (Some "thisArg", Any, non_opt_param);
            (Some "argArray", Any, opt_param);
          ]
          Any)
      else
        return Ty.(TypeOf (Ty.builtin_symbol "Function.prototype.apply"))

    | FunProtoBindT _ ->
      if C.opt_expand_internal_types then
        (* Function.prototype.bind: (thisArg: any, ...argArray: Array<any>): any *)
        return Ty.(mk_fun
          ~params:[(Some "thisArg", Any, non_opt_param)]
          ~rest:(Some "argArray", Arr Any)
          Any)
      else
         return Ty.(TypeOf (Ty.builtin_symbol "Function.prototype.bind"))

    | FunProtoCallT _ ->
      if C.opt_expand_internal_types then
        (* Function.prototype.call: (thisArg: any, ...argArray: Array<any>): any *)
        return Ty.(mk_fun
          ~params:[(Some "thisArg", Any, non_opt_param)]
          ~rest:(Some "argArray", Arr Any)
          Any)
      else
         return Ty.(TypeOf (Ty.builtin_symbol "Function.prototype.call"))

    | ModuleT (reason, _, _) -> module_t env reason t

    | DefT (_, CharSetT _)
    | NullProtoT _ ->
      terr ~kind:UnsupportedTypeCtor (Some t)


  (* Type variable normalization (input: a type variable `id`)

     Step 1: Use `root_id` as a proxy for `id`.

     Step 2: Check the cache in case root_id has been computed before. There are
             several cases here:
             a. The variable is "under resolution" state (recursive variable).
                Exit with the cached result and add the variable to the free
                variable set.
             b. The variable is "resolved". Return the result.
             c. The variable resolution led to an error. Propagate error.
             d. The variable has never been seen before. Go to step 3.

     Step 3: Start variable resolution.
  *)
  and type_variable ~env id =
    find_cons id >>= fun (root_id, constraints) ->      (* step 1 *)
    find_tvar root_id >>= function                      (* step 2 *)
      | Some (Ok Ty.(TVar (Ty.RVar v, _) as t)) ->      (* step 2a *)
        modify State.(fun st -> { st with
          free_tvars = VSet.add v st.free_tvars
        }) >>= fun _ ->
        return t
      | Some (Ok t) -> return t                         (* step 2b *)

      | Some (Error s) -> error s                       (* step 2c *)
      | None ->                                         (* step 2d *)
        resolve_tvar ~env constraints root_id           (* step 3 *)

  (* Resolve a type variable (encountered for the first time)

     Resolving a type variable can either succeed and return a Ty.t, or fail and
     return an error. Since we are caching the result of this resolution we need
     to save a `(Ty.t, error) result`. For this reason we isolate the execution
     of the monad under the current state and cache the "monadic" result.
  *)
  and resolve_tvar ~env cons root_id =
    let open State in
    fresh_num >>= fun rid ->
    let rvar = Ty.RVar rid in
    (* Set current variable "under resolution" *)
    update_tvar_cache root_id (Ok (Ty.TVar (rvar, None))) >>= fun _ ->
    get >>= fun in_st ->

    (* Resolve the tvar *)
    let ty_res, out_st = run in_st (resolve_bounds ~env cons) in

    (* Create a recursive type (if needed) *)
    let ty_res = Core_result.map
      ~f:(to_recursive out_st.free_tvars rid) ty_res
    in

    (* Reset state by removing the current tvar from the free vars set *)
    let out_st =
      { out_st with free_tvars = VSet.remove rid out_st.free_tvars }
    in
    put out_st >>= fun _ ->

    (* Update cache with final result *)
    update_tvar_cache root_id ty_res >>= fun _ ->

    (* Throw the error if one was encountered *)
    match ty_res with
    | Ok ty -> return ty
    | Error e -> error e

  (* Resolving a type variable amounts to normalizing its lower bounds and
     taking their union.
  *)
  and resolve_bounds ~env =
    let open Constraint in
    function
    | Resolved t -> type__ ~env t
    | Unresolved bounds ->
      let ts = T.TypeMap.keys bounds.lower in
      mapM (type__ ~env) ts >>|
      uniq_union

  and bound_t ~env t tparam =
    let open Type in
    let { reason; name; _ } = tparam in
    let loc = Reason.def_loc_of_reason reason in
    let default t = terr ~kind:BadBoundT (Some t) in
    Env.lookup_tparam ~default env t name loc

  and fun_ty ~env f fun_type_params =
    let {T.params; rest_param; return_t; _} = f in
    mapM (fun_param ~env) params >>= fun fun_params ->
    fun_rest_param_t ~env rest_param >>= fun fun_rest_param ->
    type__ ~env return_t >>= fun fun_return ->
    return {Ty.fun_params; fun_rest_param; fun_return; fun_type_params}

  and method_ty ~env t =
    let open Type in
    match t with
    | DefT (_, FunT (_, _, f)) ->
      fun_ty ~env f None
    | DefT (_, PolyT (ps, DefT (_, FunT (_, _, f)), _)) ->
      mapM (type_param ~env) ps >>= fun ps ->
      fun_ty ~env f (Some ps)
    | _ ->
      terr ~kind:BadMethodType (Some t)

  and fun_param ~env (x, t) =
    opt_t ~env t >>= fun (t, prm_optional) ->
    return (x, t, { Ty.prm_optional })

  and fun_rest_param_t ~env = function
    | Some (x, _, t) -> type__ ~env t >>| fun t -> Some (x,t)
    | _ -> return None

  and obj_ty ~env {T.flags = {T.exact; T.frozen; _}; props_tmap; dict_t; _} =
    let obj_exact = exact in
    let obj_frozen = frozen in
    obj_props ~env props_tmap dict_t >>| fun obj_props ->
    {Ty.obj_exact; obj_frozen; obj_props}

  and obj_props ~env id dict =
    let dispatch (x, p) =
      if x = "$call"
        then call_prop ~env p
        else obj_prop ~env x p
    in
    get_cx >>= fun cx ->
    let props = SMap.bindings (Context.find_props cx id) in
    concat_fold_m dispatch props >>= fun obj_props ->
    match dict with
    | Some d -> index_prop ~env d >>| fun i -> i::obj_props
    | None -> return obj_props

  and obj_prop ~env x p =
    match p with
    | T.Field (_, t, polarity) ->
      let fld_polarity = type_polarity polarity in
      opt_t ~env t >>| fun (t, fld_optional) ->
      [Ty.(NamedProp (x, Field (t, {fld_polarity; fld_optional})))]
    | T.Method (_, t) ->
      method_ty ~env t >>| fun t -> [Ty.NamedProp (x, Ty.Method t)]
    | T.Get (_, t) ->
      type__ ~env t >>| fun t -> [Ty.NamedProp (x, Ty.Get t)]
    | T.Set (_, t) ->
      type__ ~env t >>| fun t -> [Ty.NamedProp (x, Ty.Set t)]
    | T.GetSet (loc1, t1, loc2, t2) ->
      obj_prop ~env x (T.Get (loc1, t1)) >>= fun p1 ->
      obj_prop ~env x (T.Set (loc2, t2)) >>| fun p2 ->
      p1@p2

  and call_prop ~env =
    let intersection = function
    | T.DefT (_, T.IntersectionT rep) -> T.InterRep.members rep
    | t -> [t]
    in
    let multi_call ts =
      mapM (method_ty ~env) ts >>| fun ts ->
      List.map (fun t -> Ty.CallProp t) ts
    in
    function
    | T.Method (_, t) -> intersection t |> multi_call
    | T.Field (_, t, _) -> intersection t |> multi_call
    | _ -> terr ~kind:BadCallProp None

  and index_prop ~env d =
    let {T.dict_polarity; dict_name; key; value} = d in
    let dict_polarity = type_polarity dict_polarity in
    type__ ~env key >>= fun dict_key ->
    type__ ~env value >>| fun dict_value ->
    Ty.(IndexProp {dict_polarity; dict_name; dict_key; dict_value})

  and arr_ty ~env = function
    | T.ArrayAT (t, _)
    | T.ROArrayAT t ->
      type__ ~env t >>| fun t -> Ty.Arr t
    | T.TupleAT (_, ts) ->
      mapM (type__ ~env) ts >>| fun ts -> Ty.Tup ts
    | T.EmptyAT ->
      return Ty.Bot

  and name_of_instance_reason r =
    (* This should cover all cases but throw an error just in case. *)
    match desc_of_reason ~unwrap:false r  with
    | RType name
    | RIdentifier name -> return name
    | RReactComponent -> return "React$Component"
    | r ->
      let msg = spf "could not extract name from reason: %s"
        (Reason.string_of_desc r) in
      terr ~kind:BadInstanceT ~msg None

  and instance_t ~env r inst =
    let open Type in
    name_of_instance_reason r >>= fun name ->
    let symbol = symbol_from_reason env r name in
    let args = SMap.bindings inst.type_args in
    mapM (fun (_, (_, t)) -> type__ ~env t) args >>| function
    | [] -> Ty.Generic (symbol, inst.structural, None)
    | xs -> Ty.Generic (symbol, inst.structural, Some xs)

  and class_t ~env t ps =
    let rec class_t_aux = function
    | Ty.Class (name, structural, _) ->
      return (Ty.Class (name, structural, ps))
    | Ty.Generic (name, structural, _) ->
      return (Ty.Class (name, structural, ps))
    | (Ty.Bot | Ty.Exists | Ty.Any | Ty.Top) as b ->
      return b
    | Ty.Union (t0,t1,ts) ->
      class_t_aux t0 >>= fun t0 ->
      class_t_aux t1 >>= fun t1 ->
      mapM class_t_aux ts >>| fun ts ->
      uniq_union (t0::t1::ts)
    | Ty.Inter (t0,t1,ts) ->
      class_t_aux t0 >>= fun t0 ->
      class_t_aux t1 >>= fun t1 ->
      mapM class_t_aux ts >>| fun ts ->
      uniq_inter (t0::t1::ts)
    | Ty.Bound (Ty.Symbol (_, "this")) as t ->
      return t
    | ty ->
      let msg = spf "normalized class arg: %s" (Ty_debug.dump_t ty) in
      terr ~kind:BadClassT ~msg (Some t)
    in
    type__ ~env t >>= class_t_aux

  and this_class_t ~env t ps =
    class_t ~env t ps

  and poly_ty ~env t typeparams =
    let env, results = List.fold_left (fun (env, rs) typeparam ->
      let r = type_param ~env typeparam in
      (Env.add_typeparam env typeparam, r::rs)
    ) (env, []) typeparams in
    List.rev results |> all >>= fun ps ->
    let ps = match ps with [] -> None | _ -> Some ps in
    match t with
    | T.DefT (_, T.ClassT t) -> class_t ~env t ps
    | T.ThisClassT (_, t) -> this_class_t ~env t ps
    | T.DefT (r, T.TypeT (kind, t)) -> type_t ~env r kind t ps
    | T.DefT (_, T.FunT (_, _, f)) ->
      fun_ty ~env f ps >>| fun fun_t -> Ty.Fun fun_t
    | _ ->
      terr ~kind:BadPoly (Some t)

  (* Type Aliases *)
  and type_t =
    let open Type in
    let mk_type_alias symbol ps = function
    | Ty.TypeAlias _ -> terr ~kind:BadTypeAlias ~msg:"nested type alias" None
    | Ty.Class _ as t -> return t
    | t -> return (Ty.named_alias symbol ?ta_tparams:ps ~ta_type:t)
    in
    (* NOTE the use of the reason within `t` instead of the one passed with
       the constructor TypeT. The latter is an RType, which is somewhat more
       unwieldy as it is used more pervasively. *)
    let local env t ps =
      let reason = TypeUtil.reason_of_t t in
      match desc_of_reason ~unwrap:false reason with
      | RTypeAlias (name, _) ->
        let env = Env.{ env with under_type_alias = Some name } in
        type__ ~env t >>= fun body ->
        let symbol = symbol_from_reason env reason name in
        mk_type_alias symbol ps body
      | _ -> terr ~kind:BadTypeAlias ~msg:"local" (Some t)
    in
    let import_symbol env r t =
      match desc_of_reason ~unwrap:false r with
      | RNamedImportedType (name, _)
      | RDefaultImportedType (name, _)
      | RImportStarType name
      | RImportStarTypeOf name
      | RImportStar name ->
        return (symbol_from_reason env r name)
      | _ -> terr ~kind:BadTypeAlias ~msg:"import" (Some t)
    in
    let import env r t ps =
      import_symbol env r t >>= fun symbol ->
      let Ty.Symbol (_, name) = symbol in
      let env = Env.{ env with under_type_alias = Some name } in
      type__ ~env t >>= fun body ->
      mk_type_alias symbol ps body
    in
    let import_typeof env r t ps = import env r t ps in
    let import_fun env r t ps = import env r t ps in
    let opaque env t ps =
      match t with
      | OpaqueT (r, o) -> opaque_type_t ~env r o ps
      | _ -> terr ~kind:BadTypeAlias ~msg:"opaque" (Some t)
    in
    fun ~env r kind t ps ->
      match kind with
      | TypeAliasKind -> local env t ps
      | ImportClassKind -> class_t ~env t ps
      | ImportTypeofKind -> import_typeof env r t ps
      | ImportFunKind -> import_fun env r t ps
      | OpaqueKind -> opaque env t ps
      (* The following cases are not common *)
      | TypeParamKind -> terr ~kind:BadTypeAlias ~msg:"typeparam" (Some t)
      | InstanceKind -> terr ~kind:BadTypeAlias ~msg:"instance" (Some t)

  and exact_t ~env t =
    type__ ~env  t >>| Ty.mk_exact

  and type_app ~env t targs =
    type__ ~env t >>= fun ty ->
    mapM (type__ ~env) targs >>= fun targs ->
    match ty with
    | Ty.Class (name, _, _)
    | Ty.TypeAlias { Ty.ta_name=name; _ } ->
      return (Ty.generic_t name targs)
    | Ty.Any ->
      return Ty.Any
    (* "Fix" type application on recursive types *)
    | Ty.TVar (Ty.RVar v, None) ->
      return (Ty.TVar (Ty.RVar v, Some targs))
    | Ty.Bot ->
      return Ty.Bot
    | _ ->
      let msg = spf "Normalized receiver type: %s" (Ty_debug.dump_t ty) in
      terr ~kind:BadTypeApp ~msg (Some t)

  and opaque_t ~env reason opaque_type =
    let name = opaque_type.Type.opaque_name in
    let opaque_symbol = symbol_from_reason env reason name in
    return (Ty.named_t opaque_symbol)

  (* We are being a bit lax here with opaque types so that we don't have to
     introduce a new constructor in Ty.t to support all kinds of OpaqueT.
     If an underlying type is available, then we use that as the alias body.
     If not, we check for a super type and use that if there is one.
     Otherwise, we fall back to a bodyless TypeAlias.
  *)
  and opaque_type_t ~env reason opaque_type ta_tparams =
    let open Type in
    let name = opaque_type.opaque_name in
    get_cx >>= fun cx ->
    let current_source = Context.file cx in
    let opaque_source = Loc.source (def_loc_of_reason reason) in
    let opaque_symbol = symbol_from_reason env reason name in
    (* Compare the current file (of the query) and the file that the opaque
       type is defined. If they differ, then hide the underlying/super type.
       Otherwise, display the underlying/super type. *)
    if Some current_source <> opaque_source then
      return (Ty.named_alias ?ta_tparams opaque_symbol)
    else
      let t_opt = match opaque_type with
      | { underlying_t = Some t; _ }       (* opaque type A = number; *)
      | { super_t = Some t; _ } -> Some t  (* declare opaque type B: number; *)
      | _ -> None                          (* declare opaque type C; *)
      (* TODO: This will potentially report a remote name.
         The same fix for T25963804 should be applied here as well. *)
      in
      option (type__ ~env) t_opt >>| fun ta_type ->
      Ty.named_alias ?ta_tparams ?ta_type opaque_symbol

  and custom_fun_expanded ~env =
    let open Type in
    function
    (* Object.assign: (target: any, ...sources: Array<any>): any *)
    | ObjectAssign -> return Ty.(mk_fun
        ~params:[(Some "target", Any, non_opt_param)]
        ~rest:(Some "sources", Arr Any)
        Any
      )

    (* Object.getPrototypeOf: (o: any): any *)
    | ObjectGetPrototypeOf ->
      return Ty.(mk_fun ~params:[(Some "o", Any, non_opt_param)] Any)

    (* Object.setPrototypeOf: (o: any, p: any): any *)
    | ObjectSetPrototypeOf ->
      let params = [
        (Some "o", Ty.Any, non_opt_param);
        (Some "p", Ty.Any, non_opt_param);
      ] in
      return (mk_fun ~params Ty.Any)

    (* var idx:
       <IdxObject: AnyObj, IdxResult>
       (obj: IdxObject, pathCallback: (demaybefiedObj: IdxObject) => IdxResult)
       => ?IdxResult;
    *)
    | Idx ->
      let idxObject = Ty.builtin_t "IdxObject" in
      let idxResult = Ty.builtin_t "IdxResult" in
      let tparams = [
        mk_tparam ~bound:Ty.AnyObj "IdxObject";
        mk_tparam "IdxResult";
      ]
      in
      let pathCallback = mk_fun
        ~params:[(Some "demaybefiedObj", idxObject, non_opt_param)]
        idxResult
      in
      let params = [
        (Some "obj", idxObject, non_opt_param);
        (Some "pathCallback", pathCallback, non_opt_param);
      ]
      in
      return (mk_fun ~tparams ~params (Ty.mk_maybe idxResult))

    (* debugPrint: (_: any[]) => void *)
    | DebugPrint -> return Ty.(
        mk_fun ~params:[(Some "_", Arr Any, non_opt_param)] Void
      )

    (* debugThrow: () => empty *)
    | DebugThrow -> return (mk_fun Ty.Bot)

    (* debugSleep: (seconds: number) => void *)
    | DebugSleep -> return Ty.(
        mk_fun ~params:[(Some "seconds", Num, non_opt_param)] Void
      )

    (* reactPropType: any (TODO) *)
    | ReactPropType _ -> return Ty.Any

    (* reactCreateClass: (spec: any) => ReactClass<any> *)
    | ReactCreateClass -> return Ty.(mk_fun
        ~params:[(Some "spec", Any, non_opt_param)]
        (generic_builtin_t "ReactClass" [Any])
      )

    (* 1. Component class:
          <T>(name: ReactClass<T>, config: T, children?: any) => React$Element<T>
       2. Stateless functional component
          type SFC<T> = (config: T, context: any) => React$Element<T>
          <T>(fn: SFC<T>, config: T, children?: any) => React$Element<T>
    *)
    | ReactCreateElement
    | ReactCloneElement
    | ReactElementFactory _ -> return Ty.(
        let param_t = mk_tparam "T" in
        let tparams = [param_t] in
        let t = Bound (Symbol (Builtin, "T")) in
        let params = [
          (Some "name", generic_builtin_t "ReactClass" [t], non_opt_param);
          (Some "config", t, non_opt_param);
          (Some "children", Any, opt_param);
        ]
        in
        let reactElement = generic_builtin_t "React$Element" [t] in
        let f1 = mk_fun ~tparams ~params reactElement in
        let params = [
          (Some "config", t, non_opt_param);
          (Some "context", Any, non_opt_param);
        ]
        in
        let sfc = mk_fun ~tparams ~params reactElement in
        let params = [
          (Some "fn", sfc, non_opt_param);
          (Some "config", t, non_opt_param);
          (Some "children", Any, opt_param);
        ]
        in
        let f2 = mk_fun ~tparams ~params reactElement in
        mk_inter [f1; f2]
      )

    (* Fallback *)
    | t -> custom_fun_short ~env t

  and custom_fun_short ~env =
    let open Type in
    function
    | ObjectAssign -> return (Ty.builtin_t "Object$Assign")
    | ObjectGetPrototypeOf -> return (Ty.builtin_t "Object$GetPrototypeOf")
    | ObjectSetPrototypeOf -> return (Ty.builtin_t "Object$SetPrototypeOf")
    | Compose false -> return (Ty.builtin_t "$Compose")
    | Compose true -> return (Ty.builtin_t "$ComposeReverse")
    | ReactPropType t -> react_prop_type ~env t
    | ReactCreateClass -> return (Ty.builtin_t "React$CreateClass")
    | ReactCreateElement -> return (Ty.builtin_t "React$CreateElement")
    | ReactCloneElement -> return (Ty.builtin_t "React$CloneElement")
    | ReactElementFactory t ->
      type__ ~env t >>| fun t ->
      Ty.generic_builtin_t "React$ElementFactory" [t]
    | Idx -> return (Ty.builtin_t "$Facebookism$Idx")
    | DebugPrint -> return (Ty.builtin_t "$Flow$DebugPrint")
    | DebugThrow -> return (Ty.builtin_t "$Flow$DebugThrow")
    | DebugSleep -> return (Ty.builtin_t "$Flow$DebugSleep")

  and custom_fun env t =
    if C.opt_expand_internal_types
    then custom_fun_expanded env t
    else custom_fun_short env t

  and react_prop_type ~env =
    let open T.React.PropType in
    function
    | Primitive (_, t)   ->
      type__ ~env t >>| fun t ->
      Ty.generic_builtin_t "React$PropType$Primitive" [t]
    | Complex ArrayOf -> return (Ty.builtin_t "React$PropType$ArrayOf")
    | Complex InstanceOf -> return (Ty.builtin_t "React$PropType$ArrayOf")
    | Complex ObjectOf -> return (Ty.builtin_t "React$PropType$dbjectOf")
    | Complex OneOf -> return (Ty.builtin_t "React$PropType$OneOf")
    | Complex OneOfType -> return (Ty.builtin_t "React$PropType$OneOfType")
    | Complex Shape -> return (Ty.builtin_t "React$PropType$Shape")

  and internal_t ~env t =
    let open Type in
    function
    | ChoiceKitT _
    | ExtendsT _
    | ReposUpperT _ ->
      terr ~kind:BadInternalT (Some t)
    | OptionalChainVoidT r -> type__ ~env (DefT (r, VoidT));

  and param_bound ~env = function
    | T.DefT (_, T.MixedT _) -> return None
    | bound -> type__ ~env bound >>= fun b -> return (Some b)

  and default_t ~env = function
    | Some d -> type__ ~env d >>= fun d -> return (Some d)
    | _ -> return None

  and type_param ~env { T.name; bound; polarity; default; _ } =
    let tp_polarity = type_polarity polarity in
    param_bound ~env bound >>= fun tp_bound ->
    default_t ~env default >>= fun tp_default ->
    return { Ty.tp_name = name; tp_bound; tp_polarity; tp_default }

  and opt_t ~env t =
    let t, opt = match t with
    | T.DefT (_, T.OptionalT t) -> (t, true)
    | t -> (t, false)
    in
    type__ ~env t >>| fun t -> (t, opt)

  and type_polarity = function
    | T.Positive -> Ty.Positive
    | T.Negative -> Ty.Negative
    | T.Neutral -> Ty.Neutral

  and eval_t =
    let from_name env name t =
      type__ ~env t >>| fun ty ->
      Ty.generic_builtin_t name [ty]
    in
    fun ~env t id d ->
    match d with
    | T.DestructuringT (r, s) ->
      get_cx >>= fun cx ->
      begin try
        (* `eval_selector` may throw for `BoundT`. Catching here. *)
        return (Flow_js.eval_selector cx r t s id)
      with exn ->
        let msg = spf "Exception:%s" (Printexc.to_string exn) in
        terr ~kind:BadEvalT ~msg (Some t)
      end
      >>= type__ ~env

    | T.TypeDestructorT (_, _, d) ->
      begin match d with
      | T.NonMaybeType -> from_name env "NonMaybeType" t
      | T.ReactElementPropsType -> from_name env "React$ElementProps" t
      | T.ReactElementConfigType -> from_name env "React$ElementConfig" t
      | T.ReactElementRefType -> from_name env "React$ElementRef" t
      | _ ->
        get_cx >>= fun cx ->
        let evaluated = Context.evaluated cx in
        begin match IMap.get id evaluated with
        | Some cached_t -> type__ ~env cached_t
          (* this happens when, for example, the RHS of a destructuring is
             unconstrained, so we never evaluate the destructuring. so, make the
             destructured value also unconstrained... *)
        | _ -> return Ty.Bot
        end
      end

  and module_t env reason t =
    match desc_of_reason reason with
    | RModule name
    | RCommonJSExports name
    | RUntypedModule name ->
      let symbol = symbol_from_reason env reason name in
      return (Ty.Module symbol)
    | _ ->
      terr ~kind:UnsupportedTypeCtor (Some t)

  and use_t ~env = function
    | T.UseT (_, t) -> type__ ~env t
    | T.ReposLowerT (_, _, u) -> use_t ~env u
    | u ->
      let msg = spf "Use: %s" (Type.string_of_use_ctor u) in
      terr ~kind:BadUse ~msg None

  and merged_t ~env uses =
    if C.opt_fall_through_merged
    then return Ty.Top
    else mapM (use_t ~env) uses >>| uniq_inter


  (* Before we start normalizing the input type we populate our environment with
     aliases that are in scope due to typed imports. These are already inside the
     'imported_ts' portion of the context. This step includes the normalization
     of all imported types and the creation of a map to hold bindings of imported
     names to location of definition. This map will be used later to determine
     whether a located name (symbol) appearing is part of the file's imports or a
     remote (hidden or non-imported) name.
  *)
  let import_names cx state =
    let imported_ts = Context.imported_ts cx in
    SMap.fold (fun x t (st, m) -> Ty.(
      (* 'tparams' ought to be empty at this point *)
      let env = Env.init cx [] SMap.empty in
      match run st (type__ ~env t) with
      | (Ok (TypeAlias { ta_name = Symbol (Remote loc, _); _ }), st)
      | (Ok (Class (Symbol (Remote loc, _), _, _)), st) ->
        (st, SMap.add x loc m)
      | (_, st) ->
        (st, m)
    )) imported_ts (state, SMap.empty)

  let run_type cx state tparams imported_names t =
    let env = Env.init cx tparams imported_names in
    run state (type__ ~env t)

  (* Exposed API *)
  let from_schemes ~cx schemes =
    let state, imported_names = import_names cx (State.empty ~cx) in
    snd (ListUtils.fold_map (fun st (a, s) ->
      let Type_table.Scheme (tparams, t) = s in
      match run_type cx st tparams imported_names t with
      | Ok t, st -> (st, (a, Ok t))
      | Error s, st -> (st, (a, Error s))
    ) state schemes)

  let from_types ~cx ts =
    let state, imported_names = import_names cx (State.empty ~cx) in
    snd (ListUtils.fold_map (fun st (a, t) ->
      match run_type cx st [] imported_names t with
      | Ok t, st -> (st, (a, Ok t))
      | Error s, st -> (st, (a, Error s))
    ) state ts)

  let from_scheme ~cx scheme =
    let state, imported_names = import_names cx (State.empty ~cx) in
    let Type_table.Scheme (tparams, t) = scheme in
    fst (run_type cx state tparams imported_names t)

  let from_type ~cx t =
    let state, imported_names = import_names cx (State.empty ~cx) in
    fst (run_type cx state [] imported_names t)

  let fold_hashtbl ~cx ~f ~g ~htbl init =
    let state, imported_names = import_names cx (State.empty ~cx) in
    let _, acc = Hashtbl.fold (fun loc x (st, acc) ->
      let Type_table.Scheme (tparams, t) = g x in
      let env = Env.init cx tparams imported_names in
      let result, st' = run st (type__ ~env t) in
      let acc' = f acc (loc, result) in
      (st', acc')
    ) htbl (state, init)
    in acc

end
