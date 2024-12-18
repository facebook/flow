(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
module Ast = Flow_ast

module type S = sig
  module L : Loc_sig.S

  module Scope_api : Scope_api_sig.S with module L = L

  module Ssa_api : Ssa_api.S with module L = L

  module Provider_api : Provider_api.S with module L = L

  type def_loc_type =
    | OrdinaryNameLoc
    | FunctionParamLoc
    | PatternLoc
    | ExpressionLoc
    | ArrayProviderLoc
    | FunctionThisLoc
    | ClassSelfLoc
    | ClassInstanceThisLoc
    | ClassStaticThisLoc
    | ClassInstanceSuperLoc
    | ClassStaticSuperLoc
  [@@deriving show]

  type autocomplete_hooks = {
    id_hook: string -> L.t -> bool;
    literal_hook: L.t -> bool;
    obj_prop_decl_hook: string -> L.t -> bool;
  }

  module EnvKey : sig
    include Flow_map.OrderedType with type t = def_loc_type * L.t

    val equal : t -> t -> bool
  end

  module EnvMap : sig
    include WrappedMap.S with type key = EnvKey.t

    val add_ordinary : L.t -> 'a -> 'a t -> 'a t

    val find_ordinary : L.t -> 'a t -> 'a

    val find_opt_ordinary : L.t -> 'a t -> 'a option

    val update_ordinary : L.t -> ('a option -> 'a option) -> 'a t -> 'a t
  end

  module EnvSet : Flow_set.S with type elt = EnvKey.t

  type literal_check =
    | SingletonNum of ALoc.t * bool * float * string
    | SingletonBool of ALoc.t * bool
    | SingletonStr of ALoc.t * bool * string

  type read_loc = L.t

  type write_loc =
    | Write of L.t Reason.virtual_reason
    | EmptyArray of {
        reason: L.t Reason.virtual_reason;
        arr_providers: L.LSet.t;
      }
    | IllegalWrite of L.t Reason.virtual_reason
    | Uninitialized of L.t Reason.virtual_reason
    | Undeclared of string * L.t
    | Refinement of {
        refinement_id: int;
        writes: write_locs;
        write_id: int option;
      }
    | FunctionThis of L.t Reason.virtual_reason
    | GlobalThis of L.t Reason.virtual_reason
    | IllegalThis of L.t Reason.virtual_reason
    | ClassInstanceThis of L.t Reason.virtual_reason
    | ClassStaticThis of L.t Reason.virtual_reason
    | ClassInstanceSuper of L.t Reason.virtual_reason
    | ClassStaticSuper of L.t Reason.virtual_reason
    | ModuleScoped of string
    | Global of string
    | Projection of L.t
    | Unreachable of L.t
    | Undefined of L.t Reason.virtual_reason
    | Number of L.t Reason.virtual_reason
    | DeclaredFunction of L.t

  and write_locs = write_loc list

  type val_kind =
    | Type of {
        imported: bool;
        type_only_namespace: bool;
      }
    | Value
    | Internal

  type read = {
    def_loc: L.t option;
    write_locs: write_locs;
    val_kind: val_kind;
    name: string option;
    id: int option;
  }

  type values = read L.LMap.t

  module Refi : sig
    type refinement_kind =
      | AndR of refinement_kind * refinement_kind
      | OrR of refinement_kind * refinement_kind
      | NotR of refinement_kind
      | TruthyR
      | NullR
      | UndefinedR
      | MaybeR
      | InstanceOfR of (L.t, L.t) Ast.Expression.t
      | IsArrayR
      | BoolR of L.t
      | FunctionR
      | NumberR of L.t
      | BigIntR of L.t
      | ObjectR
      | StringR of L.t
      | SymbolR of L.t
      | SingletonBoolR of {
          loc: L.t;
          sense: bool;
          lit: bool;
        }
      | SingletonStrR of {
          loc: L.t;
          sense: bool;
          lit: string;
        }
      | SingletonNumR of {
          loc: L.t;
          sense: bool;
          lit: float * string;
        }
      | SingletonBigIntR of {
          loc: L.t;
          sense: bool;
          lit: int64 option * string;
        }
      (* `other_loc` is the location of expr in `x.foo === expr` *)
      | SentinelR of {
          prop: string;
          other_loc: L.t;
        }
      | PropExistsR of {
          propname: string;
          loc: L.t;
        }
      | PropNullishR of {
          propname: string;
          loc: L.t;
        }
      | PropIsExactlyNullR of {
          propname: string;
          loc: L.t;
        }
      | PropNonVoidR of {
          propname: string;
          loc: L.t;
        }
      | LatentR of {
          func: (L.t, L.t) Ast.Expression.t;
          targs: (L.t, L.t) Ast.Expression.CallTypeArgs.t option;
          arguments: (L.t, L.t) Ast.Expression.ArgList.t;
          index: int;
        }
      | PropTruthyR of {
          propname: string;
          loc: L.t;
        }
      | EqR of L.t
      | ImpossibleR
    [@@deriving show { with_path = false }]

    type refinement = {
      (* Locations that we can point to the user where the refinement happens. *)
      refining_locs: L.LSet.t;
      kind: refinement_kind;
    }
  end

  type refinement_kind = Refi.refinement_kind

  type refinement = Refi.refinement

  type env_entry =
    (* Not every assignment actually produces a write. For example, when a
     * const is reassigned it does not actually update the contents of the const
     * and crashes instead. In these types of writes where nothing can actually
     * be written, we don't want to produce extra error messages related to the type
     * incompatibility of the write that is never performed. When the new_env finds a
     * NonAssigningWrite it will not subtype the given type against the providers. *)
    | AssigningWrite of L.t virtual_reason
    | GlobalWrite of L.t virtual_reason
    | NonAssigningWrite

  type predicate_refinement = read * L.t * Pattern_helper.binding

  type type_guard_consistency_entry =
    (L.t, L.t) Ast.Expression.t option * L.t Reason.virtual_reason * read * read

  (* First element is havoc information. This value will be [Some empty] when
   * encountering a havocing event with no corresponding location. *)
  type type_guard_consistency_maps =
    (Loc_collections.ALocSet.t option * type_guard_consistency_entry list) L.LMap.t

  type pred_func_info =
    (L.t, L.t) Ast.Expression.t (* Call exp *)
    * (L.t, L.t) Ast.Expression.t (* Callee *)
    * (L.t, L.t) Ast.Expression.CallTypeArgs.t option
    * (L.t, L.t) Ast.Expression.ArgList.t

  type env_info = {
    scopes: Scope_api.info;
    env_values: values;
    env_refinement_invalidation_info: Refinement_invalidation.t L.LMap.t;
    env_entries: env_entry EnvMap.t;
    type_guard_consistency_maps: type_guard_consistency_maps;
    providers: Provider_api.info;
    refinement_of_id: int -> Refi.refinement;
    pred_func_map: pred_func_info L.LMap.t;
  }

  type 'l annot_loc =
    | Loc of 'l
    | Object of {
        loc: 'l;
        props: 'l list;
      }

  type env_invariant_failure =
    | NameDefOrderingFailure of {
        all: L.t list;
        roots: L.t list;
        missing_roots: L.t list;
      }
    | Impossible of string
    | ASTStructureOverride of string
    | NameDefGraphMismatch
    | MissingEnvEntry of string

  exception Env_invariant of L.t option * env_invariant_failure

  type cacheable_env_error =
    | ReferencedBeforeDeclaration of {
        def_loc: ALoc.t;
        name: string;
      }
    | BuiltinNameLookupFailed of string

  val map_result :
    f:('a -> 'b) ->
    ('a, 'a * cacheable_env_error Nel.t) result ->
    ('b, 'b * cacheable_env_error Nel.t) result

  val empty : env_info

  val has_assigning_write : def_loc_type * L.t -> env_entry EnvMap.t -> bool

  val is_global_var : read -> bool

  val write_locs_of_read_loc : values -> read_loc -> write_locs

  val writes_of_write_loc : for_type:bool -> Provider_api.info -> write_loc -> EnvKey.t list

  val refinements_of_write_loc : env_info -> write_loc -> refinement_kind list

  val show_refinement_kind : refinement_kind -> string

  val show_refinement_kind_without_locs : refinement_kind -> string
end

module Make
    (L : Loc_sig.S)
    (Ssa_api : Ssa_api.S with module L = L)
    (Scope_api : Scope_api_sig.S with module L = Ssa_api.L)
    (Provider_api : Provider_api.S with module L = Ssa_api.L) :
  S
    with module L = L
     and module Ssa_api = Ssa_api
     and module Scope_api = Scope_api
     and module Provider_api = Provider_api = struct
  module L = L
  module Ssa_api = Ssa_api
  module Scope_api = Scope_api
  module Provider_api = Provider_api

  module Scope_builder : Scope_builder_sig.S with module L = L and module Api = Scope_api =
    Scope_builder.Make (L) (Scope_api)

  type env_invariant_failure =
    | NameDefOrderingFailure of {
        all: L.t list;
        roots: L.t list;
        missing_roots: L.t list;
      }
    | Impossible of string
    | ASTStructureOverride of string
    | NameDefGraphMismatch
    | MissingEnvEntry of string

  exception Env_invariant of L.t option * env_invariant_failure

  type cacheable_env_error =
    | ReferencedBeforeDeclaration of {
        def_loc: ALoc.t;
        name: string;
      }
    | BuiltinNameLookupFailed of string

  type literal_check =
    | SingletonNum of ALoc.t * bool * float * string
    | SingletonBool of ALoc.t * bool
    | SingletonStr of ALoc.t * bool * string

  type def_loc_type =
    | OrdinaryNameLoc
    | FunctionParamLoc
    | PatternLoc
    | ExpressionLoc
    | ArrayProviderLoc
    | FunctionThisLoc
    | ClassSelfLoc
    | ClassInstanceThisLoc
    | ClassStaticThisLoc
    | ClassInstanceSuperLoc
    | ClassStaticSuperLoc
  [@@deriving show]

  type autocomplete_hooks = {
    id_hook: string -> L.t -> bool;
    literal_hook: L.t -> bool;
    obj_prop_decl_hook: string -> L.t -> bool;
  }

  module EnvKey = struct
    type t = def_loc_type * L.t

    let compare (t1, l1) (t2, l2) =
      if t1 = t2 then
        L.compare l1 l2
      else
        Stdlib.compare t1 t2

    let equal k1 k2 = compare k1 k2 = 0
  end

  module EnvSet = Flow_set.Make (EnvKey)

  module EnvMap = struct
    include WrappedMap.Make (EnvKey)

    let add_ordinary l = add (OrdinaryNameLoc, l)

    let find_ordinary l = find (OrdinaryNameLoc, l)

    let find_opt_ordinary l = find_opt (OrdinaryNameLoc, l)

    let update_ordinary l = update (OrdinaryNameLoc, l)
  end

  type read_loc = L.t

  type write_loc =
    | Write of L.t Reason.virtual_reason
    | EmptyArray of {
        reason: L.t Reason.virtual_reason;
        arr_providers: L.LSet.t;
      }
    | IllegalWrite of L.t Reason.virtual_reason
    | Uninitialized of L.t Reason.virtual_reason
    | Undeclared of string * L.t
    | Refinement of {
        refinement_id: int;
        writes: write_locs;
        write_id: int option;
      }
    | FunctionThis of L.t Reason.virtual_reason
    | GlobalThis of L.t Reason.virtual_reason
    | IllegalThis of L.t Reason.virtual_reason
    | ClassInstanceThis of L.t Reason.virtual_reason
    | ClassStaticThis of L.t Reason.virtual_reason
    | ClassInstanceSuper of L.t Reason.virtual_reason
    | ClassStaticSuper of L.t Reason.virtual_reason
    | ModuleScoped of string
    | Global of string
    | Projection of L.t
    | Unreachable of L.t
    | Undefined of L.t Reason.virtual_reason
    | Number of L.t Reason.virtual_reason
    | DeclaredFunction of L.t

  and write_locs = write_loc list

  module Refi = struct
    type refinement_kind =
      | AndR of refinement_kind * refinement_kind
      | OrR of refinement_kind * refinement_kind
      | NotR of refinement_kind
      | TruthyR
      | NullR
      | UndefinedR
      | MaybeR
      | InstanceOfR of (L.t, L.t) Ast.Expression.t
      | IsArrayR
      | BoolR of L.t
      | FunctionR
      | NumberR of L.t
      | BigIntR of L.t
      | ObjectR
      | StringR of L.t
      | SymbolR of L.t
      | SingletonBoolR of {
          loc: L.t;
          sense: bool;
          lit: bool;
        }
      | SingletonStrR of {
          loc: L.t;
          sense: bool;
          lit: string;
        }
      | SingletonNumR of {
          loc: L.t;
          sense: bool;
          lit: float * string;
        }
      | SingletonBigIntR of {
          loc: L.t;
          sense: bool;
          lit: int64 option * string;
        }
      | SentinelR of {
          prop: string;
          other_loc: L.t;
        }
      | PropExistsR of {
          propname: string;
          loc: L.t;
        }
      | PropNullishR of {
          propname: string;
          loc: L.t;
        }
      | PropIsExactlyNullR of {
          propname: string;
          loc: L.t;
        }
      | PropNonVoidR of {
          propname: string;
          loc: L.t;
        }
      | LatentR of {
          func: (L.t, L.t) Ast.Expression.t;
          targs: (L.t, L.t) Ast.Expression.CallTypeArgs.t option;
          arguments: (L.t, L.t) Ast.Expression.ArgList.t;
          index: int;
        }
      | PropTruthyR of {
          propname: string;
          loc: L.t;
        }
      | EqR of L.t
      | ImpossibleR
    [@@deriving show { with_path = false }]

    type refinement = {
      (* Locations that we can point to the user where the refinement happens. *)
      refining_locs: L.LSet.t;
      kind: refinement_kind;
    }
  end

  include Refi

  type val_kind =
    | Type of {
        imported: bool;
        type_only_namespace: bool;
      }
    | Value
    | Internal

  type read = {
    def_loc: L.t option;
    write_locs: write_locs;
    val_kind: val_kind;
    name: string option;
    id: int option;
  }

  type values = read L.LMap.t

  type env_entry =
    (* Not every assignment actually produces a write. For example, when a
     * const is reassigned it does not actually update the contents of the const
     * and crashes instead. In these types of writes where nothing can actually
     * be written, we don't want to produce extra error messages related to the type
     * incompatibility of the write that is never performed. When the new_env finds a
     * NonAssigningWrite it will not subtype the given type against the providers. *)
    | AssigningWrite of L.t virtual_reason
    | GlobalWrite of L.t virtual_reason
    | NonAssigningWrite

  type predicate_refinement = read * L.t * Pattern_helper.binding

  type type_guard_consistency_entry =
    (L.t, L.t) Ast.Expression.t option * L.t Reason.virtual_reason * read * read

  type type_guard_consistency_maps =
    (Loc_collections.ALocSet.t option * type_guard_consistency_entry list) L.LMap.t

  type pred_func_info =
    (L.t, L.t) Ast.Expression.t (* Call exp *)
    * (L.t, L.t) Ast.Expression.t (* Callee *)
    * (L.t, L.t) Ast.Expression.CallTypeArgs.t option
    * (L.t, L.t) Ast.Expression.ArgList.t

  type env_info = {
    scopes: Scope_api.info;
    env_values: values;
    env_refinement_invalidation_info: Refinement_invalidation.t L.LMap.t;
    env_entries: env_entry EnvMap.t;
    type_guard_consistency_maps: type_guard_consistency_maps;
    providers: Provider_api.info;
    refinement_of_id: int -> Refi.refinement;
    pred_func_map: pred_func_info L.LMap.t;
  }

  type 'l annot_loc =
    | Loc of 'l
    | Object of {
        loc: 'l;
        props: 'l list;
      }

  let empty =
    {
      scopes = Scope_builder.Acc.init;
      env_values = L.LMap.empty;
      env_refinement_invalidation_info = L.LMap.empty;
      env_entries = EnvMap.empty;
      type_guard_consistency_maps = L.LMap.empty;
      providers = Provider_api.empty;
      refinement_of_id = (fun _ -> raise (Env_invariant (None, Impossible "Empty env info")));
      pred_func_map = L.LMap.empty;
    }

  let map_result ~f res =
    match res with
    | Ok t -> Ok (f t)
    | Error (t, x) -> Error (f t, x)

  let has_assigning_write kind_and_loc values =
    match EnvMap.find_opt kind_and_loc values with
    | Some (AssigningWrite _)
    | Some (GlobalWrite _) ->
      true
    | Some NonAssigningWrite
    | None ->
      false

  let is_global_var { write_locs; _ } =
    let rec no_local_defs states =
      Base.List.for_all
        ~f:(function
          | Global _ -> true
          | Refinement { refinement_id = _; writes; write_id = _ } -> no_local_defs writes
          | Undefined _
          | Number _
          | DeclaredFunction _
          | Uninitialized _
          | EmptyArray _
          | Write _
          | IllegalWrite _
          | Unreachable _
          | Undeclared _
          | Projection _
          | GlobalThis _
          | IllegalThis _
          | FunctionThis _
          | ClassInstanceThis _
          | ClassStaticThis _
          | ClassInstanceSuper _
          | ClassStaticSuper _
          | ModuleScoped _ ->
            false)
        states
    in
    no_local_defs write_locs

  let write_locs_of_read_loc values read_loc =
    let { write_locs; def_loc = _; val_kind = _; name = _; id = _ } = L.LMap.find read_loc values in
    write_locs

  let writes_of_write_loc ~for_type providers write_loc =
    let rec f acc write_loc =
      match write_loc with
      | Refinement { refinement_id = _; write_id = _; writes } -> Base.List.fold writes ~init:acc ~f
      | Write r -> (OrdinaryNameLoc, Reason.loc_of_reason r) :: acc
      | EmptyArray { reason; arr_providers } ->
        (OrdinaryNameLoc, Reason.loc_of_reason reason)
        :: Base.List.fold (L.LSet.elements arr_providers) ~init:acc ~f:(fun acc l ->
               (ArrayProviderLoc, l) :: acc
           )
      | IllegalWrite _ -> acc
      | Uninitialized _ -> acc
      | Undeclared (_, loc) when for_type -> (OrdinaryNameLoc, loc) :: acc
      | Undeclared _ -> acc
      | FunctionThis r -> (FunctionThisLoc, Reason.loc_of_reason r) :: acc
      | GlobalThis _ -> acc
      | IllegalThis _ -> acc
      | ClassInstanceThis r -> (ClassInstanceThisLoc, Reason.loc_of_reason r) :: acc
      | ClassStaticThis r -> (ClassStaticThisLoc, Reason.loc_of_reason r) :: acc
      | ClassInstanceSuper r -> (ClassInstanceSuperLoc, Reason.loc_of_reason r) :: acc
      | ClassStaticSuper r -> (ClassStaticSuperLoc, Reason.loc_of_reason r) :: acc
      | ModuleScoped _ -> acc
      | Global _ -> acc
      | Projection l -> (OrdinaryNameLoc, l) :: acc
      | Unreachable _ -> acc
      | Undefined _ -> acc
      | Number _ -> acc
      | DeclaredFunction l ->
        Provider_api.providers_of_def providers l
        |> Base.Option.value_map ~f:(fun { Provider_api.providers; _ } -> providers) ~default:[]
        |> Base.List.fold ~init:acc ~f:(fun acc { Provider_api.reason; _ } ->
               (OrdinaryNameLoc, Reason.loc_of_reason reason) :: acc
           )
    in
    f [] write_loc

  let refinements_of_write_loc { refinement_of_id; _ } write_loc =
    let rec f acc = function
      | Refinement { refinement_id; writes; write_id = _ } ->
        let { refining_locs = _; kind } = refinement_of_id refinement_id in
        let acc = kind :: acc in
        Base.List.fold writes ~init:acc ~f
      | _ -> acc
    in
    f [] write_loc

  let rec show_refinement_kind_without_locs = function
    | AndR (l, r) ->
      Printf.sprintf
        "And (%s, %s)"
        (show_refinement_kind_without_locs l)
        (show_refinement_kind_without_locs r)
    | OrR (l, r) ->
      Printf.sprintf
        "Or (%s, %s)"
        (show_refinement_kind_without_locs l)
        (show_refinement_kind_without_locs r)
    | NotR r -> Printf.sprintf "Not (%s)" (show_refinement_kind_without_locs r)
    | TruthyR -> "Truthy"
    | NullR -> "Null"
    | UndefinedR -> "Undefined"
    | MaybeR -> "Maybe"
    | InstanceOfR _ -> "instanceof"
    | IsArrayR -> "isArray"
    | BoolR _ -> "bool"
    | FunctionR -> "function"
    | NumberR _ -> "number"
    | BigIntR _ -> "bigint"
    | ObjectR -> "object"
    | StringR _ -> "string"
    | SymbolR _ -> "symbol"
    | SingletonBoolR { lit; sense; _ } -> string_of_bool (lit = sense)
    | SingletonStrR { lit; sense; _ } ->
      if not sense then
        Printf.sprintf "Not (%s)" lit
      else
        lit
    | SingletonNumR { lit = (_, lit); sense; _ } ->
      if not sense then
        Printf.sprintf "Not (%s)" lit
      else
        lit
    | SingletonBigIntR { lit = (_, lit); sense; _ } ->
      if not sense then
        Printf.sprintf "Not (%s)" lit
      else
        lit
    | SentinelR { prop; _ } -> Printf.sprintf "SentinelR %s" prop
    | PropExistsR { propname; loc = _ } -> Printf.sprintf "PropExistsR (%s)" propname
    | PropNullishR { propname = prop; _ } -> Printf.sprintf "PropNullishR %s" prop
    | PropIsExactlyNullR { propname = prop; _ } -> Printf.sprintf "PropIsExactlyNullR %s" prop
    | PropNonVoidR { propname = prop; _ } -> Printf.sprintf "PropNonVoidR %s" prop
    | LatentR { func = _; targs = _; arguments = _; index } ->
      Printf.sprintf "LatentR (index = %i)" index
    | PropTruthyR { propname; loc = _ } -> Printf.sprintf "PropTruthyR (%s)" propname
    | EqR _ -> "EqR"
    | ImpossibleR -> "ImpossibleR"
end

module With_Loc =
  Make (Loc_sig.LocS) (Ssa_api.With_Loc) (Scope_api.With_Loc) (Provider_api.LocProviders)
module With_ALoc =
  Make (Loc_sig.ALocS) (Ssa_api.With_ALoc) (Scope_api.With_ALoc) (Provider_api.ALocProviders)
include With_ALoc
