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
    | GlobalExportsLoc
    | DeclareModuleExportsLoc
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
    | Exports
    | ModuleScoped of string
    | Global of string
    | Projection of L.t
    | Unreachable of L.t
    | Undefined of L.t Reason.virtual_reason
    | Number of L.t Reason.virtual_reason
    | DeclaredFunction of L.t

  and write_locs = write_loc list

  type val_kind =
    | Type of { imported: bool }
    | Value

  type read = {
    def_loc: L.t option;
    write_locs: write_locs;
    val_kind: val_kind option;
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
      (* The location here is the location of expr in x.foo === expr *)
      | SentinelR of string * L.t
      | LatentR of {
          func: (L.t, L.t) Ast.Expression.t;
          index: int;
        }
      | PropExistsR of {
          propname: string;
          loc: L.t;
        }
    [@@deriving show { with_path = false }]

    type refinement = L.LSet.t * refinement_kind
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

  type toplevel_member = Reason.name * read

  type predicate_refinement_maps = (read SMap.t * read SMap.t) L.LMap.t

  type env_info = {
    scopes: Scope_api.info;
    ssa_values: Ssa_api.values;
    env_values: values;
    env_entries: env_entry EnvMap.t;
    toplevel_members: toplevel_member list;
    module_toplevel_members: toplevel_member list L.LMap.t;
    predicate_refinement_maps: predicate_refinement_maps;
    providers: Provider_api.info;
    refinement_of_id: int -> Refi.refinement;
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
    | BuiltinLookupFailed of {
        reason_desc: Reason.reason_desc;
        potential_generator: string option;
        name: Reason.name;
      }

  val map_result :
    f:('a -> 'b) ->
    ('a, 'a * cacheable_env_error Nel.t) result ->
    ('b, 'b * cacheable_env_error Nel.t) result

  val empty : env_info

  val has_assigning_write : def_loc_type * L.t -> env_entry EnvMap.t -> bool

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
    | BuiltinLookupFailed of {
        reason_desc: Reason.reason_desc;
        potential_generator: string option;
        name: Reason.name;
      }

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
    | GlobalExportsLoc
    | DeclareModuleExportsLoc
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

  module EnvMap = struct
    include WrappedMap.Make (EnvKey)

    let add_ordinary l = add (OrdinaryNameLoc, l)

    let find_ordinary l = find (OrdinaryNameLoc, l)

    let find_opt_ordinary l = find_opt (OrdinaryNameLoc, l)

    let update_ordinary l = update (OrdinaryNameLoc, l)
  end

  module EnvSet = Flow_set.Make (EnvKey)

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
    | Exports
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
      | SentinelR of string * L.t
      | LatentR of {
          func: (L.t, L.t) Ast.Expression.t;
          index: int;
        }
      | PropExistsR of {
          propname: string;
          loc: L.t;
        }
    [@@deriving show { with_path = false }]

    type refinement = L.LSet.t * refinement_kind
  end

  include Refi

  type val_kind =
    | Type of { imported: bool }
    | Value

  type read = {
    def_loc: L.t option;
    write_locs: write_locs;
    val_kind: val_kind option;
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

  type toplevel_member = Reason.name * read

  type predicate_refinement_maps = (read SMap.t * read SMap.t) L.LMap.t

  type env_info = {
    scopes: Scope_api.info;
    ssa_values: Ssa_api.values;
    env_values: values;
    env_entries: env_entry EnvMap.t;
    toplevel_members: toplevel_member list;
    module_toplevel_members: toplevel_member list L.LMap.t;
    predicate_refinement_maps: predicate_refinement_maps;
    providers: Provider_api.info;
    refinement_of_id: int -> Refi.refinement;
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
      ssa_values = L.LMap.empty;
      env_values = L.LMap.empty;
      env_entries = EnvMap.empty;
      toplevel_members = [];
      module_toplevel_members = L.LMap.empty;
      predicate_refinement_maps = L.LMap.empty;
      providers = Provider_api.empty;
      refinement_of_id = (fun _ -> raise (Env_invariant (None, Impossible "Empty env info")));
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

  let write_locs_of_read_loc values read_loc =
    let { write_locs; def_loc = _; val_kind = _; name = _; id = _ } = L.LMap.find read_loc values in
    write_locs

  let rec writes_of_write_loc ~for_type providers write_loc =
    match write_loc with
    | Refinement { refinement_id = _; write_id = _; writes } ->
      writes |> List.map (writes_of_write_loc ~for_type providers) |> List.flatten
    | Write r -> [(OrdinaryNameLoc, Reason.poly_loc_of_reason r)]
    | EmptyArray { reason; arr_providers } ->
      (OrdinaryNameLoc, Reason.poly_loc_of_reason reason)
      :: Base.List.map ~f:(fun l -> (ArrayProviderLoc, l)) (L.LSet.elements arr_providers)
    | IllegalWrite _ -> []
    | Uninitialized _ -> []
    | Undeclared (_, loc) when for_type -> [(OrdinaryNameLoc, loc)]
    | Undeclared _ -> []
    | FunctionThis r -> [(FunctionThisLoc, Reason.poly_loc_of_reason r)]
    | GlobalThis _ -> []
    | IllegalThis _ -> []
    | ClassInstanceThis r -> [(ClassInstanceThisLoc, Reason.poly_loc_of_reason r)]
    | ClassStaticThis r -> [(ClassStaticThisLoc, Reason.poly_loc_of_reason r)]
    | ClassInstanceSuper r -> [(ClassInstanceSuperLoc, Reason.poly_loc_of_reason r)]
    | ClassStaticSuper r -> [(ClassStaticSuperLoc, Reason.poly_loc_of_reason r)]
    | Exports -> []
    | ModuleScoped _ -> []
    | Global _ -> []
    | Projection l -> [(OrdinaryNameLoc, l)]
    | Unreachable _ -> []
    | Undefined _ -> []
    | Number _ -> []
    | DeclaredFunction l ->
      Provider_api.providers_of_def providers l
      |> Base.Option.value_map ~f:(fun { Provider_api.providers; _ } -> providers) ~default:[]
      |> Base.List.map ~f:(fun { Provider_api.reason; _ } ->
             (OrdinaryNameLoc, Reason.poly_loc_of_reason reason)
         )

  let rec refinements_of_write_loc ({ refinement_of_id; _ } as env) write_loc =
    match write_loc with
    | Refinement { refinement_id; writes; write_id = _ } ->
      let writes = writes |> List.map (refinements_of_write_loc env) |> List.flatten in
      (refinement_of_id refinement_id |> snd) :: writes
    | _ -> []

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
    | SentinelR (prop, _) -> Printf.sprintf "SentinelR %s" prop
    | LatentR { func = _; index } -> Printf.sprintf "LatentR (index = %i)" index
    | PropExistsR { propname; loc = _ } -> Printf.sprintf "PropExistsR (%s)" propname
end

module With_Loc =
  Make (Loc_sig.LocS) (Ssa_api.With_Loc) (Scope_api.With_Loc) (Provider_api.LocProviders)
module With_ALoc =
  Make (Loc_sig.ALocS) (Ssa_api.With_ALoc) (Scope_api.With_ALoc) (Provider_api.ALocProviders)
include With_ALoc
