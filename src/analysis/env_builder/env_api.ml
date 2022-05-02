(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
module Ast = Flow_ast

type new_env_literal_check =
  | SingletonNum of ALoc.t * bool * float * string
  | SingletonBool of ALoc.t * bool
  | SingletonStr of ALoc.t * bool * string

module type S = sig
  module L : Loc_sig.S

  module Scope_api : Scope_api_sig.S with module L = L

  module Ssa_api : Ssa_api.S with module L = L

  module Provider_api : Provider_api.S with module L = L

  module ReasonSet : Flow_set.S with type elt = L.t virtual_reason

  type read_loc = L.t

  type write_loc =
    | Write of L.t Reason.virtual_reason
    | Uninitialized of L.t Reason.virtual_reason
    | Undeclared of string * L.t
    | UndeclaredClass of {
        def: L.t Reason.virtual_reason;
        name: string;
      }
    | Refinement of {
        refinement_id: int;
        writes: write_locs;
        write_id: int option;
      }
    | This
    | Super
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

  type env_info = {
    scopes: Scope_api.info;
    ssa_values: Ssa_api.values;
    env_values: values;
    env_entries: env_entry L.LMap.t;
    providers: Provider_api.info;
    refinement_of_id: int -> Refi.refinement;
  }

  val empty : env_info

  val write_locs_of_read_loc : values -> read_loc -> write_locs

  val writes_of_write_loc : for_type:bool -> write_loc -> L.t list

  val refinements_of_write_loc : env_info -> write_loc -> refinement_kind list

  val print_values : values -> string

  val sources_of_use : for_type:bool -> env_info -> L.t -> L.LSet.t

  val source_bindings : for_type:bool -> env_info -> L.LSet.t L.LMap.t

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

  module ReasonSet = Flow_set.Make (struct
    type t = L.t virtual_reason

    let compare = Stdlib.compare
  end)

  type read_loc = L.t

  type write_loc =
    | Write of L.t Reason.virtual_reason
    | Uninitialized of L.t Reason.virtual_reason
    | Undeclared of string * L.t
    | UndeclaredClass of {
        def: L.t Reason.virtual_reason;
        name: string;
      }
    | Refinement of {
        refinement_id: int;
        writes: write_locs;
        write_id: int option;
      }
    | This
    | Super
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

  type env_info = {
    scopes: Scope_api.info;
    ssa_values: Ssa_api.values;
    env_values: values;
    env_entries: env_entry L.LMap.t;
    providers: Provider_api.info;
    refinement_of_id: int -> refinement;
  }

  let empty =
    {
      scopes = Scope_builder.Acc.init;
      ssa_values = L.LMap.empty;
      env_values = L.LMap.empty;
      env_entries = L.LMap.empty;
      providers = Provider_api.empty;
      refinement_of_id = (fun _ -> failwith "Empty env info");
    }

  let rec refinement_ids_of_ssa_write acc = function
    | Refinement { refinement_id; writes; write_id = _ } ->
      List.fold_left refinement_ids_of_ssa_write (refinement_id :: acc) writes
    | _ -> acc

  let write_locs_of_read_loc values read_loc =
    let { write_locs; def_loc = _; val_kind = _; name = _; id = _ } = L.LMap.find read_loc values in
    write_locs

  let rec writes_of_write_loc ~for_type write_loc =
    match write_loc with
    | Refinement { refinement_id = _; write_id = _; writes } ->
      writes |> List.map (writes_of_write_loc ~for_type) |> List.flatten
    | UndeclaredClass { def; _ } when for_type -> [Reason.poly_loc_of_reason def]
    | UndeclaredClass _ -> []
    | Write r -> [Reason.poly_loc_of_reason r]
    | Uninitialized _ -> []
    | Undeclared _ -> []
    | This -> []
    | Super -> []
    | ModuleScoped _ -> []
    | Global _ -> []
    | Projection _ -> []
    | Unreachable _ -> []
    | Undefined r -> [Reason.poly_loc_of_reason r]
    | Number r -> [Reason.poly_loc_of_reason r]
    | DeclaredFunction l -> [l]

  let rec refinements_of_write_loc ({ refinement_of_id; _ } as env) write_loc =
    match write_loc with
    | Refinement { refinement_id; writes; write_id = _ } ->
      let writes = writes |> List.map (refinements_of_write_loc env) |> List.flatten in
      (refinement_of_id refinement_id |> snd) :: writes
    | _ -> []

  let sources_of_use ~for_type { env_values = vals; refinement_of_id; _ } loc =
    let write_locs =
      L.LMap.find_opt loc vals
      |> Base.Option.value_map
           ~default:[]
           ~f:(fun { write_locs; def_loc = _; val_kind = _; name = _; id = _ } ->
             List.map (writes_of_write_loc ~for_type) write_locs
         )
      |> List.flatten
      |> L.LSet.of_list
    in
    let refi_locs =
      L.LMap.find_opt loc vals
      |> Base.Option.value_map
           ~default:[]
           ~f:(fun { write_locs; def_loc = _; val_kind = _; name = _; id = _ } ->
             List.fold_left refinement_ids_of_ssa_write [] write_locs
         )
      |> List.map (fun id -> fst (refinement_of_id id))
      |> List.fold_left L.LSet.union L.LSet.empty
    in
    L.LSet.union refi_locs write_locs

  let source_bindings ~for_type ({ env_values = vals; _ } as info) =
    let keys = L.LSet.of_list (L.LMap.keys vals) in
    L.LSet.fold (fun k acc -> L.LMap.add k (sources_of_use ~for_type info k) acc) keys L.LMap.empty

  let print_values =
    let rec print_write_loc write_loc =
      match write_loc with
      | Uninitialized _ -> "(uninitialized)"
      | Undeclared _ -> "(undeclared)"
      | UndeclaredClass { def; _ } ->
        let loc = Reason.poly_loc_of_reason def in
        Utils_js.spf
          "(uninitialized class) %s: (%s)"
          (L.debug_to_string loc)
          Reason.(desc_of_reason def |> string_of_desc)
      | Projection l -> Printf.sprintf "projection at %s" (L.debug_to_string l)
      | Unreachable _ -> "unreachable"
      | Undefined _ -> "undefined"
      | Number _ -> "number"
      | DeclaredFunction l -> Printf.sprintf "declared function %s" (L.debug_to_string l)
      | Write reason ->
        let loc = Reason.poly_loc_of_reason reason in
        Utils_js.spf
          "%s: (%s)"
          (L.debug_to_string loc)
          Reason.(desc_of_reason reason |> string_of_desc)
      | Refinement { refinement_id; writes; write_id = _ } ->
        let refinement_id_str = string_of_int refinement_id in
        let writes_str = String.concat "," (List.map print_write_loc writes) in
        Printf.sprintf "{refinement_id = %s; writes = %s}" refinement_id_str writes_str
      | This -> "this"
      | Super -> "super"
      | ModuleScoped name -> "ModuleScoped " ^ name
      | Global name -> "Global " ^ name
    in
    fun values ->
      let kvlist = L.LMap.bindings values in
      let strlist =
        Base.List.map
          ~f:(fun (read_loc, { def_loc = _; val_kind = _; write_locs; name = _; id = _ }) ->
            Printf.sprintf
              "%s => { %s }"
              (L.debug_to_string read_loc)
              (String.concat ", " @@ Base.List.map ~f:print_write_loc write_locs))
          kvlist
      in
      Printf.sprintf "[ %s ]" (String.concat "; " strlist)

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
    | SentinelR (prop, _) -> Printf.sprintf "SentinelR %s" prop
    | LatentR { func = _; index } -> Printf.sprintf "LatentR (index = %i)" index
    | PropExistsR { propname; loc = _ } -> Printf.sprintf "PropExistsR (%s)" propname
end

module With_Loc =
  Make (Loc_sig.LocS) (Ssa_api.With_Loc) (Scope_api.With_Loc) (Provider_api.LocProviders)
module With_ALoc =
  Make (Loc_sig.ALocS) (Ssa_api.With_ALoc) (Scope_api.With_ALoc) (Provider_api.ALocProviders)
include With_ALoc
