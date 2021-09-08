(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason

module type S = sig
  module L : Loc_sig.S

  module Scope_api : Scope_api_sig.S with module L = L

  module Ssa_api : Ssa_api.S with module L = L

  module Provider_api : Provider_api.S with module L = L

  module ReasonSet : Flow_set.S with type elt = L.t virtual_reason

  type read_loc = L.t

  type write_loc =
    | Write of L.t Reason.virtual_reason
    | Uninitialized
    | Refinement of {
        refinement_id: int;
        writes: write_locs;
      }
    | Global of string

  and write_locs = write_loc list

  type values = write_locs L.LMap.t

  module Refi : sig
    type refinement_kind =
      | AndR of refinement_kind * refinement_kind
      | OrR of refinement_kind * refinement_kind
      | NotR of refinement_kind
      | TruthyR of L.t
      | NullR
      | UndefinedR
      | MaybeR
      | InstanceOfR of L.t
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
    [@@deriving show { with_path = false }]

    type refinement = L.LSet.t * refinement_kind
  end

  type refinement_kind = Refi.refinement_kind

  type refinement = Refi.refinement

  type env_info = {
    scopes: Scope_api.info;
    ssa_values: Ssa_api.values;
    env_values: values;
    env_entries: L.t virtual_reason list;
    providers: Provider_api.info;
    refinement_of_id: int -> Refi.refinement;
  }

  val empty : env_info

  val write_locs_of_read_loc : values -> read_loc -> write_locs

  val writes_of_write_loc : write_loc -> L.t list

  val print_values : values -> string

  val sources_of_use : env_info -> L.t -> L.LSet.t

  val source_bindings : env_info -> L.LSet.t L.LMap.t

  val show_refinement_kind : refinement_kind -> string

  val show_refinement_kind_without_locs : refinement_kind -> string
end

module Make
    (L : Loc_sig.S)
    (Ssa_api : Ssa_api.S with module L = L)
    (Scope_api : Scope_api_sig.S with module L = Ssa_api.L) :
  S with module L = L and module Ssa_api = Ssa_api and module Scope_api = Scope_api = struct
  module L = L
  module Ssa_api = Ssa_api
  module Scope_api = Scope_api

  module Scope_builder : Scope_builder_sig.S with module L = L and module Api = Scope_api =
    Scope_builder.Make (L) (Scope_api)

  module Provider_api : Provider_api.S with module L = L = Provider_api.Make (L)

  module ReasonSet = Flow_set.Make (struct
    type t = L.t virtual_reason

    let compare = Stdlib.compare
  end)

  type read_loc = L.t

  type write_loc =
    | Write of L.t Reason.virtual_reason
    | Uninitialized
    | Refinement of {
        refinement_id: int;
        writes: write_locs;
      }
    | Global of string

  and write_locs = write_loc list

  module Refi = struct
    type refinement_kind =
      | AndR of refinement_kind * refinement_kind
      | OrR of refinement_kind * refinement_kind
      | NotR of refinement_kind
      | TruthyR of L.t
      | NullR
      | UndefinedR
      | MaybeR
      | InstanceOfR of L.t
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
    [@@deriving show { with_path = false }]

    type refinement = L.LSet.t * refinement_kind
  end

  include Refi

  type values = write_locs L.LMap.t

  type env_info = {
    scopes: Scope_api.info;
    ssa_values: Ssa_api.values;
    env_values: values;
    env_entries: L.t virtual_reason list;
    providers: Provider_api.info;
    refinement_of_id: int -> refinement;
  }

  let empty =
    {
      scopes = Scope_builder.Acc.init;
      ssa_values = L.LMap.empty;
      env_values = L.LMap.empty;
      env_entries = [];
      providers = Provider_api.empty;
      refinement_of_id = (fun _ -> failwith "Empty env info");
    }

  let rec refinement_ids_of_ssa_write acc = function
    | Refinement { refinement_id; writes } ->
      List.fold_left refinement_ids_of_ssa_write (refinement_id :: acc) writes
    | _ -> acc

  let write_locs_of_read_loc values read_loc = L.LMap.find read_loc values

  let rec writes_of_write_loc write_loc =
    match write_loc with
    | Refinement { refinement_id = _; writes } ->
      writes |> List.map writes_of_write_loc |> List.flatten
    | Write r -> [Reason.poly_loc_of_reason r]
    | Uninitialized -> []
    | Global _ -> []

  let sources_of_use { env_values = vals; refinement_of_id; _ } loc =
    let write_locs =
      L.LMap.find_opt loc vals
      |> Base.Option.value_map ~default:[] ~f:(List.map writes_of_write_loc)
      |> List.flatten
      |> L.LSet.of_list
    in
    let refi_locs =
      L.LMap.find_opt loc vals
      |> Base.Option.value_map ~default:[] ~f:(List.fold_left refinement_ids_of_ssa_write [])
      |> List.map (fun id -> fst (refinement_of_id id))
      |> List.fold_left L.LSet.union L.LSet.empty
    in
    L.LSet.union refi_locs write_locs

  let source_bindings ({ env_values = vals; _ } as info) =
    let keys = L.LSet.of_list (L.LMap.keys vals) in
    L.LSet.fold (fun k acc -> L.LMap.add k (sources_of_use info k) acc) keys L.LMap.empty

  let print_values =
    let rec print_write_loc write_loc =
      match write_loc with
      | Uninitialized -> "(uninitialized)"
      | Write reason ->
        let loc = Reason.poly_loc_of_reason reason in
        Utils_js.spf
          "%s: (%s)"
          (L.debug_to_string loc)
          Reason.(desc_of_reason reason |> string_of_desc)
      | Refinement { refinement_id; writes } ->
        let refinement_id_str = string_of_int refinement_id in
        let writes_str = String.concat "," (List.map print_write_loc writes) in
        Printf.sprintf "{refinement_id = %s; writes = %s}" refinement_id_str writes_str
      | Global name -> "Global " ^ name
    in
    fun values ->
      let kvlist = L.LMap.bindings values in
      let strlist =
        Base.List.map
          ~f:(fun (read_loc, write_locs) ->
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
    | TruthyR _ -> "Truthy"
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
end

module With_Loc = Make (Loc_sig.LocS) (Ssa_api.With_Loc) (Scope_api.With_Loc)
module With_ALoc = Make (Loc_sig.ALocS) (Ssa_api.With_ALoc) (Scope_api.With_ALoc)
include With_ALoc
