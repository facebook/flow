(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type table = {
  (* This is not strictly necessary, but it allows us to check that the location source matches the
   * table source, to avoid confusing issues if we try a lookup with the wrong table. *)
  file: File_key.t;
  map: RelativeLoc.t ResizableArray.t;
}

let make_table file =
  {
    file;
    (* TODO maybe start with a rough estimate of the number of locations? *)
    map = ResizableArray.make 32;
  }

let shrink_table table = ResizableArray.shrink table.map

type key = int

let compare_key : key -> key -> int = Stdlib.compare

let string_of_key = string_of_int

type reverse_table = (RelativeLoc.t, key) Hashtbl.t

let make_empty_reverse_table () = Hashtbl.create 0

module Repr : sig
  type t

  type kind =
    | ALocNone
    | Keyed
    | Concrete

  val of_loc : Loc.t -> t

  val of_key : File_key.t option -> key -> t

  val source : t -> File_key.t option

  val update_source : (File_key.t option -> File_key.t option) -> t -> t

  (* `is_keyed x` is equivalent to `kind x = Keyed` *)
  val is_keyed : t -> bool

  val kind : t -> kind

  (* Raises unless `kind` returns `Keyed` *)
  val get_key_exn : t -> key

  (* Raises if `kind` returns `Keyed` *)
  val to_loc_exn : t -> Loc.t
end = struct
  (* This module uses `Obj.magic` to achieve zero-cost conversions between `Loc.t` and an `ALoc.t`
   * which has a concrete underlying representation. Don't modify this unless you understand how
   * OCaml lays out values in memory.
   *
   * In order to achieve the zero-cost conversion, we have to use the same representation as `Loc.t`
   * for each `ALoc.t` that has a concrete underlying representation. By doing so, we can avoid any
   * new allocations, and simply use `Obj.magic` to return a pointer to the same heap-allocated
   * object with a different type.
   *
   * This also means that we must have a representation for keyed `ALoc.t`s which is distinct from
   * every possible value of `Loc.t`. Fortunately, the start and end fields are both pointers, and
   * therefore cannot share a representation with any integer. We can take advantage of this in
   * order to differentiate between the kinds of `ALoc.t`s.
   *)
  type kind =
    | ALocNone
    | Keyed
    | Concrete

  type t = Loc.t

  type keyed_t = {
    (* This field has the same type in Loc.t *)
    keyed_source: File_key.t option;
    (* In Loc.t, this is the `start` field. We will use it as an integer key here. *)
    key: key;
  }

  let of_loc : Loc.t -> t = Obj.magic

  let of_key (source : File_key.t option) (key : key) : t =
    let loc : keyed_t = { keyed_source = source; key } in
    Obj.magic loc

  let source { Loc.source; _ } = source

  (* The `key` field is an integer in `keyed_t`, but the field in the corresponding location in
   * `Loc.t` is `start`, which is a pointer. We can use this fact to determine whether we have an
   * `keyed_t` or a `t` here, since OCaml keeps track of whether a value is an integer or a
   * pointer. If it's an integer, the value is keyed. *)
  let is_keyed (loc : t) : bool = (Obj.magic loc).key |> Obj.repr |> Obj.is_int

  let kind (loc : t) : kind =
    if is_keyed loc then
      Keyed
    else if Obj.magic loc = Loc.none then
      ALocNone
    else
      Concrete

  let get_key_exn (loc : t) : key =
    if is_keyed loc then
      let ({ key; _ } : keyed_t) = Obj.magic loc in
      key
    else
      invalid_arg "Can only get the key from a keyed location"

  let to_loc_exn (loc : t) : Loc.t =
    if is_keyed loc then
      invalid_arg "loc must be concrete"
    else
      (Obj.magic loc : Loc.t)

  let update_source f loc =
    if is_keyed loc then
      let loc : keyed_t = Obj.magic loc in
      let updated_loc : keyed_t = { loc with keyed_source = f loc.keyed_source } in
      (Obj.magic updated_loc : t)
    else
      Loc.{ loc with source = f loc.source }
end

type t = Repr.t

let pp fmt _ = Format.pp_print_string fmt "<opaque>"

let show _ = "<opaque>"

let of_loc = Repr.of_loc

let keyify table rev_table loc =
  match Repr.kind loc with
  | Repr.Keyed -> failwith "Cannot keyify a location which is already keyed"
  | Repr.ALocNone -> loc
  | Repr.Concrete ->
    let underlying_loc = Repr.to_loc_exn loc |> RelativeLoc.of_loc in
    let source = Repr.source loc in
    if source <> Some table.file then failwith "keyify: File mismatch between location and table";
    let key =
      try Hashtbl.find rev_table underlying_loc
      with Not_found ->
        let key = ResizableArray.size table.map in
        ResizableArray.push table.map underlying_loc;
        Hashtbl.add rev_table underlying_loc key;
        key
    in
    Repr.of_key source key

let to_loc_exn = Repr.to_loc_exn

let to_loc table loc =
  if Repr.is_keyed loc then
    let source = Repr.source loc in
    let key = Repr.get_key_exn loc in
    let table = Lazy.force table in
    if Some table.file <> source then
      failwith "to_loc_safe: File mismatch between location and table"
    else
      match ResizableArray.get table.map key with
      | Some loc -> RelativeLoc.to_loc loc source
      | None ->
        failwith
          (Printf.sprintf
             "Unable to look up location with key %S for file %S"
             (string_of_key key)
             (File_key.to_string table.file))
  else
    Repr.to_loc_exn loc

let to_loc_with_tables tables loc =
  let aloc_table =
    lazy
      (let source =
         match Repr.source loc with
         | Some x -> x
         | None -> failwith "Unexpectedly encountered a location without a source"
       in
       Lazy.force (Utils_js.FilenameMap.find source tables))
  in
  to_loc aloc_table loc

let none = Repr.of_loc Loc.none

let source = Repr.source

let update_source = Repr.update_source

let debug_to_string ?(include_source = false) loc =
  if Repr.is_keyed loc then
    let source = Repr.source loc in
    let key = Repr.get_key_exn loc in
    let source =
      if include_source then
        Printf.sprintf
          "%S: "
          (match source with
          | Some src -> File_key.to_string src
          | None -> "<NONE>")
      else
        ""
    in
    let key = string_of_key key in
    source ^ key
  else
    let loc = Repr.to_loc_exn loc in
    Loc.debug_to_string ~include_source loc

let compare loc1 loc2 =
  let source_compare = File_key.compare_opt (Repr.source loc1) (Repr.source loc2) in
  if source_compare = 0 then
    match (Repr.kind loc1, Repr.kind loc2) with
    | (Repr.Keyed, Repr.Keyed) ->
      let k1 = Repr.get_key_exn loc1 in
      let k2 = Repr.get_key_exn loc2 in
      compare_key k1 k2
    | (Repr.Concrete, Repr.Concrete) ->
      let l1 = Repr.to_loc_exn loc1 in
      let l2 = Repr.to_loc_exn loc2 in
      let k = Loc.pos_cmp l1.Loc.start l2.Loc.start in
      if k = 0 then
        Loc.pos_cmp l1.Loc._end l2.Loc._end
      else
        k
    | (Repr.ALocNone, Repr.ALocNone) -> 0
    | (Repr.ALocNone, (Repr.Keyed | Repr.Concrete)) -> -1
    | ((Repr.Keyed | Repr.Concrete), Repr.ALocNone) -> 1
    (* This might be too aggressive. For example, we might sort errors by location, and some errors
     * generated about a file might use concrete locations, and others might use keyed ones. For
     * now let's wait and see, and if this is too aggressive we can relax it. *)
    | (Repr.Keyed, Repr.Concrete)
    | (Repr.Concrete, Repr.Keyed) ->
      invalid_arg
        (Printf.sprintf
           "Unable to compare a keyed location with a concrete one. loc1: %s, loc2: %s"
           (debug_to_string ~include_source:true loc1)
           (debug_to_string ~include_source:true loc2))
  else
    source_compare

let quick_compare loc1 loc2 =
  (* String comparisons are expensive, so we should only evaluate this lambda if
   * the other information we have ties *)
  let source_compare () = File_key.compare_opt (Repr.source loc1) (Repr.source loc2) in
  match (Repr.kind loc1, Repr.kind loc2) with
  | (Repr.Keyed, Repr.Keyed) ->
    let k1 = Repr.get_key_exn loc1 in
    let k2 = Repr.get_key_exn loc2 in
    let key_compare = compare_key k1 k2 in
    if key_compare = 0 then
      source_compare ()
    else
      key_compare
  | (Repr.Concrete, Repr.Concrete) ->
    let l1 = Repr.to_loc_exn loc1 in
    let l2 = Repr.to_loc_exn loc2 in
    let start_compare = Loc.pos_cmp l1.Loc.start l2.Loc.start in
    if start_compare = 0 then
      let end_compare = Loc.pos_cmp l1.Loc._end l2.Loc._end in
      if end_compare = 0 then
        source_compare ()
      else
        end_compare
    else
      start_compare
  | (Repr.ALocNone, Repr.ALocNone) -> 0
  | (Repr.ALocNone, (Repr.Keyed | Repr.Concrete)) -> -1
  | ((Repr.Keyed | Repr.Concrete), Repr.ALocNone) -> 1
  | (Repr.Keyed, Repr.Concrete) -> 1
  | (Repr.Concrete, Repr.Keyed) -> -1

let equal loc1 loc2 = compare loc1 loc2 = 0

let concretize_if_possible available_tables loc =
  if Repr.is_keyed loc then
    match Repr.source loc with
    (* We shouldn't end up with a location with no source and a keyed representation. It may be
     * worth asserting here at some point. *)
    | None -> loc
    | Some source ->
      begin
        match Utils_js.FilenameMap.find_opt source available_tables with
        (* We don't have the right table, so just return the loc *)
        | None -> loc
        | Some table ->
          (* Concretize by converting to a Loc.t, then back to an ALoc.t *)
          of_loc (to_loc table loc)
      end
  else
    loc

let concretize_compare available_tables loc1 loc2 =
  if Repr.source loc1 = Repr.source loc2 && Repr.is_keyed loc1 <> Repr.is_keyed loc2 then
    let loc1 = concretize_if_possible available_tables loc1 in
    let loc2 = concretize_if_possible available_tables loc2 in
    compare loc1 loc2
  else
    compare loc1 loc2

let concretize_equal table loc1 loc2 = concretize_compare table loc1 loc2 = 0

let to_string_no_source loc =
  if Repr.is_keyed loc then
    let key = Repr.get_key_exn loc in
    string_of_key key
  else
    Loc.to_string_no_source (Repr.to_loc_exn loc)

type id = t

let id_none = none

let id_of_aloc rev_table loc =
  match Repr.kind loc with
  | Repr.Keyed
  | Repr.ALocNone ->
    loc
  | Repr.Concrete ->
    let underlying_loc = Repr.to_loc_exn loc |> RelativeLoc.of_loc in
    (match Hashtbl.find_opt (Lazy.force rev_table) underlying_loc with
    | Some key ->
      begin
        match Repr.source loc with
        | Some source -> Repr.of_key (Some source) key
        | None -> failwith "Unexpectedly encountered a location without a source"
      end
    | None -> loc)

let equal_id a b = quick_compare a b = 0

let reverse_table table = ResizableArray.to_hashtbl table.map

module ALocRepresentationDoNotUse = struct
  let is_keyed = Repr.is_keyed

  let get_key_exn = Repr.get_key_exn

  let string_of_key = string_of_key
end
