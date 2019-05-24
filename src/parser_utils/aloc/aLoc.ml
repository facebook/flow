(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type table = {
  (* This is not strictly necessary, but it allows us to check that the location source matches the
   * table source, to avoid confusing issues if we try a lookup with the wrong table. *)
  file: File_key.t;
  mutable map: Loc.t ResizableArray.t
}

let make_table file = {
  file;
  (* TODO maybe start with a rough estimate of the number of locations? *)
  map = ResizableArray.make 32;
}

type key = int
let compare_key: key -> key -> int = Pervasives.compare
let string_of_key = string_of_int

module Repr: sig
  type t
  type kind =
    | ALocNone
    | Abstract
    | Concrete

  val of_loc: Loc.t -> t
  val of_key: File_key.t option -> key -> t

  val source: t -> File_key.t option
  val update_source: (File_key.t option -> File_key.t option) -> t -> t

  val kind: t -> kind
  (* Raises unless `kind` returns `Abstract` *)
  val get_key_exn: t -> key
  (* Raises if `kind` returns `Abstract` *)
  val to_loc_exn: t -> Loc.t
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
   * This also means that we must have a representation for actually-abstract `ALoc.t`s which is
   * distinct from every possible value of `Loc.t`. Fortunately, the start and end fields are both
   * pointers, and therefore cannot share a representation with any integer. We can take advantage
   * of this in order to differentiate between the kinds of `ALoc.t`s.
   *)
  type kind =
    | ALocNone
    | Abstract
    | Concrete

  type t = Loc.t

  type abstract_t = {
    (* This field has the same type in Loc.t *)
    abstract_source: File_key.t option;
    (* In Loc.t, this is the `start` field. We will use it as an integer key here. *)
    key: key;
    (* In Loc.t, this is the `_end` field. We will use `0` as a sentinel value here to indicate that
     * this is an ALoc with an abstract underlying representation. Because the `_end` field is a
     * pointer, this will never be 0 (or any other integer, for that matter) if we are actually
     * dealing with a concrete representation.
     *
     * In the future we could optimize memory further by omitting this field, and checking whether
     * start/key is a pointer or an immediate value. *)
    kind: int;
  }

  let abstract_sentinel_value = 0

  let of_loc: Loc.t -> t = Obj.magic

  let of_key (source: File_key.t option) (key: key) : t =
    let loc: abstract_t = {
      abstract_source = source;
      key;
      kind = abstract_sentinel_value;
    } in
    Obj.magic loc

  let source {Loc.source; _} = source

  let update_source f loc = Loc.({
    loc with
    source = f loc.source;
  })

  (* See the definition site of the `kind` property for an explanation *)
  let is_abstract (loc: t): bool = (Obj.magic loc).kind = abstract_sentinel_value

  let kind (loc: t): kind =
    if is_abstract loc then
      Abstract
    else if (Obj.magic loc) = Loc.none then
      ALocNone
    else
      Concrete

  let get_key_exn (loc: t): key =
    if is_abstract loc then
      let {key; _} : abstract_t = Obj.magic loc in
      key
    else
      invalid_arg "Can only get the key from an abstract location"

  let to_loc_exn (loc: t): Loc.t =
    if is_abstract loc then
      invalid_arg "loc must be concrete"
    else
      ((Obj.magic loc): Loc.t)
end

type t = Repr.t

let of_loc = Repr.of_loc

let abstractify table loc =
  match Repr.kind loc with
  | Repr.Abstract -> failwith "Cannot abstractify a location which is already abstract"
  | Repr.ALocNone -> loc
  | Repr.Concrete ->
    let underlying_loc = Repr.to_loc_exn loc in
    let source = Repr.source loc in
    if source <> Some table.file then
      failwith "abstractify: File mismatch between location and table"
    else
      ResizableArray.push table.map underlying_loc;
      let key = ResizableArray.size table.map - 1 in
      Repr.of_key source key

let to_loc_exn = Repr.to_loc_exn

let to_loc table loc =
  match Repr.kind loc with
  | Repr.Concrete | Repr.ALocNone ->
    Repr.to_loc_exn loc
  | Repr.Abstract ->
    let source = Repr.source loc in
    let key = Repr.get_key_exn loc in
    let table = Lazy.force table in
    if Some table.file <> source then
      failwith "to_loc_safe: File mismatch between location and table"
    else
      match ResizableArray.get table.map key with
      | Some loc -> loc
      | None ->
        failwith (Printf.sprintf
          "Unable to look up location with key %S for file %S"
          (string_of_key key)
          (File_key.to_string table.file)
        )

let none = Repr.of_loc Loc.none

let source = Repr.source

let update_source = Repr.update_source

let debug_to_string ?(include_source=false) loc =
  match Repr.kind loc with
  | Repr.Concrete | Repr.ALocNone ->
    let loc = Repr.to_loc_exn loc in
    Loc.debug_to_string ~include_source loc
  | Repr.Abstract ->
    let source = Repr.source loc in
    let key = Repr.get_key_exn loc in
    let source = if include_source then
      Printf.sprintf "%S: " (
        match source with
        | Some src -> File_key.to_string src
        | None -> "<NONE>"
      )
    else
      ""
    in
    let key = string_of_key key in
    source ^ key

let compare loc1 loc2 =
  let source_compare = File_key.compare_opt (Repr.source loc1) (Repr.source loc2) in
  if source_compare = 0 then
    match Repr.kind loc1, Repr.kind loc2 with
    | Repr.Abstract, Repr.Abstract ->
      let k1 = Repr.get_key_exn loc1 in
      let k2 = Repr.get_key_exn loc2 in
      compare_key k1 k2
    | Repr.Concrete, Repr.Concrete ->
      let l1 = Repr.to_loc_exn loc1 in
      let l2 = Repr.to_loc_exn loc2 in
      let k = Loc.pos_cmp l1.Loc.start l2.Loc.start in
      if k = 0 then Loc.pos_cmp l1.Loc._end l2.Loc._end
      else k
    | Repr.ALocNone, Repr.ALocNone -> 0
    | Repr.ALocNone, (Repr.Abstract | Repr.Concrete) -> -1
    | (Repr.Abstract | Repr.Concrete), Repr.ALocNone -> 1
    (* This might be too aggressive. For example, we might sort errors by location, and some errors
     * generated about a file might use concrete locations, and others might use abstract ones. For
     * now let's wait and see, and if this is too aggressive we can relax it. *)
    | Repr.Abstract, Repr.Concrete
    | Repr.Concrete, Repr.Abstract ->
      invalid_arg (
        Printf.sprintf
          "Unable to compare an abstract location with a concrete one. loc1: %s, loc2: %s"
          (debug_to_string ~include_source:true loc1)
          (debug_to_string ~include_source:true loc2)
      )
  else source_compare

let equal loc1 loc2 = compare loc1 loc2 = 0

let to_string_no_source loc = match Repr.kind loc with
| Repr.Concrete | Repr.ALocNone ->
  Loc.to_string_no_source (Repr.to_loc_exn loc)
| Repr.Abstract ->
  let key = Repr.get_key_exn loc in
  string_of_key key

module ALocRepresentationDoNotUse = struct
  let is_abstract loc = match Repr.kind loc with
    | Repr.Abstract -> true
    | Repr.Concrete -> false
    | Repr.ALocNone -> false

  let get_key_exn = Repr.get_key_exn
end
