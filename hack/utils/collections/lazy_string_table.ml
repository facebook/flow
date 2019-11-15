(*
 * Copyright (c) 2018, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

open Core_kernel

type 'a t = {
  tbl: ('a * bool) String.Table.t;
  mutable seq: (string * 'a) Sequence.t;
  is_canonical: 'a -> bool;
  merge: earlier:'a -> later:'a -> 'a;
}

let make ~is_canonical ~merge seq =
  let tbl = String.Table.create () in
  { tbl; seq; is_canonical; merge }

type 'a advance_result =
  | Complete
      (** The cache's [seq] has been exhausted, and its [tbl] now contains the
      complete mapping of all elements. *)
  | Skipped
      (** The cache's [seq] emitted some non-canonical element, which may or may not
      have replaced a previously emitted element stored in the [tbl]. *)
  | Yield of string * 'a
      (** The cache's [seq] emitted this canonical element (along with its ID). This
      element may be immediately used without traversing the rest of the
      sequence (since canonical elements cannot be replaced or updated as we
      traverse the rest of the sequence). *)

(** Fetch the next value from the cache's [seq]. Update its [tbl] by storing the
    new value, ignoring the new value, or merging the new value with an existing
    value as necessary. *)
let advance t =
  match Sequence.next t.seq with
  | None -> Complete
  | Some ((id, v), rest) ->
    t.seq <- rest;
    let (extant_value, extant_value_is_canonical) =
      match Hashtbl.find t.tbl id with
      | None -> (None, false)
      | Some (v, canonical) -> (Some v, canonical)
    in
    if extant_value_is_canonical then
      Skipped
    else
      let replace_with v =
        let canonical = t.is_canonical v in
        Hashtbl.set t.tbl id (v, canonical);
        if canonical then
          Yield (id, v)
        else
          Skipped
      in
      (match extant_value with
      | None -> replace_with v
      | Some extant_value ->
        let v = t.merge ~earlier:extant_value ~later:v in
        if phys_equal v extant_value then
          Skipped
        else
          replace_with v)

let rec get t id =
  match Hashtbl.find t.tbl id with
  | Some (v, true) -> Some v
  | (None | Some (_, false)) as result ->
    (match advance t with
    | Complete -> Option.map result fst
    | Yield (id', v) when String.equal id' id -> Some v
    | Skipped
    | Yield _ ->
      get t id)

let rec mem t id =
  if Hashtbl.mem t.tbl id then
    true
  else
    match advance t with
    | Complete -> false
    | Yield (id', _) when String.equal id' id -> true
    | Skipped
    | Yield _ ->
      mem t id

let rec to_seq t =
  match advance t with
  | Skipped
  | Yield _ ->
    to_seq t
  | Complete ->
    Hashtbl.to_alist t.tbl
    |> Sequence.of_list
    |> Sequence.map ~f:(Tuple.T2.map_snd ~f:fst)
