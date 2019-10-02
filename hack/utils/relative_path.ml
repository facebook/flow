(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

open Hh_core
open Reordered_argument_collections
open Utils
open String_utils

type prefix =
  | Root
  | Hhi
  | Dummy
  | Tmp
[@@deriving eq, show, enum]

let root = ref None

let hhi = ref None

let tmp = ref None

let path_ref_of_prefix = function
  | Root -> root
  | Hhi -> hhi
  | Tmp -> tmp
  | Dummy -> ref (Some "")

let string_of_prefix = function
  | Root -> "root"
  | Hhi -> "hhi"
  | Tmp -> "tmp"
  | Dummy -> ""

let path_of_prefix prefix =
  match !(path_ref_of_prefix prefix) with
  | Some path -> path
  | None ->
    let message =
      Printf.sprintf "Prefix '%s' has not been set!" (string_of_prefix prefix)
    in
    raise (Invalid_argument message)

let set_path_prefix prefix v =
  let v = Path.to_string v in
  assert (String.length v > 0);

  (* Ensure that there is a trailing slash *)
  let v =
    if string_ends_with v Filename.dir_sep then
      v
    else
      v ^ Filename.dir_sep
  in
  match prefix with
  | Dummy -> raise (Failure "Dummy is always represented by an empty string")
  | _ -> path_ref_of_prefix prefix := Some v

type t = prefix * string [@@deriving eq, show]

type relative_path = t

let prefix (p : t) = fst p

let suffix (p : t) = snd p

let default = (Dummy, "")

(* We could have simply used Marshal.to_string here, but this does slightly
 * better on space usage. *)
let storage_to_string (p, rest) = string_of_prefix p ^ "|" ^ rest

let index_opt str ch = (try Some (String.index str ch) with Not_found -> None)

let storage_of_string str =
  match index_opt str '|' with
  | Some idx ->
    let (a, a') = (0, idx) in
    let b = idx + 1 in
    let b' = String.length str - b in
    let prefix = String.sub str a a' in
    let content = String.sub str b b' in
    let prefix =
      match prefix with
      | "root" -> Root
      | "hhi" -> Hhi
      | "tmp" -> Tmp
      | "" -> Dummy
      | _ -> failwith "invalid prefix"
    in
    (prefix, content)
  | None -> failwith "not a Relative_path.t"

module S = struct
  type t = relative_path

  let compare = Pervasives.compare

  let to_string = storage_to_string
end

let to_absolute (p, rest) = path_of_prefix p ^ rest

let to_tmp (_, rest) = (Tmp, rest)

let to_root (_, rest) = (Root, rest)

module Set = struct
  include Reordered_argument_set (Set.Make (S))

  let pp fmt x =
    Format.fprintf fmt "@[<2>{";
    ignore
    @@ List.fold_left (elements x) ~init:false ~f:(fun sep s ->
           if sep then Format.fprintf fmt ";@ ";
           pp fmt s;
           true);
    Format.fprintf fmt "@,}@]"

  let show x = Format.asprintf "%a" pp x
end

module Map = struct
  include Reordered_argument_map (MyMap.Make (S))

  let pp pp_data = make_pp pp pp_data

  let show pp_data x = Format.asprintf "%a" (pp pp_data) x
end

let create prefix s =
  let prefix_s = path_of_prefix prefix in
  let prefix_len = String.length prefix_s in
  if not (string_starts_with s prefix_s) then (
    Printf.eprintf "%s is not a prefix of %s" prefix_s s;
    assert_false_log_backtrace None
  );
  (prefix, String.sub s prefix_len (String.length s - prefix_len))

let create_detect_prefix s =
  let file_prefix =
    [Root; Hhi; Tmp]
    |> List.find ~f:(fun prefix ->
           String_utils.string_starts_with s (path_of_prefix prefix))
    |> fun x ->
    match x with
    | Some prefix -> prefix
    | None -> Dummy
  in
  create file_prefix s

(* Strips the root and relativizes the file if possible, otherwise returns
  original string *)
let strip_root_if_possible s =
  let prefix_s = path_of_prefix Root in
  let prefix_len = String.length prefix_s in
  if not (string_starts_with s prefix_s) then
    None
  else
    Some (String.sub s prefix_len (String.length s - prefix_len))

let from_root (s : string) : t = (Root, s)

let relativize_set prefix m =
  SSet.fold m ~init:Set.empty ~f:(fun k a -> Set.add a (create prefix k))

let set_of_list xs = List.fold_left xs ~f:Set.add ~init:Set.empty
