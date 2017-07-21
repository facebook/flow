(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Reordered_argument_collections
open Utils
open String_utils

type prefix =
  | Root
  | Hhi
  | Dummy

let root = ref None
let hhi = ref None

let path_ref_of_prefix = function
  | Root -> root
  | Hhi -> hhi
  | Dummy -> ref (Some "")

let path_of_prefix x =
  unsafe_opt_note "Prefix has not been set!" !(path_ref_of_prefix x)

let string_of_prefix = function
  | Root -> "root"
  | Hhi -> "hhi"
  | Dummy -> ""

let set_path_prefix prefix v =
  let v = Path.to_string v in
  assert (String.length v > 0);
  (* Ensure that there is a trailing slash *)
  let v =
    if string_ends_with v Filename.dir_sep then v
    else v ^ Filename.dir_sep
  in
  match prefix with
  | Dummy -> raise (Failure "Dummy is always represented by an empty string")
  | _ -> path_ref_of_prefix prefix := Some v

type relative_path = prefix * string
type t = relative_path

let prefix (p : t) = fst p

let suffix (p : t) = snd p

let default = (Dummy, "")

module S = struct
  type t = relative_path

  let compare = Pervasives.compare

  (* We could have simply used Marshal.to_string here, but this does slightly
   * better on space usage. *)
  let to_string (p, rest) = string_of_prefix p ^ "|" ^ rest
end

let to_absolute (p, rest) = path_of_prefix p ^ rest

let pp fmt rp = Format.pp_print_string fmt (S.to_string rp)

module Set = Reordered_argument_set(Set.Make(S))
module Map = Reordered_argument_map(MyMap.Make(S))

let create prefix s =
  let prefix_s = path_of_prefix prefix in
  let prefix_len = String.length prefix_s in
  if not (string_starts_with s prefix_s)
  then begin
    Printf.eprintf "%s is not a prefix of %s" prefix_s s;
    assert_false_log_backtrace None;
  end;
  prefix, String.sub s prefix_len (String.length s - prefix_len)

let from_root (s : string) : t = Root, s

let relativize_set prefix m =
  SSet.fold m ~init:Set.empty ~f:(fun k a -> Set.add a (create prefix k))

let set_of_list xs =
  List.fold_left xs ~f:Set.add ~init:Set.empty
