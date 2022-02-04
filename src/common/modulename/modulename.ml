(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Signature for module names. Such names are assumed to be "resolved", and have
   global scope in the sense that they may be used to identify modules uniquely
   in a code base. In contrast, module references (strings that are used to
   refer to modules in require/import statements) have local scope in the sense
   that their meaning is relative to the files they appear in.

   There are two ways to construct a module name:

   * A module name may be a String that is declared in a file: e.g., in the
   Haste module system such module names are declared via @providesModule.

   * A module name may be a Filename: e.g., in the Node module system a module
   is simply known by its path in the file system.
*)
type t =
  | String of string
  | Filename of File_key.t
[@@deriving show, ord]

let to_string = function
  | String m -> m
  | Filename f -> File_key.to_string f

module Key = struct
  type nonrec t = t

  let pp = pp
  let to_string = to_string
  let compare : t -> t -> int = compare
end

module Set = struct
  include Flow_set.Make (Key)

  let pp = make_pp Key.pp
  let show x = Format.asprintf "%a" pp x
end

module Map = struct
  include WrappedMap.Make (Key)

  let pp pp_data = make_pp Key.pp pp_data
  let show pp_data x = Format.asprintf "%a" (pp pp_data) x
end
