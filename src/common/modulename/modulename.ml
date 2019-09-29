(**
 * Copyright (c) Facebook, Inc. and its affiliates.
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

let to_string = function
  | String m -> m
  | Filename f -> File_key.to_string f

let compare = Pervasives.compare

module Key = struct
  type nonrec t = t

  let to_string = to_string

  let compare = compare
end

module Set = Set.Make (Key)
module Map = MyMap.Make (Key)
