(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | LibFile of string
  | SourceFile of string
  | JsonFile of string
  (* A resource that might get required, like .css, .jpg, etc. We don't parse
     these, just check that they exist *)
  | ResourceFile of string
  | Builtins
[@@deriving show]

let to_string = function
  | LibFile x
  | SourceFile x
  | JsonFile x
  | ResourceFile x ->
    x
  | Builtins -> "(global)"

let to_path = function
  | LibFile x
  | SourceFile x
  | JsonFile x
  | ResourceFile x ->
    Ok x
  | Builtins -> Error "File key refers to a builtin"

let compare =
  (* builtins, then libs, then source and json files at the same priority since
     JSON files are basically source files. We don't actually read resource
     files so they come last *)
  let order_of_filename = function
    | Builtins -> 1
    | LibFile _ -> 2
    | SourceFile _ -> 3
    | JsonFile _ -> 3
    | ResourceFile _ -> 4
  in
  fun a b ->
    let k = order_of_filename a - order_of_filename b in
    if k <> 0 then
      k
    else
      String.compare (to_string a) (to_string b)

let compare_opt a b =
  match (a, b) with
  | (Some _, None) -> -1
  | (None, Some _) -> 1
  | (None, None) -> 0
  | (Some a, Some b) -> compare a b

let is_lib_file = function
  | LibFile _ -> true
  | Builtins -> true
  | SourceFile _ -> false
  | JsonFile _ -> false
  | ResourceFile _ -> false

let map f = function
  | LibFile filename -> LibFile (f filename)
  | SourceFile filename -> SourceFile (f filename)
  | JsonFile filename -> JsonFile (f filename)
  | ResourceFile filename -> ResourceFile (f filename)
  | Builtins -> Builtins

let exists f = function
  | LibFile filename
  | SourceFile filename
  | JsonFile filename
  | ResourceFile filename ->
    f filename
  | Builtins -> false

let check_suffix filename suffix = exists (fun fn -> Filename.check_suffix fn suffix) filename

let chop_suffix filename suffix = map (fun fn -> Filename.chop_suffix fn suffix) filename

let with_suffix filename suffix = map (fun fn -> fn ^ suffix) filename
