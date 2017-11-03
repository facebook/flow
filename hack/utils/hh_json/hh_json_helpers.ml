(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 **)

open Hh_core

(* Some JSON processing helpers *)

let try_get_val key json =
  let obj = Hh_json.get_object_exn json in
  List.Assoc.find obj key

let get_string_val key ?default json =
  let v = try_get_val key json in
  match v, default with
  | Some v, _ -> Hh_json.get_string_exn v
  | None, Some def -> def
  | None, None -> raise Not_found

let get_array_val key ?default json =
  let v = try_get_val key json in
  match v, default with
  | Some v, _ -> Hh_json.get_array_exn v
  | None, Some def -> def
  | None, None -> raise Not_found

let strlist args =
  Hh_json.JSON_Array begin
    List.map args (fun arg -> Hh_json.JSON_String arg)
  end

(* Prepend a string to a JSON array of strings. pred stands for predicate,
 * because that's how they are typically represented in watchman. See e.g.
 * https://facebook.github.io/watchman/docs/expr/allof.html *)
let pred name args =
  let open Hh_json in
  JSON_Array (JSON_String name :: args)
