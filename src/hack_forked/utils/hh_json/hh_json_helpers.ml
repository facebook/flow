(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Hh_json

(************************************************************************)
(* Helpers for parsing & printing                                       *)
(************************************************************************)

module Jget = struct
  exception Parse of string

  (* Helpers for the various "option" monads in use for Json, to succinctly
     capture the spirit of JSON (tolerance for missing values) and the spirit
     of LSP (loads of nested optional members with obvious defaults)
     and the usefuless of error-checking (in case a required field is absent)...
     - We use "json option" throughout. Things which you might expect to return
       a json are instead lifted to return a json option, so you can use all the
       accessors on them more easily. When you attempt to get string "o.m", either
       it's present both because "o" is Some, and because "m" is a string member
       Or it's absent because any of those three things is false...
     - The "_opt" accessors uniformally return Some (present) or None (absent),
       regardless of which of the two things caused absence.
     - The "_d" accessors uniformally return a value (present) or default.
     - The "_exn" accessors uniformally return a value (present) or throw.

     The effect of this is you lose precise information about what exactly
     caused an absence (which is usually only of marginal benefit), and in
     return you gain a consistent way to handle both optionals and requireds.

     Note one exception to the above: if you wish to get an int/float, and it's
     present as a JSON_Number but not parseable as an int/float, then all
     accessors will throw.
  *)

  let get_opt hhjson_getter json key =
    match json with
    | None -> None
    | Some json ->
      (match hhjson_getter key (json, []) with
      | Ok (r, _keytrace) -> Some r
      | _ -> None)

  let get_exn opt_getter json key =
    match opt_getter json key with
    | None -> raise (Parse key)
    | Some v -> v

  let int_string_opt (s : string option) : int option =
    match s with
    | None -> None
    | Some s -> (try Some (int_of_string s) with Failure _ -> raise (Parse ("not an int: " ^ s)))

  let float_string_opt (s : string option) : float option =
    match s with
    | None -> None
    | Some s ->
      (try Some (float_of_string s) with Failure _ -> raise (Parse ("not a float: " ^ s)))

  let list_opt (l : 'a list option) : 'a option list option =
    match l with
    | None -> None
    | Some x -> Some (List.map (fun a -> Some a) x)

  (* Accessors which return None on absence *)
  let string_opt = get_opt Access.get_string

  let bool_opt = get_opt Access.get_bool

  let obj_opt = get_opt Access.get_obj

  let val_opt = get_opt Access.get_val

  let int_opt json key = get_opt Access.get_number json key |> int_string_opt

  let float_opt json key = get_opt Access.get_number json key |> float_string_opt

  let array_opt json key = get_opt Access.get_array json key |> list_opt

  (* array_opt lifts all the array's members into the "json option" monad *)

  (* Accessors which return a supplied default on absence *)
  let string_d json key ~default = Option.value (string_opt json key) ~default

  let bool_d json key ~default = Option.value (bool_opt json key) ~default

  let int_d json key ~default = Option.value (int_opt json key) ~default

  let float_d json key ~default = Option.value (float_opt json key) ~default

  let array_d json key ~default = Option.value (array_opt json key) ~default

  (* Accessors which throw "Error.Parse key" on absence *)
  let bool_exn = get_exn bool_opt

  let string_exn = get_exn string_opt

  let val_exn = get_exn val_opt

  let int_exn = get_exn int_opt

  let float_exn = get_exn float_opt

  let array_exn = get_exn array_opt

  let obj_exn json key = Some (get_exn obj_opt json key)

  (* obj_exn lifts the result into the "json option" monad *)
end

module Jprint = struct
  (* object_opt is like Hh_json.JSON_Object constructor except it takes
     key * (value option): if a value is None, then it omits this member. *)
  let object_opt (keyvalues : (string * json option) list) : json =
    let rec filter keyvalues =
      match keyvalues with
      | [] -> []
      | (_key, None) :: rest -> filter rest
      | (key, Some value) :: rest -> (key, value) :: filter rest
    in
    JSON_Object (filter keyvalues)

  (* Convenience function to convert string list to JSON_Array *)
  let string_array (l : string list) : json = JSON_Array (List.map string_ l)
end

(* Some ad-hoc JSON processing helpers. *)

module AdhocJsonHelpers = struct
  let try_get_val key json =
    let obj = Hh_json.get_object_exn json in
    Base.List.Assoc.find ~equal:String.equal obj key

  let get_string_val key ?default json =
    let v = try_get_val key json in
    match (v, default) with
    | (Some v, _) -> Hh_json.get_string_exn v
    | (None, Some def) -> def
    | (None, None) -> raise Not_found

  let get_number_val key ?default json =
    let v = try_get_val key json in
    match (v, default) with
    | (Some v, _) -> Hh_json.get_number_exn v
    | (None, Some def) -> def
    | (None, None) -> raise Not_found

  let get_bool_val key ?default json =
    let v = try_get_val key json in
    match (v, default) with
    | (Some v, _) -> Hh_json.get_bool_exn v
    | (None, Some def) -> def
    | (None, None) -> raise Not_found

  let get_array_val key ?default json =
    let v = try_get_val key json in
    match (v, default) with
    | (Some v, _) -> Hh_json.get_array_exn v
    | (None, Some def) -> def
    | (None, None) -> raise Not_found

  let strlist args = Hh_json.JSON_Array (List.map (fun arg -> Hh_json.JSON_String arg) args)

  (* Useful for building an array like [ "suffix", [".txt", ".js", ".php" ]] *)
  let assoc_strlist name args = Hh_json.JSON_Array [Hh_json.JSON_String name; strlist args]

  (* Prepend a string to a JSON array of strings. pred stands for predicate,
   * because that's how they are typically represented in watchman. See e.g.
   * https://facebook.github.io/watchman/docs/expr/allof.html *)
  let pred name args = Hh_json.(JSON_Array (JSON_String name :: args))
end
