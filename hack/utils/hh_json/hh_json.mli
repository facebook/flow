(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(**
 * Hh_json parsing and pretty printing library.
 *)

type json =
    JSON_Object of (string * json) list
  | JSON_Array of json list
  | JSON_String of string
  | JSON_Number of string
  | JSON_Bool of bool
  | JSON_Null

exception Syntax_error of string

val json_to_string : ?pretty:bool -> json -> string
val json_to_multiline : json -> string
val json_to_output: out_channel -> json ->  unit
val json_of_string : ?strict:bool -> string -> json
val json_of_file : ?strict:bool -> string -> json

val get_object_exn : json -> (string * json) list
val get_array_exn : json -> json list
val get_string_exn : json -> string
val get_number_exn : json -> string
val get_bool_exn : json -> bool

val int_ : int -> json

(** Types and functions for monadic API for traversing a JSON object. *)

type json_type =
  | Object_t
  | Array_t
  | String_t
  | Number_t
  | Bool_t

(**
 * This module gives monadic recursive access to values within objects by key.
 * It uses the Result.t to manage control flow in the monad when an error is
 * encountered. It also tracks the backtrace of the keys accessed to give
 * detailed error messages.
 *
 * Usage:
 *  To access the boolean value "qux" from the following json:
   *  { "foo": { "bar" : { "baz" : { "qux" : true } } } }
 * Is as follows:
   * (return json) >>=
   *   get_obj "foo" >>=
   *   get_obj "bar" >>=
   *   get_obj "baz" >>=
   *   get_bool "qux"
 *
 * If an error is encountered along the call chain, a Result.Error is returned
 * with the appropriate error and the history of key accesses that arrived
 * there (so you can trace how far it went successfully and exactly where the
 * error was encountered).
 *)
module type Access = sig
  type keytrace = string list

  type access_failure =
    (** You can't access keys on a non-object JSON thing. *)
    | Not_an_object of keytrace
    (** The key is missing. *)
    | Missing_key_error of string * keytrace
    (** The key has the wrong type. *)
    | Wrong_type_error of keytrace * json_type

  (** Our type for the result monad. It isn't just the json because it tracks
   * a history of the keys traversed to arrive at the current point. This helps
   * produce more informative error states. *)
  type 'a m = (('a * keytrace), access_failure) Result.t

  val return : 'a -> 'a m

  val (>>=) : 'a m -> (('a * keytrace) -> 'b m) -> 'b m

  (**
   * The following getters operate on a JSON_Object by accessing keys on it,
   * and asserting the returned value has the given expected type (types
   * are asserted by which getter you choose to use).
   *
   * Returns Not_an_object if the given JSON object is not a JSON_Object type,
   * since you can only access keys on those.
   *
   * Returns Wrong_type_error if the obtained value is not an object type.
   *
   * Returns Missing_key_error if the given key is not found in this object.
   *
   *)
  val get_obj : string -> json * keytrace -> json m
  val get_bool : string -> json * keytrace -> bool m
  val get_string : string -> json * keytrace -> string m
  val get_number : string -> json * keytrace -> string m
  val get_array: string -> json * keytrace -> (json list) m
end

module Access : Access
