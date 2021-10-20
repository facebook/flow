(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type Comparator = sig
  type t

  val to_string : t -> string

  val is_equal : t -> t -> bool
end

module String_comparator = struct
  type t = string

  let to_string x = x

  let is_equal x y = String.equal x y
end

module Int_comparator = struct
  type t = int

  let to_string x = string_of_int x

  let is_equal x y = x = y
end

module Bool_comparator = struct
  type t = bool

  let to_string x = string_of_bool x

  let is_equal x y =
    match (x, y) with
    | (true, true) -> true
    | (false, false) -> true
    | (true, false) -> false
    | (false, true) -> false
end

module Hh_json_json_comparator = struct
  type t = Hh_json.json

  let to_string v = Hh_json.json_to_string v

  let is_equal exp actual =
    (* Shortcut by comparing the canonical string representation. *)
    let exp = Hh_json.json_to_string exp in
    let actual = Hh_json.json_to_string actual in
    String.equal exp actual
end

module Process_status_comparator = struct
  type t = Unix.process_status

  let to_string v =
    match v with
    | Unix.WEXITED i -> Printf.sprintf "Unix.WEXITED %d" i
    | Unix.WSIGNALED i -> Printf.sprintf "Unix.WSIGNALED %d" i
    | Unix.WSTOPPED i -> Printf.sprintf "Unix.WSTOPPED %d" i

  let is_equal exp actual = exp = actual
end

module type Pattern_substitutions = sig
  (** List of key-value pairs. We perform these key to value
   * substitutions in-order.
   *
   * For example, consider the substitions:
   *   [ ("foo", "bar"); ("bar", "world"); ]
   *
   * being appied to the string:
   *   "hello {{foo}}"
   *
   * which gets transformed to:
   *   "hello {bar}"
   *
   * then finally to:
   *   "hello world"
   *
   * Note: in actuality, the keys and values aren't treated as string literals
   * but as a pattern for regex and a template for replacement.
   *)
  val substitutions : (string * string) list
end

(** Comparison between an expected pattern and an actual string. *)
module Pattern_comparator (Substitutions : Pattern_substitutions) = struct
  type t = string

  let apply_substitutions s =
    List.fold_left
      (fun acc (k, v) ->
        let re = Str.regexp ("{" ^ k ^ "}") in
        Str.global_replace re v acc)
      s
      Substitutions.substitutions

  (** Argh, due to the signature of Comparator, the "expected" and
   * "actual" have the same type, even though for this Pattern_comparator
   * we would really like them to be different. We'd like "actual" to
   * be type string, and "epxected" to be type "pattern", and
   * so we can apply the substitutions to only the pattern. But splitting
   * them out into different types for all the modules only because this module
   * needs it isn't really worth it. Oh well. So we treat actual as a pattern
   * as well and apply substitutions - oh well. *)
  let to_string s = apply_substitutions s

  let is_equal expected actual =
    let expected = apply_substitutions expected in
    expected = actual
end

(** Useful for abstracting on option types. *)
module Make_option_comparator (Comp : Comparator) : Comparator with type t = Comp.t option = struct
  type t = Comp.t option

  let to_string v =
    match v with
    | None -> "None"
    | Some v -> Comp.to_string v

  let is_equal exp actual =
    match (exp, actual) with
    | (None, None) -> true
    | (None, Some _)
    | (Some _, None) ->
      false
    | (Some exp, Some actual) -> Comp.is_equal exp actual
end

module Make_asserter (Comp : Comparator) = struct
  module Comp_option = Make_option_comparator (Comp)

  let assert_equals exp actual failure_msg =
    if Comp.is_equal exp actual then
      ()
    else
      let () =
        Printf.eprintf
          "Error: assertion failure. Expected: '%s'; But Found: '%s'\n"
          (Comp.to_string exp)
          (Comp.to_string actual)
      in
      let () = Printf.eprintf "Assertion msg: %s\n" failure_msg in
      assert false

  let assert_list_equals exp actual failure_msg =
    if List.length exp = List.length actual then
      List.iter2 (fun exp actual -> assert_equals exp actual failure_msg) exp actual
    else
      let () = Printf.eprintf "assert_list_equals failed. Counts not equal\n" in
      let exp_strs = List.map Comp.to_string exp in
      let actual_strs = List.map Comp.to_string actual in
      let () =
        Printf.eprintf
          "Error: Assertion failure. Expected:\n'%s'\n\n But Found:\n'%s'\n"
          (String.concat "\n" exp_strs)
          (String.concat "\n" actual_strs)
      in
      let () = Printf.eprintf "Assertion msg: %s" failure_msg in
      assert false

  let assert_option_equals exp actual failure_msg =
    match (exp, actual) with
    | (None, None) -> ()
    | (None, Some v) ->
      Printf.eprintf "assert_option_equals failed. Expected None but got Some(%s)" (Comp.to_string v);
      Printf.eprintf "Assertion msg: %s" failure_msg;
      assert false
    | (Some v, None) ->
      Printf.eprintf "assert_option_equals failed. Expected Some(%s) but got None" (Comp.to_string v);
      Printf.eprintf "Assertion msg: %s" failure_msg;
      assert false
    | (Some exp, Some actual) -> assert_equals exp actual failure_msg
end

module Hh_json_json_option_comparator = Make_option_comparator (Hh_json_json_comparator)
module Int_option_comparator = Make_option_comparator (Int_comparator)
module String_asserter = Make_asserter (String_comparator)
module Bool_asserter = Make_asserter (Bool_comparator)
module Hh_json_json_asserter = Make_asserter (Hh_json_json_comparator)
module Int_asserter = Make_asserter (Int_comparator)
module Process_status_asserter = Make_asserter (Process_status_comparator)
