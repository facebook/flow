(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

exception ParseException of string
exception ParseStateException of string
exception CallbackNotImplementedException


type 'a callbacks_type = {
  template_start : 'a -> string list -> int -> 'a;
  template_end : 'a -> 'a;
  array_start : 'a -> int -> 'a;
  array_end : 'a -> 'a;
  array_item_end : 'a -> 'a;
  integer_value : 'a -> int -> 'a;
  string_value : 'a -> string -> 'a;
  object_start : 'a -> int -> 'a;
  object_end : 'a -> 'a;
  field_start : 'a -> string -> 'a;
  field_end : 'a -> 'a;
  boolean_value : 'a -> bool -> 'a;
  null_value : 'a -> 'a;
}


(* `input_byte` grabs 8 bits from a channel and stores them in the
    least significant byte of an `int`, which is 31 bits wide on
    32-bit systems, and 63 bits wide on 64-bit systems. This sign
    extends in either case.
*)
let sign_extend_byte i =
  let mask = 0x80 in
  let ones = (-1) lxor 0xFF in
  if (i land mask) <> 0 then
    ones lor i
  else
    i

(* Note: this ignores overflow, so e.g. on a 64-bit machine,
   values in [-2^64; -2^62) (half open range) get truncated.
*)
let parse_int_value tag ic =
  let size_bytes = match tag with
    | '\x03' -> 1
    | '\x04' -> 2
    | '\x05' -> 4
    | '\x06' -> 8
    | _ -> raise (ParseException __LOC__)
  in

  let rec inner acc i =
    if i = 0 then
      acc
    else
      let thebyte = input_byte ic in
      let acc = (acc lsl 8) lor thebyte in
      inner acc (i - 1)
  in
  let initial = input_byte ic
    |> sign_extend_byte in
  inner initial (size_bytes - 1)

let parse_int ic =
  let tag = input_char ic in
  parse_int_value tag ic

let expect_string _ic _acc _callbacks =
  failwith "expect_string not implemented"

let expect_int_value tag ic acc callbacks =
  let value = parse_int_value tag ic in
  callbacks.integer_value acc value

let expect_real _ic _acc _callbacks =
  failwith "expect_real not implemented"

(* Entry point. Parse any bser value and make the
   appropriate callbacks.

   For the tags and a general description of the format:
   https://facebook.github.io/watchman/docs/bser.html
*)
let rec expect_toplevel ic acc callbacks =
  let tag = input_char ic in
  match tag with
  | '\x00' -> expect_array ic acc callbacks
  | '\x01' -> expect_object ic acc callbacks
  | '\x02' -> expect_string ic acc callbacks
  | '\x03'
  | '\x04'
  | '\x05'
  | '\x06' -> expect_int_value tag ic acc callbacks
  | '\x07' -> expect_real ic acc callbacks
  | '\x08' -> callbacks.boolean_value acc true
  | '\x09' -> callbacks.boolean_value acc false
  | '\x0a' -> callbacks.null_value acc
  | '\x0b' -> expect_template ic acc callbacks
  | _ -> raise (ParseException __LOC__)

and expect_array ic acc callbacks =
  let size = parse_int ic in
  if size < 0 then
    raise (ParseStateException __LOC__);
  let acc = callbacks.array_start acc size in
  let rec inner acc i =
    if i = 0 then
      acc
    else
      let acc = expect_toplevel ic acc callbacks in
      let acc = callbacks.array_item_end acc in
      inner acc (i - 1)
  in
  let acc = inner acc size in
  callbacks.array_end acc

and expect_object _ic _acc _callbacks =
  failwith "expect_object not implemented"

and expect_template _ic _acc _callbacks =
  failwith "expect_template not implemented"


let throws_visitor = {
  template_start = (fun _ _ _ -> raise CallbackNotImplementedException);
  template_end = (fun _ -> raise CallbackNotImplementedException);
  array_start = (fun _ _ -> raise CallbackNotImplementedException);
  array_end = (fun _ -> raise CallbackNotImplementedException);
  array_item_end = (fun _ -> raise CallbackNotImplementedException);
  integer_value = (fun _ _ -> raise CallbackNotImplementedException);
  string_value = (fun _ _ -> raise CallbackNotImplementedException);
  object_start = (fun _ _ -> raise CallbackNotImplementedException);
  object_end = (fun _ -> raise CallbackNotImplementedException);
  field_start = (fun _ _ -> raise CallbackNotImplementedException);
  field_end = (fun _ -> raise CallbackNotImplementedException);
  boolean_value = (fun _ _ -> raise CallbackNotImplementedException);
  null_value = (fun _ -> raise CallbackNotImplementedException);
}


type json_state =
  | Js_value of Hh_json.json
  | Js_buildingArray of Hh_json.json list

type json_acc = json_state list

let json_callbacks = {
  throws_visitor with
  array_start =
    (fun acc _size ->
      (Js_buildingArray []) :: acc);

  array_end = (function
    | (Js_buildingArray elts) :: rest ->
        Js_value (Hh_json.JSON_Array (List.rev elts)) :: rest
    | _ -> raise (ParseStateException __LOC__));

  array_item_end = (function
    | (Js_value x) :: (Js_buildingArray elts) :: rest ->
        (Js_buildingArray (x :: elts)) :: rest
    | _ -> raise (ParseStateException __LOC__));

 integer_value =
    (fun acc i ->
     (Js_value (Hh_json.JSON_Number (string_of_int i))) :: acc)
}


let json_of_bser_file path : Hh_json.json =
  let ic = Sys_utils.open_in_bin_no_fail path in
  if (input_char ic) <> '\x00' then
    raise (ParseException "zero");
  if (input_char ic) <> '\x01' then
    raise (ParseException "one");
  let output = expect_toplevel ic [] json_callbacks in
  match output with
  | (Js_value x) :: [] -> x
  | _ -> raise (ParseStateException __LOC__)


let json_to_channel oc _json : unit =
  (* TODO(pieter) test output *)
  output_string oc "42"
