(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(**
 * Json pretty printing library.
 *
 *)

type json =
    JList of json list
  | JBool of bool
  | JString of string
  | JAssoc of (string * json) list
  | JNull
  | JInt of int
  | JFloat of float

(* buf_concat :
   Designed as a substitute for String.concat that passes a buffer
   into which intermediate strings are added, and also includes left
   and right bracket (lb and rb) in addition to sep. They are strings,
   despite common case of (), [],{}, or even <>, to handle missing brackets,
   brackets with spacing and multichar brackets like OCaml's arrays ([| and |]).
   The A conc_elt function parameter performs the operation of transforming
   the list element to a string and adding it to the buffer, the simplest
   example would be fun x -> Buffer.add_string (to_string x)
 *)

let buf_concat ~buf ~lb ~rb ~sep ~concat_elt l =
  Buffer.add_string buf lb;
  (match l with
   | [] -> ()
   | elt::elts ->
      concat_elt buf elt;
      List.iter
        (fun e -> Buffer.add_string buf sep; concat_elt buf e)
        elts);
  Buffer.add_string buf rb

(* quote_string s:
   A replacement for fun s -> Printf.sprintf "%S" s
*)
let quote_string s =
  let len = String.length s in
  let r = String.create (len + 2) in
  String.set r 0 '\"';
  String.set r (len+1) '\"';
  String.unsafe_blit s 0 r 1 len;
  r

let add_char buf c = Buffer.add_char buf c
let add_string buf s = Buffer.add_string buf s

let rec add_json_to_buffer (buf:Buffer.t) (json:json): unit =
  match json with
  | JList l -> buf_concat
                 ~buf
                 ~lb:"[" ~rb:"]" ~sep:","
                 ~concat_elt:add_json_to_buffer
       l
  | JAssoc l -> buf_concat
                  ~buf
                  ~lb:"{" ~rb:"}" ~sep:","
                  ~concat_elt:add_assoc_to_buffer
                  l
  | JBool b -> if b then add_string buf "true" else add_string buf "false"
  | JString s -> add_string buf (quote_string s)
  | JNull -> add_string buf "null"
  | JInt i -> add_string buf (string_of_int i)
  | JFloat f -> add_string buf (string_of_float f)

and add_assoc_to_buffer (buf:Buffer.t) (k,v) =
  add_string buf (quote_string k);
  add_char buf ':';
  add_json_to_buffer buf v

let json_to_string (json:json): string =
  let buf = Buffer.create 1024 in (* need a better estimate! *)
  add_json_to_buffer buf json;
  Buffer.contents buf

let json_to_multiline json =
  let rec loop indent json =
    let single = json_to_string json in
    if String.length single < 80 then single else
    match json with
    | JList l ->
        let nl = List.map (loop (indent ^ "  ")) l in
        "[\n" ^ indent ^ "  " ^ (String.concat (",\n" ^ indent ^ "  ") nl) ^
          "\n" ^ indent ^ "]"
    | JAssoc l ->
        let nl = List.map (fun (k, v) ->
          indent ^ "  " ^ (json_to_string (JString k)) ^ ":" ^
            (loop (indent ^ "  ") v)
        ) l in
        "{\n" ^ (String.concat ",\n" nl) ^ "\n" ^ indent ^ "}"
    | _ -> single
  in
  loop "" json
