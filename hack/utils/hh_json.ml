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

(*
<value> ::=
  | <object>
  | <array>
  | <string>
  | <number>
  | 'true'
  | 'false'
  | 'null'

<object>   ::= '{' <members>* '}'
<members>  ::= <pair> { ',' <pair> }* [',']
<pair>     ::= <string> ':' <value>

<array>    ::= '{' <elements>* '}'
<elements> ::= <value> { ',' <value> }* [',']

<number>   ::=

Caveats:
 (+) No handling of Unicode yet
 (+) Numbers are just stored as strings
 *)

open Core

type json =
  | JSON_Object of (string * json) list
  | JSON_Array of json list
  | JSON_String of string
  | JSON_Number of string
  | JSON_Bool of bool
  | JSON_Null

let is_digit = function '0' .. '9' -> true | _ -> false

let is_whitespace = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false

type env = {
  allow_trailing_comma : bool;
  data : string;
  mutable pos : int;
}

exception Syntax_error of string

let syntax_error env msg =
  let err_msg =
    Printf.sprintf "%s at char[%d]=%c" msg env.pos env.data.[env.pos] in
  raise (Syntax_error err_msg)

(* Ignore whitespace in peek/eat/next/has_more to make code that uses them
   cleaner
 *)

let peek env = String.get env.data env.pos

let has_more env = String.length env.data > env.pos

(* skip all blank and new line characters *)
let skip_blank_chars env =
  while has_more env && is_whitespace (peek env) do
    env.pos <- env.pos + 1
  done

let create_env strict s =
  let env = { allow_trailing_comma=not strict; data=s; pos=0 } in
  skip_blank_chars env;
  env

let eat_ws env c =
  let c' = peek env in
  if c' = c then
    env.pos <- env.pos + 1
  else
    let err_msg = Printf.sprintf "eat_ws: expected %c, saw %c" c c' in
    syntax_error env err_msg

let eat env c =
  skip_blank_chars env;
  let c' = peek env in
  if c' = c then
    begin
      env.pos <- env.pos + 1;
      skip_blank_chars env
    end
  else
    let err_msg = Printf.sprintf "eat: expected %c, saw %c" c c' in
    syntax_error env err_msg

let match_substring_at s offset ss =
  let ss_len = String.length ss in
  if String.length s - offset >= ss_len then
    try
      for i = 0 to ss_len - 1 do
        if s.[i + offset] <> ss.[i] then
          raise Exit
      done;
      true
    with Exit -> false
  else
    false

let js_literal env s js =
  skip_blank_chars env;
  if match_substring_at env.data env.pos s then
    begin env.pos <- env.pos + String.length s; js end
  else
    let err_msg =
      Printf.sprintf "expected '%s'" s in
    syntax_error env err_msg

let js_true  env = js_literal env "true" (JSON_Bool(true))
let js_false env = js_literal env "false" (JSON_Bool(false))
let js_null  env = js_literal env "null" JSON_Null

let buf_eat buf env c = (eat env c; Buffer.add_char buf c)
let buf_eat_all buf env c = (eat_ws env c; Buffer.add_char buf c)

let js_string env =
  let buf = Buffer.create 128 in
  let rec loop env =
    let c = peek env in
    match c with
    | '"' -> JSON_String (Buffer.contents buf)
    | '\\' ->
        env.pos <- env.pos + 1;
        let c' = peek env in
        env.pos <- env.pos + 1;
        Buffer.add_char buf c';
        loop env
    | _ ->
        buf_eat_all buf env c;
        loop env
  in
  eat env '"';
  if peek env = '"' then
    begin eat env '"'; JSON_String("") end
  else
    let res = loop env in
    eat env '"';
    res

let rec buf_eat_digits buf env =
  if has_more env then
    let c = peek env in
    if is_digit c then
      begin buf_eat buf env c; buf_eat_digits buf env end
    else
      () (* encountered a non-digit char, stop *)
  else
    () (* end of string, stop *)

let buf_eat_exp buf env =
  let c = peek env in
  if  c = 'e' || c = 'E' then
    begin
      buf_eat buf env c;
      let sign = peek env in
      if sign = '+' || sign = '-' then
        buf_eat buf env sign;
      buf_eat_digits buf env;
    end

let js_number env =
  let buf = Buffer.create 32 in
  let c = peek env in
  if c = '-' then
    buf_eat buf env '-';
  buf_eat_digits buf env; (* ['-'] digits *)
  let c = peek env in
  if c = '.'  then (* ['.' digits ] *)
    begin
      buf_eat buf env '.';
      buf_eat_digits buf env;
    end;
  buf_eat_exp buf env; (* [exp digits] *)
  JSON_Number(Buffer.contents buf)

(* The recursive rules *)
let rec js_value env =
  match peek env with
  | '{' -> js_object env
  | '[' -> js_array env
  | '"' -> js_string env
  | c when is_digit c || c = '-' -> js_number env
  | 't' -> js_true env
  | 'f' -> js_false env
  | 'n' -> js_null env
  | _ ->
    let err_msg = "expected '{[\"0123456789' or {t,f,n}" in
    syntax_error env err_msg
and js_object env =
  let rec loop members =
    let p = js_pair env in
    if peek env <> ',' then
      JSON_Object(List.rev (p::members))
    else
      begin
        eat env ',';
        if peek env = '}' then
          if env.allow_trailing_comma then
            JSON_Object(List.rev (p::members))
          else
            syntax_error env "Hh_json.object: trailing comma"
        else
          loop (p::members)
      end
  in
  eat env '{';
  if peek env = '}' then
    begin eat env '}'; JSON_Object([]) end
  else
    let res = loop [] in
    eat env '}';
    res

and js_array env =
  let rec elements accum =
    let v = js_value env in
    if peek env <> ',' then
      JSON_Array(List.rev (v::accum))
    else
      begin
        eat env ',';
        if peek env = ']' then
          if env.allow_trailing_comma then
            JSON_Array(List.rev (v::accum))
          else
            syntax_error env "Hh_json.array: trailing comma"
        else
          elements (v::accum)
      end
  in
  eat env '[';
  if peek env = ']' then
    begin eat env ']'; JSON_Array([]) end
  else
    let res = elements [] in
    begin eat env ']'; res end

and js_pair env =
  skip_blank_chars env;
  let k = js_string env in
  skip_blank_chars env;
  eat env ':';
  let v = js_value env in
  match k with
  | JSON_String s -> (s,v)
  | _ -> syntax_error env "Hh_json.js_pair: expected a JSON String"

let string_of_file filename =
  let ic = open_in filename in
  let buf = Buffer.create 5096 in
  let rec loop () =
    match try Some(input_line ic) with e -> None with
    | None -> Buffer.contents buf
    | Some l ->
       begin
         Buffer.add_string buf l;
         Buffer.add_char buf '\n';
         loop ();
       end
  in
  loop ()

(* Writing JSON *)

(* buf_concat :
 * Designed as a substitute for String.concat that passes a buffer
 * into which intermediate strings are added, and also includes left
 * and right bracket (lb and rb) in addition to sep. They are strings,
 * despite common case of (), [],{}, or even <>, to handle missing brackets,
 * brackets with spacing and multichar brackets like OCaml's arrays ([| and |]).
 * The A conc_elt function parameter performs the operation of transforming
 * the list element to a string and adding it to the buffer, the simplest
 * example would be fun x -> Buffer.add_string (to_string x)
 *)
let buf_concat ~buf ~lb ~rb ~sep ~concat_elt l =
  Buffer.add_string buf lb;
  (match l with
   | [] -> ()
   | elt :: elts ->
       concat_elt buf elt;
       List.iter elts begin fun e ->
         Buffer.add_string buf sep; concat_elt buf e
       end);
  Buffer.add_string buf rb

let add_char buf c = Buffer.add_char buf c
let add_string buf s = Buffer.add_string buf s

let escape s =
  let b = Buffer.create ((String.length s) + 2) in
  Buffer.add_char b '"';
  s |> String.iter begin fun c ->
    let code = Char.code c in
    match c, code with
      | '\\', _ -> Buffer.add_string b "\\\\"
      | '"', _ ->  Buffer.add_string b "\\\""
      | '\n', _ -> Buffer.add_string b "\\n"
      | '\r', _ -> Buffer.add_string b "\\r"
      | '\t', _ -> Buffer.add_string b "\\t"
      | _, _ when code <= 0x1f ->
        Printf.sprintf "\\u%04x" code
        |> Buffer.add_string b
      | _ -> Buffer.add_char b c
  end;
  Buffer.add_char b '"';
  Buffer.contents b

let rec add_json_to_buffer (buf:Buffer.t) (json:json): unit =
  match json with
  | JSON_Object l ->
      buf_concat ~buf ~lb:"{" ~rb:"}" ~sep:"," ~concat_elt:add_assoc_to_buffer l
  | JSON_Array l ->
      buf_concat ~buf ~lb:"[" ~rb:"]" ~sep:"," ~concat_elt:add_json_to_buffer l
  | JSON_String s -> add_string buf (escape s)
  | JSON_Number n -> add_string buf n
  | JSON_Bool b -> if b then add_string buf "true" else add_string buf "false"
  | JSON_Null -> add_string buf "null"

and add_assoc_to_buffer (buf:Buffer.t) (k,v) =
  add_string buf (escape k);
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
    | JSON_Array l ->
        let nl = List.map l (loop (indent ^ "  ")) in
        "[\n" ^ indent ^ "  " ^ (String.concat (",\n" ^ indent ^ "  ") nl) ^
          "\n" ^ indent ^ "]"
    | JSON_Object l ->
       let nl =
         List.map l
           (fun (k, v) ->
            indent ^ "  " ^ (json_to_string (JSON_String k)) ^ ":" ^
              (loop (indent ^ "  ") v))
       in
        "{\n" ^ (String.concat ",\n" nl) ^ "\n" ^ indent ^ "}"
    | _ -> single
  in
  loop "" json

let json_of_string ?(strict=true) s =
  let lb = create_env strict s in
  js_value lb

let json_of_file ?(strict=true) filename =
  json_of_string (string_of_file filename)

let int_ n = JSON_Number (string_of_int n)

let get_object_exn = function
  | JSON_Object o -> o
  | _ -> assert false

let get_array_exn = function
  | JSON_Array a -> a
  | _ -> assert false

let get_string_exn = function
  | JSON_String s -> s
  | _ -> assert false

let get_bool_exn = function
  | JSON_Bool b -> b
  | _ -> assert false
