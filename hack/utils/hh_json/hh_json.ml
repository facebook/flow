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

module List = Core_list

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

(* Ignore whitespace in peek/eat/next/has_more to make code that uses them
   cleaner
 *)

let peek env = String.get env.data env.pos

let has_more env = String.length env.data > env.pos

let syntax_error env msg =
  let err_msg =
    if has_more env then
      Printf.sprintf "%s at char[%d]=%c" msg env.pos env.data.[env.pos]
    else
      Printf.sprintf "%s after the last character" msg in
  raise (Syntax_error err_msg)

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

let char_code env =
  let rec char_code_ (acc : int) env len =
    if len = 0 then acc
    else begin
      env.pos <- env.pos + 1;
      let c = peek env in
      let i =
        if '0' <= c && c <= '9' then (Char.code c) - (Char.code '0')
        else if 'a' <= c && c <= 'f' then 10 + (Char.code c) - (Char.code 'a')
        else syntax_error env "expected hexadecimal digit"
      in
      char_code_ (16*acc + i) env (len-1)
    end
  in
  char_code_ 0 env 4

let js_string env =
  let buf = Buffer.create 128 in
  let rec loop env =
    let c = peek env in
    match c with
    | '"' -> JSON_String (Buffer.contents buf)
    | '\\' ->
        env.pos <- env.pos + 1;
        let c' = peek env in
        let c' = match c' with
          | 'n' -> '\n'
          | 'r' -> '\r'
          | 't' -> '\t'
          | 'u' ->
            let code = char_code env in
            Char.chr code
          | x -> x in

        env.pos <- env.pos + 1;
        Buffer.add_char buf c';
        loop env
    | _ ->
        buf_eat_all buf env c;
        loop env
  in
  (match peek env with
  | '"' -> env.pos <- env.pos + 1
  | _ ->  syntax_error env "expected '\"' character");
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
  let js_value_syntax_error () =
    let err_msg = "expected '{[\"0123456789' or {t,f,n}" in
    syntax_error env err_msg in

  if not (has_more env) then
    js_value_syntax_error ()
  else
    match peek env with
    | '{' -> js_object env
    | '[' -> js_array env
    | '"' -> js_string env
    | c when is_digit c || c = '-' -> js_number env
    | 't' -> js_true env
    | 'f' -> js_false env
    | 'n' -> js_null env
    | _   -> js_value_syntax_error ()
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
    match try Some(input_line ic) with _ -> None with
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

let rec json_to_string ?(pretty=false) (json:json): string =
  if pretty
  then json_to_multiline json
  else
    let buf = Buffer.create 1024 in (* need a better estimate! *)
    add_json_to_buffer buf json;
    Buffer.contents buf

and json_to_multiline json =
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


let rec output_list oc elems output_elem : unit =
  match elems with
  | [] -> ()
  | [elem] -> output_elem oc elem
  | elem :: other_elems ->
      output_elem oc elem;
      output_string oc ",";
      output_list oc other_elems output_elem

let rec json_to_output oc (json:json): unit =
  match json with
  | JSON_Object l ->
      output_string oc "{";
      output_list oc l json_assoc_to_output;
      output_string oc "}";
  | JSON_Array l ->
      output_string oc "[";
      output_list oc l json_to_output;
      output_string oc "]";
  | JSON_String s ->
      output_string oc (escape s)
  | JSON_Number n ->
      output_string oc n
  | JSON_Bool b ->
      output_string oc (if b then "true" else "false")
  | JSON_Null ->
      output_string oc "null"

and json_assoc_to_output oc (k,v) : unit =
  output_string oc (escape k);
  output_string oc ":";
  json_to_output oc v

let json_of_string ?(strict=true) s =
  let lb = create_env strict s in
  js_value lb

let json_of_file ?strict filename =
  json_of_string ?strict (string_of_file filename)

let int_ n = JSON_Number (string_of_int n)

let string_ s = JSON_String s

let get_object_exn = function
  | JSON_Object o -> o
  | _ -> assert false

let get_array_exn = function
  | JSON_Array a -> a
  | _ -> assert false

let get_string_exn = function
  | JSON_String s -> s
  | _ -> assert false

let get_number_exn = function
  | JSON_Number s -> s
  | _ -> assert false

let get_number_int_exn = function
  | JSON_Number s -> int_of_string s
  | _ -> assert false

let get_bool_exn = function
  | JSON_Bool b -> b
  | _ -> assert false

let opt_string_to_json = function
  | Some x -> JSON_String x
  | None -> JSON_Null

let opt_int_to_json = function
  | Some x -> JSON_Number (string_of_int x)
  | None -> JSON_Null

type json_type =
  | Object_t
  | Array_t
  | String_t
  | Number_t
  | Integer_t
  | Bool_t

let json_type_to_string = function
  | Object_t -> "Object"
  | Array_t -> "Array"
  | String_t -> "String"
  | Number_t -> "Number"
  | Integer_t -> "Integer"
  | Bool_t -> "Bool"

module type Access = sig
  type keytrace = string list

  type access_failure =
    | Not_an_object of keytrace
    | Missing_key_error of string * keytrace
    | Wrong_type_error of keytrace * json_type

  type 'a m = (('a * keytrace), access_failure) Result.t

  val access_failure_to_string : access_failure -> string

  val return : 'a -> 'a m

  val (>>=) : 'a m -> (('a * keytrace) -> 'b m) -> 'b m
  val counit_with : (access_failure -> 'a) -> 'a m -> 'a
  val to_option : 'a m -> 'a option
  val get_obj : string -> json * keytrace -> json m
  val get_bool : string -> json * keytrace -> bool m
  val get_string : string -> json * keytrace -> string m
  val get_number : string -> json * keytrace -> string m
  val get_number_int : string -> json * keytrace -> int m
  val get_array: string -> json * keytrace -> (json list) m
  val get_val: string -> json * keytrace -> json m
end

module Access = struct
  type keytrace = string list

  type access_failure =
    | Not_an_object of keytrace
    | Missing_key_error of string * keytrace
    | Wrong_type_error of keytrace * json_type

  type 'a m = (('a * keytrace), access_failure) Result.t

  let keytrace_to_string x =
    if x = [] then "" else
    let res = List.map x (fun x -> "[" ^ x ^ "]")  |> String.concat " " in
    " (at field " ^ res ^ ")"

  let access_failure_to_string = function
    | Not_an_object x ->
      Printf.sprintf "Value is not an object %s" (keytrace_to_string x)
    | Missing_key_error (x, y) ->
      Printf.sprintf "Missing key: %s%s" x (keytrace_to_string y)
    | Wrong_type_error (x, y) ->
      Printf.sprintf "Value expected to be %s%s"
        (json_type_to_string y) (keytrace_to_string x)

  let return v = Result.Ok (v, [])

  let (>>=) m f = Result.bind m f

  let counit_with f m = match m with
    | Result.Ok (v, _) ->
      v
    | Result.Error e ->
      f e

  let to_option = function
    | Result.Ok (v, _) -> Some v
    | Result.Error _ -> None

  let catch_type_error exp f (v, keytrace) =
    try Result.Ok (f v, keytrace) with
      | Failure msg when (String.equal "int_of_string" msg) ->
        Result.Error (Wrong_type_error (keytrace, exp))
      | Assert_failure _ ->
        Result.Error (Wrong_type_error (keytrace, exp))

  let get_val k (v, keytrace) =
    try begin
      let obj = get_object_exn v in
      let candidate = List.fold_left obj ~init:None ~f:(fun opt (key, json) ->
        if opt <> None then opt
        else if key = k then (Some json)
        else None
      ) in
      match candidate with
      | None -> Result.Error (Missing_key_error (k, keytrace))
      | Some obj ->
        Result.Ok (obj, k :: keytrace)
    end with
    | Assert_failure _ ->
      Result.Error (Not_an_object (keytrace))

  let make_object_json v =
    JSON_Object (get_object_exn v)

  let get_obj k (v, keytrace) =
    get_val k (v, keytrace) >>= catch_type_error Object_t make_object_json

  let get_bool k (v, keytrace) =
    get_val k (v, keytrace) >>= catch_type_error Bool_t get_bool_exn

  let get_string k (v, keytrace) =
    get_val k (v, keytrace) >>= catch_type_error String_t get_string_exn

  let get_number k (v, keytrace) =
    get_val k (v, keytrace) >>= catch_type_error Number_t get_number_exn

  let get_number_int k (v, keytrace) =
    get_val k (v, keytrace) >>= catch_type_error Integer_t get_number_int_exn

  let get_array k (v, keytrace) =
    get_val k (v, keytrace) >>= catch_type_error Array_t get_array_exn
end

let ( >=@ ) : int -> int option -> bool = fun lhs rhs ->
  match rhs with
  | None -> false
  | Some rhs -> lhs >= rhs

let ( <=@ ) : int -> int option -> bool = fun lhs rhs ->
  match rhs with
  | None -> false
  | Some rhs -> lhs <= rhs

let json_truncate
    ?(max_string_length:int option)
    ?(max_child_count:int option)
    ?(max_depth:int option)
    ?(max_total_count:int option)
    ?(has_changed:bool ref option)
    (json: json)
  : json =
  let total_count = ref 0 in
  let mark_changed () = match has_changed with
    | None -> ()
    | Some r -> r := true
  in

  let rec truncate_children ~child_count children ~f =
    match children with
    | [] -> []
    | _ when !total_count >=@ max_total_count -> mark_changed (); []
    | _ when child_count >=@ max_child_count -> mark_changed (); []
    | c :: rest ->
      incr total_count;
      let c' = f c in (* because of mutable variable, it's important to do this first *)
      c' :: (truncate_children (child_count + 1) rest f)
  in

  let rec truncate ~(depth: int) (json: json) : json =
    match json with
    | JSON_Object []
    | JSON_Array []
    | JSON_Number _
    | JSON_Bool _
    | JSON_Null ->
      json
    | JSON_Object props ->
      let f (k, v) = (k, truncate (depth + 1) v) in
      if depth >=@ max_depth then begin mark_changed (); JSON_Object [] end
      else JSON_Object (truncate_children ~child_count:0 props ~f)
    | JSON_Array values ->
      let f v = truncate (depth + 1) v in
      if depth >=@ max_depth then begin mark_changed (); JSON_Array [] end
      else JSON_Array (truncate_children ~child_count:0 values ~f)
    | JSON_String s -> begin
        match max_string_length with
        | None -> json
        | Some max_string_length ->
          if String.length s <= max_string_length then JSON_String s
          else begin mark_changed (); JSON_String ((String.sub s 0 max_string_length) ^ "...") end
      end
  in
  truncate ~depth:0 json


let json_truncate_string
    ?(max_string_length:int option)
    ?(max_child_count:int option)
    ?(max_depth:int option)
    ?(max_total_count:int option)
    ?(allowed_total_length:int option)
    ?(if_reformat_multiline = true)
    (s: string)
  : string =
  if String.length s <=@ allowed_total_length then
    s (* fast zero-allocation path for the commonest case *)
  else begin
    let has_changed = ref false in
    let json = json_of_string s in
    let truncated_json =
      json_truncate
        ?max_string_length
        ?max_child_count
        ?max_depth
        ?max_total_count
        ~has_changed
        json in
    if not !has_changed then
      s (* moderately fast fewer-string-allocating for another common case *)
    else
      if if_reformat_multiline then json_to_multiline truncated_json
      else json_to_string truncated_json
  end
