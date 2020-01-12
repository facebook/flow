(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Hh_core

(* This is a lightweight library for reading and writing messages in the HTTP
   format, with headers and body. So far it only supports the small set of
   features needed by the Language Server Protocol. It follows the internet
   robustness principle of being as permissive as possible in what it expects,
   i.e. no validation beyond what it essentially needs. *)

exception Malformed of string

(** read_headers: reads LF/CRLF-terminated lines until it gets an empty line *)
let read_headers (reader : Buffered_line_reader.t) : string list =
  let rec read_internal acc =
    try
      match Buffered_line_reader.get_next_line reader with
      | "" -> acc
      | line -> read_internal (line :: acc)
    with Unix.Unix_error _ -> raise (Malformed "Can't read next header")
  in
  List.rev (read_internal [])

(** parse_headers_to_lowercase_map: turns list of "Key: Value" string headers
 * into a map, with keys normalized to lower-case. HTTP actually allows
 * multiple headers of the same key, but we prefer the simplicity of
 * returning just a string map so we only take the last header for
 * a given key. Note: if any header isn't in Key:Value format, we ignore it. *)
let parse_headers_to_lowercase_map (headers : string list) : string SMap.t =
  let rec parse_internal acc = function
    | [] -> acc
    | line :: rest ->
      begin
        match Str.bounded_split (Str.regexp ":") line 2 with
        | [k; v] ->
          let (k', v') = (String.lowercase_ascii k, String.trim v) in
          parse_internal (SMap.add k' v' acc) rest
        | _ -> parse_internal acc rest
      end
  in
  parse_internal SMap.empty headers

(** parse_charset: given a Content-Type value like "mime/type; charset=foo"
 * it returns the "foo" bit of it, if present.
 * https://www.w3.org/Protocols/rfc1341/4_Content-Type.html
 * Note: RFC822 allows comments in this string, but we don't handle them.
 *)
let parse_charset (header_value : string) : string option =
  (* charset_value: if given a param string "charset=b" then it returns Some b *)
  let charset_value param =
    match Str.bounded_split (Str.regexp "=") param 2 with
    | [k; v] when String.trim k = "charset" -> Some (String.trim v)
    | _ -> None
  in
  match Str.split (Str.regexp ";") header_value with
  | _mime_type :: params -> List.find_map params ~f:charset_value
  | _ -> None

(** read_message_utf8: reads an http-style message "Headers...Body"
 * The headers must include at least Content-Length (to know how long is
 * the body). If they also include Content-Type, then the charset must be utf-8
 * or absent. Errors in these respects produce a Malformed exception.
 * The content of all other headers are ignored.
 * This function returns an OCaml string, which is a sequence of 8bit bytes,
 * so it's up to the caller to handle any unicode characters and their
 * encoding. *)
let read_message_utf8 (reader : Buffered_line_reader.t) : string =
  let headers = read_headers reader |> parse_headers_to_lowercase_map in
  let len =
    try SMap.find "content-length" headers |> int_of_string
    with _ -> raise (Malformed "Missing Content-Length")
  in
  let charset = (try SMap.find "content-type" headers |> parse_charset with _ -> None) in
  let body = Buffered_line_reader.get_next_bytes reader len in
  if charset <> Some "utf-8" && charset <> None then raise (Malformed "Charset not utf-8");
  body

(** write_message: writes "Content-Length:...body" *)
let write_message (outchan : out_channel) (body : string) : unit =
  (* Without this, Windows will change the \r\n to \r\r\n *)
  Pervasives.set_binary_mode_out outchan true;

  Printf.fprintf outchan "Content-Length: %n\r\n" (String.length body);
  Printf.fprintf outchan "\r\n";
  Printf.fprintf outchan "%s" body;
  flush outchan
