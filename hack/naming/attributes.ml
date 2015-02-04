(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Ast
open Utils

module SN = Naming_special_names

let mem x xs =
  List.exists (fun { ua_name; _ } -> x = snd ua_name) xs

let find x xs =
  try Some (List.find (fun { ua_name; _ } -> x = snd ua_name) xs)
  with Not_found -> None

(* This evaluates single-quoted strings the same way PHP does: slashes and
 * single quotes need to be escaped, and all other characters are treated
 * literally -- i.e. '\n' is the literal slash + 'n' *)
let unescape_slashes s =
  let buf = Buffer.create (String.length s) in
  let handle_slash = function
    | '\\'
    | '\'' as c -> Buffer.add_char buf c
    | c ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c in
  let i = ref 0 in
  while !i < String.length s do
    begin match s.[!i] with
    | '\\' ->
        i := !i + 1;
        handle_slash s.[!i]
    | c ->
        Buffer.add_char buf c
    end;
    i := !i + 1
  done;
  Buffer.contents buf

let rec single_quoted_str_val = function
  | _, Binop (Dot, s1, s2) -> begin
      match single_quoted_str_val s1, single_quoted_str_val s2 with
      | Some s1, Some s2 -> Some (s1 ^ s2)
      | _ -> None
    end
  | _, String (_p, s) -> Some (unescape_slashes s)
  | p, _ ->
      Errors.attribute_param_type p "single-quoted string"; None

let deprecated ~kind (_, name) attrs =
  let attr = find SN.UserAttributes.uaDeprecated attrs in
  match attr with
  | Some { ua_name; ua_params = [msg] } -> begin
      (* we have no reason to support string interpolation, so only allow
       * single-quoted strings *)
      opt_map (fun s ->
        let name = strip_ns name in
        let deprecated_msg =
          Printf.sprintf "The %s %s is deprecated: " kind name in
        deprecated_msg ^ s)
        (single_quoted_str_val msg)
    end
  | Some { ua_name = (pos, _); _ }  ->
      Errors.attribute_arity pos SN.UserAttributes.uaDeprecated 1;
      None
  | None -> None
