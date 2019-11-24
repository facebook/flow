(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This file is based on fbsource/tools/signedsource.py *)

(* This old token was historically used as the signing token. It was replaced
   because it is 2 characters shorter than the final signature, and as a result,
   signing data with the old token forced the entire string to be rewritten
   (everything after the token needs to be shifted forwards 2 bytes).
   In this implementation, we rewrite the entire string anyway. *)
let old_token = "<<SignedSource::*O*zOeWoEQle#+L!plEphiEmie@I>>"

let token = "<<SignedSource::*O*zOeWoEQle#+L!plEphiEmie@IsG>>"

let make_signing_token = Printf.sprintf "@%s %s" "generated"

let signature_re = "SignedSource<<\\([a-f0-9]+\\)>>"

let sign_or_old_token = Str.regexp (signature_re ^ "|" ^ Str.quote old_token)

let signing_regexp = Str.regexp (make_signing_token signature_re)

let token_regexp = Str.regexp_string token

let hash data = Digest.to_hex (Digest.string data)

let signing_token = make_signing_token token

let sign_file data =
  let data = Str.global_replace sign_or_old_token token data in
  if not @@ String_utils.is_substring token data then raise Not_found;
  let signature = "SignedSource<<" ^ hash data ^ ">>" in
  Str.global_replace token_regexp signature data

let is_signed data =
  try
    let (_ : int) = Str.search_forward signing_regexp data 0 in
    true
  with Not_found -> false

type sign_check_response =
  | Sign_ok
  | Sign_unsigned
  | Sign_invalid

let verify_signature data =
  if not @@ is_signed data then
    Sign_unsigned
  else
    let expected_md5 = Str.matched_group 1 data in
    let valid =
      [token; old_token]
      |> List.exists @@ fun tok ->
         let replacement = make_signing_token tok in
         let unsigned_data =
           Str.global_replace signing_regexp replacement data
         in
         let actual_md5 = hash unsigned_data in
         expected_md5 = actual_md5
    in
    if valid then
      Sign_ok
    else
      Sign_invalid
