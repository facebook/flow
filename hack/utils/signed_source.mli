(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The signing token, which you must embed in the file you wish to sign.
    Generally, you should put this in a header comment. *)
val signing_token : string

(** Sign a source file into which you have previously embedded a signing token.
    Signing modifies only the signing token, so the semantics of the file will
    not change if the token is put in a comment.
    @raise {!Not_found} if no signing token is present. *)
val sign_file : string -> string

(** Determine if a file is signed or not. This does NOT verify the signature. *)
val is_signed : string -> bool

type sign_check_response =
  | Sign_ok
  | Sign_unsigned
  | Sign_invalid

(** Verify a file's signature. *)
val verify_signature : string -> sign_check_response
