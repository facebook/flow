(*
 * Copyright (c) 2006-2009 Citrix Systems Inc.
 * Copyright (c) 2010 Thomas Gazagnaire <thomas@gazagnaire.com>
 * Copyright (c) 2014-2016 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2018 Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(** Base64 RFC4648 implementation.

    Base64 is a group of similar binary-to-text encoding schemes that represent
    binary data in an ASCII string format by translating it into a radix-64
    representation. It is specified in RFC 4648.

    {e Release %%VERSION%% - %%PKG_HOMEPAGE%%} *)

type alphabet
(** Type of alphabet. *)

type sub = string * int * int
(** Type of sub-string: [str, off, len]. *)

val default_alphabet : alphabet
(** A 64-character alphabet specifying the regular Base64 alphabet. *)

val uri_safe_alphabet : alphabet
(** A 64-character alphabet specifying the URI- and filename-safe Base64
    alphabet. *)

val make_alphabet : string -> alphabet
(** Make a new alphabet. *)

val length_alphabet : alphabet -> int
(** Returns length of the alphabet, should be 64. *)

val alphabet : alphabet -> string
(** Returns the alphabet. *)

val decode_exn :
  ?pad:bool -> ?alphabet:alphabet -> ?off:int -> ?len:int -> string -> string
(** [decode_exn ?off ?len s] decodes [len] bytes (defaults to
    [String.length s - off]) of the string [s] starting from [off] (defaults to
    [0]) that is encoded in Base64 format. Will leave trailing NULLs on the
    string, padding it out to a multiple of 3 characters. [alphabet] defaults to
    {!default_alphabet}. [pad = true] specifies to check if [s] is padded or
    not, otherwise, it raises an exception.

    Decoder can fail when character of [s] is not a part of [alphabet] or is not
    [padding] character. If input is not padded correctly, decoder does the
    best-effort but it does not ensure [decode_exn (encode ~pad:false x) = x].

    @raise if Invalid_argument [s] is not a valid Base64 string. *)

val decode_sub :
  ?pad:bool ->
  ?alphabet:alphabet ->
  ?off:int ->
  ?len:int ->
  string ->
  (sub, [> `Msg of string ]) result
(** Same as {!decode_exn} but it returns a result type instead to raise an
    exception. Then, it returns a {!sub} string. Decoded input [(str, off, len)]
    will starting to [off] and will have [len] bytes - by this way, we ensure to
    allocate only one time result. *)

val decode :
  ?pad:bool ->
  ?alphabet:alphabet ->
  ?off:int ->
  ?len:int ->
  string ->
  (string, [> `Msg of string ]) result
(** Same as {!decode_exn}, but returns an explicit error message {!result} if it
    fails. *)

val encode :
  ?pad:bool ->
  ?alphabet:alphabet ->
  ?off:int ->
  ?len:int ->
  string ->
  (string, [> `Msg of string ]) result
(** [encode s] encodes the string [s] into base64. If [pad] is false, no
    trailing padding is added. [pad] defaults to [true], and [alphabet] to
    {!default_alphabet}.

    [encode] fails when [off] and [len] do not designate a valid range of [s]. *)

val encode_string : ?pad:bool -> ?alphabet:alphabet -> string -> string
(** [encode_string s] encodes the string [s] into base64. If [pad] is false, no
    trailing padding is added. [pad] defaults to [true], and [alphabet] to
    {!default_alphabet}. *)

val encode_sub :
  ?pad:bool ->
  ?alphabet:alphabet ->
  ?off:int ->
  ?len:int ->
  string ->
  (sub, [> `Msg of string ]) result
(** Same as {!encode} but return a {!sub}-string instead a plain result. By this
    way, we ensure to allocate only one time result. *)

val encode_exn :
  ?pad:bool -> ?alphabet:alphabet -> ?off:int -> ?len:int -> string -> string
(** Same as {!encode} but raises an invalid argument exception if we retrieve an
    error. *)
