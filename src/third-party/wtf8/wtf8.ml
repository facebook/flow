(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*
 * WTF-8 is a superset of UTF-8 that allows unpaired surrogates.
 *
 * From ES6 6.1.4, "The String Type":
 *
 *   Where ECMAScript operations interpret String values, each element is
 *   interpreted as a single UTF-16 code unit. However, ECMAScript does not
 *   place any restrictions or requirements on the sequence of code units in
 *   a String value, so they may be ill-formed when interpreted as UTF-16 code
 *   unit sequences. Operations that do not interpret String contents treat
 *   them as sequences of undifferentiated 16-bit unsigned integers.
 *
 * If we try to encode these ill-formed code units into UTF-8, we similarly
 * get ill-formed UTF-8. WTF-8 is a fun name for that encoding.
 *
 * https://simonsapin.github.io/wtf-8/
 *)

type codepoint =
  | Point of int
  | Malformed

type 'a folder = 'a -> int -> codepoint -> 'a

(* WTF-8 is a variable length encoding. The first byte in each codepoint
   determines how many other bytes follow. *)
let needed_bytes c =
  if 0x00 <= c && c <= 0x7F then 1 else
  if 0xC2 <= c && c <= 0xDF then 2 else
  if 0xE0 <= c && c <= 0xEF then 3 else
  if 0xF0 <= c && c <= 0xF4 then 4 else
  0

let codepoint s i = function
  | 1 -> Char.code s.[i]
  | 2 ->
    let b0 = Char.code s.[i] in
    let b1 = Char.code s.[i + 1] in
    ((b0 land 0x1F) lsl 6) lor (b1 land 0x3F)
  | 3 ->
    let b0 = Char.code s.[i] in
    let b1 = Char.code s.[i + 1] in
    let b2 = Char.code s.[i + 2] in
    ((b0 land 0x0F) lsl 12) lor
    ((b1 land 0x3F) lsl 6) lor
    (b2 land 0x3F)
  | 4 ->
    let b0 = Char.code s.[i] in
    let b1 = Char.code s.[i + 1] in
    let b2 = Char.code s.[i + 2] in
    let b3 = Char.code s.[i + 3] in
    ((b0 land 0x07) lsl 18) lor
    ((b1 land 0x3F) lsl 12) lor
    ((b2 land 0x3F) lsl 6) lor
    (b3 land 0x3F)
  | _ -> assert false

(* Fold over the WTF-8 code units in a string *)
let fold_wtf_8 ?(pos = 0) ?len f acc s =
  let rec loop acc f s i l =
    if i = l then acc else
    let need = needed_bytes (Char.code s.[i]) in
    if need = 0 then (loop [@tailcall]) (f acc i Malformed) f s (i + 1) l else
    let rem = l - i in
    if rem < need then f acc i Malformed else
    (loop [@tailcall]) (f acc i (Point (codepoint s i need))) f s (i + need) l
  in
  let len = match len with
  | None -> String.length s - pos
  | Some l -> l
  in
  loop acc f (Bytes.unsafe_of_string s) pos len

(* Add a UTF-16 code unit to a buffer, encoded in WTF-8. *)
let add_wtf_8 buf code =
  let w byte = Buffer.add_char buf (Char.unsafe_chr byte) [@@inline] in
  if code >= 0x10000 then begin
  (* 4 bytes *)
    w (0xf0 lor (code lsr 18));
    w (0x80 lor ((code lsr 12) land 0x3F));
    w (0x80 lor ((code lsr 6) land 0x3F));
    w (0x80 lor (code land 0x3F))
  end else if code >= 0x800 then begin
  (* 3 bytes *)
    w (0xe0 lor (code lsr 12));
    w (0x80 lor ((code lsr 6) land 0x3F));
    w (0x80 lor (code land 0x3F))
  end else if code >= 0x80 then begin
  (* 2 bytes *)
    w (0xc0 lor (code lsr 6));
    w (0x80 lor (code land 0x3F))
  end else
  (* 1 byte *)
    w code
