(*
 * Copyright (c) 2006-2009 Citrix Systems Inc.
 * Copyright (c) 2010 Thomas Gazagnaire <thomas@gazagnaire.com>
 * Copyright (c) 2014-2016 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2016 David Kaloper Meršinjak
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

type alphabet = { emap : int array; dmap : int array }

type sub = string * int * int

let ( // ) x y =
  if y < 1 then raise Division_by_zero ;
  if x > 0 then 1 + ((x - 1) / y) else 0
  [@@inline]

let unsafe_get_uint8 t off = Char.code (String.unsafe_get t off)

let unsafe_set_uint8 t off v = Bytes.unsafe_set t off (Char.chr v)

let unsafe_set_uint16 = Unsafe.unsafe_set_uint16

external unsafe_get_uint16 : string -> int -> int = "%caml_string_get16u"
  [@@noalloc]

external swap16 : int -> int = "%bswap16" [@@noalloc]

let none = -1

(* We mostly want to have an optional array for [dmap] (e.g. [int option
   array]). So we consider the [none] value as [-1]. *)

let make_alphabet alphabet =
  if String.length alphabet <> 64
  then invalid_arg "Length of alphabet must be 64" ;
  if String.contains alphabet '='
  then invalid_arg "Alphabet can not contain padding character" ;
  let emap =
    Array.init (String.length alphabet) (fun i -> Char.code alphabet.[i]) in
  let dmap = Array.make 256 none in
  String.iteri (fun idx chr -> dmap.(Char.code chr) <- idx) alphabet ;
  { emap; dmap }

let length_alphabet { emap; _ } = Array.length emap

let alphabet { emap; _ } =
  String.init (Array.length emap) (fun i -> Char.chr emap.(i))

let default_alphabet =
  make_alphabet
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let uri_safe_alphabet =
  make_alphabet
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

let unsafe_set_be_uint16 =
  if Sys.big_endian
  then fun t off v -> unsafe_set_uint16 t off v
  else fun t off v -> unsafe_set_uint16 t off (swap16 v)

(* We make this exception to ensure to keep a control about which exception we
   can raise and avoid appearance of unknown exceptions like an ex-nihilo
   magic rabbit (or magic money?). *)
exception Out_of_bounds

exception Too_much_input

let get_uint8 t off =
  if off < 0 || off >= String.length t then raise Out_of_bounds ;
  unsafe_get_uint8 t off

let padding = int_of_char '='

let error_msgf fmt = Format.ksprintf (fun err -> Error (`Msg err)) fmt

let encode_sub pad { emap; _ } ?(off = 0) ?len input =
  let len =
    match len with Some len -> len | None -> String.length input - off in

  if len < 0 || off < 0 || off > String.length input - len
  then error_msgf "Invalid bounds"
  else
    let n = len in
    let n' = n // 3 * 4 in
    let res = Bytes.create n' in

    let emap i = Array.unsafe_get emap i in

    let emit b1 b2 b3 i =
      unsafe_set_be_uint16 res i
        ((emap ((b1 lsr 2) land 0x3f) lsl 8)
        lor emap ((b1 lsl 4) lor (b2 lsr 4) land 0x3f)) ;
      unsafe_set_be_uint16 res (i + 2)
        ((emap ((b2 lsl 2) lor (b3 lsr 6) land 0x3f) lsl 8)
        lor emap (b3 land 0x3f)) in

    let rec enc j i =
      if i = n
      then ()
      else if i = n - 1
      then emit (unsafe_get_uint8 input (off + i)) 0 0 j
      else if i = n - 2
      then
        emit
          (unsafe_get_uint8 input (off + i))
          (unsafe_get_uint8 input (off + i + 1))
          0 j
      else (
        emit
          (unsafe_get_uint8 input (off + i))
          (unsafe_get_uint8 input (off + i + 1))
          (unsafe_get_uint8 input (off + i + 2))
          j ;
        enc (j + 4) (i + 3)) in

    let rec unsafe_fix = function
      | 0 -> ()
      | i ->
          unsafe_set_uint8 res (n' - i) padding ;
          unsafe_fix (i - 1) in

    enc 0 0 ;

    let pad_to_write = (3 - (n mod 3)) mod 3 in

    if pad
    then (
      unsafe_fix pad_to_write ;
      Ok (Bytes.unsafe_to_string res, 0, n'))
    else Ok (Bytes.unsafe_to_string res, 0, n' - pad_to_write)

(* [pad = false], we don't want to write them. *)

let encode ?(pad = true) ?(alphabet = default_alphabet) ?off ?len input =
  match encode_sub pad alphabet ?off ?len input with
  | Ok (res, off, len) -> Ok (String.sub res off len)
  | Error _ as err -> err

let encode_string ?pad ?alphabet input =
  match encode ?pad ?alphabet input with
  | Ok res -> res
  | Error _ -> assert false

let encode_sub ?(pad = true) ?(alphabet = default_alphabet) ?off ?len input =
  encode_sub pad alphabet ?off ?len input

let encode_exn ?pad ?alphabet ?off ?len input =
  match encode ?pad ?alphabet ?off ?len input with
  | Ok v -> v
  | Error (`Msg err) -> invalid_arg err

let decode_sub ?(pad = true) { dmap; _ } ?(off = 0) ?len input =
  let len =
    match len with Some len -> len | None -> String.length input - off in

  if len < 0 || off < 0 || off > String.length input - len
  then error_msgf "Invalid bounds"
  else
    let n = len // 4 * 4 in
    let n' = n // 4 * 3 in
    let res = Bytes.create n' in
    let invalid_pad_overflow = pad in

    let get_uint8_or_padding =
      if pad
      then (fun t i ->
        if i >= len then raise Out_of_bounds ;
        get_uint8 t (off + i))
      else
        fun t i ->
        try if i < len then get_uint8 t (off + i) else padding
        with Out_of_bounds -> padding in

    let set_be_uint16 t off v =
      (* can not write 2 bytes. *)
      if off < 0 || off + 1 > Bytes.length t
      then () (* can not write 1 byte but can write 1 byte *)
      else if off < 0 || off + 2 > Bytes.length t
      then unsafe_set_uint8 t off (v lsr 8) (* can write 2 bytes. *)
      else unsafe_set_be_uint16 t off v in

    let set_uint8 t off v =
      if off < 0 || off >= Bytes.length t then () else unsafe_set_uint8 t off v
    in

    let emit a b c d j =
      let x = (a lsl 18) lor (b lsl 12) lor (c lsl 6) lor d in
      set_be_uint16 res j (x lsr 8) ;
      set_uint8 res (j + 2) (x land 0xff) in

    let dmap i =
      let x = Array.unsafe_get dmap i in
      if x = none then raise Not_found ;
      x in

    let only_padding pad idx =
      (* because we round length of [res] to the upper bound of how many
         characters we should have from [input], we got at this stage only padding
         characters and we need to delete them, so for each [====], we delete 3
         bytes. *)
      let pad = ref (pad + 3) in
      let idx = ref idx in

      while !idx + 4 < len do
        (* use [unsafe_get_uint16] instead [unsafe_get_uint32] to avoid allocation
           of [int32]. Of course, [3d3d3d3d] is [====]. *)
        if unsafe_get_uint16 input (off + !idx) <> 0x3d3d
           || unsafe_get_uint16 input (off + !idx + 2) <> 0x3d3d
        then raise Not_found ;

        (* We got something bad, should be a valid character according to
           [alphabet] but outside the scope. *)
        idx := !idx + 4 ;
        pad := !pad + 3
      done ;
      while !idx < len do
        if unsafe_get_uint8 input (off + !idx) <> padding then raise Not_found ;

        incr idx
      done ;
      !pad in

    let rec dec j i =
      if i = n
      then 0
      else
        let d, pad =
          let x = get_uint8_or_padding input (i + 3) in
          try (dmap x, 0) with Not_found when x = padding -> (0, 1) in
        (* [Not_found] iff [x ∉ alphabet and x <> '='] can leak. *)
        let c, pad =
          let x = get_uint8_or_padding input (i + 2) in
          try (dmap x, pad)
          with Not_found when x = padding && pad = 1 -> (0, 2) in
        (* [Not_found] iff [x ∉ alphabet and x <> '='] can leak. *)
        let b, pad =
          let x = get_uint8_or_padding input (i + 1) in
          try (dmap x, pad)
          with Not_found when x = padding && pad = 2 -> (0, 3) in
        (* [Not_found] iff [x ∉ alphabet and x <> '='] can leak. *)
        let a, pad =
          let x = get_uint8_or_padding input i in
          try (dmap x, pad)
          with Not_found when x = padding && pad = 3 -> (0, 4) in

        (* [Not_found] iff [x ∉ alphabet and x <> '='] can leak. *)
        emit a b c d j ;

        if i + 4 = n (* end of input in anyway *)
        then
          match pad with
          | 0 -> 0
          | 4 ->
              (* assert (invalid_pad_overflow = false) ; *)
              3
          (* [get_uint8] lies and if we get [4], that mean we got one or more (at
             most 4) padding character. In this situation, because we round length
             of [res] (see [n // 4]), we need to delete 3 bytes. *)
          | pad -> pad
        else
          match pad with
          | 0 -> dec (j + 3) (i + 4)
          | 4 ->
              (* assert (invalid_pad_overflow = false) ; *)
              only_padding 3 (i + 4)
          (* Same situation than above but we should get only more padding
             characters then. *)
          | pad ->
              if invalid_pad_overflow = true then raise Too_much_input ;
              only_padding pad (i + 4) in

    match dec 0 0 with
    | 0 -> Ok (Bytes.unsafe_to_string res, 0, n')
    | pad -> Ok (Bytes.unsafe_to_string res, 0, n' - pad)
    | exception Out_of_bounds ->
        error_msgf "Wrong padding"
        (* appear only when [pad = true] and when length of input is not a multiple of 4. *)
    | exception Not_found ->
        (* appear when one character of [input] ∉ [alphabet] and this character <> '=' *)
        error_msgf "Malformed input"
    | exception Too_much_input -> error_msgf "Too much input"

let decode ?pad ?(alphabet = default_alphabet) ?off ?len input =
  match decode_sub ?pad alphabet ?off ?len input with
  | Ok (res, off, len) -> Ok (String.sub res off len)
  | Error _ as err -> err

let decode_sub ?pad ?(alphabet = default_alphabet) ?off ?len input =
  decode_sub ?pad alphabet ?off ?len input

let decode_exn ?pad ?alphabet ?off ?len input =
  match decode ?pad ?alphabet ?off ?len input with
  | Ok res -> res
  | Error (`Msg err) -> invalid_arg err
