(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module defines a binary encoding for type signatures which we use to
 * store signature data in shared memory.
 *
 * The encoding is designed to support granular reads -- consumers can navigate
 * to parts of the signature and read only those parts. This is implemented by
 * storing "index" tables at well-known byte offsets. The index contains offsets
 * to other data.
 *
 * For example, the first 4 bytes of the buffer contains the offset of module
 * exports. The first 4 bytes at the module exports offset contains the offset
 * of the type exports, and so on.
 *
 * Data in this buffer is 4 byte aligned. Byte offsets are stored as 4 byte
 * integers in system byte order.
 *
 * We employ a few tricks to reduce size:
 * - Offsets to empty opt values are stored as 0 instead of an offset to 0
 * - Offsets to empty tbl values are stored as 0 instead of an offset to 0
 * - Offsets to variants are themselves tagged instead of pointing to a tag
 *
 * The above tricks are possible because offsets are typed (we know when reading
 * an offset what we expect it to point to) and because opt/tbl values can not
 * possibly be at offset 0 (the main sig index is there).
 *)

open Type_sig_collections

type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type 'k pos = int

type 'k tbl

type 'k opt

type 'k hashed

type 'k serialized

type str

type dyn_module

type packed

type local_def

type remote_ref

type pattern

type type_export

type cjs_module_info

type cjs_module

type es_export

type es_module_info

type es_module

external ( .!() ) : 'a array -> int -> 'a = "%array_unsafe_get"

external buf_set32 : buf -> int -> int32 -> unit = "%caml_bigstring_set32"

external buf_set64 : buf -> int -> int64 -> unit = "%caml_bigstring_set64"

external buf_write_string : buf -> int -> int -> string -> unit = "type_sig_bin_write_string"

external buf_get32 : buf -> int -> int32 = "%caml_bigstring_get32"

external buf_get64 : buf -> int -> int64 = "%caml_bigstring_get64"

external buf_read_string : buf -> int -> int -> string = "type_sig_bin_read_string"

external buf_read_serialized : buf -> int -> 'a = "type_sig_bin_read_serialized"

external hash_serialized : buf -> int -> int64 = "type_sig_bin_hash_serialized"

let serialize x = Marshal.to_string x []

(* The heap is 4-byte aligned. In order to store data like strings which have
 * byte-based lengths, we pad to the nearest aligned offset. *)
let aligned size = (size + 3) land lnot 3

(* We use Xx to generate an 8 byte hash. *)
let hash_size = 8

(* We store tables of offsets in the binary representation to support
 * navigation. These offsets are 4 bytes, more than enough. *)
let offset_size = 4

(* Strings and tables are prefixed by their length, which is also stored as a
 * fixed 4 byte integer. *)
let len_size = 4

let str_size x =
  let len = String.length x in
  len_size + aligned len

let serialized_size x =
  let len = String.length x in
  aligned len

let opt_size f = function
  | None -> 0
  | Some x -> f x

let hashed_size f x = hash_size + f x

let tbl_size f xs =
  let len = Array.length xs in
  if len = 0 then
    0
  else
    let size = len_size + (len * offset_size) in
    Array.fold_left (fun acc x -> acc + f x) size xs

(* Strings are prefixed with their byte length. *)
let write_str buf pos_ref x =
  let pos = !pos_ref in
  let len = String.length x in
  buf_set32 buf pos (Int32.of_int len);
  buf_write_string buf (pos + 4) len x;
  pos_ref := pos + 4 + aligned len;
  pos

(* Serialized values do not need a length prefix because the encoding starts
 * with a header containing that information already. *)
let write_serialized buf pos_ref x =
  let pos = !pos_ref in
  let len = String.length x in
  buf_write_string buf pos len x;
  pos_ref := pos + aligned len;
  pos

(* Tables are prefixed with the length, followed by offsets for each entry,
 * followed by the values in order. Note that nothing is written for empty
 * arrays, which are represented via the 0 position value. *)
let write_tbl f buf pos_ref xs =
  let len = Array.length xs in
  if len = 0 then
    0
  else
    let pos = !pos_ref in
    buf_set32 buf pos (Int32.of_int len);
    let ipos = ref (pos + 4) in
    pos_ref := !ipos + (4 * len);
    for i = 0 to len - 1 do
      let dpos = f buf pos_ref xs.!(i) in
      buf_set32 buf !ipos (Int32.of_int dpos);
      ipos := !ipos + 4
    done;
    pos

(* Optional values are written out directly when present. Nothing is written for
 * missing values, which are represented via the 0 position value. *)
let write_opt f buf pos_ref = function
  | None -> 0
  | Some x -> f buf pos_ref x

let write_hash = buf_set64

(* Hashed values are prefixed with an 8 byte hash, initially 0. *)
let write_hashed f buf pos_ref x =
  let pos = !pos_ref in
  write_hash buf pos 0L;
  pos_ref := pos + 8;
  ignore (f buf pos_ref x : int);
  pos

(* Modules are a variant, but note that the tag is encoded in the returned
 * position itself, not in the data, saving 4 bytes per module. *)
let write_module = function
  | Type_sig_pack.CJSModule { type_exports; exports; info } ->
    let type_exports = Array.map serialize type_exports in
    let exports = Option.map serialize exports in
    let info = serialize info in
    let index_size = 3 * offset_size in
    let size =
      index_size
      + tbl_size (hashed_size serialized_size) type_exports
      + opt_size (hashed_size serialized_size) exports
      + hashed_size serialized_size info
    in
    let write buf pos_ref =
      let pos = !pos_ref in
      pos_ref := pos + index_size;
      let type_exports_pos = write_tbl (write_hashed write_serialized) buf pos_ref type_exports in
      let exports_pos = write_opt (write_hashed write_serialized) buf pos_ref exports in
      let info_pos = write_hashed write_serialized buf pos_ref info in
      buf_set32 buf pos (Int32.of_int type_exports_pos);
      buf_set32 buf (pos + 4) (Int32.of_int exports_pos);
      buf_set32 buf (pos + 8) (Int32.of_int info_pos);
      pos
    in
    (size, write)
  | Type_sig_pack.ESModule { type_exports; exports; info } ->
    let type_exports = Array.map serialize type_exports in
    let exports = Array.map serialize exports in
    let info = serialize info in
    let index_size = 3 * offset_size in
    let size =
      index_size
      + tbl_size (hashed_size serialized_size) type_exports
      + tbl_size (hashed_size serialized_size) exports
      + hashed_size serialized_size info
    in
    let write buf pos_ref =
      let pos = !pos_ref in
      pos_ref := pos + index_size;
      let type_exports_pos = write_tbl (write_hashed write_serialized) buf pos_ref type_exports in
      let exports_pos = write_tbl (write_hashed write_serialized) buf pos_ref exports in
      let info_pos = write_hashed write_serialized buf pos_ref info in
      buf_set32 buf pos (Int32.of_int type_exports_pos);
      buf_set32 buf (pos + 4) (Int32.of_int exports_pos);
      buf_set32 buf (pos + 8) (Int32.of_int info_pos);
      pos lor 1
    in
    (size, write)

let write type_sig =
  let {
    Packed_type_sig.Module.module_kind;
    module_refs;
    local_defs;
    remote_refs;
    pattern_defs;
    patterns;
  } =
    type_sig
  in
  let (module_size, write_module) = write_module module_kind in
  let module_refs = Module_refs.to_array module_refs in
  let local_defs = Local_defs.to_array_map serialize local_defs in
  let remote_refs = Remote_refs.to_array_map serialize remote_refs in
  let pattern_defs = Pattern_defs.to_array_map serialize pattern_defs in
  let patterns = Patterns.to_array_map serialize patterns in
  let index_size = 6 * offset_size in
  let size =
    index_size
    + module_size
    + tbl_size str_size module_refs
    + tbl_size serialized_size local_defs
    + tbl_size serialized_size remote_refs
    + tbl_size serialized_size pattern_defs
    + tbl_size serialized_size patterns
  in
  let write buf =
    let pos_ref = ref index_size in
    let module_pos = write_module buf pos_ref in
    let module_refs_pos = write_tbl write_str buf pos_ref module_refs in
    let local_defs_pos = write_tbl write_serialized buf pos_ref local_defs in
    let remote_refs_pos = write_tbl write_serialized buf pos_ref remote_refs in
    let pattern_defs_pos = write_tbl write_serialized buf pos_ref pattern_defs in
    let patterns_pos = write_tbl write_serialized buf pos_ref patterns in
    buf_set32 buf 0 (Int32.of_int module_pos);
    buf_set32 buf 4 (Int32.of_int module_refs_pos);
    buf_set32 buf 8 (Int32.of_int local_defs_pos);
    buf_set32 buf 12 (Int32.of_int remote_refs_pos);
    buf_set32 buf 16 (Int32.of_int pattern_defs_pos);
    buf_set32 buf 20 (Int32.of_int patterns_pos);
    assert (!pos_ref = size)
  in
  (size, write)

let module_kind buf = Int32.to_int (buf_get32 buf 0)

let module_refs buf = Int32.to_int (buf_get32 buf 4)

let local_defs buf = Int32.to_int (buf_get32 buf 8)

let remote_refs buf = Int32.to_int (buf_get32 buf 12)

let pattern_defs buf = Int32.to_int (buf_get32 buf 16)

let patterns buf = Int32.to_int (buf_get32 buf 20)

let cjs_module_type_exports buf pos = Int32.to_int (buf_get32 buf pos)

let cjs_module_exports buf pos = Int32.to_int (buf_get32 buf (pos + 4))

let cjs_module_info buf pos = Int32.to_int (buf_get32 buf (pos + 8))

let es_module_type_exports buf pos = Int32.to_int (buf_get32 buf pos)

let es_module_exports buf pos = Int32.to_int (buf_get32 buf (pos + 4))

let es_module_info buf pos = Int32.to_int (buf_get32 buf (pos + 8))

let read_str buf pos =
  let len = Int32.to_int (buf_get32 buf pos) in
  buf_read_string buf (pos + 4) len

let read_tbl_generic f buf pos init =
  if pos = 0 then
    init 0 Obj.magic
  else
    let len = Int32.to_int (buf_get32 buf pos) in
    init len (fun i ->
        let ipos = pos + (4 * (i + 1)) in
        let dpos = Int32.to_int (buf_get32 buf ipos) in
        f buf dpos
    )

let read_tbl f buf pos = read_tbl_generic f buf pos Array.init

let read_opt f buf pos =
  if pos = 0 then
    None
  else
    Some (f buf pos)

let read_hashed f buf pos = f buf (pos + 8)

let read_hash = buf_get64

let read_type_export : buf -> int -> Locs.index Type_sig_pack.type_export = buf_read_serialized

let read_packed : buf -> int -> Locs.index Type_sig_pack.packed = buf_read_serialized

let read_cjs_info : buf -> int -> Locs.index Type_sig_pack.cjs_module_info = buf_read_serialized

let read_es_export : buf -> int -> Locs.index Type_sig_pack.export = buf_read_serialized

let read_es_info : buf -> int -> Locs.index Type_sig_pack.es_module_info = buf_read_serialized

let read_local_def : buf -> int -> Locs.index Type_sig_pack.packed_def = buf_read_serialized

let read_remote_ref : buf -> int -> Locs.index Type_sig_pack.remote_ref = buf_read_serialized

let read_pattern : buf -> int -> Locs.index Type_sig_pack.pattern = buf_read_serialized

let read_cjs_module buf pos =
  let type_exports_pos = cjs_module_type_exports buf pos in
  let exports_pos = cjs_module_exports buf pos in
  let info_pos = cjs_module_info buf pos in
  let type_exports = read_tbl (read_hashed read_type_export) buf type_exports_pos in
  let exports = read_opt (read_hashed read_packed) buf exports_pos in
  let info = read_hashed read_cjs_info buf info_pos in
  Type_sig_pack.CJSModule { type_exports; exports; info }

let read_es_module buf pos =
  let type_exports_pos = es_module_type_exports buf pos in
  let exports_pos = es_module_exports buf pos in
  let info_pos = es_module_info buf pos in
  let type_exports = read_tbl (read_hashed read_type_export) buf type_exports_pos in
  let exports = read_tbl (read_hashed read_es_export) buf exports_pos in
  let info = read_hashed read_es_info buf info_pos in
  Type_sig_pack.ESModule { type_exports; exports; info }

let read_module_kind read_cjs_module read_es_module buf pos =
  let tag = pos land 1 in
  let pos' = pos land lnot 1 in
  if tag = 0 then
    read_cjs_module buf pos'
  else
    read_es_module buf pos'

let read buf =
  let module_kind_pos = module_kind buf in
  let module_refs_pos = module_refs buf in
  let local_defs_pos = local_defs buf in
  let remote_refs_pos = remote_refs buf in
  let pattern_defs_pos = pattern_defs buf in
  let patterns_pos = patterns buf in
  let module_kind = read_module_kind read_cjs_module read_es_module buf module_kind_pos in
  let module_refs = read_tbl_generic read_str buf module_refs_pos Module_refs.init in
  let local_defs = read_tbl_generic read_local_def buf local_defs_pos Local_defs.init in
  let remote_refs = read_tbl_generic read_remote_ref buf remote_refs_pos Remote_refs.init in
  let pattern_defs = read_tbl_generic read_packed buf pattern_defs_pos Pattern_defs.init in
  let patterns = read_tbl_generic read_pattern buf patterns_pos Patterns.init in
  {
    Packed_type_sig.Module.module_kind;
    module_refs;
    local_defs;
    remote_refs;
    pattern_defs;
    patterns;
  }
