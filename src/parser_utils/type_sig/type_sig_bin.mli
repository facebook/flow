(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module provides an API for reading/writing signature data which is
 * stored in a compact binary encoding.
 *
 * The data itself is represented as a bigarray, in practice representing a
 * slice of the shared heap where signatures are stored. That is, the signature
 * data itself is, in practice, not in the OCaml heap.
 *
 * Readers do not need to read the entire signature at once. Instead, readers
 * can find the "position" of the data they are interested in and only read that
 * part.
 *
 * These navigation APIs are type-safe through the use of `'k pos` type where
 * `'k` is a phantom type parameter.
 *)

open Type_sig_collections

type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type 'k pos [@@immediate]

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

val write : Locs.index Packed_type_sig.Module.t -> int * (buf -> unit)

val read : buf -> Locs.index Packed_type_sig.Module.t

val module_kind : buf -> dyn_module pos

val module_refs : buf -> str tbl pos

val local_defs : buf -> local_def serialized tbl pos

val remote_refs : buf -> remote_ref serialized tbl pos

val pattern_defs : buf -> packed serialized tbl pos

val patterns : buf -> pattern serialized tbl pos

val cjs_module_exports : buf -> cjs_module pos -> packed serialized hashed opt pos

val cjs_module_type_exports : buf -> cjs_module pos -> type_export serialized hashed tbl pos

val cjs_module_info : buf -> cjs_module pos -> cjs_module_info serialized hashed pos

val es_module_exports : buf -> es_module pos -> es_export serialized hashed tbl pos

val es_module_type_exports : buf -> es_module pos -> type_export serialized hashed tbl pos

val es_module_info : buf -> es_module pos -> es_module_info serialized hashed pos

val read_str : buf -> str pos -> string

val read_tbl_generic :
  (buf -> 'k pos -> 'a) -> buf -> 'k tbl pos -> (int -> (int -> 'a) -> 'b) -> 'b

val read_tbl : (buf -> 'k pos -> 'a) -> buf -> 'k tbl pos -> 'a array

val iter_tbl : (buf -> 'k pos -> 'a) -> ('a -> unit) -> buf -> 'k tbl pos -> unit

val fold_tbl : (buf -> 'k pos -> 'a) -> ('a -> 'b -> 'b) -> buf -> 'k tbl pos -> 'b -> 'b

val read_opt : (buf -> 'k pos -> 'a) -> buf -> 'k opt pos -> 'a option

val read_hashed : (buf -> 'k pos -> 'a) -> buf -> 'k hashed pos -> 'a

val read_hash : buf -> _ hashed pos -> int64

val read_type_export : buf -> type_export serialized pos -> Locs.index Type_sig_pack.type_export

val read_packed : buf -> packed serialized pos -> Locs.index Type_sig_pack.packed

val read_cjs_info :
  buf -> cjs_module_info serialized pos -> Locs.index Type_sig_pack.cjs_module_info

val read_es_export : buf -> es_export serialized pos -> Locs.index Type_sig_pack.export

val read_es_info : buf -> es_module_info serialized pos -> Locs.index Type_sig_pack.es_module_info

val read_local_def : buf -> local_def serialized pos -> Locs.index Type_sig_pack.packed_def

val read_remote_ref : buf -> remote_ref serialized pos -> Locs.index Type_sig_pack.remote_ref

val read_pattern : buf -> pattern serialized pos -> Locs.index Type_sig_pack.pattern

val read_cjs_module : buf -> cjs_module pos -> Locs.index Type_sig_pack.module_kind

val read_es_module : buf -> es_module pos -> Locs.index Type_sig_pack.module_kind

val read_module_kind :
  (buf -> cjs_module pos -> 'a) -> (buf -> es_module pos -> 'a) -> buf -> dyn_module pos -> 'a

val hash_serialized : buf -> _ serialized pos -> int64

val write_hash : buf -> _ hashed pos -> int64 -> unit
