(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type heap

type chunk

type size

type 'a addr

type 'a addr_map

let addr_map_size _ = failwith "unimplemented"

let write_addr_map _ _ _ = failwith "unimplemented"

let read_addr_map _ _ _ = failwith "unimplemented"
