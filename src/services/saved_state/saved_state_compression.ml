(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type compressed = {
  compressed_data: string;
  compressed_size: int;
  uncompressed_size: int;
}

external marshal_and_compress : 'a -> compressed = "marshal_and_compress_stub"

external decompress_and_unmarshal : compressed -> 'a = "decompress_and_unmarshal_stub"

let compressed_size { compressed_size; _ } = compressed_size

let uncompressed_size { uncompressed_size; _ } = uncompressed_size
