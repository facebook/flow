/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use serde::Serialize;
use serde::de::DeserializeOwned;

use crate::saved_state::InvalidReason;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
pub struct Compressed {
    pub compressed_data: Vec<u8>,
    pub compressed_size: usize,
    pub uncompressed_size: usize,
}

pub fn marshal_and_compress<T: Serialize>(data: &T) -> Result<Compressed, InvalidReason> {
    let serialized = bincode::serde::encode_to_vec(data, bincode::config::legacy())
        .map_err(|err| InvalidReason::Failed_to_marshal(err.to_string()))?;
    let uncompressed_size = serialized.len();
    let compressed_data = lz4_flex::compress_prepend_size(&serialized);
    let compressed_size = compressed_data.len();
    Ok(Compressed {
        compressed_data,
        compressed_size,
        uncompressed_size,
    })
}

pub fn decompress_and_unmarshal<T: DeserializeOwned>(
    compressed: &Compressed,
) -> Result<T, InvalidReason> {
    let decompressed = lz4_flex::decompress_size_prepended(&compressed.compressed_data)
        .map_err(|err| InvalidReason::Failed_to_decompress(err.to_string()))?;
    bincode::serde::decode_from_slice(&decompressed, bincode::config::legacy())
        .map(|(v, _)| v)
        .map_err(|err| InvalidReason::Failed_to_marshal(err.to_string()))
}

pub fn compressed_size(compressed: &Compressed) -> usize {
    compressed.compressed_size
}

pub fn uncompressed_size(compressed: &Compressed) -> usize {
    compressed.uncompressed_size
}
