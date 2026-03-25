/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// (* Stores all the necessary information about current persistent connections *)
// type t

// From lspProt.ml:
// type client_id = int
pub type ClientId = i32;

// From persistent_connection.ml:
// type t = Prot.client_id list
// let empty = []
#[derive(Debug, Clone)]
pub struct PersistentConnection(pub Vec<ClientId>);

impl PersistentConnection {
    // let empty = []
    pub fn empty() -> Self {
        Self(Vec::new())
    }
}
