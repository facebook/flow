(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Trust

let infer_trust _cx = bogus_trust ()

let with_trust_inference cx constructor = infer_trust cx |> constructor
