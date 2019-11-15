(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module is documented in state_reader.ml *)

type t =
  | State_reader of State_reader.t
  | Mutator_state_reader of Mutator_state_reader.t
