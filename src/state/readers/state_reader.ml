(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* State_reader, Mutator_state_reader, and Abstract_state_reader are modules which basically do
 * nothing. All they do is leverage the OCaml type system in an attempt to force the Flow team to
 * read from the correct part of the shared memory.
 *
 * Init and rechecks should use the Mutator_state_reader that is created for them when the
 * transaction starts. This ensures that they are always reading from the new shared memory,
 * unless they explicitly try to read oldified data.
 *
 * Everything else should use State_reader. This reads from the new shared memory too. However,
 * if we're in the middle of a recheck and there is oldified data available for a file, it instead
 * uses that data. This allows code using a State_reader to run in parallel with a recheck.
 *
 * Some library functions can run inside a init/recheck or outside one. This code will take an
 * Abstract_state_reader, which allows us to share that code.
 *)

type t = unit

let create () = ()
