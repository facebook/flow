(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ambiguity_strategy =
  | Fail
  | Temporary
  | Generalize
  | Specialize
  | Fixme
  | Suppress

let ambiguity_strategies =
  [
    ("fail", Fail);
    ("temporary", Temporary);
    ("generalize", Generalize);
    ("specialize", Specialize);
    ("fixme", Fixme);
    ("suppress", Suppress);
  ]
