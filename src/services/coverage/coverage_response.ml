(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Expressions can be in one of four states:
     - Uncovered: This is an any or any-like type and is not covered by Flow.
     - Empty: This is an empty type, representing unreachable code, and is not covered by Flow.
     - Tainted: This is covered in that it has a static type, but trust analysis has indicated
                that this type may not accurately represent its type at runtime.
     - Untainted: This is covered, and its static type can be shown to accurately reflect
                its runtime type.
  *)
type expression_coverage =
  | Uncovered
  | Empty
  | Tainted
  | Untainted

type file_coverage = {
  untainted: int;
  tainted: int;
  uncovered: int;
  empty: int;
}
