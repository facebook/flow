(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Base

(** The equality function in Pervasives is backed by compiler magic (called
    compare_val), which operates on the memory representation of values to
    perform a comparison between values of any type. This is convenient, but it
    has the wrong semantics for stdlib sets and maps (since the same value may
    be represented with trees of differing structure), will not compare on-heap
    values with off-heap values (unless the compiler was configured with the
    -no-naked-pointers option), and is slower than monomorphic comparison
    functions. As a result, we forbid the use of the = operator from Pervasives
    here.

    Instead of using polymorphic equality, use ppx_deriving to derive eq for
    your data type and use the derived `equal` function.

    If you are performing many comparisons of the same kind, consider binding
    the = operator locally with something like `let ( = ) = Int.equal`.

    If your module only compares values of a single builtin type (e.g., int or
    string), you can add `open Int.Replace_polymorphic_compare` to the top of
    the file. *)
let ( = ) : [ `Do_not_use_polymorphic_compare ] = `Do_not_use_polymorphic_compare

(** The nonequal function in Pervasives is backed by compiler magic (called
    compare_val), which operates on the memory representation of values to
    perform a comparison between values of any type. This is convenient, but it
    has the wrong semantics for stdlib sets and maps (since the same value may
    be represented with trees of differing structure), will not compare on-heap
    values with off-heap values (unless the compiler was configured with the
    -no-naked-pointers option), and is slower than monomorphic comparison
    functions. As a result, we forbid the use of the <> operator from Pervasives
    here.

    Instead of using polymorphic equality, use ppx_deriving to derive eq for
    your data type and use the derived `equal` function.

    If you are performing many comparisons of the same kind, consider binding
    the <> operator locally with something like `let ( <> ) = Int.( <> )`.

    If your module only compares values of a single builtin type (e.g., int or
    string), you can add `open Int.Replace_polymorphic_compare` to the top of
    the file. *)
let ( <> ) : [ `Do_not_use_polymorphic_compare ] = `Do_not_use_polymorphic_compare
