(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

type 'a t_or_error = (t, 'a * string) result

val empty : t

val name : t -> string option

val main : t -> string option

val parse : options:Options.t -> (Loc.t, Loc.t) Flow_ast.program -> Loc.t t_or_error
