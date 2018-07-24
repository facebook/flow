(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t
val empty: t
val name: t -> string option
val main: t -> string option
val parse: (Loc.t, Loc.t) Ast.program -> (t, string) result
