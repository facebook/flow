(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t
val make : Full_fidelity_source_text.t -> t
val root : t -> Full_fidelity_minimal_syntax.t
val text : t -> Full_fidelity_source_text.t
val errors : t -> Full_fidelity_syntax_error.t list
val language : t -> string
val mode : t -> string
val is_hack : t -> bool
val is_php : t -> bool
val is_strict : t -> bool
