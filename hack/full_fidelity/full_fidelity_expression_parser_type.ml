(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
module type ExpressionParserType = sig
  type t
  val make : Full_fidelity_lexer.t -> Full_fidelity_syntax_error.t list -> t
  val lexer : t -> Full_fidelity_lexer.t
  val errors : t -> Full_fidelity_syntax_error.t list
  val parse_expression : t -> t * Full_fidelity_minimal_syntax.t
end
