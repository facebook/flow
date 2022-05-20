(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val jsx_child : Lex_env.t -> Lex_env.t * Lex_result.t

val regexp : Lex_env.t -> Lex_env.t * Lex_result.t

val jsx_tag : Lex_env.t -> Lex_env.t * Lex_result.t

val template_tail : Lex_env.t -> Lex_env.t * Lex_result.t

val type_token : Lex_env.t -> Lex_env.t * Lex_result.t

val token : Lex_env.t -> Lex_env.t * Lex_result.t

val is_valid_identifier_name : Flow_sedlexing.lexbuf -> bool
