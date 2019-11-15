(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Translate (Impl : Translator_intf.S) : sig
  type t

  val token : Offset_utils.t -> Parser_env.token_sink_result -> t

  val token_list : Offset_utils.t -> Parser_env.token_sink_result list -> t
end
with type t = Impl.t = struct
  type t = Impl.t

  let token offset_table { Parser_env.token_loc; token; token_context } =
    Loc.(
      Impl.obj
        [
          ("type", Impl.string (Token.token_to_string token));
          ( "context",
            Impl.string
              Parser_env.Lex_mode.(
                match token_context with
                | NORMAL -> "normal"
                | TYPE -> "type"
                | JSX_TAG -> "jsxTag"
                | JSX_CHILD -> "jsxChild"
                | TEMPLATE -> "template"
                | REGEXP -> "regexp") );
          ( "loc",
            Impl.obj
              [
                ( "start",
                  Impl.obj
                    [
                      ("line", Impl.number (float token_loc.start.line));
                      ("column", Impl.number (float token_loc.start.column));
                    ] );
                ( "end",
                  Impl.obj
                    [
                      ("line", Impl.number (float token_loc._end.line));
                      ("column", Impl.number (float token_loc._end.column));
                    ] );
              ] );
          ( "range",
            Impl.array
              [
                Impl.number (float (Offset_utils.offset offset_table token_loc.start));
                Impl.number (float (Offset_utils.offset offset_table token_loc._end));
              ] );
          ("value", Impl.string (Token.value_of_token token));
        ])

  let token_list offset_table tokens =
    Impl.array (List.rev_map (token offset_table) tokens |> List.rev)
end
