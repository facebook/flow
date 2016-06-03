(** Pos_source is the underlying structure that provides position information.
 *
 * There will be two kinds of Pos_source
   * Lexbuf_based - this is used by the original Hack lexer.
   * Node_based - position data is held by a node.
*)

module Lexbuf_based_pos_source = struct
  type t = Lexing.lexbuf
end

include Lexbuf_based_pos_source
