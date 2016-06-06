(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* TODO: Integrate these with the rest of the Hack error messages. *)

type t = {
  start_offset : int;
  end_offset : int;
  message : string
}

let make start_offset end_offset message =
  { start_offset; end_offset; message }

let to_string error =
  Printf.sprintf "(%d-%d) %s" error.start_offset error.end_offset error.message

(* Lexical errors *)
let error0001 = "A hexadecimal literal needs at least one digit."
let error0002 = "A binary literal needs at least one digit."
let error0003 = "A floating literal with an exponent needs at least " ^
                " one digit in the exponent."
let error0004 = "An octal literal must contain only digits 0 through 7."
let error0005 = "This string literal escape sequence is invalid."
let error0006 = "This character is invalid."
let error0007 = "This delimited comment is not terminated."
let error0008 = "A name is expected here."
let error0009 = "An unqualified name is expected here."
let error0010 = "A single quote is expected here."
let error0011 = "A newline is expected here."
let error0012 = "This string literal is not terminated."
let error0013 = "This XHP body is not terminated."

(* Syntactic errors *)
let error1001 = "A source file must begin with '<?hh'."
let error1002 = "An inclusion directive or type, function, " ^
                "namespace or use declaration is expected here."
let error1003 = "The 'function' keyword is expected here."
let error1004 = "A name is expected here."
let error1005 = "A block of statements is expected here."
let error1006 = "A right brace is expected here."
let error1007 = "A type specifier is expected here."
let error1008 = "A variable name is expected here."
let error1009 = "A comma or right parenthesis is expected here."
let error1010 = "A semicolon is expected here."
let error1011 = "A right parenthesis is expected here."
let error1012 = "A type is expected here."
let error1013 = "A closing angle bracket is expected here."
let error1014 = "A closing angle bracket or comma is expected here."
let error1015 = "An expression is expected here."
let error1016 = "An assignment is expected here."
let error1017 = "An XHP attribute value is expected here."
let error1018 = "The 'while' keyword is expected here"
let error1019 = "A left parenthesis is expected here."
let error1020 = "A colon is expected here."
