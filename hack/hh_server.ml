(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(**
 * Hack for HipHop: type checker daemon code.
 *
 * See README for high level overview.
 *
 * Interesting files/directory:
 * - hh_server.ml:       contains mostly the ugly inotify code.
 *
 * Parser code:
 * The parser was written using yacc, now it's a recursive descent parser.
 * The lexer still uses lex. It only supports a subset of PHP by design.
 * - parsing/lexer_hack.mll:  the lexer (lex)
 * - parsing/parser_hack.ml: the recursive descent parser
 *
 * Naming:
 * Naming consists in "solving" all the names (making sure every
 * class/variable/etc. are bound).
 * - naming/nast.ml:
 *   Named abstract syntax tree (the datastructure).
 * - naming/naming.ml:
 *   Naming operations (takes an ast and returns a nast).
 * - naming/nastInitCheck.ml:
 *   Checks that all the members in a class are always properly initialized.
 *
 * Typing:
 * - typing/typing_defs.ml:
 *   The datastructures required for typing.
 * - typing/typing_env.ml:
 *   All the operations on the typing environment (e.g. unifying types).
 * - typing/typing_reasong.ml:
 *   Documents why something has a given type (witness system).
 * - typing/typing.ml:
 *   Where everything happens, in two phases:
 *   1. type declarations: we build the environment where we know the type of
 *      everything. (see make_env).
 *   2. for every function and method definition, we check that their
 *      implementation matches their signature (assuming that all other
 *      signatures are correct).
 *)

let () = HhServerMonitor.start ()
