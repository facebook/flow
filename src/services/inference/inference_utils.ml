(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

let string_of_docblock_error = function
  | Docblock.MultipleFlowAttributes ->
    "Unexpected @flow declaration. Only one per file is allowed."
  | Docblock.MultipleProvidesModuleAttributes ->
    "Unexpected @providesModule declaration. Only one per file is allowed."
  | Docblock.MultipleJSXAttributes ->
    "Unexpected @jsx declaration. Only one per file is allowed."
  | Docblock.InvalidJSXAttribute first_error ->
    "Invalid @jsx declaration. Should have form `@jsx LeftHandSideExpression` "^
    "with no spaces."^
    (match first_error with
    | None -> ""
    | Some first_error -> spf " Parse error: %s" first_error)

let error_of_docblock_error (loc, err) =
  Errors.mk_error ~kind:Errors.ParseError [loc, [string_of_docblock_error err]]

let set_of_docblock_errors errors =
  List.fold_left (fun acc err ->
    Errors.ErrorSet.add (error_of_docblock_error err) acc
  ) Errors.ErrorSet.empty errors

let error_of_parse_error (loc, err) =
  Errors.mk_error ~kind:Errors.ParseError [loc, [Parse_error.PP.error err]]

let set_of_parse_error error =
  Errors.ErrorSet.singleton (error_of_parse_error error)
