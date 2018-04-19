(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


type flow_mode = OptIn | OptInStrict | OptInStrictLocal | OptInWeak | OptOut

type jsx_pragma =
  (**
   * Specifies a function that should be invoked instead of React.createElement
   * when interpreting JSX syntax. Otherwise, the usual rules of JSX are
   * followed: children are varargs after a props argument.
   *)
  | Jsx_pragma of (string * Loc.t Ast.Expression.t)

  (**
   * Alternate mode for interpreting JSX syntax. The element name is treated
   * as a function to be directly invoked, e.g. <Foo /> -> Foo({}).
   * Children are part of props instead of a separate argument.
   *)
  | Csx_pragma

type t = {
  flow: flow_mode option;
  preventMunge: bool option;
  providesModule: string option;
  isDeclarationFile: bool;
  jsx: jsx_pragma option;
}

let default_info = {
  flow = None;
  preventMunge = None;
  providesModule = None;
  isDeclarationFile = false;
  jsx = None;
}

(* accessors *)
let flow info = info.flow
let preventMunge info = info.preventMunge
let providesModule info = info.providesModule
let isDeclarationFile info = info.isDeclarationFile
let jsx info = info.jsx

let is_flow info = match info.flow with
  | Some OptIn
  | Some OptInStrict
  | Some OptInStrictLocal
  | Some OptInWeak -> true
  | Some OptOut
  | None -> false

(* debugging *)
let json_of_docblock info =
  let open Hh_json in
  let flow = match flow info with
  | Some OptIn -> JSON_String "OptIn"
  | Some OptInStrict -> JSON_String "OptInStrict"
  | Some OptInStrictLocal -> JSON_String "OptInStrictLocal"
  | Some OptInWeak -> JSON_String "OptInWeak"
  | Some OptOut -> JSON_String "OptOut"
  | None -> JSON_Null in

  let preventsMunge = match preventMunge info with
  | Some b -> JSON_Bool b
  | None -> JSON_Null in

  let providesModule = match providesModule info with
  | Some str -> JSON_String str
  | None -> JSON_Null in

  let isDeclarationFile = JSON_Bool (isDeclarationFile info) in

  JSON_Object [
    "flow", flow;
    "preventMunge", preventsMunge;
    "providesModule", providesModule;
    "isDeclarationFile", isDeclarationFile;
  ]
