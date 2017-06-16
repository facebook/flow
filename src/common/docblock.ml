(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


type flow_mode = OptIn | OptInWeak | OptOut

type t = {
  flow: flow_mode option;
  preventMunge: bool option;
  providesModule: string option;
  isDeclarationFile: bool;
  jsx: Options.jsx_mode option;
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
  | Some OptInWeak -> true
  | Some OptOut
  | None -> false

(* debugging *)
let json_of_docblock info =
  let open Hh_json in
  let flow = match flow info with
  | Some OptIn -> JSON_String "OptIn"
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
