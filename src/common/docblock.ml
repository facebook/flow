(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type flow_mode =
  | OptIn
  | OptInStrict
  | OptInStrictLocal
  | OptInWeak
  | OptOut

(*
  * Specifies a function that should be invoked instead of React.createElement
  * when interpreting JSX syntax. Otherwise, the usual rules of JSX are
  * followed: children are varargs after a props argument.
  *)
type jsx_pragma = string * (Loc.t, Loc.t) Flow_ast.Expression.t

type t = {
  flow: flow_mode option;
  typeAssert: bool;
  preventMunge: bool;
  providesModule: string option;
  jsx: jsx_pragma option;
}

let default_info =
  { flow = None; typeAssert = false; preventMunge = false; providesModule = None; jsx = None }

(* accessors *)
let flow info = info.flow

let typeAssert info = info.typeAssert

let preventMunge info = info.preventMunge

let providesModule info = info.providesModule

let jsx info = info.jsx

let is_strict info =
  match info.flow with
  | Some OptInStrict -> true
  | Some OptIn
  | Some OptInStrictLocal
  | Some OptInWeak
  | Some OptOut
  | None ->
    false

let is_flow info =
  match info.flow with
  | Some OptIn
  | Some OptInStrict
  | Some OptInStrictLocal
  | Some OptInWeak ->
    true
  | Some OptOut
  | None ->
    false

let set_flow_mode_for_ide_command info =
  let flow =
    match flow info with
    (* If the file does not specify a @flow pragma, we still want to try
       to infer something, but the file might be huge and unannotated,
       which can cause performance issues (including non-termination).
       To avoid this case, we infer the file using "weak mode." *)
    | None -> OptInWeak
    (* Respect @flow pragma *)
    | Some OptIn -> OptIn
    (* Respect @flow strict pragma *)
    | Some OptInStrict -> OptInStrict
    (* Respect @flow strict-local pragma *)
    | Some OptInStrictLocal -> OptInStrictLocal
    (* Respect @flow weak pragma *)
    | Some OptInWeak -> OptInWeak
    (* Respect @noflow, which `apply_docblock_overrides` does not by
       default. Again, large files can cause non-termination, so
       respecting this pragma gives programmers a way to tell Flow to
       avoid inference on such files. *)
    | Some OptOut -> OptInWeak
  in
  { info with flow = Some flow }

(* debugging *)
let json_of_docblock info =
  Hh_json.(
    let flow =
      match flow info with
      | Some OptIn -> JSON_String "OptIn"
      | Some OptInStrict -> JSON_String "OptInStrict"
      | Some OptInStrictLocal -> JSON_String "OptInStrictLocal"
      | Some OptInWeak -> JSON_String "OptInWeak"
      | Some OptOut -> JSON_String "OptOut"
      | None -> JSON_Null
    in
    let preventsMunge =
      if preventMunge info then
        JSON_Bool true
      else
        JSON_Null
    in
    let providesModule =
      match providesModule info with
      | Some str -> JSON_String str
      | None -> JSON_Null
    in
    let typeAssert = JSON_Bool (typeAssert info) in
    JSON_Object
      [
        ("flow", flow);
        ("typeAssert", typeAssert);
        ("preventMunge", preventsMunge);
        ("providesModule", providesModule);
      ])
