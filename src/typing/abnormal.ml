(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* we model abnormal control flows using exceptions during traversal *)

(* control directives encountered during traversal *)
type t =
  | Return
  | Throw
  | Break of string option
  | Continue of string option

let opt_label name = function
  | None -> name
  | Some s -> name ^ " " ^ s

let to_string = function
  | Return -> "Return"
  | Throw -> "Throw"
  | Break label -> opt_label "Break" label
  | Continue label -> opt_label "Continue" label

type payload =
  | Stmt of (Loc.t, Loc.t * Type.t) Ast.Statement.t
  | Stmts of (Loc.t, Loc.t * Type.t) Ast.Statement.t list

exception Exn of payload * t

open Utils_js

(* called from traversal. abnormal indicates control flow directive encountered *)
let throw_stmt_control_flow_exception stmt abnormal =
  raise (Exn (Stmt stmt, abnormal))
let throw_stmts_control_flow_exception stmts abnormal =
  raise (Exn (Stmts stmts, abnormal))

(* if argument is Some abnormal, throw it *)
let check_stmt_control_flow_exception = function
  | stmt, None -> stmt
  | stmt, Some abnormal -> throw_stmt_control_flow_exception stmt abnormal
let check_stmts_control_flow_exception = function
  | stmts, None -> stmts
  | stmts, Some abnormal -> throw_stmts_control_flow_exception stmts abnormal

(* helper *)
let check_env_depth depth =
  let new_depth = Env.env_depth () in
  if new_depth = depth then ()
  else assert_false (spf
    "env depth %d != %d after no control flow catch"
    new_depth depth)

(* catch_stmt_control_flow_exception runs a function which is expected to either
    return a statement or raise Exn (Stmt _, _). The function should never raise
    Exn (Stmts _, _).
  Similarly, the function passed into catch_stmt_control_flow_exception should
    return a statement list or raise Exn (Stmts _, _), and never raise
    Exn (Stmt _, _).
  For both:
    If the passed-in function returns an AST,
      then we return that AST and None.
    Otherwise, if it raises with some AST payload and an abnormal flow,
      then we return the payload AST and Some <abnormal flow>.
*)
let
  catch_stmt_control_flow_exception,
  catch_stmts_control_flow_exception =
  let catch_control_flow_exception p f =
    let depth = Env.env_depth () in
    try (
      let res = f () in
      check_env_depth depth;
      res, None
    ) with
    | Exn (payload, abnormal) ->
      Env.trunc_env depth;
      p payload, Some abnormal
    | exn ->
      raise exn
  in
  catch_control_flow_exception (function
  | Stmt stmt -> stmt
  | Stmts _ -> assert_false "Statement expected"),
  catch_control_flow_exception (function
  | Stmts stmts -> stmts
  | Stmt _ -> assert_false "Statement list expected")

(* like check_control_flow_exception, except break statements
   specifying the given label (or None) are ignored *)
let ignore_break_to_label label = function
  | ast, Some (Break break_label) when break_label = label -> ast, None
  | result -> result

(* like ignore_break_to_label, except continue statements
   on the same label (or None) are also ignored *)
let ignore_break_or_continue_to_label label res =
  match ignore_break_to_label label res with
    | ast, Some (Continue cont_label) when cont_label = label -> ast, None
    | result -> result

(********************************************************************)

(** at some points we need to record control flow directives in addition
    to responding to them. *)

module AbnormalMap : MyMap.S with type key = t = MyMap.Make (struct
  type abnormal = t
  type t = abnormal
  let compare = Pervasives.compare
end)

let abnormals: Env.t AbnormalMap.t ref = ref AbnormalMap.empty

(** record the appearance of a control flow directive.
    associate the given env if passed *)
let save ?(env=[]) abnormal =
  abnormals := AbnormalMap.add abnormal env !abnormals

(** set or remove a given control flow directive's value,
    and return the current one *)
let swap_saved abnormal value =
  let old = AbnormalMap.get abnormal !abnormals in
  if old <> value then begin
    abnormals := match value with
      | None -> AbnormalMap.remove abnormal !abnormals
      | Some env -> AbnormalMap.add abnormal env !abnormals
  end;
  old

(** remove a given control flow directive's value,
    and return the current one *)
let clear_saved abnormal =
  swap_saved abnormal None

let string = function
  | Return -> "return"
  | Throw -> "throw"
  | Break (Some lbl) -> spf "break `%s`" lbl
  | Break None -> "break"
  | Continue (Some lbl) -> spf "continue `%s`" lbl
  | Continue None -> "continue"
