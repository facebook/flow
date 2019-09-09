(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Destructuring in the presence of default values gives rise to a list-like
 * structure, which is encapsulated herein. While a destructuring pattern only
 * creates bindings at its leaves, defaults can be provided at every level.
 * Each default can provide additional lower bounds, and must be accumulated.
 *
 * For example, consider the following function:
 *
 * function f({x=0}={x:""}) { return x; }
 *
 * In the above, x might be a number, a string, or something else provided
 * downstream. We can represent the default value associated with x as follows:
 *
 * Cons (
 *   (Expr `0`),
 *   (Selector
 *     (Expr `{x:""}`)
 *     (Prop "x")))
 *
 * We can fold over the list structure of default values to "evaluate" them.
 * In practice, we only ever evaluate defaults to turn them into types. Still,
 * the little interpreter here allows us to be careful about the environment in
 * which they are evaluated, which is crucial, as later default expressions can
 * depend on bindings established by earlier ones.
 *)

open Reason
open Type

type 'a t =
  | Expr of 'a
  | Cons of 'a * 'a t
  | Selector of reason * 'a t * TypeTerm.selector

let expr ?default e =
  match default with
  | Some default -> Cons (e, default)
  | None -> Expr e

let elem key reason default = Selector (reason, default, Elem key)

let prop x reason default = Selector (reason, default, Prop x)

let arr_rest i reason default = Selector (reason, default, ArrRest i)

let obj_rest xs reason default = Selector (reason, default, ObjRest xs)

let rec fold ~expr ~cons ~selector = function
  | Expr e -> expr e
  | Cons (e, d) -> cons (expr e) (fold ~expr ~selector ~cons d)
  | Selector (r, d, s) -> selector r (fold ~expr ~selector ~cons d) s
