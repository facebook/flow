(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast_utils = Flow_ast_utils

let spf = Printf.sprintf

module Sort = Signature_builder_kind.Sort

module Error = struct
  type t =
    | ExpectedSort of Sort.t * string * Loc.t
    | ExpectedAnnotation of Loc.t
    | InvalidTypeParamUse of Loc.t
    | UnexpectedObjectKey of Loc.t
    | UnexpectedObjectSpread of Loc.t
    | UnexpectedArraySpread of Loc.t
    | UnexpectedArrayHole of Loc.t
    | EmptyArray of Loc.t
    | EmptyObject of Loc.t
    | UnexpectedExpression of Loc.t * Ast_utils.ExpressionSort.t
    | SketchyToplevelDef of Loc.t
    | TODO of string * Loc.t

  let compare = Pervasives.compare

  let debug_to_string = function
    | ExpectedSort (sort, x, loc) ->
      spf "%s @ %s is not a %s"
        x (Loc.to_string loc) (Sort.to_string sort)
    | ExpectedAnnotation loc -> spf "Expected annotation @ %s" (Loc.to_string loc)
    | InvalidTypeParamUse loc -> spf "Invalid use of type parameter @ %s" (Loc.to_string loc)
    | UnexpectedObjectKey loc -> spf "Expected simple object key @ %s" (Loc.to_string loc)
    | UnexpectedObjectSpread loc -> spf "Unexpected object spread @ %s" (Loc.to_string loc)
    | UnexpectedArraySpread loc -> spf "Unexpected array spread @ %s" (Loc.to_string loc)
    | UnexpectedArrayHole loc -> spf "Unexpected array hole @ %s" (Loc.to_string loc)
    | EmptyArray loc -> spf "Cannot determine element type of empty array @ %s" (Loc.to_string loc)
    | EmptyObject loc -> spf "Cannot determine types of initialized properties of empty object @ %s" (Loc.to_string loc)
    | UnexpectedExpression (loc, esort) ->
      spf "Expected literal expression instead of %s @ %s"
        (Ast_utils.ExpressionSort.to_string esort) (Loc.to_string loc)
    | SketchyToplevelDef loc ->
      spf "Unexpected toplevel definition that needs hoisting @ %s" (Loc.to_string loc)
    | TODO (msg, loc) -> spf "TODO: %s @ %s" msg (Loc.to_string loc)

end
module ErrorSet = Set.Make (Error)

module Dep = struct
  type t =
    | Local of local
    | Dynamic of dynamic
    | Remote of remote

  and local = Sort.t * string

  and dynamic =
    | DynamicImport of Loc.t
    | DynamicRequire of Loc.t

  and remote =
    | ImportNamed of {
        sort: Sort.t;
        source: Loc.t Ast_utils.source;
        name: Loc.t Ast_utils.ident;
      }
    | ImportStar of {
        sort: Sort.t;
        source: Loc.t Ast_utils.source;
      }
    | Require of {
        source: Loc.t Ast_utils.source;
      }
    | Global of local

  let compare = Pervasives.compare

  let expectation sort x loc = Error.ExpectedSort (sort, x, loc)

  let remote = function
    | Remote _ -> true
    | Local _ | Dynamic _ -> false

  let local_uses dep acc = match dep with
    | Local (_, n) -> SSet.add n acc
    | Remote _ | Dynamic _ -> acc

  let to_string =
    let string_of_import_sort = function
      | Sort.Value -> "import"
      | Sort.Type -> "import type" in
    let string_of_local (sort, x) =
      spf "%s: %s" (Sort.to_string sort) x in
    let string_of_dynamic = function
      | DynamicImport loc -> spf "import @ %s" (Loc.to_string loc)
      | DynamicRequire loc -> spf "require @ %s" (Loc.to_string loc) in
    let string_of_remote = function
      | ImportNamed { sort; name = (_, n); source = (_, m) } ->
        spf "%s { %s } from '%s'" (string_of_import_sort sort) n m
      | ImportStar { sort; source = (_, m) } ->
        spf "%s * from '%s'" (string_of_import_sort sort) m
      | Require { source = (_, m) } -> spf "require('%s')" m
      | Global local -> spf "global %s" (string_of_local local)
    in function
      | Local local -> string_of_local local
      | Dynamic dynamic -> string_of_dynamic dynamic
      | Remote remote -> string_of_remote remote
end

module DepSet = Set.Make (Dep)

type t = DepSet.t * ErrorSet.t

let join ((deps1, msgs1), (deps2, msgs2)) =
  DepSet.union deps1 deps2, ErrorSet.union msgs1 msgs2

let bot = DepSet.empty, ErrorSet.empty
let top msg = DepSet.empty, ErrorSet.singleton msg

let unreachable = bot
let todo loc msg = top (Error.TODO (msg, loc))

let unit dep = DepSet.singleton dep, ErrorSet.empty

let type_ atom = unit Dep.(Local (Sort.Type, atom))
let value atom = unit Dep.(Local (Sort.Value, atom))

let dynamic_import loc = unit Dep.(Dynamic (DynamicImport loc))
let dynamic_require loc = unit Dep.(Dynamic (DynamicRequire loc))

let import_named sort source name = unit Dep.(Remote (ImportNamed { sort; source; name }))
let import_star sort source = unit Dep.(Remote (ImportStar { sort; source }))
let require source = unit Dep.(Remote (Require { source }))
let global local = unit Dep.(Remote (Global local))

let reduce_join f deps x =
  join (deps, f x)

let recurse f (deps, msgs) =
  DepSet.fold (fun dep msgs -> ErrorSet.union (f dep) msgs) deps msgs
