(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let spf = Printf.sprintf

module Error = struct
  type t =
    | ExpectedType of string * Loc.t
    | ExpectedValue of string * Loc.t
    | ExpectedAnnotation of Loc.t
    | InvalidTypeParamUse of Loc.t
    | UnexpectedObjectKey of Loc.t
    | UnexpectedExpression of Loc.t
    | TODO of string

  let compare = Pervasives.compare

  let to_string = function
    | ExpectedType (x, loc) -> spf "%s @ %s is not a type" x (Loc.to_string loc)
    | ExpectedValue (x, loc) -> spf "%s @ %s is not a value" x (Loc.to_string loc)
    | ExpectedAnnotation loc -> spf "Expected annotation @ %s" (Loc.to_string loc)
    | InvalidTypeParamUse loc -> spf "Invalid use of type parameter @ %s" (Loc.to_string loc)
    | UnexpectedObjectKey loc -> spf "Expected simple object key @ %s" (Loc.to_string loc)
    | UnexpectedExpression loc -> spf "Expected literal expression @ %s" (Loc.to_string loc)
    | TODO msg -> spf "TODO: %s" msg

end
module ErrorSet = Set.Make (Error)

module Dep = struct
  type t =
    | Type of string
    | Value of string
    | ImportNamed of {
        kind: Ast.Statement.ImportDeclaration.importKind;
        source: Loc.t * string;
        name: Loc.t * string
      }
    | ImportStar of {
        kind: Ast.Statement.ImportDeclaration.importKind;
        source: Loc.t * string
      }
    | Require of {
        source: Loc.t * string
      }
    | GlobalType of string
    | GlobalValue of string

  let compare = Pervasives.compare

  let expectation = function
    | Type x -> Some (x, fun loc -> Error.ExpectedType (x, loc))
    | Value x -> Some (x, fun loc -> Error.ExpectedValue (x, loc))
    | ImportNamed _ | ImportStar _ | Require _ | GlobalType _ | GlobalValue _ -> None

  let remote = function
    | Type _ | Value _ -> false
    | ImportNamed _ | ImportStar _ | Require _ | GlobalType _ | GlobalValue _ -> true

  let to_string =
    let string_of_import_kind = function
      | Ast.Statement.ImportDeclaration.ImportValue -> "import"
      | Ast.Statement.ImportDeclaration.ImportType -> "import type"
      | Ast.Statement.ImportDeclaration.ImportTypeof -> "import typeof"
    in function
      | Type x -> spf "type: %s" x
      | Value x -> spf "value: %s" x
      | ImportNamed { kind; name = (_, n); source = (_, m) } ->
        spf "%s { %s } from '%s'" (string_of_import_kind kind) n m
      | ImportStar { kind; source = (_, m) } ->
        spf "%s * from '%s'" (string_of_import_kind kind) m
      | Require { source = (_, m) } -> spf "require('%s')" m
      | GlobalType x -> spf "global type: %s" x
      | GlobalValue x -> spf "global value: %s" x
end

module DepSet = Set.Make (Dep)

type t =
  | Known of DepSet.t
  | Unknown of ErrorSet.t

let join = function
  | Known deps1, Known deps2 -> Known (DepSet.union deps1 deps2)
  | Unknown msgs1, Unknown msgs2 -> Unknown (ErrorSet.union msgs1 msgs2)
  | unknown, Known _ | Known _, unknown -> unknown

let bot = Known DepSet.empty
let top msg = Unknown (ErrorSet.singleton msg)

let unreachable = bot
let todo msg = top (Error.TODO msg)

let unit dep = Known (DepSet.singleton dep)

let type_ atom = unit (Dep.Type atom)
let value atom = unit (Dep.Value atom)
let import_named kind source name = unit (Dep.ImportNamed { kind; source; name })
let import_star kind source = unit (Dep.ImportStar { kind; source })
let global_type atom = unit (Dep.GlobalType atom)
let global_value atom = unit (Dep.GlobalValue atom)

let reduce_join f deps x =
  join (deps, f x)

let recurse f = function
  | Known deps -> DepSet.fold (fun dep msgs -> ErrorSet.union (f dep) msgs) deps ErrorSet.empty
  | Unknown msgs -> msgs
