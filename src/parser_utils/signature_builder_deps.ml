(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast_utils = Flow_ast_utils

let spf = Printf.sprintf

module Sort = Signature_builder_kind.Sort

module Make (L : Loc_sig.S) : Signature_builder_deps_sig.S with module L = L = struct
  module L = L

  module Error = struct
    type t = L.t Signature_error.t [@@deriving show]

    let compare = Signature_error.compare
  end

  module PrintableErrorSet = Set.Make (Error)

  module Dep = struct
    type t =
      | Local of local
      | Dynamic of dynamic
      | Remote of remote

    and local = Sort.t * string

    and dynamic =
      | Class of L.t * string
      | DynamicImport of L.t
      | DynamicRequire of L.t

    and remote =
      | ImportNamed of {
          sort: Sort.t;
          source: L.t Ast_utils.source;
          name: L.t Ast_utils.ident;
        }
      | ImportStar of {
          sort: Sort.t;
          source: L.t Ast_utils.source;
        }
      | Require of {
          source: L.t Ast_utils.source;
          name: L.t Ast_utils.ident Nel.t option;
        }
      | Global of local

    let compare = Stdlib.compare

    let expectation sort x loc = Signature_error.ExpectedSort (sort, x, loc)

    let remote = function
      | Remote _ -> true
      | Local _
      | Dynamic _ ->
        false

    let local_uses dep acc =
      match dep with
      | Local (_, n) -> SSet.add n acc
      | Remote _
      | Dynamic _ ->
        acc

    let to_string =
      let string_of_import_sort = function
        | Sort.Value -> "import"
        | Sort.Type -> "import type"
      in
      let string_of_local (sort, x) = spf "%s: %s" (Sort.to_string sort) x in
      let string_of_dynamic = function
        | Class (loc, x) -> spf "class %s @ %s" x (L.debug_to_string loc)
        | DynamicImport loc -> spf "import @ %s" (L.debug_to_string loc)
        | DynamicRequire loc -> spf "require @ %s" (L.debug_to_string loc)
      in
      let string_of_remote = function
        | ImportNamed { sort; name = (_, n); source = (_, m) } ->
          spf "%s { %s } from '%s'" (string_of_import_sort sort) n m
        | ImportStar { sort; source = (_, m) } ->
          spf "%s * from '%s'" (string_of_import_sort sort) m
        | Require { source = (_, m); name } ->
          begin
            match name with
            | None -> spf "require('%s')" m
            | Some ns -> spf "require('%s').%s" m (ListUtils.to_string "." snd @@ Nel.to_list ns)
          end
        | Global local -> spf "global %s" (string_of_local local)
      in
      function
      | Local local -> string_of_local local
      | Dynamic dynamic -> string_of_dynamic dynamic
      | Remote remote -> string_of_remote remote
  end

  module DepSet = Set.Make (Dep)

  type t = DepSet.t * PrintableErrorSet.t

  let join ((deps1, msgs1), (deps2, msgs2)) =
    (DepSet.union deps1 deps2, PrintableErrorSet.union msgs1 msgs2)

  let bot = (DepSet.empty, PrintableErrorSet.empty)

  let top msg = (DepSet.empty, PrintableErrorSet.singleton msg)

  let unreachable = bot

  let todo loc msg = top (Signature_error.TODO (msg, loc))

  let unit dep = (DepSet.singleton dep, PrintableErrorSet.empty)

  let type_ atom = unit Dep.(Local (Sort.Type, atom))

  let value atom = unit Dep.(Local (Sort.Value, atom))

  let dynamic_import loc = unit Dep.(Dynamic (DynamicImport loc))

  let dynamic_require loc = unit Dep.(Dynamic (DynamicRequire loc))

  let import_named sort source name = unit Dep.(Remote (ImportNamed { sort; source; name }))

  let import_star sort source = unit Dep.(Remote (ImportStar { sort; source }))

  let require ?name source = unit Dep.(Remote (Require { source; name }))

  let global local = unit Dep.(Remote (Global local))

  let reduce_join f deps x = join (deps, f x)

  let recurse f (deps, msgs) =
    DepSet.fold (fun dep msgs -> PrintableErrorSet.union (f dep) msgs) deps msgs

  let replace_local_with_dynamic_class (loc, x) (deps, msgs) =
    let acc =
      DepSet.fold
        (fun dep acc ->
          match dep with
          | Dep.Local (_, y) when x = y -> acc
          | _ -> join (acc, unit dep))
        deps
        (DepSet.empty, msgs)
    in
    join (acc, unit (Dep.Dynamic (Dep.Class (loc, x))))
end

module With_Loc = Make (Loc_sig.LocS)
module With_ALoc = Make (Loc_sig.ALocS)
include With_Loc
