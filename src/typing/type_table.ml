(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type id_kind =
  | PropertyAccess of Type.t (* receiver type *)
  | Import of string (* remote name *) * Type.t (* module type *)
  | Other

type name = string

type tparam_env = (name * Loc.t) list

type type_scheme = Scheme of (tparam_env * Type.t) [@@unboxed]

type 'a entry = name * 'a * id_kind
type type_entry = Type.t entry
type scheme_entry = type_scheme entry

type t = {
  (* This stores type information about expressions. Every expression in the program should have a
   * type here. This means that there is some nesting, e.g. in the expression `5 + 4` there will be
   * a type for `5`, a type for `4`, and a type for the entire addition expression. *)
  coverage: (Loc.t, type_scheme) Hashtbl.t;
  (* This stores type information about identifiers only. There should be no overlap or nesting of
   * locations here. *)
  type_info: (Loc.t, scheme_entry) Hashtbl.t;
}

let create () = {
  coverage = Hashtbl.create 0;
  type_info = Hashtbl.create 0;
}

let set tparams {coverage; _} loc t =
  let scheme = Scheme (tparams, t) in
  Hashtbl.replace coverage loc scheme

let set_info tparams {type_info; _} loc (name, t, i) =
  Hashtbl.replace type_info loc (name, Scheme (tparams, t), i)

let fold_coverage f t init = Hashtbl.fold f t.coverage init

let find_unsafe_coverage t k = Hashtbl.find t.coverage k

let find_unsafe_coverage_type t k =
  let Scheme (_, t) = Hashtbl.find t.coverage k in
  t

let reset {coverage; type_info} =
  Hashtbl.reset coverage;
  Hashtbl.reset type_info

let copy {coverage; type_info} = {
  coverage = Hashtbl.copy coverage;
  type_info = Hashtbl.copy type_info;
}

let find_type_info ~pred t =
  Hashtbl.fold (fun k v a ->
    if pred k then Some (k, v) else a
  ) t.type_info None

let type_info_hashtbl t =
  t.type_info

let coverage_to_list t =
  let r = ref [] in
  Hashtbl.iter (fun l t -> r := (l, t) :: !r) t.coverage;
  !r

let coverage_hashtbl t =
  t.coverage

(**
 * Use the loc for the function name in the types table. When the function
 * has no name (i.e. for `export default function() ...`), generate a loc
 * that will span the `function` keyword as a next-best-thing location.
 *)
let function_decl_loc id loc =
  match id with
  | Some (loc, _) -> loc
  | None ->
    Loc.({ loc with
      _end = {
        line = loc.start.line;
        (* len('function') is 8 *)
        column = loc.start.column + 8;
        offset = loc.start.offset + 8;
      };
    })
