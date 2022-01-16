(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Same_line of {
      start: Loc.position;
      column_offset: int;
    }
  | Diff_line of {
      start: Loc.position;
      line_offset: int;
      column: int;
    }
[@@deriving eq]

module Hash_t_set = Hash_set.Make (struct
  type nonrec t = t

  let equal = equal

  let hash = Hashtbl.hash
end)

module Hash_pos_set = Hash_set.Make (struct
  type t = Loc.position

  let equal x y = Loc.equal_position x y

  let hash = Hashtbl.hash
end)

let tbl = Hash_t_set.create 1024

let pos_tbl = Hash_pos_set.create 1024

let of_loc ({ Loc.start = base_pos; _end = pos; _ } : Loc.t) : t =
  let start = Hash_pos_set.add pos_tbl base_pos in
  let line_offset = pos.Loc.line - start.Loc.line in
  let loc =
    if line_offset = 0 then
      Same_line { start; column_offset = pos.Loc.column - start.Loc.column }
    else
      Diff_line { start; line_offset; column = pos.Loc.column }
  in
  Hash_t_set.add tbl loc

let to_loc relative_loc source : Loc.t =
  match relative_loc with
  | Same_line { start; column_offset } ->
    let start = Hash_pos_set.add pos_tbl start in
    let _end =
      Hash_pos_set.add
        pos_tbl
        { Loc.line = start.Loc.line; column = start.Loc.column + column_offset }
    in
    { Loc.start; _end; source }
  | Diff_line { start; line_offset; column } ->
    let start = Hash_pos_set.add pos_tbl start in
    let _end = Hash_pos_set.add pos_tbl { Loc.line = start.Loc.line + line_offset; column } in
    { Loc.start; _end; source }

type ast = (Loc.t, Loc.t) Flow_ast.Program.t

(* There's some redundancy in the visitors here, but an attempt to avoid repeated code led,
 * inexplicably, to a shared heap size regression under types-first: D15481813 *)
let loc_compactifier =
  object
    inherit [Loc.t, Loc.t, t, t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot = of_loc

    method on_type_annot = of_loc
  end

let loc_decompactifier source =
  object
    inherit [t, t, Loc.t, Loc.t] Flow_polymorphic_ast_mapper.mapper

    method private decompactify_loc loc = to_loc loc source

    method on_loc_annot loc = to_loc loc source

    method on_type_annot loc = to_loc loc source
  end

let compactify_loc ast = loc_compactifier#program ast

let decompactify_loc file ast = (loc_decompactifier (Some file))#program ast

let deserialize_ast (obj : string) (file_key : File_key.t) =
  Hash_t_set.clear tbl;
  Hash_pos_set.clear pos_tbl;
  let deserialize x = Marshal.from_string x 0 in
  decompactify_loc file_key (deserialize obj)

let serialize_ast ast : string =
  Hash_t_set.clear tbl;
  Hash_pos_set.clear pos_tbl;
  Marshal.to_string (compactify_loc ast) []
