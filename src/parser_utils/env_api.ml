(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  module L : Loc_sig.S

  type read_loc = L.t

  type write_loc =
    | Write of L.t Reason.virtual_reason
    | Uninitialized
    | Refinement of {
        refinement_id: int;
        writes: write_locs;
      }

  and write_locs = write_loc list

  type values = write_locs L.LMap.t

  val write_locs_of_read_loc : values -> read_loc -> write_locs

  val writes_of_write_loc : write_loc -> L.t list

  val print_values : values -> string
end

module Make (L : Loc_sig.S) : S with module L = L = struct
  module L = L

  type read_loc = L.t

  type write_loc =
    | Write of L.t Reason.virtual_reason
    | Uninitialized
    | Refinement of {
        refinement_id: int;
        writes: write_locs;
      }

  and write_locs = write_loc list

  type values = write_locs L.LMap.t

  let write_locs_of_read_loc values read_loc = L.LMap.find read_loc values

  let rec writes_of_write_loc write_loc =
    match write_loc with
    | Refinement { refinement_id = _; writes } ->
      writes |> List.map writes_of_write_loc |> List.flatten
    | Write r -> [Reason.poly_loc_of_reason r]
    | Uninitialized -> []

  let print_values =
    let rec print_write_loc write_loc =
      match write_loc with
      | Uninitialized -> "(uninitialized)"
      | Write reason ->
        let loc = Reason.poly_loc_of_reason reason in
        Utils_js.spf
          "%s: (%s)"
          (L.debug_to_string loc)
          Reason.(desc_of_reason reason |> string_of_desc)
      | Refinement { refinement_id; writes } ->
        let refinement_id_str = string_of_int refinement_id in
        let writes_str = String.concat "," (List.map print_write_loc writes) in
        Printf.sprintf "{refinement_id = %s; writes = %s}" refinement_id_str writes_str
    in
    fun values ->
      let kvlist = L.LMap.bindings values in
      let strlist =
        Base.List.map
          ~f:(fun (read_loc, write_locs) ->
            Printf.sprintf
              "%s => { %s }"
              (L.debug_to_string read_loc)
              (String.concat ", " @@ Base.List.map ~f:print_write_loc write_locs))
          kvlist
      in
      Printf.sprintf "[ %s ]" (String.concat "; " strlist)
end

module With_Loc = Make (Loc_sig.LocS)
module With_ALoc = Make (Loc_sig.ALocS)
