(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

module Types = struct
  type saved_state_target_info = {
    changes: Relative_path.t list;
    naming_changes: Relative_path.t list;
    corresponding_base_revision: string;
    deptable_fn: string;
    prechecked_changes: Relative_path.t list;
    saved_state_fn: string;
  }

  (* The idea of a file range necessarily means that the hypothetical list
    of them is sorted in some way. It is valid to have None as either endpoint
    because that simply makes it open-ended. For example, a range of files
    { None - "/some/path" } includes all files with path less than /some/path *)
  type files_to_check_range = {
    from_prefix_incl: Relative_path.t option;
    to_prefix_excl: Relative_path.t option;
  }

  type files_to_check_spec =
  | Range of files_to_check_range
  | Prefix of Relative_path.t

  type save_state_spec_info = {
    files_to_check: files_to_check_spec list;
    (* The base name of the file into which we should save the naming table. *)
    filename: string;
    (* Indicates whether we should generate a state in the presence of errors. *)
    gen_with_errors: bool;
  }
end

module type S = sig
  include module type of Types

  val save_state_spec_json_descr: string
  val get_save_state_spec: string option -> (save_state_spec_info option, string) result
  val get_save_state_spec_json: save_state_spec_info -> string

  val saved_state_json_descr: string
  val get_saved_state_spec: string option -> (saved_state_target_info option, string) result
end
