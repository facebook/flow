(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  file_options: Files.options;
  haste_module_system: bool;
  get_haste_module_info: File_key.t -> Haste_module_info.t option;
  get_package_info: File_key.t -> (Package_json.t, unit) result option;
  is_package_file: module_path:string -> module_name:string -> bool;
  node_resolver_root_relative_dirnames: (string option * string) list;
  resolves_to_real_path: from:string -> to_real_path:string -> bool;
}
