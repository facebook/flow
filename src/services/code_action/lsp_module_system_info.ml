(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  file_options: Files.options;
  haste_module_system: bool;
  get_haste_name: File_key.t -> string option;
  get_package_info: File_key.t -> (Package_json.t, unit) result option;
  is_package_file: string -> bool;
}
