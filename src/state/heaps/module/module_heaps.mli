(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

 val get_file: (Modulename.t -> File_key.t option) Expensive.t
 val get_file_unsafe: (Modulename.t -> File_key.t) Expensive.t
 val add_file: (Modulename.t -> File_key.t -> unit) Expensive.t
 val module_exists: Modulename.t -> bool
 val remove_file_batch: Modulename.Set.t -> unit
