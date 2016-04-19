(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

val dependent_files:
  Worker.t list option -> (* workers *)
  FilenameSet.t -> (* unmodified_files *)
  FilenameSet.t -> (* inferred_files *)
  Module_js.NameSet.t -> (* touched_modules *)
  FilenameSet.t * FilenameSet.t

val calc_dependencies:
  Worker.t list option -> (* workers *)
  filename list -> (* files *)
  FilenameSet.t FilenameMap.t
