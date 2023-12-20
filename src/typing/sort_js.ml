(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

module IndirectFilenameMap = struct
  type key = File_key.t

  type value = FilenameSet.t

  type t = FilenameGraph.t

  let find file graph = FilenameGraph.find file graph
end

include Tarjan.Make (File_key) (FilenameSet) (IndirectFilenameMap)
