(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

let make_fake_file_key filename = File_key.SourceFile ("/tmp/fake/path/" ^ filename ^ ".js")

let make_filename_set filenames = filenames |> List.map make_fake_file_key |> FilenameSet.of_list

let make_dependency_graph lst =
  List.fold_left
    (fun map (file, dependencies) ->
      let file = make_fake_file_key file in
      if FilenameMap.mem file map then failwith "Duplicate key when constructing map";
      let dependency_set = make_filename_set dependencies in
      FilenameMap.add file dependency_set map)
    FilenameMap.empty
    lst
  |> FilenameGraph.of_map
