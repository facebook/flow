(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let loc_of_aloc ~reader aloc =
  let table = Parsing_heaps.Reader.get_sig_ast_aloc_table_unsafe_lazy ~reader aloc in
  ALoc.to_loc table aloc
