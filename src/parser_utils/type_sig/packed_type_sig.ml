(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'loc t = {
  exports: 'loc Type_sig_pack.exports;
  export_def: 'loc Type_sig_pack.packed option;
  module_refs: string Type_sig_collections.Module_refs.t;
  local_defs: 'loc Type_sig_pack.packed_def Type_sig_collections.Local_defs.t;
  remote_refs: 'loc Type_sig_pack.remote_ref Type_sig_collections.Remote_refs.t;
  pattern_defs: 'loc Type_sig_pack.packed Type_sig_collections.Pattern_defs.t;
  patterns: 'loc Type_sig_pack.pattern Type_sig_collections.Patterns.t;
}
