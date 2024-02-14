(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Module = struct
  type 'loc t = {
    module_kind: 'loc Type_sig_pack.module_kind;
    module_refs: string Type_sig_collections.Module_refs.t;
    local_defs: 'loc Type_sig_pack.packed_def Type_sig_collections.Local_defs.t;
    dirty_local_defs: int array;
    remote_refs: 'loc Type_sig_pack.remote_ref Type_sig_collections.Remote_refs.t;
    pattern_defs: 'loc Type_sig_pack.packed Type_sig_collections.Pattern_defs.t;
    dirty_pattern_defs: int array;
    patterns: 'loc Type_sig_pack.pattern Type_sig_collections.Patterns.t;
  }
end

module Builtins = struct
  type 'loc t = {
    module_refs: string Type_sig_collections.Module_refs.t;
    local_defs: 'loc Type_sig_pack.packed_def Type_sig_collections.Local_defs.t;
    remote_refs: 'loc Type_sig_pack.remote_ref Type_sig_collections.Remote_refs.t;
    pattern_defs: 'loc Type_sig_pack.packed Type_sig_collections.Pattern_defs.t;
    patterns: 'loc Type_sig_pack.pattern Type_sig_collections.Patterns.t;
    global_values: Type_sig_collections.Local_defs.index SMap.t;
    global_types: Type_sig_collections.Local_defs.index SMap.t;
    global_modules: 'loc module_def SMap.t;
  }

  and 'loc module_def = {
    loc: 'loc;
    module_kind: 'loc Type_sig_pack.module_kind;
  }
end
