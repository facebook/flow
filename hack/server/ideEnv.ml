(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

type t = {
  typechecker : IdeProcessPipe.to_typechecker;
  tcopt : TypecheckerOptions.t;
  (* Persistent client talking JSON protocol *)
  client : (in_channel * out_channel) option;
  persistent_client_requests : (IdeJson.call_id * IdeJson.call_type) list;
  (* Whether typechecker has finished full initialization. In the future, we
   * can have more granularity here, allowing some queries to run sooner. *)
  typechecker_init_done : bool;
  (* IDE process's version of ServerEnv.file_info, synchronized periodically
   * from typechecker process. *)
  files_info : FileInfo.t Relative_path.Map.t;
  (* IDE process's version of ServerEnv.errorl, synchronized periodically
   * from typechecker process. *)
  errorl : Errors.t
}

let build_env typechecker tcopt = {
  typechecker = typechecker;
  tcopt = tcopt;
  client = None;
  persistent_client_requests = [];
  typechecker_init_done = false;
  files_info = Relative_path.Map.empty;
  errorl = Errors.empty;
}
