(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val make_genv:
  ServerArgs.options -> ServerConfig.t -> ServerLocalConfig.t -> ServerEnv.genv

val make_env: ServerConfig.t -> ServerEnv.env
