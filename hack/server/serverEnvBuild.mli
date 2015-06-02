(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val make_genv: 
  ServerArgs.options -> ServerConfig.t -> Path.t list
  -> ServerEnv.genv

val make_env:
  ServerArgs.options -> ServerConfig.t -> ServerEnv.env
