(**
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val make : n:int -> SharedMem_js.handle -> MultiWorkerLwt.worker list
