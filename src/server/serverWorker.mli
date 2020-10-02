(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val make : n:int -> init_id:string -> SharedMem_js.handle -> MultiWorkerLwt.worker list
