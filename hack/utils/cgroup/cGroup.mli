(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type stats = {
  total: int;
  total_swap: int;
  anon: int;
  shmem: int;
  file: int;
}

val get_stats : unit -> (stats, string) result Lwt.t
