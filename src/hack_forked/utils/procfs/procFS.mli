(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type status = {
  rss_anon: int;
  rss_file: int;
  rss_shmem: int;
  rss_total: int;
  rss_hwm: int;
}

val status_for_pid : int -> (status, string) result

val first_cgroup_for_pid : int -> (string, string) result
