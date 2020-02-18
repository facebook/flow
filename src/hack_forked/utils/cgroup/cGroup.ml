(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core_kernel
open Result.Monad_infix

let spf = Printf.sprintf

(* Little helper module to help memoize things. Probably could be pulled out into its own module
 * at some point *)
module Memoize : sig
  val forever : f:(unit -> 'a Lwt.t) -> unit -> 'a Lwt.t

  val until : seconds:float -> f:(unit -> 'a Lwt.t) -> unit -> 'a Lwt.t
end = struct
  let forever ~f =
    let memoized_result = ref None in
    fun () ->
      match !memoized_result with
      | None ->
        let%lwt result = f () in
        memoized_result := Some result;
        Lwt.return result
      | Some result -> Lwt.return result

  let until ~seconds ~f =
    let memoized_result = ref None in
    let fetch () =
      let%lwt result = f () in
      memoized_result := Some (Unix.gettimeofday () +. seconds, result);
      Lwt.return result
    in
    fun () ->
      match !memoized_result with
      | Some (good_until, result) when Unix.gettimeofday () < good_until -> Lwt.return result
      | _ -> fetch ()
end

(* Like Sys_utils.cat but with lwt. Probably also could be moved to its own module *)
let cat file =
  (* Some low level system operations cannot be canceled and will just ignore cancellation. If we
   * wrap this code in `Lwt.protected` we get a thread that is cancellable while the underlying
   * operations proceed as normal. *)
  Lwt.protected
    (try%lwt
       let%lwt ic =
         Lwt_io.open_file
           ~flags:[Unix.O_RDONLY; Unix.O_NONBLOCK]
           ~mode:Lwt_io.Input
           ~perm:0o666
           file
       in
       let%lwt contents = Lwt_io.read ic in
       let%lwt () = Lwt_io.close ic in
       Lwt.return_ok contents
     with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_error "File doesn't exist"
    | Unix.Unix_error (Unix.EBADF, _, _) -> Lwt.return_error "Lost fd for the file")

(* I've never seen cgroup mounted elsewhere, so it's probably fine to hardcode this for now *)
let cgroup_dir = "/sys/fs/cgroup"

let assert_is_using_cgroup_v2 =
  Memoize.forever ~f:(fun () ->
      if%lwt Lwt_unix.file_exists cgroup_dir then
        (* /sys/fs/cgroup/memory exists for cgroup v1 but not v2. It's an easy way to tell the
         * difference between versions *)
          if%lwt Lwt_unix.file_exists (spf "%s/memory" cgroup_dir) then
            Lwt.return_error (spf "cgroup v1 is mounted at %s. We need v2" cgroup_dir)
          else
            Lwt.return_ok ()
      else
        Lwt.return_error (spf "%s doesn't exist" cgroup_dir))

(* I don't really expect us to switch cgroups often, but let's only cache for 5 seconds *)
let get_cgroup_name =
  Memoize.until ~seconds:5.0 ~f:(fun () ->
      ProcFS.first_cgroup_for_pid (Unix.getpid ()) |> Lwt.return)

type stats = {
  total: int;
  (* The total physical memory for the cgroup *)
  total_swap: int;
  (* The total amount of anonymous memory paged out to swap *)

  (* anon, file, and shmem are disjoint. If you add in the memory that the kernel uses, they should
   * sum roughly to `total` *)
  anon: int;
  (* The amount of physical anonymous memory not used for shared memory *)
  shmem: int;
  (* The amount of physical anonymous memory being used as shared memory *)
  file: int; (* The amount of physical memory which is not anonymous *)
}

(* Some cgroup files contain only a single integer *)
let read_single_number_file path =
  let%lwt contents_result = cat path in
  Lwt.return
    ( contents_result >>= fun contents ->
      try Ok (contents |> String.strip |> int_of_string)
      with Failure _ -> Error "Failed to parse memory.current" )

let parse_stat stat_contents =
  let stats =
    String.split stat_contents ~on:'\n'
    |> List.fold_left ~init:SMap.empty ~f:(fun stats line ->
           match String.split line ~on:' ' with
           | [key; raw_stat] ->
             int_of_string_opt raw_stat
             |> Base.Option.value_map ~default:stats ~f:(fun stat -> SMap.add key stat stats)
           | _ -> stats)
  in
  let get key =
    match SMap.find_opt key stats with
    | Some stat -> Ok stat
    | None -> Error (spf "Failed to find %S in memory.stat" key)
  in
  get "anon" >>= fun anon ->
  get "file" >>= fun file ->
  get "shmem" >>| fun shmem ->
  (* In `memory.stat` the `file` stat includes `shmem` *)
  (anon, file - shmem, shmem)

let get_stats_for_cgroup (cgroup_name : string) : (stats, string) result Lwt.t =
  (* cgroup_name starts with a /, like /my_cgroup *)
  let dir = spf "%s%s" cgroup_dir cgroup_name in
  let%lwt total_result = read_single_number_file (Filename.concat dir "memory.current")
  and total_swap_result = read_single_number_file (Filename.concat dir "memory.swap.current")
  and stat_contents_result = cat (Filename.concat dir "memory.stat") in
  Lwt.return
    ( total_result >>= fun total ->
      total_swap_result >>= fun total_swap ->
      stat_contents_result >>= fun stat_contents ->
      parse_stat stat_contents >>= fun (anon, file, shmem) ->
      Ok { total; total_swap; anon; file; shmem } )

(* Like Result's >>= but for when you're dealing with result threads *)
let ( >>% ) (type a b c) (thread : (a, b) result Lwt.t) (f : a -> (c, b) result Lwt.t) :
    (c, b) result Lwt.t =
  match%lwt thread with
  | Ok value -> f value
  | Error e -> Lwt.return_error e

let get_stats () = assert_is_using_cgroup_v2 () >>% get_cgroup_name >>% get_stats_for_cgroup
