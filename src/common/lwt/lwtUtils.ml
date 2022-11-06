(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Lwt.join is a great way to run multiple threads in parallel. However it has this really annoying
 * property where it won't exit early if one of the threads fails. It's not a big deal if you
 * expect this behavior, but it can be dangerous if you expect the same behavior as Promise.all or
 * Hack's await.
 *
 * We can instead simulate how Lwt.join should work by calling Lwt.nchoose multiple times until
 * one thread throws an exception or until all threads have finished.
 *
 * In the exceptional case, we won't cancel the still-sleeping threads. I (glevi) tried to get this
 * to work, but it wouldn't preserve stack traces. Anyway, Promise.all doesn't cancel running
 * promises either :P
 *)
let rec iter_all threads =
  if threads = [] then
    Lwt.return_unit
  else
    (* If any thread in threads fails during this nchoose, the whole all function will fail *)
    let%lwt (_, sleeping_threads) = Lwt.nchoose_split threads in
    iter_all sleeping_threads

(** Like [iter_all threads], resolves when all promises in [threads] have resolved,
    and rejects as soon as any promise rejects. Additionally, if any promise resolves
    to an [Error], [iter_all_result] immediately resolves to that [Error].

    In both the rejected promise and [Error] cases, the other threads are left alone
    and not canceled. *)
let rec iter_all_result threads =
  if threads = [] then
    Lwt.return (Ok ())
  else
    (* If any thread in threads fails during this nchoose, the whole function will fail *)
    let%lwt (resolved_threads, sleeping_threads) = Lwt.nchoose_split threads in
    match Base.List.find ~f:Base.Result.is_error resolved_threads with
    | Some (Error err) -> Lwt.return (Error err)
    | Some (Ok _)
    | None ->
      iter_all_result sleeping_threads

let get_value_unsafe thread =
  match Lwt.state thread with
  | Lwt.Return x -> x
  | _ -> failwith "Not yet completed"

let all threads =
  let%lwt () = iter_all threads in
  threads |> Base.List.map ~f:get_value_unsafe |> Lwt.return

type ('a, 'b) pair = {
  mutable x1: 'a option;
  mutable x2: 'b option;
}

(** [LwtUtils.both p1 p2] returns a promise that resolves when both [p1] and [p2] have
    resolved. If they are both fulfilled, it returns the pair of their final values.
    If either fails, it is rejected with that exception. If both fail at the same time,
    the result is chosen arbitrarily.

    This differs from [Lwt.both] in that it fails as soon as one of the promises fails,
    instead of waiting for both to resolve before failing.
 *)
let both p1 p2 =
  let pair = { x1 = None; x2 = None } in
  let p1' =
    let%lwt r1 = p1 in
    pair.x1 <- Some r1;
    Lwt.return_unit
  in
  let p2' =
    let%lwt r2 = p2 in
    pair.x2 <- Some r2;
    Lwt.return_unit
  in
  (* if either p1 or p2 rejects, this will cause the whole function to reject *)
  let%lwt () = iter_all [p1'; p2'] in
  match (pair.x1, pair.x2) with
  | (Some r1, Some r2) -> Lwt.return (r1, r2)
  | _ -> assert false

(** [LwtUtils.both_result p1 p2] returns a promise that resolves when both [p1] and
    [p2] have resolved. If they are both [Ok _], it returns an [Ok] of the pair of
    their final values. If either fails, it is rejected with that exception. If either
    returns [Error], it resolves with that [Error]. If both fail or both error at the
    same time, the result is chosen arbitrarily.

    This differs from [Lwt_result.both] in that it fails as soon as one of the promises
    returns [Error], instead of waiting for both to resolve.
 *)
let both_result p1 p2 =
  let pair = { x1 = None; x2 = None } in
  let p1' =
    match%lwt p1 with
    | Ok r1 ->
      pair.x1 <- Some r1;
      Lwt.return (Ok ())
    | Error err -> Lwt.return (Error err)
  in
  let p2' =
    match%lwt p2 with
    | Ok r2 ->
      pair.x2 <- Some r2;
      Lwt.return (Ok ())
    | Error err -> Lwt.return (Error err)
  in
  match%lwt iter_all_result [p1'; p2'] with
  | Error err -> Lwt.return (Error err)
  | Ok () ->
    (match (pair.x1, pair.x2) with
    | (Some r1, Some r2) -> Lwt.return (Ok (r1, r2))
    | _ -> assert false)

let output_graph out strip_root graph =
  let%lwt () = Lwt_io.fprint out "digraph {\n" in
  let%lwt () =
    Lwt_list.iter_s
      (fun (f, dep_fs) ->
        Lwt_list.iter_s
          (fun dep_f -> Lwt_io.fprintf out "  \"%s\" -> \"%s\"\n" (strip_root f) (strip_root dep_f))
          dep_fs)
      graph
  in
  Lwt_io.fprint out "}"

(** [fold_result_s ~f ~init l] calls [f init x] for each [x] in [l], where [f] returns an
    ['acc result Lwt.t], and the fold short circuits if an [Error] is returned. Each
    promise returned by [f] is resolved sequentially (hence the [_s]), and if any
    promise rejects, the entire fold rejects. This is like a combination of
    [Base.List.fold_result] and [Lwt_list.fold_left_s]. *)
let rec fold_result_s ~f ~init l =
  match l with
  | [] -> Lwt.return (Ok init)
  | x :: l ->
    (match%lwt f init x with
    | Ok init -> (fold_result_s [@ocaml.tailcall]) ~f ~init l
    | Error _ as err -> Lwt.return err)
