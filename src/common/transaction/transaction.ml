(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type mutator = {
  commit: unit -> unit Lwt.t;
  rollback: unit -> unit Lwt.t;
}

type t = { mutable mutators: mutator list }

let singleton_mutators = ref SSet.empty

let add_singleton name =
  if SSet.mem name !singleton_mutators then
    failwith (Printf.sprintf "Mutator %S is a singleton. Cannot create a second copy" name);
  singleton_mutators := SSet.add name !singleton_mutators

let remove_singleton name = singleton_mutators := SSet.remove name !singleton_mutators

(* Mutators generally need to copy the old data somewhere so that it can be restored on rollback.
 * Sometimes that somewhere is a local variable, and it's ok to have multiple of the same kind of
 * mutator.
 *
 * However, some mutators do stuff like oldifying shared memory data. If they operate on the same
 * keys, they might interfere with each other. So they can register themselves as singletons and
 * we'll enforce that no two singleton mutators are active at the same time *)
let add ?singleton ~commit ~rollback transaction =
  let (commit, rollback) =
    match singleton with
    | Some singleton_name ->
      add_singleton singleton_name;
      let commit () =
        remove_singleton singleton_name;
        commit ()
      in
      let rollback () =
        remove_singleton singleton_name;
        rollback ()
      in
      (commit, rollback)
    | None -> (commit, rollback)
  in
  transaction.mutators <- { commit; rollback } :: transaction.mutators

let commit transaction = Lwt_list.iter_s (fun mutator -> mutator.commit ()) transaction.mutators

let rollback transaction = Lwt_list.iter_s (fun mutator -> mutator.rollback ()) transaction.mutators

let with_transaction f =
  let transaction = { mutators = [] } in
  let%lwt result =
    try%lwt f transaction with
    | exn ->
      let exn = Exception.wrap exn in
      let%lwt () = rollback transaction in
      Exception.reraise exn
  in
  let%lwt () = commit transaction in
  Lwt.return result
