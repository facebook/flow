(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

type 'a nextlist =
  unit -> 'a list

let single_threaded_call job merge neutral next =
  let x = ref (next()) in
  let acc = ref neutral in
  (* This is a just a sanity check that the job is serializable and so
   * that the same code will work both in single threaded and parallel
   * mode.
   *)
  let _ = Marshal.to_string job [Marshal.Closures] in
  while !x != [] do
    acc := job !acc !x;
    x := next()
  done;
  !acc

let call workers ~job ~merge ~neutral ~next =
  match workers with
  | None -> single_threaded_call job merge neutral next
  | Some workers ->
      let acc = ref neutral in
      let procs = ref workers in
      let busy = ref 0 in
      try
        while true do
          List.iter !procs begin fun proc ->
            let bucket = next () in
            if bucket = [] then raise Exit;
            incr busy;
            ignore (Worker.call proc
                      begin fun xl ->
                        job neutral xl
                      end
                      bucket
                   );
          end;
          let ready_procs = Worker.select workers in
          List.iter ready_procs begin fun proc ->
            let res = Worker.get_result proc (Obj.magic 0) in
            decr busy;
            acc := merge res !acc;
          end;
          procs := ready_procs;
        done;
        assert false
      with Exit ->
        while !busy > 0 do
          List.iter (Worker.select workers) begin fun proc ->
            let res = Worker.get_result proc (Obj.magic 0) in
            decr busy;
            acc := merge res !acc;
          end;
        done;
        !acc

type 'a nextlist_dynamic =
  unit -> 'a list option

(* Mostly a duplicate of the function `single_threaded_call` above, except that
   this function runs job and then merge separately, whereas the other function
   only runs job assuming that merge satisfies the equation:

   job acc list = merge acc (job neutral list)

   Running merge is especially important for dynamically changing workloads
   because it provides perhaps the only hook to trigger such changes!
*)
let single_threaded_call_dynamic job merge neutral next =
  let x = ref (next()) in
  let acc = ref neutral in
  (* This is a just a sanity check that the job is serializable and so
   * that the same code will work both in single threaded and parallel
   * mode.
   *)
  let _ = Marshal.to_string job [Marshal.Closures] in
  while !x != Some [] do
    match !x with
    | None ->
        (* this state should never be reached in single threaded mode, since
           there is no hope for ever getting out of this state *)
        failwith "stuck!"
    | Some l ->
      let res = job neutral l in
        acc := merge !acc res;
        x := next()
  done;
  !acc

(* Adaptation of the function `call` above to handle dynamic workloads. *)
let call_dynamic workers ~job ~merge ~neutral ~next =
  match workers with
  | None -> single_threaded_call_dynamic job merge neutral next
  | Some workers ->
    let acc = ref neutral in
    let procs = ref workers in
    let busy = ref 0 in
    let waiting_procs = ref [] in
      try
        while true do
          List.iter !procs begin fun proc ->
            let bucket_opt = next () in
            match bucket_opt with
            | None -> waiting_procs := proc::!waiting_procs (* wait *)
            | Some bucket -> (
              if bucket = [] then raise Exit;
              incr busy;
              ignore (Worker.call proc
                        begin fun xl ->
                          job neutral xl
                        end
                        bucket
              );
            )
          end;
          let ready_procs = Worker.select workers in
          List.iter ready_procs begin fun proc ->
            let res = Worker.get_result proc (Obj.magic 0) in
            decr busy;
            acc := merge res !acc
          end;
          if ready_procs = [] then
            (* nothing changed, so no use calling next with the waiting procs,
               since they would end up waiting again *)
            procs := []
          else (
            (* call next with read_procs and waiting_procs *)
            procs := List.rev_append !waiting_procs ready_procs;
            waiting_procs := [];
          )
        done;
        assert false
      with Exit ->
        while !busy > 0 do
          List.iter (Worker.select workers) begin fun proc ->
            let res = Worker.get_result proc (Obj.magic 0) in
            decr busy;
            acc := merge res !acc;
          end;
        done;
        !acc
