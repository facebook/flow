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

type 'a bucket =
| Job of 'a list
| Wait

type 'a nextlist_dynamic =
  unit -> 'a bucket

let single_threaded_call_dynamic job merge neutral next =
  let x = ref (next()) in
  let acc = ref neutral in
  (* This is a just a sanity check that the job is serializable and so
   * that the same code will work both in single threaded and parallel
   * mode.
   *)
  let _ = Marshal.to_string job [Marshal.Closures] in
  while !x <> Job [] do
    match !x with
    | Wait ->
        (* this state should never be reached in single threaded mode, since
           there is no hope for ever getting out of this state *)
        failwith "stuck!"
    | Job l ->
        let res = job neutral l in
        acc := merge !acc res;
        x := next()
  done;
  !acc

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
            | Wait -> waiting_procs := proc::!waiting_procs (* wait *)
            | Job bucket -> (
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

(* special case of call_dynamic with no waiting, useful for static worklists *)
type 'a nextlist =
  unit -> 'a list

let call workers ~job ~merge ~neutral ~next =
  let next = fun () -> Job (next ()) in
  call_dynamic workers ~job ~merge ~neutral ~next
