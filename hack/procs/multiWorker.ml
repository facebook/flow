(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

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
          List.iter begin fun proc ->
            let bucket = next () in
            if bucket = [] then raise Exit;
            incr busy;
            ignore (Worker.call proc 
                      begin fun xl ->
                        job neutral xl
                      end
                      bucket
                   );
          end !procs;
          let ready_procs = Worker.select workers in
          List.iter begin fun proc ->
            let res = Worker.get_result proc (Obj.magic 0) in
            decr busy;
            acc := merge res !acc;
          end ready_procs;
          procs := ready_procs;
        done;
        assert false
      with Exit ->
        while !busy > 0 do
          List.iter begin fun proc ->
            let res = Worker.get_result proc (Obj.magic 0) in
            decr busy;
            acc := merge res !acc;
          end (Worker.select workers);
        done;
        !acc
