(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)



let pipe() =
  let ic, oc = Unix.pipe() in
  Unix.in_channel_of_descr ic,
  Unix.out_channel_of_descr oc

let parallel: int -> (string -> 'b -> 'b) -> string list -> 'b ->
  ('b -> 'b -> 'b) -> 'b = fun 
    (procs: int) (f : string -> 'b -> 'b) (args: string list) (neutral: 'b) 
      (merge: 'b -> 'b -> 'b) ->
  let (slaves: (in_channel * out_channel * in_channel) list ref) = ref [] in
  Printf.printf "STARTING parallel\n"; flush stdout;
  if List.length args < 200 && false
  then
    let l = List.map (fun x -> f x neutral) args in
    List.fold_left begin fun acc x ->
      merge acc x
    end neutral l 
  else begin
    for i = 0 to procs - 1 do
      let input_status, output_status = pipe() in
      let input_task, output_task = pipe() in 
      let input_result, output_result = pipe() in
      match Unix.fork() with
      | -1 -> assert false
      | 0 ->
          let t = Unix.time() in
          close_in input_status;
          close_out output_task;
          close_in input_result;
          let acc = ref neutral in
          begin try
            while true do
              output_string output_status "READY\n"; 
              flush output_status;
              let msg = input_line input_task in
              match msg with
              | "STOP" ->
                  close_out output_status;
                  Marshal.to_channel output_result !acc [];
                  close_out output_result;
                  Printf.printf "Time %f\n" (Unix.time() -. t); flush stdout;
                  exit 0
              | "GO" ->
                  let task_size = int_of_string (input_line input_task) in
                  Printf.printf "Task received\n"; flush stdout;
                  let filel = ref [] in
                  for i = 0 to task_size - 1 do
                    let file = input_line input_task in
                    filel := file :: !filel;
                  done;
                  List.iter begin fun file ->
                    acc := f file !acc;
                  end !filel;
              | msg -> failwith ("bad message: "^msg)
            done;
            assert false
          with End_of_file ->
            failwith "The worker failed"
          end
      | pid ->
          close_out output_result;
          slaves := (input_status, output_task, input_result) :: !slaves
    done;
    try
      Printf.printf "ARGS %d\n" (List.length args); flush stdout;
      if args = [] then raise Exit;
      let todo = ref args in
      let workers_fdl = List.map begin fun (ic, _, _) -> 
        Unix.descr_of_in_channel ic end !slaves in
      while true do
        let _, _, _ = Unix.select workers_fdl [] [] 0.0 in
        List.iter begin fun (ic_status, oc_task, _) ->
          let fdl = [Unix.descr_of_in_channel ic_status] in
          let readyl, _, _ = Unix.select fdl [] [] (-1.0) in
          if readyl <> [] then
            let msg = input_line ic_status in
            assert (msg = "READY");
            let task = ref [] in
            let bucket_size = 400 * 1000 in
            let task_size = ref bucket_size in
            let finished = ref false in
            while !task_size > 0 do
              match !todo with
              | [] -> 
                  finished := true;
                  task_size := 0
              | file :: rl ->
                  let file_size = (Unix.stat file).Unix.st_size in
                  task_size := !task_size - file_size;
                  task := file :: !task;
                  todo := rl
            done;
            let size = List.length !task in
            if size > 0 then begin
              output_string oc_task "GO\n";
              flush oc_task;
              output_string oc_task (string_of_int size);
              output_string oc_task "\n";
              List.iter begin fun file ->
                output_string oc_task file;
                output_string oc_task "\n";
              end !task;
              flush oc_task;
            end;
            if !finished 
            then raise Exit
            else ()
        end !slaves
      done;
      assert false
    with Exit ->
      let t = Unix.time() in
      List.iter begin fun (ic_status, otask, iresult) ->
        let msg = input_line ic_status in
        assert (msg = "READY");
        output_string otask "STOP\n";
        close_out otask;
      end !slaves;
      let results = List.map begin fun (_, otask, iresult) ->
        Marshal.from_channel iresult
      end !slaves in
      Printf.printf "Time master %f\n" (Unix.time() -. t); flush stdout;
      List.fold_right begin fun acc' acc ->
        let acc = merge acc' acc in
        acc
      end results neutral
  end
