(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(**
 * The Measure module is primarily useful for debugging. It's particularly
 * useful for gathering stats about something that happens a lot. Let's say you
 * have some code like this
 *
 *   let number_bunnies = count_bunnies () in
 *
 * If you want to debug how many bunnies are being counted, you could do
 * something like
 *
 *   let number_bunnies = count_bunnies () in
 *   Utils.prerr_endlinef "Num bunnies: %d" number_bunnies;
 *
 * but what if this code is called 1000 times? Then you end up with log spew.
 * Using the Measure module helps with this. You can now do
 *
 *   let number_bunnies = count_bunnies () in
 *   Measure.sample "num_bunnies" number_bunnies;
 *
 * and then later you do
 *
 *   Measure.print_stats ();
 *
 * which will print the number of samples, the total, the average, the
 * variance, the max and the min.
 *
 * Measure can keep track of the distribution of measurements if you give it a
 * bucket size. Before we collect our measurements, call
 *
 *   Measure.track_distribution "num_bunnies" ~bucket_size:10 =
 *   ...do logging
 *   Measure.print_distribution ();
 *
 * And this will print how many samples fall in the 0-9 bucket, how many fall
 * into the 10-19 bucket, etc
 *
 * A common use case is timing, and there's an easy helper method. Let's say we
 * wanted to see how long our code takes
 *
 *   let number_bunnies = Measure.time "count_bunnies_time" (fun () ->
 *     count_bunnies ()
 *   ) in
 *
 * now when we call print_stats we'll see how fast count_bunnies is and how
 * much total time we spend counting bunnies.
 *
 * Measurements are stored in a stateful way in a record. You can either use a
 * global record or a local record.
 *
 * Using a global record:
 *   Measure.sample "num_bunnies" number_bunnies;
 *   Measure.print_stats ();
 *
 * You can push and pop the global record. This is useful if you want to reset
 * some counters without throwing away that data
 *
 *   Measure.push_global ();
 *   ...measure stuff
 *   let record = Measure.pop_global () in
 *   Measure.print_stats ~record ();
 *
 * Using a local record:
 *   let record = Measure.create () in
 *   Measure.sample ~record "num_bunnies" number_bunnies;
 *   Measure.print_stats ~record ();
 *
 * A record does not store the individual measurements, just the aggregate
 * stats, which are updated online. Records can be serialized in order to be
 * sent across pipes.
 *)

module List = Core.List
module SMap = Utils.SMap
module FloatMap = Utils.MyMap(struct type t = float let compare = compare end)

type distribution = {
  bucket_size: float;
  buckets: int FloatMap.t;
}

type record_entry = {
  count: int;
  mean: float;
  variance_sum: float;
  max: float;
  min: float;
  distribution: distribution option;
}
type record_data = record_entry SMap.t
type record = record_data ref

(* Creates a new empty record *)
let create () = ref SMap.empty

let global: (record list) ref = ref [create ()]
let push_global record =
  global := (create ()) :: (!global)
let pop_global () =
  match !global with
  | ret::globals ->
      global := globals;
      ret
  | _ -> failwith "Measure.pop_global called with empty stack"


let serialize record = !record
let deserialize data = ref data

let new_entry = {
  count = 0;
  mean = 0.0;
  variance_sum = 0.0;
  max = min_float;
  min = max_float;
  distribution = None;
}

let new_distribution ~bucket_size = Some {
  bucket_size;
  buckets = FloatMap.empty;
}

let get_record = function
  | Some record -> record
  | None -> (match List.hd (!global) with
      | Some record -> record
      | None ->
          failwith ("No global record available! " ^
            "Did you forget to call Measure.push_global?"))


(* Measure can track how the values are distributed by creating buckets and
 * keeping track of how many samples fall into each buckets. It will not track
 * distribution by default, so call this function to turn it on *)
let track_distribution ?record name ~bucket_size =
  let record = get_record record in
  let entry = match SMap.get name (!record) with
  | None -> new_entry
  | Some entry -> entry in
  let entry = { entry with distribution = new_distribution ~bucket_size; } in
  record := SMap.add name entry (!record)

let round_down ~bucket_size value =
  bucket_size *. (floor (value /. bucket_size))

let update_distribution value = function
  | None -> None
  | Some { bucket_size; buckets } ->
      let bucket = round_down ~bucket_size value in
      let bucket_count = match FloatMap.get bucket buckets with
      | None -> 1
      | Some count -> count + 1 in
      let buckets = FloatMap.add bucket bucket_count buckets in
      Some { bucket_size; buckets; }

let sample ?record name value =
  let record = get_record record in
  let {
    count;
    mean = old_mean;
    variance_sum;
    max;
    min;
    distribution;
  } = match SMap.get name (!record) with
    | None -> new_entry
    | Some entry -> entry in

  let count = count + 1 in
  let mean = old_mean +. ((value -. old_mean) /. (float count)) in
  (* Knuth's online variance approximation algorithm *)
  let variance_sum = variance_sum +. (value -. old_mean) *. (value -. mean) in

  let max = Pervasives.max max value in
  let min = Pervasives.min min value in

  let distribution = update_distribution value distribution in

  let entry = { count; mean; variance_sum; max; min; distribution; } in
  record := SMap.add name entry (!record)

let merge_entries name from into = match (from, into) with
  | None, into -> into
  | from, None -> from
  | Some from, Some into ->
      let count = from.count + into.count in

      (* Using this algorithm to combine the variance sums
       * https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm
       *)
      (* d = meanB - meanA *)
      let delta = from.mean -. into.mean in
      (* mean = meanA + delta * (countB/count) *)
      let mean = into.mean +. (delta *. (float from.count) /. (float count)) in
      (* VarSum = VarSumA + VarSumB + delta * delta * countA * countB / count *)
      let variance_sum = from.variance_sum +. into.variance_sum +.
        delta *. delta *. (float into.count) *. (float from.count) /. (float count) in

      let max = Pervasives.max from.max into.max in
      let min = Pervasives.min from.min into.min in

      let distribution = match (from.distribution, into.distribution) with
      | None, into -> into
      | from, None -> from
      | Some { bucket_size = from; _; }, Some { bucket_size = into; _; }
      when from <> into -> Printf.kprintf failwith
          "Merging buckets for %s failed: bucket sizes %f, %f"
          name from into
      | Some { bucket_size; buckets = from; }, Some { buckets = into; _; } ->
          let buckets = FloatMap.merge (fun bucket from_count into_count ->
            match (from_count, into_count) with
            | None, into -> into
            | from, None -> from
            | Some from_count, Some into_count -> Some (from_count + into_count))
          from into in
          Some { bucket_size; buckets; } in
      Some { count; mean; variance_sum; max; min; distribution; }

(* Merges all the samples from "from" into "record". If "record" is omitted
 * then it uses the global record *)
let merge ?record ~from =
  let into = get_record record in
  into := SMap.merge merge_entries (!from) (!into)

let time (type a) ?record name (f: unit -> a) : a =
  let record = get_record record in
  let start_time = Unix.gettimeofday () in
  let ret = f () in
  let end_time = Unix.gettimeofday () in
  sample ~record name (end_time -. start_time);
  ret

let print_entry_stats ?record name =
  let record = get_record record in
  Printf.eprintf "%s stats -- " name;
  match SMap.get name (!record) with
  | None
  | Some { count = 0; _; } -> prerr_endline "NO DATA"
  | Some { count; mean; variance_sum; max; min; distribution=_; } ->
      let total = (float count) *. mean in
      let std_dev = sqrt (variance_sum /. (float count)) in
      Utils.prerr_endlinef
        "samples: %d, total: %f, avg: %f, stddev: %f, max: %f, min: %f)"
        count total mean std_dev max min

let print_stats ?record () =
  let record = get_record record in
  SMap.iter (fun name _ -> print_entry_stats ~record name) (!record)

let rec print_buckets ~low ~high ~bucket_size buckets =
  if low <= high
  then begin
    let count = match FloatMap.get low buckets with
    | None -> 0
    | Some count -> count in
    Printf.eprintf "[%02f: %03d]  " low count;
    let low = low +. bucket_size in
    print_buckets ~low ~high ~bucket_size buckets
  end

let print_entry_distribution ?record name =
  let record = get_record record in
  Printf.eprintf "%s distribution -- " name;
  match SMap.get name (!record) with
  | None
  | Some { count = 0; _; } -> prerr_endline "NO DATA"
  | Some { distribution = None; _; } ->
      prerr_endline "NO DATA (did you forget to call track_distribution?)"
  | Some { max; min; distribution = Some { bucket_size; buckets; }; _; } ->
      let low = round_down ~bucket_size min in
      let high = round_down ~bucket_size max in
      print_buckets ~low ~high ~bucket_size buckets;
      prerr_newline ()

let print_distributions ?record () =
  let record = get_record record in
  SMap.iter (fun name { distribution; _; } -> match distribution with
    | None -> ()
    | Some _ -> print_entry_distribution ~record name) (!record)
