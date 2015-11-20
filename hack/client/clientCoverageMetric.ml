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
open Coverage_level
open Hh_json
open Utils

let result_to_json r = JSON_Object begin
  List.map (SMap.elements r) begin fun (kind, counts) ->
    let counts = JSON_Object (List.map (CLMap.elements counts)
        (fun (k, v) -> string_of_level k, int_ v.count)) in
    kind, counts
  end
end

let rec entry_to_json = function
  | Leaf r -> JSON_Object [
      "type"   , JSON_String "file";
      "result" , result_to_json r;
    ]
  | Node (r, el) -> JSON_Object [
      "type"     , JSON_String "directory";
      "result"   , result_to_json r;
      "children" , JSON_Object (SMap.elements (SMap.map entry_to_json el));
    ]

let print_json r_opt =
  let json = match r_opt with
  | Some e -> entry_to_json e
  | None -> JSON_Object [ "internal_error", JSON_Bool true ]
  in print_string (json_to_string json)

(* Calculate the percentage of code we have covered as a ratio of typed
 * expressions : total expressions. partial_weight is a number between
 * 0 and 1. *)
let calc_percentage partial_weight ctr =
  let total = CLMap.fold (fun k v acc -> v.count + acc) ctr 0 in
  let mult = function
    | Unchecked -> 0.0
    | Partial -> partial_weight
    | Checked -> 1.0
  in
  let score = CLMap.fold
    (fun k v acc -> mult k *. float_of_int v.count +. acc) ctr 0.0 in
  if total = 0
  then 1.0
  else score /. float_of_int total

let print_reasons reasons_stats =
  if Coverage_level.sample_rate = 0 then () else
  let reasons_list = List.map
    (SMap.elements reasons_stats)
    (fun (reason, pos_map) ->
      (reason,
       pos_map,
       Pos.Map.fold (fun _ x acc -> acc + x.pos_count) pos_map 0)
    ) in
  let sorted_reasons = List.sort
    (fun (_ , _, x) (_, _, y) -> y - x) reasons_list in
  List.iter sorted_reasons (fun (r, pos_map, count) ->
    Printf.printf "  Reason %s: %d\n" r count;
    Printf.printf "    Reason position:\n";
    let pos_list = List.sort
      (fun (_, p1) (_, p2) -> p2.pos_count - p1.pos_count)
      (Pos.Map.elements pos_map) in
    let pos_list = List.take pos_list Coverage_level.display_limit in
    List.iter pos_list (fun (p, pos_stats) ->
      Printf.printf "    %s %d\n"
        (Pos.string (Pos.to_relative_string p)) pos_stats.pos_count;
        Printf.printf "      Reason samples:\n";
          List.iter pos_stats.samples (fun p ->
          Printf.printf "      %s\n" (Pos.string (Pos.to_relative_string p))
      )
    )
  )

let print_counts counts =
  CLMap.iter (fun k v ->
    let level_name = String.capitalize (string_of_level k) in
    Printf.printf "%s: %d\n" level_name v.count;
    print_reasons v.reason_stats) counts;
  Printf.printf "Checked / Total: %f\n" (calc_percentage 0.0 counts);
  Printf.printf "(Checked + Partial * 0.5) / Total: %f\n"
    (calc_percentage 0.5 counts)

let print_pretty_entry = function
  | Leaf r
  | Node (r, _) ->
      if r = SMap.empty
      then Printf.printf "No relevant expressions found!\n%!"
      else begin
        SMap.iter (fun kind counts ->
          Printf.printf "== %s ==\n" kind;
          print_counts counts) r;
        let total_counts = SMap.fold (fun _ counts acc ->
          merge_and_sum counts acc) r empty_counter in
        Printf.printf "== total ==\n";
        print_counts total_counts;
        flush stdout
      end

let print_pretty = function
  | None -> Printf.fprintf stderr "Internal error\n%!"
  | Some e -> print_pretty_entry e

let go ~json = if json then print_json else print_pretty
