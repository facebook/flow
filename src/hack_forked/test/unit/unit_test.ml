(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

exception Expected_throw_missing

exception Thrown_exception_mismatched of (exn * exn)

let expect_throws e f x =
  try
    let _ = f x in
    Caml.Printf.eprintf "Error. Did not throw expected: %s\n" (Exn.to_string e);
    false
  with
  | err ->
    if Poly.(e <> err) then
      let () =
        Caml.Printf.eprintf
          "Error. Expected exn: %s. But got : %s\n"
          (Exn.to_string e)
          (Exn.to_string err)
      in
      false
    else
      true

let run (name, f) =
  Caml.Printf.printf "Running %s ... %!" name;
  let result =
    try f () with
    | e ->
      let exn = Exception.wrap e in
      let () = Caml.Printf.printf "Exception %s\n" (Exception.get_ctor_string exn) in
      let () = Caml.Printf.printf "Backtrace %s\n" (Exception.get_backtrace_string exn) in
      false
  in
  if result then
    Caml.Printf.printf "ok\n%!"
  else
    Caml.Printf.printf "fail\n%!";
  result

(** List.for_all but without shortcircuiting "&&", so runs all failures too. *)
let for_all_non_shortcircuit tests f =
  List.fold_left tests ~init:true ~f:(fun acc test -> f test && acc)

let run_all (tests : (string * (unit -> bool)) list) =
  Exception.record_backtrace true;
  Caml.exit
    ( if for_all_non_shortcircuit tests run then
      0
    else
      1
    )

let run_only tests names =
  let f =
    match names with
    | [] -> (fun _ -> true)
    | _ -> (fun (n, _) -> List.mem names n ~equal:Poly.( = ))
  in
  let tests = List.filter tests ~f in
  run_all tests

let main tests =
  let names = Option.value (List.tl @@ Array.to_list (Sys.get_argv ())) ~default:[] in
  run_only tests names
