(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type verbose_mode =
  | Quiet
  | Verbose
  | Normal

type error_reason =
  | Missing_parse_error
  | Unexpected_parse_error of (Loc.t * Parse_error.t)
  | Uncaught_exn of string

type test_name = string * bool (* filename * strict *)

type test_result = {
  name: test_name;
  result: (unit, error_reason) result;
}

module SMap = Flow_map.Make (String)

module Progress_bar = struct
  type t = {
    mutable count: int;
    mutable last_update: float;
    total: int;
    chunks: int;
    frequency: float;
  }

  let percentage bar = Printf.sprintf "%3d%%" (bar.count * 100 / bar.total)

  let meter bar =
    let chunks = bar.chunks in
    let c = bar.count * chunks / bar.total in
    if c = 0 then
      String.make chunks ' '
    else if c = chunks then
      String.make chunks '='
    else
      String.make (c - 1) '=' ^ ">" ^ String.make (chunks - c) ' '

  let incr bar = bar.count <- succ bar.count

  let to_string (passed, failed, errored) bar =
    let total = float_of_int (passed + failed) in
    Printf.sprintf
      "\r%s [%s] %d/%d -- Passed: %d (%.2f%%), Failed: %d (%.2f%%), Errored: %d (%.2f%%)%!"
      (percentage bar)
      (meter bar)
      bar.count
      bar.total
      passed
      (float_of_int passed /. total *. 100.)
      failed
      (float_of_int failed /. total *. 100.)
      errored
      (float_of_int errored /. total *. 100.)

  let print status bar = Printf.printf "\r%s%!" (to_string status bar)

  let print_throttled status bar =
    let now = Unix.gettimeofday () in
    if now -. bar.last_update > bar.frequency then (
      bar.last_update <- now;
      print status bar
    )

  let clear status bar =
    let len = String.length (to_string status bar) in
    let spaces = String.make len ' ' in
    Printf.printf "\r%s\r%!" spaces

  let print_final status bar =
    print status bar;
    Printf.printf "\n%!"

  let make ~chunks ~frequency total = { count = 0; last_update = 0.; total; chunks; frequency }
end

let is_fixture =
  let suffix = "FIXTURE.js" in
  let slen = String.length suffix in
  fun filename ->
    let len = String.length filename in
    len >= slen && String.sub filename (len - slen) slen = suffix

let files_of_path path =
  let filter fn = not (is_fixture fn) in
  File_utils.fold_files
    ~file_only:true
    ~filter
    [path]
    (fun kind acc ->
      match kind with
      | File_utils.Dir _dir -> assert false
      | File_utils.File filename -> filename :: acc)
    []
  |> List.rev

let print_name ~strip_root (filename, use_strict) =
  let filename =
    match strip_root with
    | Some root ->
      let len = String.length root in
      String.sub filename len (String.length filename - len)
    | None -> filename
  in
  let strict =
    if use_strict then
      "(strict mode)"
    else
      "(default)"
  in
  let cr =
    if Unix.isatty Unix.stdout then
      "\r"
    else
      ""
  in
  Printf.printf "%s%s %s\n%!" cr filename strict

let print_error err =
  match err with
  | Missing_parse_error -> Printf.printf "  Missing parse error\n%!"
  | Unexpected_parse_error (loc, err) ->
    Printf.printf "  %s at %s\n%!" (Parse_error.PP.error err) (Loc.debug_to_string loc)
  | Uncaught_exn msg -> Printf.printf "  Uncaught exception: %s\n%!" msg

module Frontmatter = struct
  type t = {
    es5id: string option;
    es6id: string option;
    esid: string option;
    features: string list;
    flags: strictness;
    negative: negative option;
  }

  and negative = {
    phase: string;
    type_: string;
  }

  and strictness =
    | Raw
    | Only_strict
    | No_strict
    | Both_strictnesses

  let of_string =
    let start_regexp = Str.regexp "/\\*---" in
    let end_regexp = Str.regexp "---\\*/" in
    let cr_regexp = Str.regexp "\r\n?" in
    let es5id_regexp = Str.regexp "^ *es5id: *\\(.*\\)" in
    let es6id_regexp = Str.regexp "^ *es6id: *\\(.*\\)" in
    let esid_regexp = Str.regexp "^ *esid: *\\(.*\\)" in
    let features_regexp = Str.regexp "^ *features: *\\[\\(.*\\)\\]" in
    let flags_regexp = Str.regexp "^ *flags: *\\[\\(.*\\)\\]" in
    let only_strict_regexp = Str.regexp_string "onlyStrict" in
    let no_strict_regexp = Str.regexp_string "noStrict" in
    let raw_regexp = Str.regexp_string "raw" in
    let negative_regexp = Str.regexp "^ *negative:$" in
    let type_regexp = Str.regexp "^ +type: *\\(.*\\)" in
    let phase_regexp = Str.regexp "^ +phase: *\\(.*\\)" in
    let matched_group regex str =
      let _ = Str.search_forward regex str 0 in
      Str.matched_group 1 str
    in
    let opt_matched_group regex str =
      try Some (matched_group regex str) with
      | Not_found -> None
    in
    let contains needle haystack =
      try
        let _ = Str.search_forward needle haystack 0 in
        true
      with
      | Not_found -> false
    in
    fun source ->
      try
        let start_idx = Str.search_forward start_regexp source 0 + 5 in
        let end_idx = Str.search_forward end_regexp source start_idx in
        let text = String.sub source start_idx (end_idx - start_idx) in
        let text = Str.global_replace cr_regexp "\n" text in
        let es5id = opt_matched_group es5id_regexp text in
        let es6id = opt_matched_group es6id_regexp text in
        let esid = opt_matched_group esid_regexp text in
        let features =
          match opt_matched_group features_regexp text with
          | Some str -> Str.split (Str.regexp ", *") str
          | None -> []
        in
        let flags =
          match opt_matched_group flags_regexp text with
          | Some flags ->
            let only_strict = contains only_strict_regexp flags in
            let no_strict = contains no_strict_regexp flags in
            let raw = contains raw_regexp flags in
            begin
              match (only_strict, no_strict, raw) with
              | (true, false, false) -> Only_strict
              | (false, true, false) -> No_strict
              | (false, false, true) -> Raw
              | _ -> Both_strictnesses (* validate other nonsense combos? *)
            end
          | None -> Both_strictnesses
        in
        let negative =
          if contains negative_regexp text then
            Some { phase = matched_group phase_regexp text; type_ = matched_group type_regexp text }
          else
            None
        in
        Some { es5id; es6id; esid; features; flags; negative }
      with
      | Not_found -> None

  let to_string fm =
    let opt_cons key x acc =
      match x with
      | Some x -> Printf.sprintf "%s: %s" key x :: acc
      | None -> acc
    in
    []
    |> opt_cons "es5id" fm.es5id
    |> opt_cons "es6id" fm.es6id
    |> opt_cons "esid" fm.esid
    |> (fun acc ->
         let flags =
           match fm.flags with
           | Only_strict -> Some "onlyStrict"
           | No_strict -> Some "noStrict"
           | Raw -> Some "raw"
           | Both_strictnesses -> None
         in
         match flags with
         | Some flags -> Printf.sprintf "flags: [%s]" flags :: acc
         | None -> acc)
    |> List.rev
    |> String.concat "\n"

  let negative_phase fm =
    match fm.negative with
    | Some { phase; _ } -> Some phase
    | None -> None
end

let options_by_strictness frontmatter =
  Parser_env.(
    Frontmatter.(
      let o = Parser_env.default_parse_options in
      match frontmatter.flags with
      | Both_strictnesses -> failwith "should have been split by split_by_strictness"
      | Only_strict -> { o with use_strict = true }
      | No_strict
      | Raw ->
        { o with use_strict = false }))

let split_by_strictness frontmatter =
  Frontmatter.(
    match frontmatter.flags with
    | Both_strictnesses ->
      [{ frontmatter with flags = Only_strict }; { frontmatter with flags = No_strict }]
    | Only_strict
    | No_strict
    | Raw ->
      [frontmatter])

let parse_test acc filename =
  let content = Sys_utils.cat filename in
  match Frontmatter.of_string content with
  | Some frontmatter ->
    List.fold_left
      (fun acc frontmatter ->
        let input = (filename, Frontmatter.(frontmatter.flags = Only_strict)) in
        (input, frontmatter, content) :: acc)
      acc
      (split_by_strictness frontmatter)
  | None -> acc

let run_test (name, frontmatter, content) =
  let (filename, use_strict) = name in
  let parse_options =
    {
      Parser_env.enums = true;
      esproposal_class_instance_fields = true;
      esproposal_class_static_fields = true;
      esproposal_decorators = false;
      esproposal_export_star_as = false;
      esproposal_optional_chaining = true;
      esproposal_nullish_coalescing = true;
      types = false;
      use_strict;
    }
  in
  let result =
    try
      let (_ast, errors) =
        Parser_flow.program_file
          ~fail:false
          ~parse_options:(Some parse_options)
          content
          (Some (File_key.SourceFile filename))
      in
      match (errors, Frontmatter.negative_phase frontmatter) with
      | ([], Some ("early" | "parse")) ->
        (* expected a parse error, didn't get it *)
        Error Missing_parse_error
      | (_, Some ("early" | "parse")) ->
        (* expected a parse error, got one *)
        Ok ()
      | ([], Some _)
      | ([], None) ->
        (* did not expect a parse error, didn't get one *)
        Ok ()
      | (err :: _, Some _)
      | (err :: _, None) ->
        (* did not expect a parse error, got one incorrectly *)
        Error (Unexpected_parse_error err)
    with
    | exn ->
      let msg = Printexc.to_string exn in
      Error (Uncaught_exn msg)
  in
  { name; result }

let incr_result (passed, failed, errored) result =
  match result with
  | `Passed -> (succ passed, failed, errored)
  | `Failed -> (passed, succ failed, errored)
  | `Errored -> (passed, failed, succ errored)

let fold_test
    ~verbose
    ~strip_root
    ~bar
    (passed_acc, failed_acc, errored_acc, features_acc)
    (name, frontmatter, content) =
  if verbose then print_name ~strip_root name;
  let passed =
    let { name; result } = run_test (name, frontmatter, content) in
    match result with
    | Ok _ -> `Passed
    | Error err ->
      Base.Option.iter ~f:(Progress_bar.clear (passed_acc, failed_acc, errored_acc)) bar;
      if not verbose then print_name ~strip_root name;
      print_error err;
      (match err with
      | Uncaught_exn _ -> `Errored
      | _ -> `Failed)
  in
  let (passed_acc, failed_acc, errored_acc) =
    incr_result (passed_acc, failed_acc, errored_acc) passed
  in
  let features_acc =
    List.fold_left
      (fun acc name ->
        let feature =
          try SMap.find name acc with
          | Not_found -> (0, 0, 0)
        in
        let feature = incr_result feature passed in
        SMap.add name feature acc)
      features_acc
      frontmatter.Frontmatter.features
  in
  Base.Option.iter
    ~f:(fun bar ->
      Progress_bar.incr bar;
      match passed with
      | `Passed -> Progress_bar.print_throttled (passed_acc, failed_acc, errored_acc) bar
      | _ -> Progress_bar.print (passed_acc, failed_acc, errored_acc) bar)
    bar;
  (passed_acc, failed_acc, errored_acc, features_acc)

let main () =
  let verbose_ref = ref Normal in
  let path_ref = ref None in
  let strip_root_ref = ref false in
  let speclist =
    [
      ("-q", Arg.Unit (fun () -> verbose_ref := Quiet), "Enables quiet mode");
      ("-v", Arg.Unit (fun () -> verbose_ref := Verbose), "Enables verbose mode");
      ("-s", Arg.Set strip_root_ref, "Print paths relative to root directory");
    ]
  in
  let usage_msg = "Runs flow parser on test262 tests. Options available:" in
  Arg.parse speclist (fun anon -> path_ref := Some anon) usage_msg;
  let path =
    match !path_ref with
    | Some ""
    | None ->
      prerr_endline "Invalid usage";
      exit 1
    | Some path ->
      if path.[String.length path - 1] <> '/' then
        path ^ "/"
      else
        path
  in
  let strip_root =
    if !strip_root_ref then
      Some path
    else
      None
  in
  let quiet = !verbose_ref = Quiet in
  let verbose = !verbose_ref = Verbose in
  let files = files_of_path path |> List.sort String.compare in
  let tests = List.fold_left parse_test [] files |> List.rev in
  let test_count = List.length tests in
  let bar =
    if quiet || not (Unix.isatty Unix.stdout) then
      None
    else
      Some (Progress_bar.make ~chunks:40 ~frequency:0.1 test_count)
  in
  let (passed, failed, errored, results_by_feature) =
    List.fold_left (fold_test ~verbose ~strip_root ~bar) (0, 0, 0, SMap.empty) tests
  in
  begin
    match bar with
    | Some bar -> Progress_bar.clear (passed, failed, errored) bar
    | None -> ()
  end;

  let total = float_of_int (passed + failed + errored) in
  Printf.printf "\n=== Summary ===\n";
  Printf.printf "Passed:  %d (%.2f%%)\n" passed (float_of_int passed /. total *. 100.);
  Printf.printf "Failed:  %d (%.2f%%)\n" failed (float_of_int failed /. total *. 100.);
  Printf.printf "Errored: %d (%.2f%%)\n" errored (float_of_int errored /. total *. 100.);

  if not (SMap.is_empty results_by_feature) then (
    Printf.printf "\nFeatures:\n";
    SMap.iter
      (fun name (passed, failed, errored) ->
        if failed > 0 || errored > 0 || verbose then
          let total = passed + failed + errored in
          let total_f = float_of_int total in
          Printf.printf
            "  %s: %d/%d (%.2f%%)\n"
            name
            passed
            total
            (float_of_int passed /. total_f *. 100.))
      results_by_feature
  );

  ()

let () = main ()
