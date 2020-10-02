(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type args = {
  loose: bool;
  ranges: Semver_range.t list;
  verbose: bool;
  versions: string list;
}

let parse_range_arg range =
  (try Semver.range_of_string range with Semver.Parse_error msg -> raise (Arg.Bad msg))

let parse_args () =
  let loose = ref false in
  let ranges = ref [] in
  let verbose = ref false in
  let rev_versions = ref [] in
  let speclist =
    [
      ("-l", Arg.Set loose, "Use \"loose\" parsing, allowing versions to start with \"v\"");
      ( "-r",
        Arg.String (fun r -> ranges := parse_range_arg r :: !ranges),
        "Print versions that match this range (if passed multiple times, must pass all ranges)." );
      ("-v", Arg.Set verbose, "Enables verbose mode");
    ]
  in
  let usage_msg = "Usage: semver [options] <version> [<version> [...]]\n\nOptions:" in
  Arg.parse speclist (fun version -> rev_versions := version :: !rev_versions) usage_msg;
  {
    loose = !loose;
    ranges = List.rev !ranges;
    verbose = !verbose;
    versions = List.rev !rev_versions;
  }

let main () =
  let { loose; ranges; verbose; versions = version_strs } = parse_args () in
  let rev_versions =
    List.fold_left
      (fun acc str ->
        let str =
          let len = String.length str in
          if loose && len > 1 && str.[0] = 'v' then
            String.sub str 1 (len - 1)
          else
            str
        in
        try Semver.version_of_string str :: acc
        with Semver.Parse_error msg ->
          if verbose then prerr_endline msg;
          acc)
      []
      version_strs
  in
  let rev_versions =
    List.filter
      (fun version -> List.for_all (fun range -> Semver_range.satisfies range version) ranges)
      rev_versions
  in
  if rev_versions = [] then exit 1;

  let sorted = List.stable_sort Semver_version.compare_precedence rev_versions in
  List.iter (fun ver -> print_endline (Semver_version.to_string ver)) sorted

let () = main ()
