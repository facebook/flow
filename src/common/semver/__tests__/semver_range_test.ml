(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let v0_0_1 = Semver_version.({ zero with major = 0; minor = 0; patch = 1 })
let v0_0_2 = Semver_version.({ zero with major = 0; minor = 0; patch = 2 })
let v0_1_0 = Semver_version.({ zero with major = 0; minor = 1; patch = 0 })
let v0_1_2 = Semver_version.({ zero with major = 0; minor = 1; patch = 2 })
let v0_2_0 = Semver_version.({ zero with major = 0; minor = 2; patch = 0 })

let v1 = Semver_version.({ zero with major = 1 })
let v1_2_0 = Semver_version.({ zero with major = 1; minor = 2; patch = 0 })
let v1_2_3 = Semver_version.({ zero with major = 1; minor = 2; patch = 3 })
let v1_2_3_alpha_3 = Semver_version.({ zero with
  major = 1; minor = 2; patch = 3; prerelease = [Str "alpha"; Int 3]
})
let v1_2_3_alpha_7 = Semver_version.({ zero with
  major = 1; minor = 2; patch = 3; prerelease = [Str "alpha"; Int 7]
})
let v1_2_4 = Semver_version.({ zero with major = 1; minor = 2; patch = 4 })

let v2 = Semver_version.({ zero with major = 2 })

let v3_4_5_alpha_9 = Semver_version.({ zero with
  major = 3; minor = 4; patch = 5; prerelease = [Str "alpha"; Int 9]
})

let ge version = Semver_comparator.({ op = Some GreaterOrEqual; version })
let lt version = Semver_comparator.({ op = Some Less; version })

let string_of_comparators comparators =
  comparators
  |> List.map Semver_comparator.to_string
  |> String.concat " "

let tests = "range" >::: [
  "comparators_of_range" >:: begin fun ctxt ->
    let open Semver_range in
    let cases = [
      [Caret v1], [ge v1; lt v2];
      [Caret v1_2_0], [ge v1_2_0; lt v2];
      [Caret v1_2_3], [ge v1_2_3; lt v2];

      (* when major = 0, minor acts like the major version *)
      [Caret v0_1_0], [ge v0_1_0; lt v0_2_0];
      [Caret v0_1_2], [ge v0_1_2; lt v0_2_0];

      (* when major = 0 and minor = 0, patch acts like the major version *)
      [Caret v0_0_1], [ge v0_0_1; lt v0_0_2];
    ] in

    List.iter (fun (input, expected) ->
      assert_equal ~ctxt ~printer:string_of_comparators expected (comparators_of_range input);
    ) cases;
    assert_bool "done" true; (* fixes ounit error reporting *)
  end;

  "satisfies" >:: begin fun ctxt ->
    let open Semver_range in
    let cases = [
      [Caret v1], v1, true;
      [Caret v1], v2, false;
      [Comparator (ge v1_2_3_alpha_3)], v1_2_3_alpha_7, true;
      [Comparator (ge v1_2_3_alpha_3)], v3_4_5_alpha_9, false; (* prereleases from diff versions *)
      [Caret v1_2_3_alpha_7], v1_2_3_alpha_3, false;
      [Caret v1_2_3_alpha_7], v1_2_3_alpha_7, true;
      [Caret v1_2_3_alpha_7], v1_2_3, true;
      [Caret v1_2_3_alpha_7], v1_2_4, true;
    ] in
    List.iter (fun (range, version, expected) ->
      let msg = Printf.sprintf "Expected %s %sto satisfy %s"
        (Semver_version.to_string version)
        (if expected then "" else "NOT ")
        (Semver_range.to_string range)
      in
      assert_equal ~ctxt ~msg expected (satisfies range version);
    ) cases;
    assert_bool "done" true; (* fixes ounit error reporting *)
  end;
]
