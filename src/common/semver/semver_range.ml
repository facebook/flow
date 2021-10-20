(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type part =
  | Comparator of Semver_comparator.t
  | Caret of Semver_version.t

(* TODO: support unions (`||`), like this:
   type comparator_set = part list
   type t = comparator_set list
*)
type t = part list

let expand_caret version =
  Semver_version.(
    let upper =
      match version with
      | { major = 0; minor = 0; _ } -> incr_patch version
      | { major = 0; _ } -> incr_minor version
      | _ -> incr_major version
    in
    [
      Semver_comparator.{ op = Some GreaterOrEqual; version };
      Semver_comparator.{ op = Some Less; version = upper };
    ]
  )

let fold_comparators_of_range f acc t =
  List.fold_left
    (fun acc part ->
      match part with
      | Comparator c -> f acc c
      | Caret v -> List.fold_left f acc (expand_caret v))
    acc
    t

let comparators_of_range t : Semver_comparator.t list =
  let comparators = fold_comparators_of_range (fun acc comp -> comp :: acc) [] t in
  List.rev comparators

(* Determines if the version is matched by the range.
 *
 * If the range and the version both have a prerelease, then they must be for the same
 * version (major, minor, patch). for example, `>1.2.3-alpha` matches `1.2.3-beta` and
 * `1.2.4`, but not `1.2.4-alpha`. this is so that opting into one prerelease version
 * does not also opt you into all future prereleases. this behavior can be overridden
 * with `~include_prereleases:true`.
 *)
let satisfies ?(include_prereleases = false) range version =
  Semver_version.(
    Semver_comparator.(
      let satisfied =
        fold_comparators_of_range
          (fun acc comp ->
            if not acc then
              acc
            else
              Semver_comparator.satisfies version comp)
          true
          range
      in
      if not satisfied then
        false
      else
        let { major; minor; patch; prerelease; build = _ } = version in
        if prerelease = [] || include_prereleases then
          true
        else
          fold_comparators_of_range
            (fun acc { version = allowed; op = _ } ->
              if acc then
                acc
              else
                match allowed with
                | { major = major'; minor = minor'; patch = patch'; prerelease = _ :: _; build = _ }
                  ->
                  major = major' && minor = minor' && patch = patch'
                | _ -> false)
            false
            range
    )
  )

let string_of_part = function
  | Comparator c -> Semver_comparator.to_string c
  | Caret ver -> "^" ^ Semver_version.to_string ver

let to_string t = t |> List.map string_of_part |> String.concat " "
