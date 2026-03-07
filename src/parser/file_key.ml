(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* File_key.t stores relative path suffixes internally, following Hack's
   Relative_path.t approach. The project root and flowlib root are set once
   at startup via global refs. to_string returns the absolute path on demand.

   LibFile paths under the flowlib root use a marker prefix internally to
   distinguish them from project-root LibFiles. This marker is stripped in
   to_string/to_absolute and replaced with the flowlib root.

   IMPORTANT: The global root refs (project_root, flowlib_root) are plain
   OCaml ref cells. This is safe with fork-based MultiWorkerLwt workers
   (each gets a copy-on-write snapshot). If OCaml 5 domains are ever
   introduced, these must become domain-local or atomic. *)

type t =
  | LibFile of string
  | SourceFile of string
  | JsonFile of string
  (* A resource that might get required, like .css, .jpg, etc. We don't parse
     these, just check that they exist *)
  | ResourceFile of string
[@@deriving show, eq]

(* Marker for flowlib LibFile paths. This is the single source of truth —
   saved_state.ml uses File_key.flowlib_marker, not a separate constant. *)
let flowlib_marker = "<BUILTIN_FLOW_LIB>///"

(* Global root refs — set once at startup, before any File_key construction
   or to_string calls. All roots have enforced trailing slashes so that
   strip_prefix always splits at a directory boundary.

   Using [string option ref] (not [string ref]) so that "not yet initialized"
   is distinguishable from "set to empty". Following Hack's Relative_path
   convention: accessing an unset root raises an immediate, descriptive error
   rather than silently producing a broken path.

   See set_project_root/set_flowlib_root for the setters. *)
let project_root = ref None

let flowlib_root = ref None

let enforce_trailing_sep ~dir_sep s =
  let len = String.length s in
  if len > 0 && s.[len - 1] <> '/' && s.[len - 1] <> '\\' then
    s ^ dir_sep
  else
    s

let resolve_root_with ~is_relative get_root suffix =
  let len = String.length suffix in
  if len = 0 || suffix = "-" then
    suffix
  else if not (is_relative suffix) then
    suffix
  else
    get_root () ^ suffix

let enforce_trailing_slash s = enforce_trailing_sep ~dir_sep:Filename.dir_sep s

let set_project_root root = project_root := Some (enforce_trailing_slash root)

let set_flowlib_root root = flowlib_root := Some (enforce_trailing_slash root)

(* Retrieve a root, crashing immediately if not set. Following Hack's
   Relative_path convention: an unset root is a programmer error that
   should be caught at the call site, not masked downstream. *)
let get_project_root () =
  match !project_root with
  | Some r -> r
  | None -> failwith "File_key: project_root has not been set"

let get_flowlib_root () =
  match !flowlib_root with
  | Some r -> r
  | None -> failwith "File_key: flowlib_root has not been set"

(* Returns the relative suffix — used internally for comparison and
   SharedMem hashing. For flowlib LibFiles, includes the flowlib_marker
   prefix. This is the zero-cost path for hot operations like compare. *)
let suffix = function
  | LibFile x
  | SourceFile x
  | JsonFile x
  | ResourceFile x ->
    x

(* Resolves a stored suffix back to an absolute path.
   - Empty suffix ("") and the stdin sentinel ("-") are returned unchanged.
   - Suffixes that are already absolute (Filename.is_relative returns false)
     are returned unchanged — this handles out-of-root files whose absolute
     path is stored directly as the suffix.
   - Relative suffixes are prepended with get_root (). The root must have a
     trailing separator (enforced by set_project_root/set_flowlib_root) so
     concatenation produces a valid path. *)
let resolve_root get_root suffix =
  resolve_root_with ~is_relative:Filename.is_relative get_root suffix

(* Returns the full absolute path — resolves the root on demand.
   Allocates a new string via concatenation. Use suffix for hot paths
   that don't need the absolute path (compare, SharedMem hashing). *)
let to_absolute = function
  | LibFile x ->
    if String.starts_with ~prefix:flowlib_marker x then
      let marker_len = String.length flowlib_marker in
      resolve_root get_flowlib_root (String.sub x marker_len (String.length x - marker_len))
    else
      resolve_root get_project_root x
  | SourceFile x
  | JsonFile x
  | ResourceFile x ->
    resolve_root get_project_root x

(* to_string returns the absolute path so all existing callers
   (destructuring, file I/O, display, path manipulation) work correctly
   without changes. For hot paths that only need identity/comparison,
   use suffix instead to avoid the string allocation. *)
let to_string = to_absolute

let to_path t = Ok (to_absolute t)

let compare =
  (* libs, then source and json files at the same priority since JSON files are
   * basically source files. We don't actually read resource files so they come
   * last *)
  let order_of_filename = function
    | LibFile _ -> 1
    | SourceFile _ -> 2
    | JsonFile _ -> 2
    | ResourceFile _ -> 3
  in
  fun a b ->
    let k = order_of_filename a - order_of_filename b in
    if k <> 0 then
      k
    else
      String.compare (suffix a) (suffix b)

let compare_opt a b =
  match (a, b) with
  | (Some _, None) -> -1
  | (None, Some _) -> 1
  | (None, None) -> 0
  | (Some a, Some b) -> compare a b

let is_lib_file = function
  | LibFile _ -> true
  | SourceFile _ -> false
  | JsonFile _ -> false
  | ResourceFile _ -> false

let map f = function
  | LibFile filename -> LibFile (f filename)
  | SourceFile filename -> SourceFile (f filename)
  | JsonFile filename -> JsonFile (f filename)
  | ResourceFile filename -> ResourceFile (f filename)

let exists f = function
  | LibFile filename
  | SourceFile filename
  | JsonFile filename
  | ResourceFile filename ->
    f filename

let check_suffix filename sfx = exists (fun fn -> Filename.check_suffix fn sfx) filename

let chop_suffix filename sfx = map (fun fn -> Filename.chop_suffix fn sfx) filename

let with_suffix filename sfx = map (fun fn -> fn ^ sfx) filename

(* Strip a root prefix from an absolute path. The prefix must have a
   trailing slash (enforced by set_project_root/set_flowlib_root) so that
   stripping always occurs at a directory boundary — e.g., root "/data/foo/"
   won't match "/data/foobar/file.js". *)
let strip_prefix prefix path =
  let plen = String.length prefix in
  if plen > 0 && String.starts_with ~prefix path then
    String.sub path plen (String.length path - plen)
  else
    path

(* Strip the project root from an absolute path. If the root has not been
   set yet, returns the path unchanged — the suffix will be an absolute
   path, and resolve_root handles that case correctly. *)
let strip_project_root path =
  match !project_root with
  | Some root -> strip_prefix root path
  | None -> path

(* Create File_key values from absolute paths, stripping the appropriate root.
   Safe to call before roots are initialized — the full path is stored as the
   suffix, and to_absolute handles absolute suffixes correctly. *)
let source_file_of_absolute path = SourceFile (strip_project_root path)

let json_file_of_absolute path = JsonFile (strip_project_root path)

let resource_file_of_absolute path = ResourceFile (strip_project_root path)

let lib_file_of_absolute path =
  match !flowlib_root with
  | Some fl when String.length fl > 0 && String.starts_with ~prefix:fl path ->
    LibFile (flowlib_marker ^ strip_prefix fl path)
  | _ -> LibFile (strip_project_root path)

module For_tests = struct
  let enforce_trailing_sep = enforce_trailing_sep

  let resolve_root_with = resolve_root_with
end
