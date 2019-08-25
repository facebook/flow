(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

open Core_kernel
include Cli_args_sig.Types

let files_to_check_range_to_json (range : files_to_check_range) : Hh_json.json
    =
  let range_properties =
    match range.to_prefix_excl with
    | Some to_prefix_excl ->
      [ ( "to_prefix_excl",
          Hh_json.JSON_String (Relative_path.suffix to_prefix_excl) ) ]
    | None -> []
  in
  let range_properties =
    match range.from_prefix_incl with
    | Some from_prefix_incl ->
      let from_prefix_incl =
        ( "from_prefix_incl",
          Hh_json.JSON_String (Relative_path.suffix from_prefix_incl) )
      in
      from_prefix_incl :: range_properties
    | None -> range_properties
  in
  Hh_json.JSON_Object range_properties

let files_to_check_spec_to_json (files_to_check_spec : files_to_check_spec) :
    Hh_json.json =
  match files_to_check_spec with
  | Range (range : files_to_check_range) -> files_to_check_range_to_json range
  | Prefix (prefix : Relative_path.t) ->
    Hh_json.JSON_String (Relative_path.suffix prefix)

let get_save_state_spec_json (spec : save_state_spec_info) : string =
  let files_to_check_spec_list =
    List.map ~f:files_to_check_spec_to_json spec.files_to_check
  in
  let (properties : (string * Hh_json.json) list) =
    [ ("gen_with_errors", Hh_json.JSON_Bool spec.gen_with_errors);
      ("files_to_check", Hh_json.JSON_Array files_to_check_spec_list);
      ("filename", Hh_json.JSON_String spec.filename) ]
  in
  Hh_json.json_to_string ~pretty:true (Hh_json.JSON_Object properties)

let save_state_spec_json_example =
  {
    files_to_check =
      [ Prefix (Relative_path.from_root "/some/path/prefix1");
        Range
          {
            from_prefix_incl =
              Some (Relative_path.from_root "/from/path/prefix1");
            to_prefix_excl = Some (Relative_path.from_root "/to/path/prefix1");
          };
        Range
          {
            from_prefix_incl =
              Some (Relative_path.from_root "/from/path/prefix2");
            to_prefix_excl = Some (Relative_path.from_root "/to/path/prefix2");
          };
        Range
          {
            from_prefix_incl = Some (Relative_path.from_root "/from/path/only");
            to_prefix_excl = None;
          };
        Range
          {
            from_prefix_incl = None;
            to_prefix_excl = Some (Relative_path.from_root "/to/path/only");
          } ];
    filename = "/some/dir/some_filename";
    gen_with_errors = true;
  }

let save_state_spec_json_descr =
  Printf.sprintf
    {|A JSON specification of how and what to save, e.g.:
%s
|}
    (get_save_state_spec_json save_state_spec_json_example)

(* TODO: gen examples *)
let saved_state_json_descr =
  {|A JSON specification for how to initialize with a saved state.

Saved state JSON looks like this:
{
  "state": <saved state filename>,
  "corresponding_base_revision" : <global rev #>,
  "deptable": <dependency table filename>,
  "changes": [array of files changed since that saved state]
}

For example:
{
  "deptable": "/home/unixname/saved-states/ss1.sql",
  "state": "/home/unixname/saved-states/ss1",
  "changes": [],
  "prechecked_changes": [],
  "corresponding_base_revision": "-1"
}

You can put this saved state JSON into a file and pass this JSON as the argument:
{
  "from_file": "/home/unixname/saved-states/ss1.json"
}

Alternatively, you can pass this JSON as the argument, with the saved state JSON embedded:
{
  "data_dump":
  {
    "deptable": "/home/unixname/saved-states/ss1.sql",
    "state": "/home/unixname/saved-states/ss1",
    "changes": [],
    "prechecked_changes": [],
    "corresponding_base_revision": "-1"
  }
}
|}

let get_path (key : string) json_obj : Relative_path.t option =
  let value = Hh_json.Access.get_string key json_obj in
  match value with
  | Ok ((value : string), _keytrace) -> Some (Relative_path.from_root value)
  | Error _ -> None

let get_spec (spec_json : Hh_json.json) : files_to_check_spec =
  try Prefix (Hh_json.get_string_exn spec_json |> Relative_path.from_root)
  with _ ->
    let from_prefix_incl = get_path "from_prefix_incl" (spec_json, []) in
    let to_prefix_excl = get_path "to_prefix_excl" (spec_json, []) in
    Range { from_prefix_incl; to_prefix_excl }

let parse_save_state_json ((json : Hh_json.json), _keytrace) =
  Hh_json.Access.(
    let files_to_check =
      Option.value
        ~default:[]
        (Hh_json.(get_field_opt (get_array "files_to_check")) json)
    in
    let files_to_check = List.map files_to_check ~f:get_spec in
    let json = return json in
    json
    >>= get_string "filename"
    >>= fun (filename, _filename_keytrace) ->
    json
    >>= get_bool "gen_with_errors"
    >>= fun (gen_with_errors, _gen_with_errors_keytrace) ->
    return { files_to_check; filename; gen_with_errors })

let get_save_state_spec (v : string option) :
    (save_state_spec_info option, string) result =
  match v with
  | None -> Ok None
  | Some blob ->
    Hh_json.Access.(
      let json = Hh_json.json_of_string blob in
      let json = return json in
      let parsed_spec_result = json >>= parse_save_state_json in
      (match parsed_spec_result with
      | Ok ((parsed_spec : save_state_spec_info), _keytrace) ->
        Hh_logger.log "Parsed save state spec, everything's good";
        Ok (Some parsed_spec)
      | Error spec_failure ->
        let message =
          Printf.sprintf
            "Parsing failed:\n%s\nSee input: %s\n"
            (access_failure_to_string spec_failure)
            blob
        in
        Error message))

let parse_saved_state_json (json, _keytrace) =
  let array_to_path_list =
    List.map ~f:(fun file ->
        Hh_json.get_string_exn file |> Relative_path.from_root)
  in
  let prechecked_changes =
    Hh_json.(get_field_opt (Access.get_array "prechecked_changes")) json
  in
  let prechecked_changes = Option.value ~default:[] prechecked_changes in
  let json = Hh_json.Access.return json in
  Hh_json.Access.(
    json
    >>= get_string "state"
    >>= fun (state, _state_keytrace) ->
    json
    >>= get_string "corresponding_base_revision"
    >>= fun (for_base_rev, _for_base_rev_keytrace) ->
    json
    >>= get_string "deptable"
    >>= fun (deptable, _deptable_keytrace) ->
    json
    >>= get_array "changes"
    >>= fun (changes, _) ->
    let naming_changes =
      match json >>= get_val "naming_changes" with
      | Ok (Hh_json.JSON_Array files, _) -> array_to_path_list files
      | _ -> []
    in
    let prechecked_changes = array_to_path_list prechecked_changes in
    let changes = array_to_path_list changes in
    return
      {
        saved_state_fn = state;
        corresponding_base_revision = for_base_rev;
        deptable_fn = deptable;
        prechecked_changes;
        changes;
        naming_changes;
      })

let get_saved_state_spec (v : string option) :
    (saved_state_target_info option, string) result =
  match v with
  | None -> Ok None
  | Some blob ->
    let json = Hh_json.json_of_string blob in
    let json = Hh_json.Access.return json in
    Hh_json.Access.(
      let data_dump_parse_result =
        json >>= get_obj "data_dump" >>= parse_saved_state_json
      in
      let from_file_parse_result =
        json
        >>= get_string "from_file"
        >>= fun (filename, _filename_keytrace) ->
        let contents = Sys_utils.cat filename in
        let json = Hh_json.json_of_string contents in
        Hh_json.Access.return json >>= parse_saved_state_json
      in
      (match (data_dump_parse_result, from_file_parse_result) with
      | (Ok (parsed_data_dump, _), Ok (_parsed_from_file, _)) ->
        Hh_logger.log
          "Warning - %s"
          ( "Parsed saved state target from both JSON blob data dump"
          ^ " and from contents of file." );
        Hh_logger.log "Preferring data dump result";
        Ok (Some parsed_data_dump)
      | (Ok (parsed_data_dump, _), Error _) -> Ok (Some parsed_data_dump)
      | (Error _, Ok (parsed_from_file, _)) -> Ok (Some parsed_from_file)
      | (Error data_dump_failure, Error from_file_failure) ->
        let message =
          Printf.sprintf
            "Parsing failed:\n  data dump failure: %s\n  from_file failure: %s\nSee input: %s\n"
            (access_failure_to_string data_dump_failure)
            (access_failure_to_string from_file_failure)
            blob
        in
        Error message))
