(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let per_file_time_guess = 0.003

let per_merge_file_time_guess = 0.001

let per_check_file_time_guess = 0.005

let per_file_time_key = "per_file_time"

let per_merge_file_time_key = "per_merge_file_time"

let per_check_file_time_key = "per_check_file_time"

let estimates_key = "estimates"

let estimated_time_to_recheck_key = "estimated_time_to_recheck"

let estimated_time_to_restart_key = "estimated_time_to_restart"

let estimated_time_to_init_key = "estimated_time_to_init"

let estimated_time_per_file_key = "estimated_time_per_file"

let estimated_files_to_recheck_key = "estimated_files_to_recheck"

let estimated_files_to_init_key = "estimated_files_to_init"

type estimates = {
  estimated_time_to_recheck: float;
  estimated_time_to_restart: float;
  estimated_time_to_init: float;
  estimated_time_per_file: float;
  estimated_files_to_recheck: int;
  estimated_files_to_init: int;
}

type averages = {
  init_time: float;
  per_file_time: float;
  per_merge_file_time: float;
  per_check_file_time: float;
  parsed_count: int;
}

let averages = ref None

(* window should be a positive integer *)
let moving_average ~window ~avg ~sample ~sample_count =
  let window = float_of_int window in
  let sample_count = float_of_int sample_count in
  if sample_count >= window then
    sample
  else
    ((avg *. (window -. sample_count)) +. (sample *. sample_count)) /. window

let get_file ~options =
  let root = Options.root options in
  let tmp_dir = Options.temp_dir options in
  let flowconfig_name = Options.flowconfig_name options in
  Server_files_js.recheck_stats_file ~flowconfig_name ~tmp_dir root

let load_stats ~options =
  Lwt_result.(
    let file = get_file ~options in
    let%lwt result =
      (try%lwt
         Lwt_result.ok
           (Lwt_io.open_file
              ~flags:[Unix.O_RDONLY; Unix.O_NONBLOCK]
              ~mode:Lwt_io.Input
              ~perm:0o666
              file
           )
       with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt_result.fail "File doesn't exist")
      >>= fun ic ->
      let%lwt contents = Lwt_io.read ic in
      let%lwt () = Lwt_io.close ic in
      Lwt_result.lift
        (try
           let json = Some (Hh_json.json_of_string contents) in
           let per_file_time =
             match Hh_json_helpers.Jget.float_opt json per_file_time_key with
             | Some v -> v
             | None -> per_file_time_guess
           in
           let per_merge_file_time =
             match Hh_json_helpers.Jget.float_opt json per_merge_file_time_key with
             | Some v -> v
             | None -> per_merge_file_time_guess
           in
           let per_check_file_time =
             match Hh_json_helpers.Jget.float_opt json per_check_file_time_key with
             | Some v -> v
             | None -> per_check_file_time_guess
           in
           Result.Ok (per_file_time, per_merge_file_time, per_check_file_time)
         with
        | Hh_json.Syntax_error str ->
          Result.Error (Printf.sprintf "Failed to parse as JSON contents. %S: %S" str contents)
        | Hh_json_helpers.Jget.Parse key ->
          Result.Error (Printf.sprintf "Failed to find key %S in estimates object. %S" key contents))
    in
    match result with
    | Result.Ok (per_file_time, per_merge_file_time, per_check_file_time) ->
      Lwt.return (per_file_time, per_merge_file_time, per_check_file_time)
    | Result.Error reason ->
      Hh_logger.info "Failed to load recheck stats from %S. Reason: %S" file reason;
      Lwt.return (per_file_time_guess, per_merge_file_time_guess, per_check_file_time_guess)
  )

let save_averages ~options ?estimates new_averages =
  averages := Some new_averages;

  let estimates =
    Base.Option.value_map
      estimates
      ~default:[]
      ~f:(fun
           {
             estimated_time_to_recheck;
             estimated_time_to_restart;
             estimated_time_to_init;
             estimated_time_per_file;
             estimated_files_to_recheck;
             estimated_files_to_init;
           }
         ->
        [
          ( estimates_key,
            Hh_json.JSON_Object
              [
                ( estimated_time_to_recheck_key,
                  Hh_json.JSON_Number (Dtoa.ecma_string_of_float estimated_time_to_recheck)
                );
                ( estimated_time_to_restart_key,
                  Hh_json.JSON_Number (Dtoa.ecma_string_of_float estimated_time_to_restart)
                );
                ( estimated_time_to_init_key,
                  Hh_json.JSON_Number (Dtoa.ecma_string_of_float estimated_time_to_init)
                );
                ( estimated_time_per_file_key,
                  Hh_json.JSON_Number (Dtoa.ecma_string_of_float estimated_time_per_file)
                );
                ( estimated_files_to_recheck_key,
                  Hh_json.JSON_Number (string_of_int estimated_files_to_recheck)
                );
                ( estimated_files_to_init_key,
                  Hh_json.JSON_Number (string_of_int estimated_files_to_init)
                );
              ]
          );
        ]
    )
  in
  let json_str =
    Hh_json.(
      json_to_string
      @@ JSON_Object
           ([
              (per_file_time_key, JSON_Number (Dtoa.ecma_string_of_float new_averages.per_file_time));
              ( per_merge_file_time_key,
                JSON_Number (Dtoa.ecma_string_of_float new_averages.per_merge_file_time)
              );
              ( per_check_file_time_key,
                JSON_Number (Dtoa.ecma_string_of_float new_averages.per_check_file_time)
              );
            ]
           @ estimates
           )
    )
  in
  let file = get_file ~options in
  Lwt_result.(
    let%lwt result =
      (try%lwt
         Lwt_result.ok
         @@ Lwt_io.open_file
              ~flags:[Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
              ~mode:Lwt_io.Output
              ~perm:0o666
              file
       with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt_result.fail "File doesn't exist"
      | exn ->
        let exn = Exception.wrap exn in
        Lwt_result.fail (Printf.sprintf "Failed to open file\n%s" (Exception.to_string exn)))
      >>= fun oc ->
      try%lwt
        let%lwt () = Lwt_io.write oc json_str in
        Lwt_result.ok @@ Lwt_io.close oc
      with
      | exn ->
        let exn = Exception.wrap exn in
        Lwt_result.fail (Printf.sprintf "Failed to write file\n%s" (Exception.to_string exn))
    in
    begin
      match result with
      | Result.Ok () -> ()
      | Result.Error msg -> Hh_logger.error "Failed to save per_file_time to %S. %s" file msg
    end;

    Lwt.return_unit
  )

let init ~options ~init_time ~parsed_count =
  let%lwt (per_file_time, per_merge_file_time, per_check_file_time) = load_stats ~options in
  averages :=
    Some { init_time; per_file_time; per_merge_file_time; per_check_file_time; parsed_count };
  Lwt.return_unit

let with_averages f =
  match !averages with
  | None -> failwith "Recheck_stats needs to be initialized before it can be used"
  | Some averages -> f averages

let record_recheck_time ~options ~merge_time ~check_time ~merged_files ~checked_files =
  with_averages
  @@ fun { init_time; per_file_time; per_merge_file_time; per_check_file_time; parsed_count } ->
  (* What should we do for tiny repositories? Let's make the window at least 15 samples big *)
  let window = max parsed_count 15 in
  let total_files = merged_files + checked_files in
  let per_file_time =
    if total_files > 0 then
      moving_average
        ~window
        ~avg:per_file_time
        ~sample:((merge_time +. check_time) /. float_of_int total_files)
        ~sample_count:total_files
    else
      per_file_time
  in
  let per_merge_file_time =
    if merged_files > 0 then
      moving_average
        ~window
        ~avg:per_merge_file_time
        ~sample:(merge_time /. float_of_int merged_files)
        ~sample_count:merged_files
    else
      per_merge_file_time
  in
  let per_check_file_time =
    if checked_files > 0 then
      moving_average
        ~window
        ~avg:per_check_file_time
        ~sample:(check_time /. float_of_int checked_files)
        ~sample_count:checked_files
    else
      per_check_file_time
  in
  save_averages
    ~options
    { init_time; per_file_time; per_merge_file_time; per_check_file_time; parsed_count }

let record_last_estimates ~options ~estimates =
  with_averages @@ fun averages -> save_averages ~options ~estimates averages

let get_init_time () = with_averages @@ fun { init_time; _ } -> init_time

let get_per_merge_file_time () =
  with_averages @@ fun { per_merge_file_time; _ } -> per_merge_file_time

let get_per_check_file_time () =
  with_averages @@ fun { per_check_file_time; _ } -> per_check_file_time
