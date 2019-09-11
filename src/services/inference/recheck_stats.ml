(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let per_file_time_guess = 0.003

let per_file_time_key = "per_file_time"

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

let load_per_file_time ~options =
  Lwt_result.(
    let file = get_file ~options in
    let%lwt result =
      (try%lwt
         Lwt_result.ok
           (Lwt_io.open_file
              ~flags:[Unix.O_RDONLY; Unix.O_NONBLOCK]
              ~mode:Lwt_io.Input
              ~perm:0o666
              file)
       with Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt_result.fail "File doesn't exist")
      >>= fun ic ->
      let%lwt contents = Lwt_io.read ic in
      let%lwt () = Lwt_io.close ic in
      Lwt_result.lift
        (try
           let json = Some (Hh_json.json_of_string contents) in
           match Hh_json_helpers.Jget.float_opt json per_file_time_key with
           | None ->
             Result.Error
               (Printf.sprintf "Failed to find key %S in JSON %S" per_file_time_key contents)
           | Some v ->
             Hh_json_helpers.Jget.(
               let last_estimates =
                 match obj_opt json estimates_key with
                 | None -> None
                 | Some json ->
                   let json = Some json in
                   Some
                     {
                       estimated_time_to_recheck = float_exn json estimated_time_to_recheck_key;
                       estimated_time_to_restart = float_exn json estimated_time_to_restart_key;
                       estimated_time_to_init = float_exn json estimated_time_to_init_key;
                       estimated_time_per_file = float_exn json estimated_time_per_file_key;
                       estimated_files_to_recheck = int_exn json estimated_files_to_recheck_key;
                       estimated_files_to_init = int_exn json estimated_files_to_init_key;
                     }
               in
               Result.Ok (v, last_estimates))
         with
        | Hh_json.Syntax_error str ->
          Result.Error (Printf.sprintf "Failed to parse as JSON contents. %S: %S" str contents)
        | Hh_json_helpers.Jget.Parse key ->
          Result.Error
            (Printf.sprintf "Failed to find key %S in estimates object. %S" key contents))
    in
    match result with
    | Result.Ok (per_file_time, last_estimates) -> Lwt.return (per_file_time, last_estimates)
    | Result.Error reason ->
      Hh_logger.info "Failed to load recheck stats from %S. Reason: %S" file reason;
      Lwt.return (per_file_time_guess, None))

let save_averages ~options ?estimates new_averages =
  averages := Some new_averages;

  let estimates =
    Option.value_map
      estimates
      ~default:[]
      ~f:(fun {
                estimated_time_to_recheck;
                estimated_time_to_restart;
                estimated_time_to_init;
                estimated_time_per_file;
                estimated_files_to_recheck;
                estimated_files_to_init;
              }
              ->
        Hh_json.
          [
            ( estimates_key,
              JSON_Object
                [
                  ( estimated_time_to_recheck_key,
                    JSON_Number (Dtoa.ecma_string_of_float estimated_time_to_recheck) );
                  ( estimated_time_to_restart_key,
                    JSON_Number (Dtoa.ecma_string_of_float estimated_time_to_restart) );
                  ( estimated_time_to_init_key,
                    JSON_Number (Dtoa.ecma_string_of_float estimated_time_to_init) );
                  ( estimated_time_per_file_key,
                    JSON_Number (Dtoa.ecma_string_of_float estimated_time_per_file) );
                  ( estimated_files_to_recheck_key,
                    JSON_Number (string_of_int estimated_files_to_recheck) );
                  (estimated_files_to_init_key, JSON_Number (string_of_int estimated_files_to_init));
                ] );
          ])
  in
  let json_str =
    Hh_json.(
      json_to_string
      @@ JSON_Object
           ( (per_file_time_key, JSON_Number (Dtoa.ecma_string_of_float new_averages.per_file_time))
           :: estimates ))
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
      with exn ->
        let exn = Exception.wrap exn in
        Lwt_result.fail (Printf.sprintf "Failed to write file\n%s" (Exception.to_string exn))
    in
    begin
      match result with
      | Result.Ok () -> ()
      | Result.Error msg -> Hh_logger.error "Failed to save per_file_time to %S. %s" file msg
    end;

    Lwt.return_unit)

let init ~options ~init_time ~parsed_count =
  let%lwt (per_file_time, last_estimates) = load_per_file_time ~options in
  averages := Some { init_time; per_file_time; parsed_count };
  Lwt.return last_estimates

let with_averages f =
  match !averages with
  | None -> failwith "Recheck_stats needs to be initialized before it can be used"
  | Some averages -> f averages

let record_recheck_time ~options ~total_time ~rechecked_files =
  (* rechecked_files should be non-negative. If it's 0, then we have no new information to add *)
  if rechecked_files > 0 then
    with_averages
    @@ fun { init_time; per_file_time; parsed_count } ->
    (* What should we do for tiny repositories? Let's make the window at least 15 samples big *)
    let window = max parsed_count 15 in
    let per_file_time =
      moving_average
        ~window
        ~avg:per_file_time
        ~sample:(total_time /. float_of_int rechecked_files)
        ~sample_count:rechecked_files
    in
    save_averages ~options { init_time; per_file_time; parsed_count }
  else
    Lwt.return_unit

let record_last_estimates ~options ~estimates =
  with_averages @@ (fun averages -> save_averages ~options ~estimates averages)

let get_init_time () =
  with_averages @@ (fun { init_time; per_file_time = _; parsed_count = _ } -> init_time)

let get_per_file_time () =
  with_averages @@ (fun { init_time = _; per_file_time; parsed_count = _ } -> per_file_time)
