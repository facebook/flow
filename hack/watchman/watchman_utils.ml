(** State_enter and State_leave events contains a JSON blob specifying
 * the revision we are moving to. This gets it. *)
let rev_in_state_change json =
  Hh_json.Access.(
    return json >>= get_string "rev" |> function
    | Error _ ->
      let () =
        Hh_logger.log
          "Watchman_utils failed to get rev in json: %s"
          (Hh_json.json_to_string json)
      in
      None
    | Ok (v, _) -> Some v)

let merge_in_state_change json =
  Hh_json.Access.(
    return json >>= get_bool "merge" |> function
    | Error _ ->
      let () =
        Hh_logger.log
          "Watchman_utils failed to get merge in json: %s"
          (Hh_json.json_to_string json)
      in
      None
    | Ok (v, _) -> Some v)
