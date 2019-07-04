(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let hh_logger_level_of_env env =
  match Sys_utils.get_env env with
  | Some "off" -> Some Hh_logger.Level.Off
  | Some "fatal" -> Some Hh_logger.Level.Fatal
  | Some "error" -> Some Hh_logger.Level.Error
  | Some "warn" -> Some Hh_logger.Level.Warn
  | Some "info" -> Some Hh_logger.Level.Info
  | Some "debug" -> Some Hh_logger.Level.Debug
  | Some _ (* ignore invalid values *)
  | None -> None

(* TODO: min_level should probably default to warn, but was historically info *)
let set_hh_logger_min_level ?(min_level=Hh_logger.Level.Info) options =
  Hh_logger.Level.set_min_level (
    if Options.is_quiet options then
      Hh_logger.Level.Off
    else if Options.verbose options != None || Options.is_debug_mode options then
      Hh_logger.Level.Debug
    else match hh_logger_level_of_env "FLOW_LOG_LEVEL" with
      | Some level -> level
      | None -> min_level
  )

let init_loggers ~options ?min_level () =
  set_hh_logger_min_level ?min_level options

let set_server_options ~server_options =
  let lazy_mode = match Options.lazy_mode server_options with
  | Options.LAZY_MODE_FILESYSTEM -> "fs"
  | Options.LAZY_MODE_IDE -> "ide"
  | Options.LAZY_MODE_WATCHMAN -> "watchman"
  | Options.NON_LAZY_MODE -> "off"
  in
  let arch = match Options.arch server_options with
  | Options.Classic -> "classic"
  | Options.TypesFirst -> "types_first"
  in

  FlowEventLogger.set_server_options ~lazy_mode ~arch

let disable_logging () =
  EventLogger.disable_logging ();
  FlowInteractionLogger.disable_logging ();
  Flow_server_profile.disable_logging ()
