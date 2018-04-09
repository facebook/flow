(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO: min_level should probably default to warn, but was historically info *)
let set_hh_logger_min_level ?(min_level=Hh_logger.Level.Info) options =
  Hh_logger.Level.set_min_level (
    if Options.is_quiet options then
      Hh_logger.Level.Off
    else if Options.verbose options != None || Options.is_debug_mode options then
      Hh_logger.Level.Debug
    else match Sys_utils.get_env "FLOW_LOG_LEVEL" with
      | Some "off" -> Hh_logger.Level.Off
      | Some "fatal" -> Hh_logger.Level.Fatal
      | Some "error" -> Hh_logger.Level.Error
      | Some "warn" -> Hh_logger.Level.Warn
      | Some "info" -> Hh_logger.Level.Info
      | Some "debug" -> Hh_logger.Level.Debug
      | Some _ (* ignore invalid values *)
      | None -> min_level
  )

let init_loggers ~from ~options ?min_level () =
  FlowEventLogger.set_from from;
  set_hh_logger_min_level ?min_level options
