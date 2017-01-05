(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(**
 * Main type representing a message sent from editor to server.
 * This message might have different JSON representations based on the version
 * of API established during intialization call.
 *)

type position = File_content.content_pos
type text_edit = File_content.code_edit
type error = Pos.absolute Errors.error_

type file_position = {
  filename : string;
  position : position;
}

type request =
  | Init of init_params
  | Autocomplete of file_position
  | Did_open_file of did_open_file_params
  | Did_close_file of did_close_file_params
  | Did_change_file of did_change_file_params
  | Disconnect (* TODO: document or remove this *)
  | Subscribe_diagnostics (* Nuclide-rpc specific *)
  | Unsubscribe_call (* Nuclide-rpc specific *)
  | Sleep_for_test (* TODO: port the tests that use it to integration_ml
                      framework and remove it*)

and init_params = {
  client_name : string;
  client_api_version : int;
}

and did_open_file_params = {
  did_open_file_filename : string;
  did_open_file_text : string;
}

and did_close_file_params = {
  did_close_file_filename : string;
}

and did_change_file_params = {
  did_change_file_filename : string;
  changes : text_edit list;
}

type response =
  | Init_response of init_response
  | Autocomplete_response of autocomplete_response
  | Diagnostics_notification of diagnostics_notification

and init_response = {
  server_api_version : int;
}

and autocomplete_response = autocomplete_item list

and autocomplete_item = {
  autocomplete_item_text : string;
  autocomplete_item_type : string;
  callable_details : callable_details option;
}

and callable_details = {
  return_type : string;
  params : callable_param list;
}

and callable_param = {
  name : string;
  type_ : string;
}

and diagnostics_notification = {
  subscription_id : int; (* Nuclide-rpc spcific *)
  diagnostics_notification_filename : string;
  diagnostics : error list;
}
