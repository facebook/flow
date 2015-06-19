(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


type client_mode =
| MODE_LIST_FILES
| MODE_LIST_MODES
| MODE_TYPE_AT_POS of string
| MODE_AUTO_COMPLETE
| MODE_STATUS
| MODE_UNSPECIFIED
| MODE_SHOW of string
| MODE_COLORING of string
| MODE_COVERAGE of string
| MODE_FIND_REFS of string
| MODE_IDENTIFY_FUNCTION of string
| MODE_OUTLINE
| MODE_METHOD_JUMP_CHILDREN of string
| MODE_METHOD_JUMP_ANCESTORS of string
| MODE_REFACTOR
| MODE_FIND_CLASS_REFS of string
| MODE_ARGUMENT_INFO of string
(* TODO figure out why we can't reference FuzzySearchService from here *)
| MODE_SEARCH of string * string
| MODE_LINT of string list
| MODE_LINT_ALL of int
| MODE_DUMP_SYMBOL_INFO of string
| MODE_CREATE_CHECKPOINT of string
| MODE_RETRIEVE_CHECKPOINT of string
| MODE_DELETE_CHECKPOINT of string

type client_check_env = {
  mode: client_mode;
  root: Path.t;
  from: string;
  output_json: bool;
  retry_if_init: bool;
  retries: int;
  timeout: float option;
  autostart: bool;
  no_load: bool;
}
