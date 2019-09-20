(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include
  FlowServerMonitorConnection.CONNECTION
    with type in_message := LspProt.request_with_metadata
     and type out_message := LspProt.response
