(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Buffered_line_reader_sig.S with type 'a result = 'a Lwt.t and type fd = Lwt_unix.file_descr
