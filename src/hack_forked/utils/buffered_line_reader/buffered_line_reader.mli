(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Buffered_line_reader_sig.S with type 'a result = 'a and type fd = Unix.file_descr

module Functor (Reader : Buffered_line_reader_sig.READER) :
  Buffered_line_reader_sig.S with type 'a result = 'a Reader.result and type fd = Reader.fd
