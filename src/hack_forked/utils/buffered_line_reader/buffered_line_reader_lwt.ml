(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Regular_reader :
  Buffered_line_reader_sig.READER with type 'a result = 'a Lwt.t and type fd = Lwt_unix.file_descr =
struct
  type 'a result = 'a Lwt.t

  type fd = Lwt_unix.file_descr

  let return = Lwt.return

  let fail = Lwt.fail

  let ( >>= ) = Lwt.( >>= )

  let read fd ~buffer ~offset ~size = Lwt_unix.read fd buffer offset size

  let is_readable = Lwt_unix.readable

  let open_devnull () = Lwt_unix.openfile "/dev/null" [Unix.O_RDONLY] 0o440
end

include Buffered_line_reader.Functor (Regular_reader)
