(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Unsigned = struct
  let rec read_helper read_byte result shift =
    let byte = read_byte () in
    let result = result lor ((byte land 0x7F) lsl shift) in
    if byte land 0x80 = 0 then
      result
    else
      read_helper read_byte result (shift + 7)

  let rec write_helper write_byte i =
    let byte = i land 0x7F in
    let i = i lsr 7 in
    if i != 0 then begin
      write_byte (byte lor 0x80);
      write_helper write_byte i
    end else
      write_byte byte

  let read read_byte = read_helper read_byte 0 0

  let write write_byte i =
    assert (i >= 0);
    write_helper write_byte i
end
