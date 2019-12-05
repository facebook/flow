(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(**
 * We override some things in the Ocaml system so that we can have our own
 * implementations. This is useful for mocking/testing/injecting modules
 * that override Ocaml's default behavior.
 *
 * Please include this everywhere you use Sys and Unix.
 *)

module Ocaml_unix = Unix
module Ocaml_Sys = Sys

module Unix = struct
  include Ocaml_unix

  let getcwd () = Disk.getcwd ()

  let chdir = Disk.chdir

  let mkdir = Disk.mkdir

  let rename = Disk.rename
end

module Sys = struct
  include Ocaml_Sys

  let getcwd () = Disk.getcwd ()

  let chdir = Disk.chdir

  let is_directory = Disk.is_directory

  let rename = Disk.rename

  let file_exists = Disk.file_exists

  let readdir = Disk.readdir
end
