(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let string_of_signal n =
  match n with
  | _ when n = Sys.sigabrt -> "sigabrt"
  | _ when n = Sys.sigalrm -> "sigalrm"
  | _ when n = Sys.sigfpe -> "sigfpe"
  | _ when n = Sys.sighup -> "sighup"
  | _ when n = Sys.sigill -> "sigill"
  | _ when n = Sys.sigint -> "sigint"
  | _ when n = Sys.sigkill -> "sigkill"
  | _ when n = Sys.sigpipe -> "sigpipe"
  | _ when n = Sys.sigquit -> "sigquit"
  | _ when n = Sys.sigsegv -> "sigsegv"
  | _ when n = Sys.sigterm -> "sigterm"
  | _ when n = Sys.sigusr1 -> "sigusr1"
  | _ when n = Sys.sigusr2 -> "sigusr2"
  | _ when n = Sys.sigchld -> "sigchld"
  | _ when n = Sys.sigcont -> "sigcont"
  | _ when n = Sys.sigstop -> "sigstop"
  | _ when n = Sys.sigtstp -> "sigtstp"
  | _ when n = Sys.sigttin -> "sigttin"
  | _ when n = Sys.sigttou -> "sigttou"
  | _ when n = Sys.sigvtalrm -> "sigvtalrm"
  | _ when n = Sys.sigprof -> "sigprof"
  | _ -> assert false
