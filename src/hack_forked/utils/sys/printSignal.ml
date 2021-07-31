(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
  (**** Signals added in 4.03 ****)
  | _ when n = Sys.sigbus -> "sigbus"
  | _ when n = Sys.sigpoll -> "sigpoll"
  | _ when n = Sys.sigsys -> "sigsys"
  | _ when n = Sys.sigtrap -> "sigtrap"
  | _ when n = Sys.sigurg -> "sigurg"
  | _ when n = Sys.sigxcpu -> "sigxcpu"
  | _ when n = Sys.sigxfsz -> "sigxfsz" (*******************************)
  (* The integer value in the OCaml signal Sys.sigkill is -7. In POSIX, sigkill is 9. This
   * illustrates that it's generally a bad idea to interact with the integer values inside of
   * OCaml's signals. To be honest, I'm not sure why OCaml doesn't just use some abstract type for
   * its signals, since you generally can't use the integer values directly *)
  | _ -> Printf.sprintf "unknown signal %d" n
