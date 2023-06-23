(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

type regexp

val chars: Sedlex_cset.t -> regexp
val seq: regexp -> regexp -> regexp
val alt: regexp -> regexp -> regexp
val rep: regexp -> regexp
val plus: regexp -> regexp
val eps: regexp

val compl: regexp -> regexp option
   (* If the argument is a single [chars] regexp, returns a regexp
      which matches the complement set.  Otherwise returns [None]. *)
val subtract: regexp -> regexp -> regexp option
   (* If each argument is a single [chars] regexp, returns a regexp
      which matches the set (arg1 - arg2).  Otherwise returns [None]. *)
val intersection: regexp -> regexp -> regexp option
   (* If each argument is a single [chars] regexp, returns a regexp
      which matches the intersection set.  Otherwise returns [None]. *)

val compile: regexp array -> ((Sedlex_cset.t * int) array * bool array) array
