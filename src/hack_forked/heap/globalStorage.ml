(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*****************************************************************************)
(* Module implementing a global storage system, an efficient way for the
 * master to communicate data with the workers (cf hh_shared.c for the
 * underlying C implementation).
 *
 * The master can store data in the global storage, after that, the data
 * is visible to all the workers.
 *)
(*****************************************************************************)

module Make : functor
  (Value : sig
     type t
   end)
  -> sig
  (* "store v" stores the value v in the global storage.
   * Can only be called by the master.
   * 'hh_shared_init' must have been called prior to the first call.
   * The store must be empty.
   *)
  val store : Value.t -> unit

  (* "load()" returns the value stored in the global storage.
   * Can be called by any process (master or workers), "store" must have
   * been called by the master before the call.
   *)
  val load : unit -> Value.t

  (* "clear()" empties the global storage.
   * Can only be called by the master.
   *)
  val clear : unit -> unit
end =
functor
  (Value : sig
     type t
   end)
  ->
  struct
    external hh_shared_store : string -> unit = "hh_shared_store"

    external hh_shared_load : unit -> string = "hh_shared_load"

    external hh_shared_clear : unit -> unit = "hh_shared_clear"

    let store (x : Value.t) = hh_shared_store (Marshal.to_string x [])

    let load () = (Marshal.from_string (hh_shared_load ()) 0 : Value.t)

    let clear () = hh_shared_clear ()
  end
