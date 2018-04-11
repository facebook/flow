(**
 * Copyright (c) 2018, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

let go workers effort =
 (* Run the collection function in one of the workers. *)
 let wrapper = fun f ->
   MultiWorker.call
     workers
     ~job:(fun () (_bucket : unit list) -> f ())
     ~neutral:()
     ~merge:(fun () () -> ())
     ~next:(MultiWorker.next workers [()])
 in
 (* Check this in master to avoid worker overhead when no collection will
  * happen *)
 if SharedMem.should_collect effort then
  SharedMem.collect ~wrapper ~allow_in_worker:true effort
