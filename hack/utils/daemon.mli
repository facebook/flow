(**
 *  Copyright 2015 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *)

(* Type-safe versions of the channels in Pervasives. *)
type 'a in_channel
type 'a out_channel
type ('in_, 'out) handle = 'in_ in_channel * 'out out_channel

val to_channel : 'a out_channel -> 'a -> unit
val from_channel : 'a in_channel -> 'a
(* This breaks the type safety, but is necessary in order to allow select() *)
val descr_of_in_channel : 'a in_channel -> Unix.file_descr
val descr_of_out_channel : 'a out_channel -> Unix.file_descr

(* Fork and run a function that communicates via the typed channels *)
val fork : (('a, 'b) handle -> unit) -> ('b, 'a) handle
