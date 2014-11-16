(**
 *  Copyright 2014 Facebook.
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

open Utils

(* given a map from keys to dependencies, returns whether the dependencies are
   cyclic, as well as a topologically sorted list of key lists where any keys in
   a list only depend on keys in a subsequent list
*)
val topsort: (string * SSet.t) SMap.t -> (bool * string list list)
