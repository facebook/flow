(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

(* This file provides only the interface, so injector configuration
 * can be retreived without depending on the *actual* implementation
 * file. This is because we want libraries to be able to refer to the config,
 * but the actual injector to be chosen by the binary being built.
 *
 * Note: Buck doesn't currently have a build rule to only build .mli files
 * into .cmi, so you need to compile against this file directly. *)
val use_test_stubbing : bool
