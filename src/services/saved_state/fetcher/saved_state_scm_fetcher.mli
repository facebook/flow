(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Saved_state_fetcher.FETCHER

val output_filename : Options.t -> (string, string) result Lwt.t
