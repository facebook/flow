(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Trust

let infer_trust cx =
  if Context.trust_tracking cx then
    Flow_js.mk_trust_var cx ~initial:(unknown_qualifier ()) () |> from_ident
  else
    bogus_trust ()

let with_trust_inference cx constructor = infer_trust cx |> constructor

let strengthen newtrust cx trust err =
  if is_qualifier trust then
    as_qualifier trust |> join_trust newtrust |> from_qualifier
  else (
    Flow_js.strengthen_trust cx (as_ident trust) newtrust err;
    trust
  )

let make_trusted = unknown_qualifier () |> make_trusted |> strengthen

let make_private = unknown_qualifier () |> make_private |> strengthen

(* Get the trust of a trust_rep, whether it's an ident or a fixed trust. *)
let actual_trust cx t =
  Trust_constraint.(
    match expand t with
    | QualifiedTrust trust -> trust
    | InferredTrust ident ->
      begin
        match Context.find_trust_graph cx ident with
        | TrustResolved trust -> trust
        | TrustUnresolved bounds -> get_trust bounds
      end)
