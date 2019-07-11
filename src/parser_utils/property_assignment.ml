(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* NOTE: This is a WIP and should not be used for anything yet *)

module Ast = Flow_ast

let public_property loc ident =
  let _, ({ Ast.Identifier.name; comments = _ } as r) = ident in
  loc, { r with Ast.Identifier.name = "this." ^ name }

let private_property loc ident =
  let _, (_, ({ Ast.Identifier.name; comments = _ } as r)) = ident in
  loc, { r with Ast.Identifier.name = "this.#" ^ name }

class property_assignment = object(this)
  inherit Ssa_builder.With_ALoc.ssa_builder

  (* ABRUPT COMPLETIONS *)

  method expecting_return_or_throw (f: unit -> unit): unit =
    let completion_state = this#run_to_completion f in
    this#commit_abrupt_completion_matching
      Ssa_builder.With_ALoc.AbruptCompletion.(mem [return; throw])
      completion_state
end

let eval_property_assignment properties ctor_body =
  let bindings: ALoc.t Hoister.Bindings.t =
    List.fold_left (fun bindings property ->
      Hoister.Bindings.add property bindings
    ) Hoister.Bindings.empty properties
  in

  let ssa_walk = new property_assignment in
  ignore @@ ssa_walk#with_bindings ALoc.none bindings (fun body ->
    ssa_walk#expecting_return_or_throw (fun () ->
      ignore @@ ssa_walk#block ALoc.none body
    );
    body
  ) ctor_body
