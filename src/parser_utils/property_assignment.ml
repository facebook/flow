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

class property_assignment = object
  inherit Ssa_builder.With_ALoc.ssa_builder
end

let eval_property_assignment properties ctor_body =
  let bindings: ALoc.t Hoister.Bindings.t =
    List.fold_left (fun bindings property ->
      Hoister.Bindings.add property bindings
    ) Hoister.Bindings.empty properties
  in

  let ssa_walk = new property_assignment in
  ignore @@ ssa_walk#with_bindings ALoc.none bindings (fun body ->
    ignore @@ ssa_walk#run_to_completion (fun () ->
      ignore @@ ssa_walk#block ALoc.none body
    );
    body
  ) ctor_body
