(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  module L: Loc_sig.S
  module Api: Scope_api_sig.S with module L = L

  module Acc : sig
    type t = Api.info
  end

  val program: ?ignore_toplevel:bool -> (L.t, L.t) Flow_ast.program -> Acc.t

  class scope_builder: object
    inherit [Acc.t, L.t] Flow_ast_visitor.visitor

    method with_bindings:
        'a. ?lexical:bool ->
        L.t ->
        L.t Hoister.Bindings.t ->
        ('a -> 'a) ->
        'a ->
        'a

    method private scoped_for_statement:
        L.t ->
        (L.t, L.t) Flow_ast.Statement.For.t ->
        (L.t, L.t) Flow_ast.Statement.For.t
    method private scoped_for_in_statement:
        L.t ->
        (L.t, L.t) Flow_ast.Statement.ForIn.t ->
        (L.t, L.t) Flow_ast.Statement.ForIn.t
    method private scoped_for_of_statement:
        L.t ->
        (L.t, L.t) Flow_ast.Statement.ForOf.t ->
        (L.t, L.t) Flow_ast.Statement.ForOf.t
    method private lambda:
        L.t ->
        (L.t, L.t) Flow_ast.Function.Params.t ->
        (L.t, L.t) Flow_ast.Function.body ->
        unit
  end
end
