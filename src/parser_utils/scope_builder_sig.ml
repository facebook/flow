(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  module L : Loc_sig.S

  module Api : Scope_api_sig.S with module L = L

  module Acc : sig
    type t = Api.info

    val init : t
  end

  val program :
    ?flowmin_compatibility:bool -> with_types:bool -> (L.t, L.t) Flow_ast.Program.t -> Acc.t

  class scope_builder :
    flowmin_compatibility:bool
    -> with_types:bool
    -> object
         inherit [Acc.t, L.t] Flow_ast_visitor.visitor

         method with_bindings : 'a. ?lexical:bool -> L.t -> L.t Bindings.t -> ('a -> 'a) -> 'a -> 'a

         method private scoped_for_statement :
           L.t -> (L.t, L.t) Flow_ast.Statement.For.t -> (L.t, L.t) Flow_ast.Statement.For.t

         method private scoped_for_in_statement :
           L.t -> (L.t, L.t) Flow_ast.Statement.ForIn.t -> (L.t, L.t) Flow_ast.Statement.ForIn.t

         method private scoped_for_of_statement :
           L.t -> (L.t, L.t) Flow_ast.Statement.ForOf.t -> (L.t, L.t) Flow_ast.Statement.ForOf.t

         method private switch_cases :
           (L.t, L.t) Flow_ast.Expression.t ->
           (L.t, L.t) Flow_ast.Statement.Switch.Case.t list ->
           (L.t, L.t) Flow_ast.Statement.Switch.Case.t list

         method private lambda :
           (L.t, L.t) Flow_ast.Function.Params.t -> (L.t, L.t) Flow_ast.Function.body -> unit

         method private function_tparams_and_return :
           (L.t, L.t) Flow_ast.Type.TypeParams.t option ->
           (L.t, L.t) Flow_ast_mapper.Ast.Type.annotation_or_hint ->
           unit

         method private hoisted_function_tparams_and_return :
           (L.t, L.t) Flow_ast.Type.TypeParams.t option ->
           (L.t, L.t) Flow_ast_mapper.Ast.Type.annotation_or_hint ->
           unit

         method private hoisted_type_annotation : (L.t, L.t) Flow_ast.Type.annotation -> unit
       end
end
