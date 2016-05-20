module Ast = Spider_monkey_ast
module Sig = Classy_sig

open Reason_js

type self_t = Type.t
type tparams_map_t = Type.t SMap.t
type expr_fn_t = Context.t -> tparams_map_t -> Ast.Expression.t -> Type.t
type stmt_fn_t = Context.t -> tparams_map_t -> Ast.Statement.t list -> unit

module Class : sig
  type classy_ast_t = Ast.Class.t
  val mk_sig: Context.t -> tparams_map_t -> expr_fn_t -> Loc.t -> reason ->
    self_t -> classy_ast_t -> Sig.t
  val classtype: Context.t -> Sig.t -> Type.t
  val generate_tests: Context.t -> (Sig.t -> unit) -> Sig.t -> unit
  val check_super: Context.t -> Sig.t -> unit
end

module Interface : sig
  type classy_ast_t = Ast.Statement.Interface.t
  val mk_sig: Context.t -> tparams_map_t -> expr_fn_t -> Loc.t -> reason ->
    self_t -> classy_ast_t -> Sig.t
  val classtype: Context.t -> Sig.t -> Type.t
  val generate_tests: Context.t -> (Sig.t -> unit) -> Sig.t -> unit
  val check_super: Context.t -> Sig.t -> unit
end

module DeclClass : sig
  type classy_ast_t = Ast.Statement.Interface.t
  val mk_sig: Context.t -> tparams_map_t -> expr_fn_t -> Loc.t -> reason ->
    self_t -> classy_ast_t -> Sig.t
  val classtype: Context.t -> Sig.t -> Type.t
  val generate_tests: Context.t -> (Sig.t -> unit) -> Sig.t -> unit
  val check_super: Context.t -> Sig.t -> unit
end

val toplevels: Context.t -> decls:stmt_fn_t -> stmts:stmt_fn_t ->
  expr:expr_fn_t -> Sig.t -> unit
