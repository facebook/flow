type self_t = Type.t
type tparams_map_t = Type.t SMap.t
type expr_fn_t = Context.t -> tparams_map_t -> Spider_monkey_ast.Expression.t -> Type.t
type stmt_fn_t = Context.t -> tparams_map_t -> Spider_monkey_ast.Statement.t list -> unit

module Class : sig
  type classy_ast_t = Spider_monkey_ast.Class.t
  val mk_sig: Context.t -> tparams_map_t -> expr_fn_t -> Loc.t -> Reason_js.t ->
    self_t -> classy_ast_t -> Classy_sig.t
  val classtype: Context.t -> Classy_sig.t -> Type.t
  val generate_tests: Context.t -> (Classy_sig.t -> unit) -> Classy_sig.t -> unit
  val check_super: Context.t -> Classy_sig.t -> unit
end

module Interface : sig
  type classy_ast_t = Spider_monkey_ast.Statement.Interface.t
  val mk_sig: Context.t -> tparams_map_t -> expr_fn_t -> Loc.t -> Reason_js.t ->
    self_t -> classy_ast_t -> Classy_sig.t
  val classtype: Context.t -> Classy_sig.t -> Type.t
  val generate_tests: Context.t -> (Classy_sig.t -> unit) -> Classy_sig.t -> unit
  val check_super: Context.t -> Classy_sig.t -> unit
end

module DeclClass : sig
  type classy_ast_t = Spider_monkey_ast.Statement.Interface.t
  val mk_sig: Context.t -> tparams_map_t -> expr_fn_t -> Loc.t -> Reason_js.t ->
    self_t -> classy_ast_t -> Classy_sig.t
  val classtype: Context.t -> Classy_sig.t -> Type.t
  val generate_tests: Context.t -> (Classy_sig.t -> unit) -> Classy_sig.t -> unit
  val check_super: Context.t -> Classy_sig.t -> unit
end

val toplevels: Context.t -> decls:stmt_fn_t -> stmts:stmt_fn_t ->
  expr:expr_fn_t -> Classy_sig.t -> unit
