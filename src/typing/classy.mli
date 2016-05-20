module Ast = Spider_monkey_ast
module Sig = Classy_sig

module Class : sig
  val mk_sig: Context.t ->
    Type.t SMap.t ->
    (Context.t -> Type.t SMap.t -> Ast.Expression.t -> Type.t) ->
    Loc.t ->
    Reason_js.t ->
    Type.t ->
    Ast.Class.t ->
    Sig.t
  val classtype: Context.t -> Sig.t -> Type.t
  val generate_tests: Context.t ->
    (Sig.t -> unit) ->
    Sig.t ->
    unit
  val check_super: Context.t -> Sig.t -> unit
end

module Interface : sig
  val mk_sig: Context.t ->
    Type.t SMap.t ->
    (Context.t -> Type.t SMap.t -> Ast.Expression.t -> Type.t) ->
    Loc.t ->
    Reason_js.t ->
    Type.t ->
    Ast.Statement.Interface.t ->
    Sig.t
  val classtype: Context.t -> Sig.t -> Type.t
  val generate_tests: Context.t ->
    (Sig.t -> unit) ->
    Sig.t ->
    unit
  val check_super: Context.t -> Sig.t -> unit
end

module DeclClass : sig
  val mk_sig: Context.t ->
    Type.t SMap.t ->
    (Context.t -> Type.t SMap.t -> Ast.Expression.t -> Type.t) ->
    Loc.t ->
    Reason_js.t ->
    Type.t ->
    Ast.Statement.Interface.t ->
    Sig.t
  val classtype: Context.t -> Sig.t -> Type.t
  val generate_tests: Context.t ->
    (Sig.t -> unit) ->
    Sig.t ->
    unit
  val check_super: Context.t -> Sig.t -> unit
end

val toplevels: Context.t ->
  decls:(Context.t -> Type.t SMap.t -> Ast.Statement.t list -> unit) ->
  stmts:(Context.t -> Type.t SMap.t -> Ast.Statement.t list -> unit) ->
  expr:(Context.t -> Type.t SMap.t -> Ast.Expression.t -> Type.t) ->
  Sig.t ->
  unit
