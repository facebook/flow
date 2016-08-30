(** Like Typing_defs.locl ty, but without TVar, and with explicit null type. *)
type tast_ty_ =
  (** This type not yet implemented in Typed AST. *)
  | TNotImpl
  | TAny
  (** During typechecking, the null value gives rise to an unresolved optional.
   * But we know an expression whose unresolved type hasn't been refined
   * can only have a null value - so we have a singleton type for the
   * null value. *)
  | TNull
  | TClass of Nast.sid * tast_ty_ list
  | TUnion of tast_ty_ list
  | TOption of tast_ty_
  | TPrim of Nast.tprim
