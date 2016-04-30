type t

(* build up a params value *)
val empty: Type.t -> t
val mk: Context.t ->
  (Type.t SMap.t) -> (* type params map *)
  Spider_monkey_ast.Function.t ->
  Type.t -> (* implicit `this` pseudoparameter *)
  t

(* type of the this pseudoparameter*)
val this: t -> Type.t

(* name of each param, in order *)
(* destructured params will be "_" *)
val names: t -> string list

(* type of each param in the param list *)
val tlist: t -> Type.t list

(* iterates over all bindings, traversing through any destructued
   bindings as well, in source order of declaration *)
val iter: (string * Type.t * Loc.t -> unit) -> t -> unit

(* if there is a default for this binding, run provided function *)
val with_default: string ->
  (Spider_monkey_ast.Expression.t Default.t -> unit) -> (* handler fn *)
  t -> unit

val subst: Context.t ->
  (Type.t SMap.t) -> (* type params map *)
  t -> t
