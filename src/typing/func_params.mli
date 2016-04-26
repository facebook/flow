type t

(* build up a params value *)
val empty: t
val add: Context.t ->
  (Type.t SMap.t) -> (* type params map *)
  t ->
  Spider_monkey_ast.Pattern.t ->
  Spider_monkey_ast.Expression.t option -> (* default expr *)
  t
val add_rest: Context.t ->
  (Type.t SMap.t) -> (* type params map *)
  t ->
  Spider_monkey_ast.Identifier.t ->
  t

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
