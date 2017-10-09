type t

(* build up a params value *)
val empty: t

val mk: Context.t ->
  (Type.t SMap.t) -> (* type params map *)
  expr:(Context.t -> Loc.t Ast.Expression.t -> Type.t) ->
  Loc.t Ast.Function.t ->
  t

val convert: Context.t ->
  (Type.t SMap.t) -> (* type params map *)
  Loc.t Ast.Type.Function.t ->
  t

(* name of each param, in order *)
(* destructured params will be "_" *)
val names: t -> string list

(* type of each param in the param list *)
val tlist: t -> Type.t list

(* The rest param *)
val rest: t -> (string option * Loc.t * Type.t) option

(* iterates over all bindings, traversing through any destructued
   bindings as well, in source order of declaration *)
val iter: (string * Type.t * Loc.t -> unit) -> t -> unit

(* if there is a default for this binding, run provided function *)
val with_default: string ->
  (Loc.t Ast.Expression.t Default.t -> unit) -> (* handler fn *)
  t -> unit

val subst: Context.t ->
  (Type.t SMap.t) -> (* type params map *)
  t -> t
