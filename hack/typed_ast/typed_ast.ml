let rec save_ty pos_ty_map ty pos t_env =
  let ty = to_tast_ty t_env ty in
  pos_ty_map := Pos.Map.add pos ty !pos_ty_map

and to_tast_ty t_env ty = match ty with
    | _, Typing_defs.Tprim n_prim -> Typed_ast_defs.TPrim n_prim
    | _, Typing_defs.Tclass (name, tyl) ->
      Typed_ast_defs.TClass (name, to_tast_tyl t_env tyl)
    | _, Typing_defs.Toption ty ->
      let ty = to_tast_ty t_env ty in
      (match ty with
      | Typed_ast_defs.TAny -> Typed_ast_defs.TNull
      | _ -> Typed_ast_defs.TOption ty)
    | _, Typing_defs.Tunresolved tyl ->
      Typed_ast_defs.TUnion (to_tast_tyl t_env tyl)
    (** Locl types in the map should be expanded already. *)
    | _, Typing_defs.Tvar _ ->
      (** We discard the env from expanding. I don't think we need it since
       * it's only being modified as a perf optimization - shortening tvar
       * expansion paths. *)
      let _, ty = Typing_env.expand_type t_env ty in
      to_tast_ty t_env ty
    | _, Typing_defs.Tany -> Typed_ast_defs.TAny
    | _, _ -> Typed_ast_defs.TNotImpl

and to_tast_tyl t_env tyl =
  List.map (to_tast_ty t_env) tyl
