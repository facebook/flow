open Core
open Nast
open Typed_ast_defs

(** Like Haskell's ">=>". Function composition in reverse.
 *
 * This symbol is borrowed from Ocaml Batteries Included, which
 * chooses % as the first character because of Ocaml's infix precedence
 * rules, which allows this to play well with pipes. *)
let ( %> ) f g = fun x -> g (f x)

type string_builder = {
  acc : string;
  indent : int;
  (** True if whitespace must be inserted on the next "append" *)
  needs_whitespace : bool;
  ty_map : Typed_ast_defs.tast_ty_ Pos.Map.t;
}

let rec append str builder =
  if builder.needs_whitespace then
    { builder with acc = builder.acc ^ " " ^ str}
  else
    { builder with acc = builder.acc ^ str; needs_whitespace = true }

(** Like append but ignores needs_whitespace. *)
and append_raw str builder =
  { builder with acc = builder.acc ^ str }

(** Ensures the next append does not add whitespace before it. *)
and no_whitespace builder =
  { builder with needs_whitespace = false }

and repeat n c builder =
  { builder with acc = builder.acc ^ (String.make n c) }

and newline builder =
  { builder with acc = builder.acc ^ "\n"; needs_whitespace = false }
  |> repeat builder.indent ' '

and with_indent f builder =
  let old_indent = builder.indent in
  let builder = f { builder with indent = builder.indent + 1 } in
  { builder with indent = old_indent }

and paren f =
  with_indent ((append_raw "(") %> no_whitespace %> f %> (append_raw ")"))

(** List.fold_left has the wrong param order in the ~f to do
  * easy composition and piping. Here we flip it. *)
and fold f xs builder =
  List.fold_left xs ~init:builder ~f:begin fun builder x ->
    f x builder
  end

let t_prim n_ty = append (match n_ty with
  | Nast.Tvoid -> "Tvoid"
  | Nast.Tint -> "Tint"
  | Nast.Tbool -> "Tbool"
  | Nast.Tfloat -> "Tbool"
  | Nast.Tstring -> "Tstring"
  | Nast.Tresource -> "Tresource"
  | Nast.Tnum -> "Tnum"
  | Nast.Tarraykey -> "Tarraykey"
  | Nast.Tnoreturn -> "Tnoreturn"
  )

let rec ty p builder = builder |> match (Pos.Map.get p builder.ty_map) with
  | None -> append "TAny"
  | Some t -> ty_ t

and ty_ = function
  | TNotImpl -> append "TNotImpl"
  | TAny -> append "TAny"
  | TNull -> append "TNull"
  | TOption ty -> paren (
    append "TOption"
    %> ty_ ty
  )
  | TClass ((_, name), tyl) -> paren (
    append "TClass"
    %> append name
    %> append "<"
    %> (fold ty_ tyl)
    %> append ">"
  )
  | TUnion tyl -> paren (
    append "TUnion"
    %> (fold ty_ tyl)
  )
  | TPrim n_ty -> paren (
    t_prim n_ty
    %> append "TPrim"
  )

let rec expr (p, n_e) builder = builder |>
  paren ((ty p) %> (append "expr")
    %> (match n_e with
      | Id (_, id) ->
          (append "Id")
          %> (append id)
      | Lvar (_, id) ->
          (append "Lvar")
          %> (append (Local_id.to_string id))
      | Int _ ->
          (append "Int")
      | Binop (_, e1, e2) ->
          (append "binop")
          %> expr e1
          %> expr e2
      | Call (_call_type, n_fun, n_args, n_unpacked_args) ->
          (append "Call")
          %> expr n_fun
          %> fold expr n_args
          %> fold expr n_unpacked_args
      | Fun_id (_p, id) ->
          (append "Fun_id")
          %> (append id)
      | Method_id (e, (_p, id)) ->
          (append "Method_id")
          %> (append id)
          %> expr e
      | _ -> (append "other")))

let rec stmt t_stmt builder = builder |>
  paren ((append "stmt")
    %> begin match t_stmt with
      | Expr t_expr -> newline %> expr t_expr
      | If (e, b1, b2) -> paren (
        append "If"
          %> newline
          %> expr e
          %> newline
          %> block b1
          %> newline
          %> block b2
        )
      | While (e, b) -> paren (
        append "While"
          %> newline
          %> expr e
          %> newline
          %> block b
        )
      | _ -> append "other stmt"
    end)
  %> newline

(** Full point-free-form doesn't work with recursive lets. *)
and block statements =
  fold stmt statements

let rec print_string ty_map ((n_funs : Nast.fun_ list), _, _, _) =
  let result = List.fold_left n_funs
    ~init:{acc = ""; indent = 0; needs_whitespace = false; ty_map = ty_map}
    ~f:begin fun builder n_fun ->
      builder |> fun_ n_fun |> newline
    end
  in
  result.acc

and fun_ {Nast.f_name; f_body; _} builder =
  builder |> (paren ( (append "fun")
    %> newline
    %> (paren (append ("name: " ^ (snd (f_name)))))
    %> newline
    %> func_body f_body))

and func_body f_body builder = builder |>
  (paren ( (append "body:")
    %> newline
    %> (match f_body with
      | NamedBody named_body -> block named_body.fnb_nast
      | UnnamedBody _ -> assert false)))
