(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

let normalize_type_loc_and_comment_mapper =
  object
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    method! syntax_opt _ = None

    method! identifier (_, id) = (Loc.none, id)

    method! type_ (_, t) = super#type_ (Loc.none, t)
  end

module AstTypeSet = Flow_set.Make (struct
  type t = (Loc.t, Loc.t) Ast.Type.t

  let compare = Stdlib.compare
end)

let mod_renders_arg ~unwrap_iterable =
  let open Ast.Type in
  (* Ok Some -> transformed
   * Ok None -> dropped
   * Error -> unrecognized, do nothing *)
  let mod_t = function
    | (_, Nullable { Nullable.argument; _ }) -> Ok (Some argument)
    | (_, BooleanLiteral { Ast.BooleanLiteral.value = false; _ }) -> Ok None
    | (_, Null _) -> Ok None
    | (_, Void _) -> Ok None
    | (_, Array { Array.argument; _ }) when unwrap_iterable -> Ok (Some argument)
    | ( _,
        Generic
          {
            Generic.id =
              Generic.Identifier.Unqualified
                (_, { Ast.Identifier.name = "$ReadOnlyArray" | "Array" | "Iterable" | "Set"; _ });
            targs = Some (_, { TypeArgs.arguments = [t]; _ });
            _;
          }
      ) ->
      Ok (Some t)
    | _ -> Error ()
  in
  let mod_ts ts =
    let (ts_rev, transformed) =
      Base.List.fold ts ~init:([], false) ~f:(fun (collector, transformed) t ->
          match mod_t t with
          | Ok (Some t) -> (t :: collector, true)
          | Ok None -> (collector, true)
          | Error () -> (t :: collector, transformed)
      )
    in
    match (ts_rev, transformed) with
    | ([], _) -> None
    | (_, false) -> None
    | (last :: ts_rev_rest, true) ->
      (* People might write `renders* (Foo | Array<Foo>)` thinking that they need to do the
       * extra nesting themselves. A naive fix will produce renders* (Foo | Foo).
       * We can deduplicate with a set by stripping comments and locations. *)
      Some
        (Base.List.fold
           ts_rev_rest
           ~init:
             (AstTypeSet.singleton (normalize_type_loc_and_comment_mapper#type_ last), Nel.one last)
           ~f:(fun (seen, acc) t ->
             let normalized = normalize_type_loc_and_comment_mapper#type_ t in
             if AstTypeSet.mem normalized seen then
               (seen, acc)
             else
               (AstTypeSet.add normalized seen, Nel.cons t acc))
        |> snd
        )
  in
  function
  | (loc, Union { Union.types = (t0, t1, ts); comments }) ->
    (match mod_ts (t0 :: t1 :: ts) with
    | None -> None
    | Some (t, []) -> Some t
    | Some (t0, t1 :: ts) -> Some (loc, Union { Union.types = (t0, t1, ts); comments }))
  | t ->
    (match mod_t t with
    | Ok (Some t) -> Some t
    (* If we are dealing with a single type, we can't do anything with it
     * if it can't be transformed or it needs to be completely removed. *)
    | Ok None
    | Error () ->
      None)

class change_to_renders_maybe_mapper render_arg_loc =
  object (_this)
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    method! render_type t =
      let open Ast.Type.Renders in
      let { argument; _ } = t in
      if Loc.equal render_arg_loc (fst argument) then
        match mod_renders_arg ~unwrap_iterable:false argument with
        | None -> super#render_type t
        | Some argument' -> { t with variant = Maybe; argument = argument' }
      else
        super#render_type t
  end

class change_to_renders_star_mapper render_arg_loc =
  object (_this)
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    method! render_type t =
      let open Ast.Type.Renders in
      let { argument; _ } = t in
      if Loc.equal render_arg_loc (fst argument) then
        match mod_renders_arg ~unwrap_iterable:true argument with
        | None -> super#render_type t
        | Some argument' -> { t with variant = Star; argument = argument' }
      else
        super#render_type t
  end

let to_renders_maybe_with_best_effort_fixes ast render_arg_loc =
  (new change_to_renders_maybe_mapper render_arg_loc)#program ast

let to_renders_star_with_best_effort_fixes ast render_arg_loc =
  (new change_to_renders_star_mapper render_arg_loc)#program ast
