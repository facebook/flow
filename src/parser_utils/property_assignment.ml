(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* NOTE: This is a WIP and should not be used for anything yet *)

module Ast = Flow_ast

let public_property loc ident =
  let _, ({ Ast.Identifier.name; comments = _ } as r) = ident in
  loc, { r with Ast.Identifier.name = "this." ^ name }

let private_property loc ident =
  let _, (_, ({ Ast.Identifier.name; comments = _ } as r)) = ident in
  loc, { r with Ast.Identifier.name = "this.#" ^ name }

let not_definitively_initialized (write_locs: Ssa_api.With_ALoc.write_locs): bool =
  (* write_locs = [] corresponds to a binding whose "value" in the ssa_env is
   * Val.empty. It typically will not be the final result unless (1) something
   * is declared but never initialized (2) something needs to be recorded after
   * an abnormal control flow exit (e.g., after return, the environment is
   * "empty"d out).
   *)
  write_locs = [] || List.mem Ssa_api.With_ALoc.uninitialized write_locs

(* NOTE: This function should only be called after the ssa walk that produces
 * the `ssa_env` argument has finished. Simplifying the values in the ssa
 * environment mid-walk will throw an exception if all values have not been
 * resolved. The invariant we are relying on here is that by the time the walk
 * completes, all values must be resolved so it is safe to call this function.
 *)
let filter_uninitialized
  (ssa_env: Ssa_builder.With_ALoc.Env.t)
  (properties: (ALoc.t, ALoc.t) Flow_ast.Identifier.t list)
: (ALoc.t, ALoc.t) Flow_ast.Identifier.t list =
  let ssa_env = SMap.map Ssa_builder.With_ALoc.Val.simplify ssa_env in
  Core_list.filter ~f:(fun id ->
    match SMap.get (Flow_ast_utils.name_of_ident id) ssa_env with
    | Some write_locs -> not_definitively_initialized write_locs
    | None -> true
  ) properties;

class property_assignment = object(this)
  inherit Ssa_builder.With_ALoc.ssa_builder as super

  (* ABRUPT COMPLETIONS *)

  method expecting_return_or_throw (f: unit -> unit): unit =
    let completion_state = this#run_to_completion f in
    this#commit_abrupt_completion_matching
      Ssa_builder.With_ALoc.AbruptCompletion.(mem [return; throw])
      completion_state

  (* WRITES *)

  (* Keep track of the final_ssa_env so that we can check that all properties
   * are initialized when the constructor exits.
   *)
  val mutable final_ssa_env: Ssa_builder.With_ALoc.Env.t = SMap.empty
  method final_ssa_env = final_ssa_env
  method! pop_ssa_env saved_state =
    final_ssa_env <- this#ssa_env;
    super#pop_ssa_env saved_state

  method! pattern_expression (expr: (ALoc.t, ALoc.t) Ast.Expression.t) =
    (match expr with
    | loc, Ast.Expression.Member {
        Ast.Expression.Member._object = (_, Ast.Expression.This);
        property = ( Ast.Expression.Member.PropertyIdentifier _
                   | Ast.Expression.Member.PropertyPrivateName _
                   ) as property;
      } ->
      ignore @@ this#pattern_identifier @@
        Ast.Expression.Member.(match property with
        | PropertyIdentifier id -> public_property loc id
        | PropertyPrivateName id -> private_property loc id
        | PropertyExpression _ -> failwith "match on expr makes this impossible"
        );
      expr
    | _ -> super#pattern_expression expr
    )

  (* EVALUATION ORDER *)

  method! assignment loc (expr: (ALoc.t, ALoc.t) Ast.Expression.Assignment.t) =
    let open Ast.Expression.Assignment in
    let { operator; left; right } = expr in
    (match left with
    | _, Ast.Pattern.Expression (member_loc, Ast.Expression.Member ({
        Ast.Expression.Member._object = (_, Ast.Expression.This);
        property = ( Ast.Expression.Member.PropertyIdentifier _
                   | Ast.Expression.Member.PropertyPrivateName _
                   );
      } as left_member)) ->
      (match operator with
      | None ->
        (* given `this.x = e`, read e then write x *)
        ignore @@ this#expression right;
        ignore @@ this#assignment_pattern left
      | Some _ ->
        (* given `this.x += e`, read x then read e *)
        ignore @@ this#member member_loc left_member;
        ignore @@ this#expression right
        (* This expression technically also writes to x, but we don't model that
         * here since in order for `this.x += e` to not cause an error, x must
         * already be assigned anyway. Also, not writing to x here leads to
         * more understandable error messages, as the write would mask the
         * PropertyNotDefinitivelyInitialized error.
         *)
      );
      expr
    | _ -> super#assignment loc expr
    )
end

let eval_property_assignment properties ctor_body =
  let bindings: ALoc.t Hoister.Bindings.t =
    List.fold_left (fun bindings property ->
      Hoister.Bindings.add property bindings
    ) Hoister.Bindings.empty properties
  in

  let ssa_walk = new property_assignment in
  ignore @@ ssa_walk#with_bindings ALoc.none bindings (fun body ->
    ssa_walk#expecting_return_or_throw (fun () ->
      ignore @@ ssa_walk#block ALoc.none body
    );
    body
  ) ctor_body;

  filter_uninitialized ssa_walk#final_ssa_env properties
