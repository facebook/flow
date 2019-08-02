(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type 'loc error =
  { loc: 'loc; desc: Lints.property_assignment_kind }

type 'loc errors = {
  public_property_errors: 'loc error list SMap.t;
  private_property_errors: 'loc error list SMap.t;
}

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

  (* READS *)

  method! identifier (ident: (ALoc.t, ALoc.t) Ast.Identifier.t) =
    ident

  method! jsx_identifier (ident: ALoc.t Ast.JSX.Identifier.t) =
    ident

  method! member loc (expr: (ALoc.t, ALoc.t) Ast.Expression.Member.t) =
    (match expr with
    | {
        Ast.Expression.Member._object = (_, Ast.Expression.This);
        property = ( Ast.Expression.Member.PropertyIdentifier _
                   | Ast.Expression.Member.PropertyPrivateName _
                   ) as property;
      } ->
      ignore @@ this#any_identifier loc @@ Flow_ast_utils.name_of_ident @@
        Ast.Expression.Member.(match property with
        | PropertyIdentifier id -> public_property loc id
        | PropertyPrivateName id -> private_property loc id
        | PropertyExpression _ -> failwith "match on expr makes this impossible"
        );
      expr
    | _ -> super#member loc expr
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

  (* PREVENT THIS FROM ESCAPING *)

  val mutable this_escape_errors:
    (ALoc.t * Lints.property_assignment_kind * Ssa_builder.With_ALoc.Env.t) list = []
  method this_escape_errors = this_escape_errors
  method private add_this_escape_error error =
    this_escape_errors <- error :: this_escape_errors

  method! expression expr =
    (match expr with
    | (loc, Ast.Expression.This) ->
      this#add_this_escape_error (loc, Lints.ThisBeforeEverythingInitialized, this#ssa_env)
    | _ -> ()
    );
    super#expression expr

  method! call loc (expr: ('loc, 'loc) Ast.Expression.Call.t) =
    (match expr.Ast.Expression.Call.callee with
    (* match on method calls *)
    | (_, Ast.Expression.Member {
        Ast.Expression.Member._object = (_, Ast.Expression.This);
        property = ( Ast.Expression.Member.PropertyIdentifier _
                   | Ast.Expression.Member.PropertyPrivateName _
                   );
      }) ->
      this#add_this_escape_error (loc, Lints.MethodCallBeforeEverythingInitialized, this#ssa_env)
    | _ -> ()
    );
    super#call loc expr
end

let eval_property_assignment class_body =
  let open Ast.Class in
  let properties =
    Core_list.filter_map ~f:(function
      | Body.Property (_, {
          Property.key = Ast.Expression.Object.Property.Identifier ((loc, _) as id);
          value = None;
          static = false;
          annot = _;
          variance = _;
        }) -> Some (public_property loc id)
      | Body.PrivateField (_, {
          PrivateField.key = (loc, _) as id;
          value = None;
          static = false;
          annot = _;
          variance = _;
        }) -> Some (private_property loc id)
      | _ -> None
    ) class_body
  in

  let ctor_body: (ALoc.t, ALoc.t) Ast.Statement.Block.t =
    Core_list.find_map ~f:(function
      | Body.Method (_, {
          Method.kind = Method.Constructor;
          value = (_, {
            Ast.Function.body = Ast.Function.BodyBlock (_, block);
            id = _; params = _; async = _; generator = _; predicate = _;
            return = _; tparams = _; sig_loc = _;
          });
          key = _; static = _; decorators = _;
        }) -> Some block
      | _ -> None
    ) class_body
    |> Option.value ~default:{ Ast.Statement.Block.body = [] }
  in

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

  (* We make heavy use of the Core_list.rev_* functions below because they are
   * tail recursive. We can do this freely because the order in which we
   * process the errors doesn't actually matter (Flow will sort the errors
   * before printing them anyway).
   *)
  let uninitialized_properties: (ALoc.t error * string) list =
    properties
    |> filter_uninitialized ssa_walk#final_ssa_env
    |> Core_list.rev_map ~f:(fun id -> (
        {
          loc = Flow_ast_utils.loc_of_ident id;
          desc = Lints.PropertyNotDefinitivelyInitialized;
        },
        Flow_ast_utils.name_of_ident id
      ))
  in
  let read_before_initialized: ALoc.t error list =
    ssa_walk#values
    |> Loc_collections.ALocMap.bindings
    |> Core_list.rev_filter_map ~f:(fun (read_loc, write_locs) ->
      if not_definitively_initialized write_locs then
        Some {
          loc = read_loc;
          desc = Lints.ReadFromUninitializedProperty;
        }
      else
        None
      )
  in
  let this_errors: ALoc.t error list =
    Core_list.rev_filter_map ~f:(fun (loc, desc, ssa_env) ->
      match filter_uninitialized ssa_env properties with
      | [] -> None
      | _ -> Some { loc; desc; }
    ) ssa_walk#this_escape_errors
  in
  (* It's better to append new to old b/c new is always a singleton *)
  let combine_voidable_checks old new_ = Core_list.rev_append new_ old in
  let single_property_errors errors checks =
    List.fold_left (fun acc (error, prefixed_name) ->
      (* Check if it is private first since `this.` is a prefix of `this.#` *)
      if String_utils.string_starts_with prefixed_name "this.#" then
        { acc with private_property_errors =
          SMap.add ~combine:combine_voidable_checks
            (String_utils.lstrip prefixed_name "this.#")
            [error] acc.private_property_errors
        }
      else if String_utils.string_starts_with prefixed_name "this." then
        { acc with public_property_errors =
          SMap.add ~combine:combine_voidable_checks
            (String_utils.lstrip prefixed_name "this.")
            [error] acc.public_property_errors
        }
      else
        acc
    ) checks errors
  in
  { public_property_errors = SMap.empty; private_property_errors = SMap.empty; }
  |> single_property_errors uninitialized_properties,
  Core_list.rev_append read_before_initialized this_errors
