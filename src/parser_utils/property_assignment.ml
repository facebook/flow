(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type 'loc error = {
  loc: 'loc;
  desc: Lints.property_assignment_kind;
}

type 'loc errors = {
  public_property_errors: 'loc error list SMap.t;
  private_property_errors: 'loc error list SMap.t;
}

let public_property loc ident =
  let (_, ({ Ast.Identifier.name; comments = _ } as r)) = ident in
  (loc, { r with Ast.Identifier.name = "this." ^ name })

let private_property loc ident =
  let (_, (_, ({ Ast.Identifier.name; comments = _ } as r))) = ident in
  (loc, { r with Ast.Identifier.name = "this.#" ^ name })

let not_definitively_initialized (write_locs : Ssa_api.With_ALoc.write_locs) : bool =
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
    (ssa_env : Ssa_builder.With_ALoc.Env.t)
    (properties : (ALoc.t, ALoc.t) Flow_ast.Identifier.t list) :
    (ALoc.t, ALoc.t) Flow_ast.Identifier.t list =
  let ssa_env = SMap.map Ssa_builder.With_ALoc.Val.simplify ssa_env in
  Base.List.filter
    ~f:(fun id ->
      match SMap.find_opt (Flow_ast_utils.name_of_ident id) ssa_env with
      | Some write_locs -> not_definitively_initialized write_locs
      | None -> true)
    properties

class property_assignment (property_names : SSet.t) =
  object (this)
    inherit Ssa_builder.With_ALoc.ssa_builder as super

    (* ABRUPT COMPLETIONS *)
    method expecting_return_or_throw (f : unit -> unit) : unit =
      let completion_state = this#run_to_completion f in
      this#commit_abrupt_completion_matching
        Ssa_builder.With_ALoc.AbruptCompletion.(mem [return; throw])
        completion_state

    (* WRITES *)

    (* Keep track of the final_ssa_env so that we can check that all properties
     * are initialized when the constructor exits.
     *)
    val mutable final_ssa_env : Ssa_builder.With_ALoc.Env.t = SMap.empty

    method final_ssa_env = final_ssa_env

    method! pop_ssa_env saved_state =
      final_ssa_env <- this#ssa_env;
      super#pop_ssa_env saved_state

    method initialize_property property_id value =
      (match snd value with
      | Ast.Expression.ArrowFunction _
      | Ast.Expression.Function _ ->
        ()
      | _ -> ignore @@ this#expression value);
      ignore @@ this#pattern_identifier property_id

    (* READS *)
    val mutable read_loc_metadata : string Loc_collections.ALocMap.t = Loc_collections.ALocMap.empty

    method metadata_of_read_loc (read_loc : ALoc.t) : string option =
      Loc_collections.ALocMap.find_opt read_loc read_loc_metadata

    method! identifier (ident : (ALoc.t, ALoc.t) Ast.Identifier.t) = ident

    method! jsx_identifier (ident : ALoc.t Ast.JSX.Identifier.t) = ident

    method! member loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Member.t) =
      match expr with
      | {
       Ast.Expression.Member._object = (_, Ast.Expression.This);
       property =
         (Ast.Expression.Member.PropertyIdentifier _ | Ast.Expression.Member.PropertyPrivateName _)
         as property;
      } ->
        let property_name : string =
          Flow_ast_utils.name_of_ident
            Ast.Expression.Member.(
              match property with
              | PropertyIdentifier id -> public_property loc id
              | PropertyPrivateName id -> private_property loc id
              | PropertyExpression _ -> failwith "match on expr makes this impossible")
        in
        read_loc_metadata <- Loc_collections.ALocMap.add loc property_name read_loc_metadata;
        ignore @@ this#any_identifier loc property_name;
        expr
      | _ -> super#member loc expr

    (* EVALUATION ORDER *)
    method! assignment loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Assignment.t) =
      Ast.Expression.Assignment.(
        let { operator; left; right } = expr in
        match left with
        | ( _,
            Ast.Pattern.Expression
              ( member_loc,
                Ast.Expression.Member
                  ( {
                      Ast.Expression.Member._object = (_, Ast.Expression.This);
                      property =
                        ( Ast.Expression.Member.PropertyIdentifier _
                        | Ast.Expression.Member.PropertyPrivateName _ ) as property;
                    } as left_member ) ) ) ->
          (match operator with
          | None ->
            (* given `this.x = e`, read e then write x *)
            this#initialize_property
              Ast.Expression.Member.(
                match property with
                | PropertyIdentifier id -> public_property member_loc id
                | PropertyPrivateName id -> private_property member_loc id
                | PropertyExpression _ -> failwith "match on expr makes this impossible")
              right
          | Some _ ->
            (* given `this.x += e`, read x then read e *)
            ignore @@ this#member member_loc left_member;
            ignore @@ this#expression right)
          (* This expression technically also writes to x, but we don't model that
           * here since in order for `this.x += e` to not cause an error, x must
           * already be assigned anyway. Also, not writing to x here leads to
           * more understandable error messages, as the write would mask the
           * PropertyNotDefinitelyInitialized error.
           *);

          expr
        | _ -> super#assignment loc expr)

    (* PREVENT THIS FROM ESCAPING *)
    val mutable this_escape_errors
        : (ALoc.t * Lints.property_assignment_kind * Ssa_builder.With_ALoc.Env.t) list =
      []

    method this_escape_errors = this_escape_errors

    method private add_this_escape_error error = this_escape_errors <- error :: this_escape_errors

    method! expression expr =
      (match expr with
      | (loc, Ast.Expression.This) ->
        this#add_this_escape_error (loc, Lints.ThisBeforeEverythingInitialized, this#ssa_env)
      | _ -> ());
      super#expression expr

    method! call loc (expr : ('loc, 'loc) Ast.Expression.Call.t) =
      (match expr.Ast.Expression.Call.callee with
      (* match on method calls *)
      | ( member_loc,
          Ast.Expression.Member
            {
              Ast.Expression.Member._object = (_, Ast.Expression.This);
              property =
                ( Ast.Expression.Member.PropertyIdentifier _
                | Ast.Expression.Member.PropertyPrivateName _ ) as property;
            } ) ->
        let name =
          Flow_ast_utils.name_of_ident
          @@ Ast.Expression.Member.(
               match property with
               | PropertyIdentifier id -> public_property member_loc id
               | PropertyPrivateName id -> private_property member_loc id
               | PropertyExpression _ -> failwith "match on expr.callee makes this impossible")
        in
        let error =
          if SSet.mem name property_names then
            Lints.PropertyFunctionCallBeforeEverythingInitialized
          else
            Lints.MethodCallBeforeEverythingInitialized
        in
        this#add_this_escape_error (loc, error, this#ssa_env)
      | _ -> ());
      super#call loc expr
  end

let eval_property_assignment class_body =
  Ast.Class.(
    let property_declarations =
      Base.List.filter_map
        ~f:(function
          | Body.Property
              ( _,
                {
                  Property.key = Ast.Expression.Object.Property.Identifier ((loc, _) as id);
                  value;
                  static = false;
                  annot = _;
                  variance = _;
                } ) ->
            Some (public_property loc id, value)
          | Body.PrivateField
              ( _,
                {
                  PrivateField.key = (loc, _) as id;
                  value;
                  static = false;
                  annot = _;
                  variance = _;
                } ) ->
            Some (private_property loc id, value)
          | _ -> None)
        class_body
    in
    let ctor_body : (ALoc.t, ALoc.t) Ast.Statement.Block.t =
      Base.List.find_map
        ~f:(function
          | Body.Method
              ( _,
                {
                  Method.kind = Method.Constructor;
                  value =
                    ( _,
                      {
                        Ast.Function.body = Ast.Function.BodyBlock (_, block);
                        id = _;
                        params = _;
                        async = _;
                        generator = _;
                        predicate = _;
                        return = _;
                        tparams = _;
                        sig_loc = _;
                      } );
                  key = _;
                  static = _;
                  decorators = _;
                } ) ->
            Some block
          | _ -> None)
        class_body
      |> Option.value ~default:{ Ast.Statement.Block.body = [] }
    in
    let properties = Base.List.map ~f:fst property_declarations in
    let bindings : ALoc.t Hoister.Bindings.t =
      List.fold_left
        (fun bindings property -> Hoister.Bindings.add property bindings)
        Hoister.Bindings.empty
        properties
    in
    let property_names =
      List.fold_left
        (fun acc property -> SSet.add (Flow_ast_utils.name_of_ident property) acc)
        SSet.empty
        properties
    in
    let ssa_walk = new property_assignment property_names in
    ignore
    @@ ssa_walk#with_bindings
         ALoc.none
         bindings
         (fun body ->
           ssa_walk#expecting_return_or_throw (fun () ->
               List.iter
                 (function
                   | (property_id, Some default_initializer) ->
                     ssa_walk#initialize_property property_id default_initializer
                   | _ -> ())
                 property_declarations;
               ignore @@ ssa_walk#block ALoc.none body);
           body)
         ctor_body;

    (* We make heavy use of the Base.List.rev_* functions below because they are
     * tail recursive. We can do this freely because the order in which we
     * process the errors doesn't actually matter (Flow will sort the errors
     * before printing them anyway).
     *)
    let uninitialized_properties : (ALoc.t error * string) list =
      properties
      |> filter_uninitialized ssa_walk#final_ssa_env
      |> Base.List.rev_map ~f:(fun id ->
             ( {
                 loc = Flow_ast_utils.loc_of_ident id;
                 desc = Lints.PropertyNotDefinitelyInitialized;
               },
               Flow_ast_utils.name_of_ident id ))
    in
    let read_before_initialized : (ALoc.t error * string) list =
      ssa_walk#values
      |> Loc_collections.ALocMap.bindings
      |> Base.List.rev_filter_map ~f:(fun (read_loc, write_locs) ->
             if not_definitively_initialized write_locs then
               ssa_walk#metadata_of_read_loc read_loc
               |> Option.map ~f:(fun name ->
                      ({ loc = read_loc; desc = Lints.ReadFromUninitializedProperty }, name))
             else
               None)
    in
    let this_errors : (ALoc.t error * string Nel.t) list =
      Base.List.rev_filter_map
        ~f:(fun (loc, desc, ssa_env) ->
          filter_uninitialized ssa_env properties
          |> Base.List.map ~f:Flow_ast_utils.name_of_ident
          |> Nel.of_list
          |> Option.map ~f:(fun uninitialized_properties ->
                 ({ loc; desc }, uninitialized_properties)))
        ssa_walk#this_escape_errors
    in
    (* It's better to append new to old b/c new is always a singleton *)
    let combine_voidable_checks old new_ = Base.List.rev_append new_ old in
    let add_to_errors error errors prefixed_name =
      (* Check if it is private first since `this.` is a prefix of `this.#` *)
      if String_utils.string_starts_with prefixed_name "this.#" then
        {
          errors with
          private_property_errors =
            SMap.add
              ~combine:combine_voidable_checks
              (String_utils.lstrip prefixed_name "this.#")
              [error]
              errors.private_property_errors;
        }
      else if String_utils.string_starts_with prefixed_name "this." then
        {
          errors with
          public_property_errors =
            SMap.add
              ~combine:combine_voidable_checks
              (String_utils.lstrip prefixed_name "this.")
              [error]
              errors.public_property_errors;
        }
      else
        errors
    in
    let single_property_errors errors checks =
      List.fold_left
        (fun acc (error, prefixed_name) -> add_to_errors error acc prefixed_name)
        checks
        errors
    in
    let multi_property_errors errors checks =
      List.fold_left
        (fun acc (error, names) -> Nel.fold_left (add_to_errors error) acc names)
        checks
        errors
    in
    { public_property_errors = SMap.empty; private_property_errors = SMap.empty }
    |> single_property_errors uninitialized_properties
    |> single_property_errors read_before_initialized
    |> multi_property_errors this_errors)
