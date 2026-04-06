(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Flow_ast_visitor

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
  let (_, { Ast.PrivateName.name; comments }) = ident in
  (loc, { Ast.Identifier.name = "this.#" ^ name; comments })

module AbruptCompletion = Ssa_builder.With_ALoc.AbruptCompletion

(* Boolean init environment: true = definitely initialized, false = potentially uninitialized *)
type init_env = bool SMap.t

let merge_envs (env1 : init_env) (env2 : init_env) : init_env =
  SMap.merge
    (fun _ a b ->
      Some
        (match (a, b) with
        | (Some a, Some b) -> a && b
        | _ -> false))
    env1
    env2

(* Unreachable environment: all properties are "true" so AND-merging
   with a reachable env yields the reachable env's values *)
let unreachable_env (env : init_env) : init_env = SMap.map (fun _ -> true) env

let run_to_completion = Ssa_builder.With_ALoc.run_to_completion

let from_completion = Ssa_builder.With_ALoc.from_completion

let merge_completion_states = Ssa_builder.With_ALoc.merge_completion_states

let run_cf = Ssa_builder.With_ALoc.run_cf

class property_assignment
  (property_names : SSet.t) (properties : (ALoc.t, ALoc.t) Flow_ast.Identifier.t list) =
  object (this)
    inherit [ALoc.t] Flow_ast_mapper.mapper as super

    val mutable init_env : init_env =
      List.fold_left
        (fun acc id -> SMap.add (Flow_ast_utils.name_of_ident id) false acc)
        SMap.empty
        properties

    method init_env = init_env

    val mutable possible_labeled_continues = []

    (* ABRUPT COMPLETIONS *)
    val mutable abrupt_completion_envs : (AbruptCompletion.t * init_env) list = []

    method private raise_abrupt_completion : 'a. AbruptCompletion.t -> 'a =
      fun ac ->
        let env = init_env in
        init_env <- unreachable_env init_env;
        abrupt_completion_envs <- (ac, env) :: abrupt_completion_envs;
        raise (AbruptCompletion.Exn ac)

    method private expecting_abrupt_completions f =
      let saved = abrupt_completion_envs in
      abrupt_completion_envs <- [];
      run_cf f ~finally:(fun () ->
          abrupt_completion_envs <- List.rev_append saved abrupt_completion_envs
      )

    method private commit_abrupt_completion_matching filter completion_state =
      let (matching, non_matching) =
        List.partition (fun (ac, _env) -> filter ac) abrupt_completion_envs
      in
      if matching <> [] then (
        List.iter (fun (_ac, env) -> this#merge_remote_env env) matching;
        abrupt_completion_envs <- non_matching
      ) else
        match completion_state with
        | Some ac when not (filter ac) -> raise (AbruptCompletion.Exn ac)
        | _ -> ()

    method expecting_return_or_throw (f : unit -> unit) : unit =
      let completion_state = run_to_completion f in
      this#commit_abrupt_completion_matching AbruptCompletion.(mem [return; throw]) completion_state

    (* WRITES *)
    method private merge_remote_env (env : init_env) : unit = init_env <- merge_envs init_env env

    method private merge_self_env (env : init_env) : unit = init_env <- merge_envs init_env env

    method private reset_env (env : init_env) : unit = init_env <- env

    method initialize_property property_id value =
      (match snd value with
      | Ast.Expression.ArrowFunction _
      | Ast.Expression.Function _ ->
        ()
      | _ -> ignore @@ this#expression value);
      let name = Flow_ast_utils.name_of_ident property_id in
      if SMap.mem name init_env then init_env <- SMap.add name true init_env

    (* READS *)
    val mutable read_errors : (ALoc.t * string) list = []

    method read_errors = read_errors

    method! identifier (ident : (ALoc.t, ALoc.t) Ast.Identifier.t) = ident

    method! jsx_element_name_identifier (ident : (ALoc.t, ALoc.t) Ast.JSX.Identifier.t) = ident

    method! member loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Member.t) =
      match expr with
      | {
       Ast.Expression.Member._object = (_, Ast.Expression.This _);
       property =
         (Ast.Expression.Member.PropertyIdentifier _ | Ast.Expression.Member.PropertyPrivateName _)
         as property;
       comments = _;
      } ->
        let property_name : string =
          Flow_ast_utils.name_of_ident
            Ast.Expression.Member.(
              match property with
              | PropertyIdentifier id -> public_property loc id
              | PropertyPrivateName id -> private_property loc id
              | PropertyExpression _ -> failwith "match on expr makes this impossible"
            )
        in
        (* Check if the property is tracked and not yet initialized *)
        (match SMap.find_opt property_name init_env with
        | Some false -> read_errors <- (loc, property_name) :: read_errors
        | _ -> ());
        expr
      | _ -> super#member loc expr

    (* EVALUATION ORDER *)
    method! assignment loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Assignment.t) =
      let open Ast.Expression.Assignment in
      let { operator; left; right; comments = _ } = expr in
      let (left, _) = Flow_ast_utils.unwrap_nonnull_lhs left in
      match left with
      | ( _,
          Ast.Pattern.Expression
            ( member_loc,
              Ast.Expression.Member
                ( {
                    Ast.Expression.Member._object = (_, Ast.Expression.This _);
                    property =
                      ( Ast.Expression.Member.PropertyIdentifier _
                      | Ast.Expression.Member.PropertyPrivateName _ ) as property;
                    comments = _;
                  } as left_member
                )
            )
        ) ->
        (match operator with
        | None ->
          (* given `this.x = e`, read e then write x *)
          this#initialize_property
            Ast.Expression.Member.(
              match property with
              | PropertyIdentifier id -> public_property member_loc id
              | PropertyPrivateName id -> private_property member_loc id
              | PropertyExpression _ -> failwith "match on expr makes this impossible"
            )
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
      | _ -> super#assignment loc expr

    (* PREVENT THIS FROM ESCAPING *)
    val mutable this_escape_errors : (ALoc.t * Lints.property_assignment_kind * init_env) list = []

    method this_escape_errors = this_escape_errors

    method private add_this_escape_error error = this_escape_errors <- error :: this_escape_errors

    method! expression expr =
      (match expr with
      | (loc, Ast.Expression.This _) ->
        this#add_this_escape_error (loc, Lints.ThisBeforeEverythingInitialized, init_env)
      | _ -> ());
      super#expression expr

    method! call loc (expr : ('loc, 'loc) Ast.Expression.Call.t) =
      (match expr.Ast.Expression.Call.callee with
      | ( member_loc,
          Ast.Expression.Member
            {
              Ast.Expression.Member._object = (_, Ast.Expression.This _);
              property =
                ( Ast.Expression.Member.PropertyIdentifier _
                | Ast.Expression.Member.PropertyPrivateName _ ) as property;
              comments = _;
            }
        ) ->
        let name =
          Flow_ast_utils.name_of_ident
          @@ Ast.Expression.Member.(
               match property with
               | PropertyIdentifier id -> public_property member_loc id
               | PropertyPrivateName id -> private_property member_loc id
               | PropertyExpression _ -> failwith "match on expr.callee makes this impossible"
             )
        in
        let error =
          if SSet.mem name property_names then
            Lints.PropertyFunctionCallBeforeEverythingInitialized
          else
            Lints.MethodCallBeforeEverythingInitialized
        in
        this#add_this_escape_error (loc, error, init_env)
      | _ -> ());
      super#call loc expr

    (* CONTROL FLOW *)

    method! if_statement _loc (stmt : (ALoc.t, ALoc.t) Ast.Statement.If.t) =
      let open Ast.Statement.If in
      let { test; consequent; alternate; _ } = stmt in
      ignore @@ this#expression test;
      let env0 = init_env in
      let then_completion_state =
        run_to_completion (fun () ->
            ignore @@ this#if_consequent_statement ~has_else:(alternate <> None) consequent
        )
      in
      let env1 = init_env in
      this#reset_env env0;
      let else_completion_state =
        run_to_completion (fun () ->
            run_opt
              (fun (loc, { Alternate.body; comments }) ->
                (loc, { Alternate.body = this#statement body; comments }))
              alternate
        )
      in
      this#merge_self_env env1;
      merge_completion_states (then_completion_state, [else_completion_state]);
      stmt

    method! while_ _loc (stmt : (ALoc.t, ALoc.t) Ast.Statement.While.t) =
      this#expecting_abrupt_completions (fun () ->
          let continues = AbruptCompletion.continue None :: possible_labeled_continues in
          let open Ast.Statement.While in
          let { test; body; comments = _ } = stmt in
          ignore @@ this#expression test;
          let env_after_test = init_env in
          let loop_completion_state = run_to_completion (fun () -> ignore @@ this#statement body) in
          let loop_completion_state =
            run_to_completion (fun () ->
                this#commit_abrupt_completion_matching
                  (AbruptCompletion.mem continues)
                  loop_completion_state
            )
          in
          (* Post-while = post-test env (test always runs at least once) *)
          this#reset_env env_after_test;
          let while_completion_states = (None, [loop_completion_state]) in
          let completion_state =
            run_to_completion (fun () -> merge_completion_states while_completion_states)
          in
          this#commit_abrupt_completion_matching
            AbruptCompletion.(mem [break None])
            completion_state
      );
      stmt

    method! do_while _loc (stmt : (ALoc.t, ALoc.t) Ast.Statement.DoWhile.t) =
      this#expecting_abrupt_completions (fun () ->
          let continues = AbruptCompletion.continue None :: possible_labeled_continues in
          let open Ast.Statement.DoWhile in
          let { body; test; _ } = stmt in
          let loop_completion_state = run_to_completion (fun () -> ignore @@ this#statement body) in
          let loop_completion_state =
            run_to_completion (fun () ->
                this#commit_abrupt_completion_matching
                  (AbruptCompletion.mem continues)
                  loop_completion_state
            )
          in
          (match loop_completion_state with
          | None -> ignore @@ this#expression test
          | _ -> ());
          let do_while_completion_states = (loop_completion_state, []) in
          let completion_state =
            run_to_completion (fun () -> merge_completion_states do_while_completion_states)
          in
          this#commit_abrupt_completion_matching
            AbruptCompletion.(mem [break None])
            completion_state
      );
      stmt

    method! for_statement _loc (stmt : (ALoc.t, ALoc.t) Ast.Statement.For.t) =
      this#expecting_abrupt_completions (fun () ->
          let continues = AbruptCompletion.continue None :: possible_labeled_continues in
          let open Ast.Statement.For in
          let { init; test; update; body; comments = _ } = stmt in
          run_opt this#for_statement_init init;
          run_opt this#expression test;
          let env_after_test = init_env in
          let loop_completion_state = run_to_completion (fun () -> ignore @@ this#statement body) in
          let loop_completion_state =
            run_to_completion (fun () ->
                this#commit_abrupt_completion_matching
                  (AbruptCompletion.mem continues)
                  loop_completion_state
            )
          in
          (match loop_completion_state with
          | None -> run_opt this#expression update
          | _ -> ());
          (* Post-for = post-test env (test always runs at least once) *)
          this#reset_env env_after_test;
          let for_completion_states = (None, [loop_completion_state]) in
          let completion_state =
            run_to_completion (fun () -> merge_completion_states for_completion_states)
          in
          this#commit_abrupt_completion_matching
            AbruptCompletion.(mem [break None])
            completion_state
      );
      stmt

    method! for_in_statement _loc (stmt : (ALoc.t, ALoc.t) Ast.Statement.ForIn.t) =
      this#expecting_abrupt_completions (fun () ->
          let continues = AbruptCompletion.continue None :: possible_labeled_continues in
          let open Ast.Statement.ForIn in
          let { left; right; body; each = _; comments = _ } = stmt in
          ignore @@ this#expression right;
          let env0 = init_env in
          ignore @@ this#for_in_statement_lhs left;
          let loop_completion_state = run_to_completion (fun () -> ignore @@ this#statement body) in
          let loop_completion_state =
            run_to_completion (fun () ->
                this#commit_abrupt_completion_matching
                  (AbruptCompletion.mem continues)
                  loop_completion_state
            )
          in
          this#reset_env env0;
          let for_in_completion_states = (None, [loop_completion_state]) in
          let completion_state =
            run_to_completion (fun () -> merge_completion_states for_in_completion_states)
          in
          this#commit_abrupt_completion_matching
            AbruptCompletion.(mem [break None])
            completion_state
      );
      stmt

    method! for_of_statement _loc (stmt : (ALoc.t, ALoc.t) Ast.Statement.ForOf.t) =
      this#expecting_abrupt_completions (fun () ->
          let continues = AbruptCompletion.continue None :: possible_labeled_continues in
          let open Ast.Statement.ForOf in
          let { left; right; body; await = _; comments = _ } = stmt in
          ignore @@ this#expression right;
          let env0 = init_env in
          ignore @@ this#for_of_statement_lhs left;
          let loop_completion_state = run_to_completion (fun () -> ignore @@ this#statement body) in
          let loop_completion_state =
            run_to_completion (fun () ->
                this#commit_abrupt_completion_matching
                  (AbruptCompletion.mem continues)
                  loop_completion_state
            )
          in
          this#reset_env env0;
          let for_of_completion_states = (None, [loop_completion_state]) in
          let completion_state =
            run_to_completion (fun () -> merge_completion_states for_of_completion_states)
          in
          this#commit_abrupt_completion_matching
            AbruptCompletion.(mem [break None])
            completion_state
      );
      stmt

    method! switch _loc (switch : (ALoc.t, ALoc.t) Ast.Statement.Switch.t) =
      let open Ast.Statement.Switch in
      let { discriminant; cases; comments = _; exhaustive_out = _ } = switch in
      ignore @@ this#expression discriminant;
      this#expecting_abrupt_completions (fun () ->
          let (env, case_completion_states) =
            List.fold_left
              (fun acc stuff ->
                let (_loc, case) = stuff in
                this#handle_switch_case acc case)
              (unreachable_env init_env, [])
              cases
          in
          this#merge_self_env env;
          let switch_completion_states = (None, case_completion_states) in
          let completion_state =
            run_to_completion (fun () -> merge_completion_states switch_completion_states)
          in
          this#commit_abrupt_completion_matching
            AbruptCompletion.(mem [break None])
            completion_state
      );
      switch

    method private handle_switch_case
        (env, case_completion_states) (case : (ALoc.t, ALoc.t) Ast.Statement.Switch.Case.t') =
      let open Ast.Statement.Switch.Case in
      let { test; case_test_loc = _; consequent; comments = _ } = case in
      run_opt this#expression test;
      let env0 = init_env in
      init_env <- merge_envs env0 env;
      let case_completion_state =
        run_to_completion (fun () -> ignore @@ this#statement_list consequent)
      in
      let env' = init_env in
      this#reset_env env0;
      (env', case_completion_state :: case_completion_states)

    method! try_catch _loc (stmt : (ALoc.t, ALoc.t) Ast.Statement.Try.t) =
      this#expecting_abrupt_completions (fun () ->
          let open Ast.Statement.Try in
          let { block = (loc, block); handler; finalizer; comments = _ } = stmt in
          let pre_env = init_env in
          let try_completion_state = run_to_completion (fun () -> ignore @@ this#block loc block) in
          let env1 = init_env in
          let (catch_completion_state_opt, env2) =
            match handler with
            | Some (loc, clause) ->
              (* In the catch block, an exception could have been thrown at any point
                 in the try block. Conservatively, reset to pre-try env: properties
                 initialized before the try are still initialized, but properties
                 initialized during the try might not be. *)
              this#reset_env pre_env;
              let catch_completion_state =
                run_to_completion (fun () -> ignore @@ this#catch_clause loc clause)
              in
              ([catch_completion_state], init_env)
            | None -> ([], unreachable_env init_env)
          in
          init_env <- merge_envs env1 env2;
          let try_catch_completion_states = (try_completion_state, catch_completion_state_opt) in
          let completion_state =
            run_to_completion (fun () -> merge_completion_states try_catch_completion_states)
          in
          this#commit_abrupt_completion_matching AbruptCompletion.all completion_state;
          (match finalizer with
          | Some (_loc, block) ->
            (* Finally block runs regardless; conservatively reset to pre-try env *)
            this#reset_env pre_env;
            ignore @@ this#block loc block
          | None -> ());
          from_completion completion_state
      );
      stmt

    method! logical _loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Logical.t) =
      let open Ast.Expression.Logical in
      let { operator = _; left; right; comments = _ } = expr in
      ignore @@ this#expression left;
      let env1 = init_env in
      ignore @@ this#expression right;
      this#merge_self_env env1;
      expr

    method! conditional _loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Conditional.t) =
      let open Ast.Expression.Conditional in
      let { test; consequent; alternate; comments = _ } = expr in
      ignore @@ this#expression test;
      let env0 = init_env in
      ignore @@ this#expression consequent;
      let env1 = init_env in
      this#reset_env env0;
      ignore @@ this#expression alternate;
      this#merge_self_env env1;
      expr

    (* ABRUPT COMPLETION RAISES *)
    method! break _loc (stmt : ALoc.t Ast.Statement.Break.t) =
      let open Ast.Statement.Break in
      let { label; comments = _ } = stmt in
      this#raise_abrupt_completion (AbruptCompletion.break label)

    method! continue _loc (stmt : ALoc.t Ast.Statement.Continue.t) =
      let open Ast.Statement.Continue in
      let { label; comments = _ } = stmt in
      this#raise_abrupt_completion (AbruptCompletion.continue label)

    method! return _loc (stmt : (ALoc.t, ALoc.t) Ast.Statement.Return.t) =
      let open Ast.Statement.Return in
      let { argument; comments = _; return_out = _ } = stmt in
      run_opt this#expression argument;
      this#raise_abrupt_completion AbruptCompletion.return

    method! throw _loc (stmt : (ALoc.t, ALoc.t) Ast.Statement.Throw.t) =
      let open Ast.Statement.Throw in
      let { argument; comments = _ } = stmt in
      ignore @@ this#expression argument;
      this#raise_abrupt_completion AbruptCompletion.throw

    (* LABELED STATEMENTS *)
    method! labeled_statement _loc (stmt : (ALoc.t, ALoc.t) Ast.Statement.Labeled.t) =
      this#expecting_abrupt_completions (fun () ->
          let open Ast.Statement.Labeled in
          let { label; body; comments = _ } = stmt in
          possible_labeled_continues <-
            AbruptCompletion.continue (Some label) :: possible_labeled_continues;
          let completion_state = run_to_completion (fun () -> ignore @@ this#statement body) in
          possible_labeled_continues <- [];
          this#commit_abrupt_completion_matching
            AbruptCompletion.(mem [break (Some label)])
            completion_state
      );
      stmt

    method! statement (stmt : (ALoc.t, ALoc.t) Ast.Statement.t) =
      let open Ast.Statement in
      (match stmt with
      | (_, While _)
      | (_, DoWhile _)
      | (_, For _)
      | (_, ForIn _)
      | (_, ForOf _)
      | (_, Labeled _) ->
        ()
      | _ -> possible_labeled_continues <- []);
      super#statement stmt

    (* LAMBDAS *)
    method private visit_lambda body_f =
      let saved_env = init_env in
      let saved_completions = abrupt_completion_envs in
      let saved_continues = possible_labeled_continues in
      abrupt_completion_envs <- [];
      possible_labeled_continues <- [];
      init_env <- SMap.map (fun _ -> false) init_env;
      let completion_state = run_to_completion body_f in
      ignore completion_state;
      abrupt_completion_envs <- saved_completions;
      possible_labeled_continues <- saved_continues;
      init_env <- saved_env

    method! function_ loc expr =
      this#visit_lambda (fun () -> ignore @@ super#function_ loc expr);
      expr

    method! class_expression loc cls =
      this#visit_lambda (fun () -> ignore @@ super#class_expression loc cls);
      cls
  end

let eval_property_assignment class_body =
  let open Ast.Class in
  let property_declarations =
    Base.List.filter_map
      ~f:(function
        | Body.Property
            ( _,
              {
                Property.key = Ast.Expression.Object.Property.Identifier ((loc, _) as id);
                value;
                static = false;
                override = _;
                optional = _;
                annot = _;
                variance = _;
                ts_accessibility = _;
                decorators = _;
                comments = _;
              }
            ) ->
          Some (public_property loc id, value)
        | Body.PrivateField
            ( _,
              {
                PrivateField.key = (loc, _) as id;
                value;
                static = false;
                override = _;
                optional = _;
                annot = _;
                variance = _;
                ts_accessibility = _;
                decorators = _;
                comments = _;
              }
            ) ->
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
                      effect_ = _;
                      predicate = _;
                      return = _;
                      tparams = _;
                      sig_loc = _;
                      comments = _;
                    }
                  );
                key = _;
                static = _;
                override = _;
                ts_accessibility = _;
                decorators = _;
                comments = _;
              }
            ) ->
          Some block
        | _ -> None)
      class_body
    |> Base.Option.value ~default:{ Ast.Statement.Block.body = []; comments = None }
  in
  let properties = Base.List.map ~f:fst property_declarations in
  let property_names =
    List.fold_left
      (fun acc property -> SSet.add (Flow_ast_utils.name_of_ident property) acc)
      SSet.empty
      properties
  in
  let walk = new property_assignment property_names properties in
  walk#expecting_return_or_throw (fun () ->
      List.iter
        (function
          | (property_id, Ast.Class.Property.Initialized default_initializer) ->
            walk#initialize_property property_id default_initializer
          | _ -> ())
        property_declarations;
      ignore @@ walk#block ALoc.none ctor_body
  );

  (* We make heavy use of the Base.List.rev_* functions below because they are
   * tail recursive. We can do this freely because the order in which we
   * process the errors doesn't actually matter (Flow will sort the errors
   * before printing them anyway).
   *)
  let uninitialized_properties : (ALoc.t error * string) list =
    properties
    |> Base.List.filter ~f:(fun id ->
           not (SMap.find (Flow_ast_utils.name_of_ident id) walk#init_env)
       )
    |> Base.List.rev_map ~f:(fun id ->
           ( { loc = Flow_ast_utils.loc_of_ident id; desc = Lints.PropertyNotDefinitelyInitialized },
             Flow_ast_utils.name_of_ident id
           )
       )
  in
  let read_before_initialized : (ALoc.t error * string) list =
    Base.List.rev_map walk#read_errors ~f:(fun (read_loc, name) ->
        ({ loc = read_loc; desc = Lints.ReadFromUninitializedProperty }, name)
    )
  in
  let filter_uninitialized_in_env env =
    Base.List.filter properties ~f:(fun id ->
        match SMap.find_opt (Flow_ast_utils.name_of_ident id) env with
        | Some false -> true
        | _ -> false
    )
  in
  let this_errors : (ALoc.t error * string Nel.t) list =
    Base.List.rev_filter_map
      ~f:(fun (loc, desc, env) ->
        filter_uninitialized_in_env env
        |> Base.List.map ~f:Flow_ast_utils.name_of_ident
        |> Nel.of_list
        |> Base.Option.map ~f:(fun uninitialized_properties ->
               ({ loc; desc }, uninitialized_properties)
           ))
      walk#this_escape_errors
  in
  let combine_voidable_checks old new_ = Base.List.rev_append new_ old in
  let add_to_errors error errors prefixed_name =
    if String.starts_with ~prefix:"this.#" prefixed_name then
      {
        errors with
        private_property_errors =
          SMap.add
            ~combine:combine_voidable_checks
            (String_utils.lstrip prefixed_name "this.#")
            [error]
            errors.private_property_errors;
      }
    else if String.starts_with ~prefix:"this." prefixed_name then
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
  |> multi_property_errors this_errors
