(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module L = ALoc
module Ast = Flow_ast

(* State tracking whether super() has been called on the current control flow path *)
type super_call_state =
  | NotCalled (* super() definitely not called on this path *)
  | Called (* super() definitely called on this path *)
  | MaybeCalled (* super() called on some but not all paths *)

let merge_state s1 s2 =
  match (s1, s2) with
  | (Called, Called) -> Called
  | (NotCalled, NotCalled) -> NotCalled
  | _ -> MaybeCalled

let state_allows_access = function
  | Called -> true
  | NotCalled
  | MaybeCalled ->
    false

class checker ~add_output this_def_loc super_def_loc =
  object (this)
    inherit [L.t] Flow_ast_mapper.mapper as super

    (* Current state: has super() been called? *)
    val mutable super_call_state : super_call_state = NotCalled

    (* Track error locations *)
    val mutable this_before_super_locs : L.t list = []

    val mutable super_before_super_call_locs : L.t list = []

    val mutable duplicate_super_call_locs : L.t list = []

    method private get_state = super_call_state

    method private set_state s = super_call_state <- s

    method private mark_super_called loc =
      match super_call_state with
      | Called -> duplicate_super_call_locs <- loc :: duplicate_super_call_locs
      | NotCalled
      | MaybeCalled ->
        super_call_state <- Called

    method private run_in_branch : 'a. (unit -> 'a) -> super_call_state =
      fun f ->
        let saved_state = this#get_state in
        ignore (f ());
        let result_state = this#get_state in
        this#set_state saved_state;
        result_state

    method! this_expression loc this_ =
      if not (state_allows_access super_call_state) then
        this_before_super_locs <- loc :: this_before_super_locs;
      this_

    method! super_expression loc super_ =
      if not (state_allows_access super_call_state) then
        super_before_super_call_locs <- loc :: super_before_super_call_locs;
      super_

    method! call loc expr =
      let open Ast.Expression.Call in
      let { callee; targs; arguments; _ } = expr in
      match callee with
      | (_, Ast.Expression.Super _) ->
        (* Visit arguments first (they might reference this/super), then mark super called *)
        Base.Option.iter targs ~f:(fun t -> ignore (this#call_type_args t));
        ignore (this#arg_list arguments);
        (* Only after the super call, `this` and `super` are defined. *)
        this#mark_super_called loc;
        expr
      | _ -> super#call loc expr

    (* Skip nested classes - they have their own super() requirements *)
    method! class_ _ cls = cls

    method! record_declaration _ record = record

    (* Control flow merging: if statement *)
    method! if_statement _loc stmt =
      let open Ast.Statement.If in
      let { test; consequent; alternate; comments = _ } = stmt in
      ignore (this#expression test);
      let then_state = this#run_in_branch (fun () -> this#statement consequent) in
      let else_state =
        match alternate with
        | Some (_, { Alternate.body; comments = _ }) ->
          this#run_in_branch (fun () -> this#statement body)
        | None -> this#get_state (* No else = unchanged state *)
      in
      this#set_state (merge_state then_state else_state);
      stmt

    (* Control flow merging: conditional expression *)
    method! conditional _loc expr =
      let open Ast.Expression.Conditional in
      let { test; consequent; alternate; comments = _ } = expr in
      ignore (this#expression test);
      let then_state = this#run_in_branch (fun () -> this#expression consequent) in
      let else_state = this#run_in_branch (fun () -> this#expression alternate) in
      this#set_state (merge_state then_state else_state);
      expr

    (* Control flow merging: logical expressions (short-circuit) *)
    method! logical _loc expr =
      let open Ast.Expression.Logical in
      let { operator = _; left; right; comments = _ } = expr in
      ignore (this#expression left);
      let right_state = this#run_in_branch (fun () -> this#expression right) in
      (* Short-circuit: right may or may not execute *)
      this#set_state (merge_state this#get_state right_state);
      expr

    (* Control flow merging: switch statement *)
    method! switch _loc stmt =
      let open Ast.Statement.Switch in
      let { discriminant; cases; exhaustive_out = _; comments = _ } = stmt in
      ignore (this#expression discriminant);
      let pre_switch_state = this#get_state in
      let states =
        Base.List.map cases ~f:(fun (_, case) ->
            this#run_in_branch (fun () ->
                let { Case.test; consequent; _ } = case in
                Base.Option.iter test ~f:(fun e -> ignore (this#expression e));
                Base.List.iter consequent ~f:(fun s -> ignore (this#statement s))
            )
        )
      in
      (* Merge all case states with pre-switch state (in case no case matches) *)
      let merged = Base.List.fold states ~init:pre_switch_state ~f:merge_state in
      this#set_state merged;
      stmt

    (* Control flow merging: try/catch/finally *)
    method! try_catch _loc stmt =
      let open Ast.Statement.Try in
      let { block = (block_loc, block); handler; finalizer; comments = _ } = stmt in
      let pre_try_state = this#get_state in
      (* Try block *)
      let try_state = this#run_in_branch (fun () -> this#block block_loc block) in
      (* Catch block - exception could happen at any point in try, so start from pre_try_state *)
      let catch_state =
        match handler with
        | Some (_, { CatchClause.param; body = (catch_loc, catch_block); comments = _ }) ->
          this#set_state pre_try_state;
          this#run_in_branch (fun () ->
              Base.Option.iter param ~f:(fun p -> ignore (this#catch_clause_pattern p));
              ignore (this#block catch_loc catch_block)
          )
        | None -> pre_try_state
      in
      (* After try/catch, merge states *)
      this#set_state (merge_state try_state catch_state);
      (* Finally always runs, update state *)
      Base.Option.iter finalizer ~f:(fun (final_loc, final_block) ->
          ignore (this#block final_loc final_block)
      );
      stmt

    (* Control flow merging: while loop (body may not execute) *)
    method! while_ _loc stmt =
      let open Ast.Statement.While in
      let { test; body; comments = _ } = stmt in
      ignore (this#expression test);
      let body_state = this#run_in_branch (fun () -> this#statement body) in
      (* Loop may not execute at all, so merge with pre-loop state *)
      this#set_state (merge_state this#get_state body_state);
      stmt

    (* Control flow merging: do-while loop (body executes at least once) *)
    method! do_while _loc stmt =
      let open Ast.Statement.DoWhile in
      let { body; test; comments = _ } = stmt in
      (* Do-while body executes at least once *)
      ignore (this#statement body);
      ignore (this#expression test);
      stmt

    (* Control flow merging: for loop *)
    method! for_statement _loc stmt =
      let open Ast.Statement.For in
      let { init; test; update; body; comments = _ } = stmt in
      Base.Option.iter init ~f:(fun init ->
          match init with
          | InitDeclaration (decl_loc, decl) -> ignore (this#variable_declaration decl_loc decl)
          | InitExpression expr -> ignore (this#expression expr)
      );
      Base.Option.iter test ~f:(fun e -> ignore (this#expression e));
      let body_state =
        this#run_in_branch (fun () ->
            ignore (this#statement body);
            Base.Option.iter update ~f:(fun e -> ignore (this#expression e))
        )
      in
      this#set_state (merge_state this#get_state body_state);
      stmt

    (* Control flow merging: for-in loop *)
    method! for_in_statement _loc stmt =
      let open Ast.Statement.ForIn in
      let { left; right; body; each = _; comments = _ } = stmt in
      (match left with
      | LeftDeclaration (decl_loc, decl) -> ignore (this#variable_declaration decl_loc decl)
      | LeftPattern pat -> ignore (this#for_in_assignment_pattern pat));
      ignore (this#expression right);
      let body_state = this#run_in_branch (fun () -> this#statement body) in
      this#set_state (merge_state this#get_state body_state);
      stmt

    (* Control flow merging: for-of loop *)
    method! for_of_statement _loc stmt =
      let open Ast.Statement.ForOf in
      let { left; right; body; await = _; comments = _ } = stmt in
      (match left with
      | LeftDeclaration (decl_loc, decl) -> ignore (this#variable_declaration decl_loc decl)
      | LeftPattern pat -> ignore (this#for_of_assignment_pattern pat));
      ignore (this#expression right);
      let body_state = this#run_in_branch (fun () -> this#statement body) in
      this#set_state (merge_state this#get_state body_state);
      stmt

    method add_errors =
      Base.List.iter this_before_super_locs ~f:(fun loc ->
          add_output
            Error_message.(
              EBindingError
                (EReferencedThisSuperBeforeSuperCall, loc, Reason.OrdinaryName "this", this_def_loc)
            )
      );
      Base.List.iter super_before_super_call_locs ~f:(fun loc ->
          add_output
            Error_message.(
              EBindingError
                ( EReferencedThisSuperBeforeSuperCall,
                  loc,
                  Reason.OrdinaryName "super",
                  super_def_loc
                )
            )
      );
      Base.List.iter duplicate_super_call_locs ~f:(fun loc ->
          add_output
            Error_message.(
              EBindingError (ENameAlreadyBound, loc, Reason.OrdinaryName "this", this_def_loc)
            );
          add_output
            Error_message.(
              EBindingError (ENameAlreadyBound, loc, Reason.OrdinaryName "super", super_def_loc)
            )
      )
  end

let check ~enable_enums:_ ~add_output loc cls =
  let open Flow_ast.Class in
  let { id; extends; body = (_, { Body.body = members; _ }); _ } = cls in
  let this_def_loc = Base.Option.value_map ~default:loc ~f:fst id in
  match extends with
  | None -> ()
  | Some (super_def_loc, _) ->
    Base.List.iter members ~f:(function
        | Body.Method (_, { Method.kind = Method.Constructor; value = (loc, constructor); _ }) ->
          let checker = new checker ~add_output this_def_loc super_def_loc in
          ignore (checker#function_ loc constructor);
          checker#add_errors
        | _ -> ()
        )
