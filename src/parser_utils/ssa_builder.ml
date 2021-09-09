(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module is responsible for building a mapping from variable reads to the
 * writes those reads. This is used in type checking to determine if a variable is
 * const-like, but the env_builder is used to build the type checking envrionment.
 * The env_builder copied much of the implementation here, but with sufficient divergence
 * to warrant forking the implementation.
 * If you're here to add support for a new syntax feature, you'll likely
 * need to modify the env_builder as well, but not necessarily with identical changes.*)

module Ast = Flow_ast
open Hoister
open Reason

module Make
    (L : Loc_sig.S)
    (Ssa_api : Ssa_api.S with module L = L)
    (Scope_builder : Scope_builder_sig.S with module L = L) =
struct
  open Scope_builder

  (* For every read of a variable x, we are interested in tracking writes to x
     that can reach that read. Ultimately the writes are going to be represented
     as a list of locations, where each location corresponds to a "single static
     assignment" of the variable in the code. But for the purposes of analysis, it
     is useful to represent these writes with a data type that contains either a
     single write, or a "join" of writes (in compiler terminology, a PHI node), or
     a reference to something that is unknown at a particular point in the AST
     during traversal, but will be known by the time traversal is complete. *)
  module Val : sig
    type t

    val mk_unresolved : int -> t

    val empty : unit -> t

    val uninitialized : unit -> t

    val new_id : unit -> int

    val merge : t -> t -> t

    val one : L.t virtual_reason -> t

    val all : L.t virtual_reason list -> t

    val resolve : unresolved:t -> t -> unit

    val simplify : t -> Ssa_api.write_loc list

    val id_of_val : t -> int
  end = struct
    let curr_id = ref 0

    type ref_state =
      (* different unresolved vars are distinguished by their ids, which enables using structural
         equality for computing normal forms: see below *)
      | Unresolved of int
      | Resolved of write_state

    and write_state =
      | Uninitialized
      | Loc of L.t virtual_reason
      | PHI of write_state list
      | REF of ref_state ref

    and t = {
      id: int;
      write_state: write_state;
    }

    let new_id () =
      let id = !curr_id in
      curr_id := !curr_id + 1;
      id

    let mk_with_write_state write_state =
      let id = new_id () in
      { id; write_state }

    let mk_unresolved id = mk_with_write_state @@ REF (ref (Unresolved id))

    let empty () = mk_with_write_state @@ PHI []

    let uninitialized () = mk_with_write_state Uninitialized

    let join = function
      | [] -> PHI []
      | [t] -> t
      | ts -> PHI ts

    module WriteSet = Flow_set.Make (struct
      type t = write_state

      let compare = Stdlib.compare
    end)

    let rec normalize (t : write_state) : WriteSet.t =
      match t with
      | Uninitialized
      | Loc _
      | REF { contents = Unresolved _ } ->
        WriteSet.singleton t
      | PHI ts ->
        List.fold_left
          (fun vals' t ->
            let vals = normalize t in
            WriteSet.union vals' vals)
          WriteSet.empty
          ts
      | REF ({ contents = Resolved t } as r) ->
        let vals = normalize t in
        let t' = join (WriteSet.elements vals) in
        r := Resolved t';
        vals

    let merge t1 t2 =
      if t1.id = t2.id then
        t1
      else
        (* Merging can easily lead to exponential blowup in size of terms if we're not careful. We
           amortize costs by computing normal forms as sets of "atomic" terms, so that merging would
           correspond to set union. (Atomic terms include Uninitialized, Loc _, and REF { contents =
           Unresolved _ }.) Note that normal forms might change over time, as unresolved refs become
           resolved; thus, we do not shortcut normalization of previously normalized terms. Still, we
           expect (and have experimentally validated that) the cost of computing normal forms becomes
           smaller over time as terms remain close to their final normal forms. *)
        let vals = WriteSet.union (normalize t1.write_state) (normalize t2.write_state) in
        mk_with_write_state @@ join (WriteSet.elements vals)

    let one reason = mk_with_write_state @@ Loc reason

    let all locs = mk_with_write_state @@ join (Base.List.map ~f:(fun reason -> Loc reason) locs)

    (* Resolving unresolved to t essentially models an equation of the form
       unresolved = t, where unresolved is a reference to an unknown and t is the
       known. Since the only non-trivial operation in t is joining, it is OK to
       erase any occurrences of unresolved in t: if t = unresolved | t' then
       unresolved = t is the same as unresolved = t'. *)
    let rec resolve ~unresolved t =
      match unresolved.write_state with
      | REF ({ contents = Unresolved _ } as r) -> r := Resolved (erase r t.write_state)
      | _ -> failwith "Only an unresolved REF can be resolved"

    and erase r t =
      match t with
      | Uninitialized -> t
      | Loc _ -> t
      | PHI ts ->
        let ts' = ListUtils.ident_map (erase r) ts in
        if ts' == ts then
          t
        else
          PHI ts'
      | REF r' ->
        if r == r' then
          PHI []
        else
          let t_opt = !r' in
          let t_opt' =
            match t_opt with
            | Unresolved _ -> t_opt
            | Resolved t ->
              let t' = erase r t in
              if t == t' then
                t_opt
              else
                Resolved t'
          in
          if t_opt != t_opt' then r' := t_opt';
          t

    (* Simplification converts a Val.t to a list of locations. *)
    let simplify t =
      let vals = normalize t.write_state in
      Base.List.map
        ~f:(function
          | Uninitialized -> Ssa_api.Uninitialized
          | Loc r -> Ssa_api.Write r
          | REF { contents = Unresolved _ } -> failwith "An unresolved REF cannot be simplified"
          | PHI _
          | REF { contents = Resolved _ } ->
            failwith "A normalized value cannot be a PHI or a resolved REF")
        (WriteSet.elements vals)

    let id_of_val { id; write_state = _ } = id
  end

  (* An environment is a map from variables to values. *)
  module Env = struct
    type t = Val.t SMap.t
  end

  (* Abrupt completions induce control flows, so modeling them accurately is
     necessary for soundness. *)
  module AbruptCompletion = struct
    type label = string

    type t =
      | Break of label option
      | Continue of label option
      | Return
      | Throw

    let label_opt = Base.Option.map ~f:Flow_ast_utils.name_of_ident

    let break x = Break (label_opt x)

    let continue x = Continue (label_opt x)

    let return = Return

    let throw = Throw

    (* match particular abrupt completions *)
    let mem list : t -> bool = (fun t -> List.mem t list)

    (* match all abrupt completions *)
    let all : t -> bool = (fun _t -> true)

    (* Model an abrupt completion as an OCaml exception. *)
    exception Exn of t

    (* An abrupt completion carries an environment, which is the current
       environment at the point where the abrupt completion is "raised." This
       environment is merged wherever the abrupt completion is "handled." *)
    type env = t * Env.t
  end

  (* Collect all values assigned to a variable, as a conservative fallback when we
     don't have precise information. *)
  module Havoc = struct
    type t = {
      unresolved: Val.t;
      (* always REF *)
      mutable locs: L.t Reason.virtual_reason list;
    }
  end

  let rec list_iter3 f l1 l2 l3 =
    match (l1, l2, l3) with
    | ([], [], []) -> ()
    | (x1 :: l1, x2 :: l2, x3 :: l3) ->
      f x1 x2 x3;
      list_iter3 f l1 l2 l3
    | _ -> assert false

  type ssa = {
    val_ref: Val.t ref;
    havoc: Havoc.t;
  }

  class ssa_builder =
    object (this)
      (* TODO: with_types should probably be false, but this maintains previous behavior *)
      inherit scope_builder ~flowmin_compatibility:false ~with_types:true as super

      (* We maintain a map of read locations to raw Val.t terms, which are
         simplified to lists of write locations once the analysis is done. *)
      val mutable values : Val.t L.LMap.t = L.LMap.empty

      method values : Ssa_api.values = L.LMap.map Val.simplify values

      val mutable unbound_names : SSet.t = SSet.empty

      method unbound_names : SSet.t = unbound_names

      val mutable id = 0

      method mk_unresolved =
        id <- id + 1;
        Val.mk_unresolved id

      (* Utils to manipulate single-static-assignment (SSA) environments.

         TODO: These low-level operations should probably be replaced by
         higher-level "control-flow-graph" operations that can be implemented using
         them, e.g., those that deal with branches and loops. *)
      val mutable ssa_env : ssa SMap.t = SMap.empty

      method ssa_env : Env.t = SMap.map (fun { val_ref; _ } -> !val_ref) ssa_env

      method merge_remote_ssa_env (env : Env.t) : unit =
        (* NOTE: env might have more keys than ssa_env, since the environment it
           describes might be nested inside the current environment *)
        SMap.iter (fun x { val_ref; _ } -> val_ref := Val.merge !val_ref (SMap.find x env)) ssa_env

      method merge_ssa_env (env1 : Env.t) (env2 : Env.t) : unit =
        let env1 = SMap.values env1 in
        let env2 = SMap.values env2 in
        let ssa_env = SMap.values ssa_env in
        list_iter3
          (fun { val_ref; _ } value1 value2 -> val_ref := Val.merge value1 value2)
          ssa_env
          env1
          env2

      method merge_self_ssa_env (env : Env.t) : unit =
        let env = SMap.values env in
        let ssa_env = SMap.values ssa_env in
        List.iter2 (fun { val_ref; _ } value -> val_ref := Val.merge !val_ref value) ssa_env env

      method reset_ssa_env (env0 : Env.t) : unit =
        let env0 = SMap.values env0 in
        let ssa_env = SMap.values ssa_env in
        List.iter2 (fun { val_ref; _ } value -> val_ref := value) ssa_env env0

      method fresh_ssa_env : Env.t = SMap.map (fun _ -> this#mk_unresolved) ssa_env

      method assert_ssa_env (env0 : Env.t) : unit =
        let env0 = SMap.values env0 in
        let ssa_env = SMap.values ssa_env in
        List.iter2 (fun { val_ref; _ } value -> Val.resolve ~unresolved:value !val_ref) ssa_env env0

      method empty_ssa_env : Env.t = SMap.map (fun _ -> Val.empty ()) ssa_env

      method havoc_current_ssa_env =
        SMap.iter
          (fun _x { val_ref; havoc } ->
            (* NOTE: havoc_env should already have all writes to x, so the only
               additional thing that could come from ssa_env is "uninitialized." On
               the other hand, we *dont* want to include "uninitialized" if it's no
               longer in ssa_env, since that means that x has been initialized (and
               there's no going back). *)
            val_ref := Val.merge !val_ref havoc.Havoc.unresolved)
          ssa_env

      method havoc_uninitialized_ssa_env : unit =
        SMap.iter
          (fun _x { val_ref; havoc } ->
            val_ref := Val.merge (Val.uninitialized ()) havoc.Havoc.unresolved)
          ssa_env

      method private mk_ssa_env =
        SMap.map (fun _ ->
            {
              val_ref = ref (Val.uninitialized ());
              havoc = Havoc.{ unresolved = this#mk_unresolved; locs = [] };
            })

      method private push_ssa_env bindings =
        let old_ssa_env = ssa_env in
        let bindings = Bindings.to_map bindings in
        ssa_env <- SMap.fold SMap.add (this#mk_ssa_env bindings) old_ssa_env;
        (bindings, old_ssa_env)

      method private resolve_havocs =
        SMap.iter (fun x _loc ->
            let { havoc = { Havoc.unresolved; locs }; _ } = SMap.find x ssa_env in
            Val.resolve ~unresolved (Val.all locs))

      method private pop_ssa_env (bindings, old_ssa_env) =
        this#resolve_havocs bindings;
        ssa_env <- old_ssa_env

      method! with_bindings : 'a. ?lexical:bool -> L.t -> L.t Bindings.t -> ('a -> 'a) -> 'a -> 'a =
        fun ?lexical loc bindings visit node ->
          let saved_state = this#push_ssa_env bindings in
          this#run
            (fun () -> ignore @@ super#with_bindings ?lexical loc bindings visit node)
            ~finally:(fun () -> this#pop_ssa_env saved_state);
          node

      (* Run some computation, catching any abrupt completions; do some final work,
         and then re-raise any abrupt completions that were caught. *)
      method run f ~finally =
        let completion_state = this#run_to_completion f in
        finally ();
        this#from_completion completion_state

      method run_to_completion f =
        try
          f ();
          None
        with
        | AbruptCompletion.Exn abrupt_completion -> Some abrupt_completion

      method from_completion =
        function
        | None -> ()
        | Some abrupt_completion -> raise (AbruptCompletion.Exn abrupt_completion)

      (* When an abrupt completion is raised, it falls through any subsequent
         straight-line code, until it reaches a merge point in the control-flow
         graph. At that point, it can be re-raised if and only if all other reaching
         control-flow paths also raise the same abrupt completion.

         When re-raising is not possible, we have to save the abrupt completion and
         the current environment in a list, so that we can merge such environments
         later (when that abrupt completion and others like it are handled).

         Even when raising is possible, we still have to save the current
         environment, since the current environment will have to be cleared to model
         that the current values of all variables are unreachable.

         NOTE that raising is purely an optimization: we can have more precise
         results with raising, but even if we never raised we'd still be sound. *)
      val mutable abrupt_completion_envs : AbruptCompletion.env list = []

      method raise_abrupt_completion : 'a. AbruptCompletion.t -> 'a =
        fun abrupt_completion ->
          let env = this#ssa_env in
          this#reset_ssa_env this#empty_ssa_env;
          abrupt_completion_envs <- (abrupt_completion, env) :: abrupt_completion_envs;
          raise (AbruptCompletion.Exn abrupt_completion)

      method expecting_abrupt_completions f =
        let saved = abrupt_completion_envs in
        abrupt_completion_envs <- [];
        this#run f ~finally:(fun () ->
            abrupt_completion_envs <- List.rev_append saved abrupt_completion_envs)

      (* Given multiple completion states, (re)raise if all of them are the same
         abrupt completion. This function is called at merge points. *)
      method merge_completion_states (hd_completion_state, tl_completion_states) =
        match hd_completion_state with
        | None -> ()
        | Some abrupt_completion ->
          if
            List.for_all
              (function
                | None -> false
                | Some abrupt_completion' -> abrupt_completion = abrupt_completion')
              tl_completion_states
          then
            raise (AbruptCompletion.Exn abrupt_completion)

      (* Given a filter for particular abrupt completions to expect, find the saved
         environments corresponding to them, and merge those environments with the
         current environment. This function is called when exiting ASTs that
         introduce (and therefore expect) particular abrupt completions. *)
      method commit_abrupt_completion_matching filter completion_state =
        let (matching, non_matching) =
          List.partition
            (fun (abrupt_completion, _env) -> filter abrupt_completion)
            abrupt_completion_envs
        in
        if matching <> [] then (
          List.iter (fun (_abrupt_completion, env) -> this#merge_remote_ssa_env env) matching;
          abrupt_completion_envs <- non_matching
        ) else
          match completion_state with
          | Some abrupt_completion when not (filter abrupt_completion) ->
            raise (AbruptCompletion.Exn abrupt_completion)
          | _ -> ()

      (* Track the list of labels that might describe a loop. Used to detect which
         labeled continues need to be handled by the loop.

         The idea is that a labeled statement adds its label to the list before
         entering its child, and if the child is not a loop or another labeled
         statement, the list will be cleared. A loop will consume the list, so we
         also clear the list on our way out of any labeled statement. *)
      val mutable possible_labeled_continues = []

      (* write *)
      method! pattern_identifier ?kind (ident : (L.t, L.t) Ast.Identifier.t) =
        ignore kind;
        let (loc, { Ast.Identifier.name = x; comments = _ }) = ident in
        let reason = mk_reason (RIdentifier (OrdinaryName x)) loc in
        begin
          match SMap.find_opt x ssa_env with
          | Some { val_ref; havoc } ->
            val_ref := Val.one reason;
            Havoc.(havoc.locs <- reason :: havoc.locs)
          | _ -> ()
        end;
        super#identifier ident

      (* read *)
      method any_identifier (loc : L.t) (x : string) =
        match SMap.find_opt x ssa_env with
        | Some { val_ref; _ } -> values <- L.LMap.add loc !val_ref values
        | None -> unbound_names <- SSet.add x unbound_names

      method! identifier (ident : (L.t, L.t) Ast.Identifier.t) =
        let (loc, { Ast.Identifier.name = x; comments = _ }) = ident in
        this#any_identifier loc x;
        super#identifier ident

      method! jsx_element_name_identifier (ident : (L.t, L.t) Ast.JSX.Identifier.t) =
        let (loc, { Ast.JSX.Identifier.name; comments = _ }) = ident in
        this#any_identifier loc name;
        super#jsx_identifier ident

      method! jsx_element_name_namespaced ns =
        (* TODO: what identifiers does `<foo:bar />` read? *)
        super#jsx_element_name_namespaced ns

      (* Order of evaluation matters *)
      method! assignment _loc (expr : (L.t, L.t) Ast.Expression.Assignment.t) =
        let open Ast.Expression.Assignment in
        let { operator; left; right; comments = _ } = expr in
        begin
          match operator with
          | None ->
            let open Ast.Pattern in
            begin
              match left with
              | (_, (Identifier _ | Object _ | Array _)) ->
                (* given `x = e`, read e then write x *)
                ignore @@ this#expression right;
                ignore @@ this#assignment_pattern left
              | (_, Expression _) ->
                (* given `o.x = e`, read o then read e *)
                ignore @@ this#assignment_pattern left;
                ignore @@ this#expression right
            end
          | Some _ ->
            let open Ast.Pattern in
            begin
              match left with
              | (_, Identifier { Identifier.name; _ }) ->
                (* given `x += e`, read x then read e then write x *)
                ignore @@ this#identifier name;
                ignore @@ this#expression right;
                ignore @@ this#assignment_pattern left
              | (_, Expression _) ->
                (* given `o.x += e`, read o then read e *)
                ignore @@ this#assignment_pattern left;
                ignore @@ this#expression right
              | (_, (Object _ | Array _)) ->
                (* This is an invalid expression that will cause a runtime error, but we still visit
                   the subexpressions to find other errors *)
                ignore @@ this#expression right
            end
        end;
        expr

      (* Order of evaluation matters *)
      method! variable_declarator
          ~kind (decl : (L.t, L.t) Ast.Statement.VariableDeclaration.Declarator.t) =
        let open Ast.Statement.VariableDeclaration.Declarator in
        let (_loc, { id; init }) = decl in
        let open Ast.Pattern in
        begin
          match id with
          | (_, (Identifier _ | Object _ | Array _)) ->
            begin
              match init with
              | Some init ->
                (* given `var x = e`, read e then write x *)
                ignore @@ this#expression init;
                ignore @@ this#variable_declarator_pattern ~kind id
              | None ->
                (* `var x;` is not a write of `x` *)
                ()
            end
          | (_, Expression _) ->
            (* This is an invalid expression that will cause a runtime error, so we skip the left hand side *)
            ignore @@ Base.Option.map ~f:this#expression init
        end;
        decl

      (* read and write (when the argument is an identifier) *)
      method! update_expression _loc (expr : (L.t, L.t) Ast.Expression.Update.t) =
        let open Ast.Expression.Update in
        let { argument; operator = _; prefix = _; comments = _ } = expr in
        begin
          match argument with
          | (_, Ast.Expression.Identifier x) ->
            (* given `x++`, read x then write x *)
            ignore @@ this#identifier x;
            ignore @@ this#pattern_identifier x
          | _ ->
            (* given `o.x++`, read o *)
            ignore @@ this#expression argument
        end;
        expr

      (* things that cause abrupt completions *)
      method! break _loc (stmt : L.t Ast.Statement.Break.t) =
        let open Ast.Statement.Break in
        let { label; comments = _ } = stmt in
        this#raise_abrupt_completion (AbruptCompletion.break label)

      method! continue _loc (stmt : L.t Ast.Statement.Continue.t) =
        let open Ast.Statement.Continue in
        let { label; comments = _ } = stmt in
        this#raise_abrupt_completion (AbruptCompletion.continue label)

      method! return _loc (stmt : (L.t, L.t) Ast.Statement.Return.t) =
        let open Ast.Statement.Return in
        let { argument; comments = _ } = stmt in
        ignore @@ Flow_ast_mapper.map_opt this#expression argument;
        this#raise_abrupt_completion AbruptCompletion.return

      method! throw _loc (stmt : (L.t, L.t) Ast.Statement.Throw.t) =
        let open Ast.Statement.Throw in
        let { argument; comments = _ } = stmt in
        ignore @@ this#expression argument;
        this#raise_abrupt_completion AbruptCompletion.throw

      (** Control flow **)

      (** We describe the effect on the environment of evaluating node n using Hoare
          triples of the form [PRE] n [POST], where PRE is the environment before
          and POST is the environment after the evaluation of node n. Environments
          must be joined whenever a node is reachable from multiple nodes, as can
          happen after a branch or before a loop. **)

      (******************************************)
      (* [PRE] if (e) { s1 } else { s2 } [POST] *)
      (******************************************)
      (*    |                                   *)
      (*    e                                   *)
      (*   / \                                  *)
      (* s1   s2                                *)
      (*   \./                                  *)
      (*    |                                   *)
      (******************************************)
      (* [PRE] e [ENV0]                         *)
      (* [ENV0] s1 [ENV1]                       *)
      (* [ENV0] s2 [ENV2]                       *)
      (* POST = ENV1 | ENV2                     *)
      (******************************************)
      method! if_statement _loc (stmt : (L.t, L.t) Ast.Statement.If.t) =
        let open Ast.Statement.If in
        let { test; consequent; alternate; _ } = stmt in
        ignore @@ this#expression test;
        let env0 = this#ssa_env in
        (* collect completions and environments of every branch *)
        let then_completion_state =
          this#run_to_completion (fun () ->
              ignore @@ this#if_consequent_statement ~has_else:(alternate <> None) consequent)
        in
        let env1 = this#ssa_env in
        this#reset_ssa_env env0;
        let else_completion_state =
          this#run_to_completion (fun () ->
              ignore
              @@ Flow_ast_mapper.map_opt
                   (fun (loc, { Alternate.body; comments }) ->
                     (loc, { Alternate.body = this#statement body; comments }))
                   alternate)
        in
        (* merge environments *)
        this#merge_self_ssa_env env1;

        (* merge completions *)
        let if_completion_states = (then_completion_state, [else_completion_state]) in
        this#merge_completion_states if_completion_states;
        stmt

      (********************************)
      (* [PRE] while (e) { s } [POST] *)
      (********************************)
      (*    |                         *)
      (*    e <-.                     *)
      (*   / \ /                      *)
      (*  |   s                       *)
      (*   \                          *)
      (*    |                         *)
      (********************************)
      (* PRE = ENV0                   *)
      (* [ENV0 | ENV1] e [ENV2]       *)
      (* [ENV2] s [ENV1]              *)
      (* POST = ENV2                  *)
      (********************************)
      method! while_ _loc (stmt : (L.t, L.t) Ast.Statement.While.t) =
        this#expecting_abrupt_completions (fun () ->
            let continues = AbruptCompletion.continue None :: possible_labeled_continues in
            let open Ast.Statement.While in
            let { test; body; comments = _ } = stmt in
            (* placeholder for environment at the end of the loop body *)
            let env1 = this#fresh_ssa_env in
            this#merge_self_ssa_env env1;
            ignore @@ this#expression test;
            let env2 = this#ssa_env in
            let loop_completion_state =
              this#run_to_completion (fun () -> ignore @@ this#statement body)
            in
            (* continue exits *)
            let loop_completion_state =
              this#run_to_completion (fun () ->
                  this#commit_abrupt_completion_matching
                    (AbruptCompletion.mem continues)
                    loop_completion_state)
            in
            (* end of loop body *)
            this#assert_ssa_env env1;

            (* out of the loop! this always happens right after evaluating the loop test *)
            this#reset_ssa_env env2;

            (* we might also never enter the loop body *)
            let while_completion_states = (None, [loop_completion_state]) in
            let completion_state =
              this#run_to_completion (fun () ->
                  this#merge_completion_states while_completion_states)
            in
            (* completion_state = None *)
            (* break exits *)
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break None])
              completion_state);
        stmt

      (***********************************)
      (* [PRE] do { s } while (e) [POST] *)
      (***********************************)
      (*    |                            *)
      (*    s <-.                        *)
      (*     \ /                         *)
      (*      e                          *)
      (*      |                          *)
      (***********************************)
      (* PRE = ENV0                      *)
      (* [ENV0 | ENV1] s; e [ENV1]       *)
      (* POST = ENV1                     *)
      (***********************************)
      method! do_while _loc (stmt : (L.t, L.t) Ast.Statement.DoWhile.t) =
        this#expecting_abrupt_completions (fun () ->
            let continues = AbruptCompletion.continue None :: possible_labeled_continues in
            let open Ast.Statement.DoWhile in
            let { body; test; _ } = stmt in
            let env1 = this#fresh_ssa_env in
            this#merge_self_ssa_env env1;
            let loop_completion_state =
              this#run_to_completion (fun () -> ignore @@ this#statement body)
            in
            let loop_completion_state =
              this#run_to_completion (fun () ->
                  this#commit_abrupt_completion_matching
                    (AbruptCompletion.mem continues)
                    loop_completion_state)
            in
            begin
              match loop_completion_state with
              | None -> ignore @@ this#expression test
              | _ -> ()
            end;
            this#assert_ssa_env env1;
            let do_while_completion_states = (loop_completion_state, []) in
            let completion_state =
              this#run_to_completion (fun () ->
                  this#merge_completion_states do_while_completion_states)
            in
            (* completion_state = loop_completion_state *)
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break None])
              completion_state);
        stmt

      (**************************************)
      (* [PRE] for (e; e1; e2) { s } [POST] *)
      (**************************************)
      (*    |                               *)
      (*    e                               *)
      (*    |                               *)
      (*   e1 <---.                         *)
      (*   / \    |                         *)
      (*  |   s   |                         *)
      (*  |    \ /                          *)
      (*  |    e2                           *)
      (*   \                                *)
      (*    |                               *)
      (**************************************)
      (* [PRE] e [ENV0]                     *)
      (* [ENV0 | ENV1] e1 [ENV2]            *)
      (* [ENV2] s; e2 [ENV1]                *)
      (* POST = ENV2                        *)
      (**************************************)
      method! scoped_for_statement _loc (stmt : (L.t, L.t) Ast.Statement.For.t) =
        this#expecting_abrupt_completions (fun () ->
            let continues = AbruptCompletion.continue None :: possible_labeled_continues in
            let open Ast.Statement.For in
            let { init; test; update; body; comments = _ } = stmt in
            ignore @@ Flow_ast_mapper.map_opt this#for_statement_init init;
            let env1 = this#fresh_ssa_env in
            this#merge_self_ssa_env env1;
            ignore @@ Flow_ast_mapper.map_opt this#expression test;
            let env2 = this#ssa_env in
            let loop_completion_state =
              this#run_to_completion (fun () -> ignore @@ this#statement body)
            in
            (* continue *)
            let loop_completion_state =
              this#run_to_completion (fun () ->
                  this#commit_abrupt_completion_matching
                    (AbruptCompletion.mem continues)
                    loop_completion_state)
            in
            begin
              match loop_completion_state with
              | None -> ignore @@ Flow_ast_mapper.map_opt this#expression update
              | _ -> ()
            end;
            this#assert_ssa_env env1;
            this#reset_ssa_env env2;
            let for_completion_states = (None, [loop_completion_state]) in
            let completion_state =
              this#run_to_completion (fun () -> this#merge_completion_states for_completion_states)
            in
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break None])
              completion_state);
        stmt

      (*************************************)
      (* [PRE] for (e1 in e2) { s } [POST] *)
      (*************************************)
      (*    |                              *)
      (*    e2                             *)
      (*    |                              *)
      (*    . <---.                        *)
      (*   / \    |                        *)
      (*  |   e1  |                        *)
      (*  |    \ /                         *)
      (*  |     s                          *)
      (*   \                               *)
      (*    |                              *)
      (*************************************)
      (* [PRE] e2 [ENV0]                   *)
      (* ENV2 = ENV0 | ENV1                *)
      (* [ENV2] e2 [ENV0]                  *)
      (* [ENV0 | ENV1] e1; s [ENV1]        *)
      (* POST = ENV2                       *)
      (*************************************)
      method! scoped_for_in_statement _loc (stmt : (L.t, L.t) Ast.Statement.ForIn.t) =
        this#expecting_abrupt_completions (fun () ->
            let continues = AbruptCompletion.continue None :: possible_labeled_continues in
            let open Ast.Statement.ForIn in
            let { left; right; body; each = _; comments = _ } = stmt in
            ignore @@ this#expression right;
            let env1 = this#fresh_ssa_env in
            this#merge_self_ssa_env env1;
            let env2 = this#ssa_env in
            ignore @@ this#for_in_statement_lhs left;
            let loop_completion_state =
              this#run_to_completion (fun () -> ignore @@ this#statement body)
            in
            (* continue *)
            let loop_completion_state =
              this#run_to_completion (fun () ->
                  this#commit_abrupt_completion_matching
                    (AbruptCompletion.mem continues)
                    loop_completion_state)
            in
            this#assert_ssa_env env1;
            this#reset_ssa_env env2;
            let for_in_completion_states = (None, [loop_completion_state]) in
            let completion_state =
              this#run_to_completion (fun () ->
                  this#merge_completion_states for_in_completion_states)
            in
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break None])
              completion_state);
        stmt

      (*************************************)
      (* [PRE] for (e1 of e2) { s } [POST] *)
      (*************************************)
      (*    |                              *)
      (*    e2                             *)
      (*    |                              *)
      (*    . <---.                        *)
      (*   / \    |                        *)
      (*  |   e1  |                        *)
      (*  |    \ /                         *)
      (*  |     s                          *)
      (*   \                               *)
      (*    |                              *)
      (*************************************)
      (* [PRE] e2 [ENV0]                   *)
      (* ENV2 = ENV0 | ENV1                *)
      (* [ENV2] e2 [ENV0]                  *)
      (* [ENV0 | ENV1] e1; s [ENV1]        *)
      (* POST = ENV2                       *)
      (*************************************)
      method! scoped_for_of_statement _loc (stmt : (L.t, L.t) Ast.Statement.ForOf.t) =
        this#expecting_abrupt_completions (fun () ->
            let continues = AbruptCompletion.continue None :: possible_labeled_continues in
            let open Ast.Statement.ForOf in
            let { left; right; body; await = _; comments = _ } = stmt in
            ignore @@ this#expression right;
            let env1 = this#fresh_ssa_env in
            this#merge_self_ssa_env env1;
            let env2 = this#ssa_env in
            ignore @@ this#for_of_statement_lhs left;
            let loop_completion_state =
              this#run_to_completion (fun () -> ignore @@ this#statement body)
            in
            (* continue *)
            let loop_completion_state =
              this#run_to_completion (fun () ->
                  this#commit_abrupt_completion_matching
                    (AbruptCompletion.mem continues)
                    loop_completion_state)
            in
            this#assert_ssa_env env1;
            this#reset_ssa_env env2;
            let for_of_completion_states = (None, [loop_completion_state]) in
            let completion_state =
              this#run_to_completion (fun () ->
                  this#merge_completion_states for_of_completion_states)
            in
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break None])
              completion_state);
        stmt

      (***********************************************************)
      (* [PRE] switch (e) { case e1: s1 ... case eN: sN } [POST] *)
      (***********************************************************)
      (*     |                                                   *)
      (*     e                                                   *)
      (*    /                                                    *)
      (*   e1                                                    *)
      (*   | \                                                   *)
      (*   .  s1                                                 *)
      (*   |   |                                                 *)
      (*   ei  .                                                 *)
      (*   | \ |                                                 *)
      (*   .  si                                                 *)
      (*   |   |                                                 *)
      (*   eN  .                                                 *)
      (*   | \ |                                                 *)
      (*   |  sN                                                 *)
      (*    \  |                                                 *)
      (*      \|                                                 *)
      (*       |                                                 *)
      (***********************************************************)
      (* [PRE] e [ENV0]                                          *)
      (* ENV0' = empty                                           *)
      (* \forall i = 0..N-1:                                     *)
      (*   [ENVi] ei+1 [ENVi+1]                                  *)
      (*   [ENVi+1 | ENVi'] si+1 [ENVi+1']                       *)
      (* POST = ENVN | ENVN'                                     *)
      (***********************************************************)
      method! switch_cases _discriminant cases =
        this#expecting_abrupt_completions (fun () ->
            let (env, case_completion_states) =
              List.fold_left
                (fun acc stuff ->
                  let (_loc, case) = stuff in
                  this#ssa_switch_case acc case)
                (this#empty_ssa_env, [])
                cases
            in
            this#merge_self_ssa_env env;

            (* In general, cases are non-exhaustive. TODO: optimize with `default`. *)
            let switch_completion_states = (None, case_completion_states) in
            let completion_state =
              this#run_to_completion (fun () ->
                  this#merge_completion_states switch_completion_states)
            in
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break None])
              completion_state);
        cases

      method private ssa_switch_case
          (env, case_completion_states) (case : (L.t, L.t) Ast.Statement.Switch.Case.t') =
        let open Ast.Statement.Switch.Case in
        let { test; consequent; comments = _ } = case in
        ignore @@ Flow_ast_mapper.map_opt this#expression test;
        let env0 = this#ssa_env in
        this#merge_ssa_env env0 env;
        let case_completion_state =
          this#run_to_completion (fun () -> ignore @@ this#statement_list consequent)
        in
        let env' = this#ssa_env in
        this#reset_ssa_env env0;
        (env', case_completion_state :: case_completion_states)

      (****************************************)
      (* [PRE] try { s1 } catch { s2 } [POST] *)
      (****************************************)
      (*    |                                 *)
      (*    s1 ..~                            *)
      (*    |    |                            *)
      (*    |   s2                            *)
      (*     \./                              *)
      (*      |                               *)
      (****************************************)
      (* [PRE] s1 [ENV1]                      *)
      (* [HAVOC] s2 [ENV2 ]                   *)
      (* POST = ENV1 | ENV2                   *)
      (****************************************)
      (*******************************************************)
      (* [PRE] try { s1 } catch { s2 } finally { s3 } [POST] *)
      (*******************************************************)
      (*    |                                                *)
      (*    s1 ..~                                           *)
      (*    |    |                                           *)
      (*    |   s2 ..~                                       *)
      (*     \./     |                                       *)
      (*      |______|                                       *)
      (*             |                                       *)
      (*            s3                                       *)
      (*             |                                       *)
      (*******************************************************)
      (* [PRE] s1 [ENV1]                                     *)
      (* [HAVOC] s2 [ENV2 ]                                  *)
      (* [HAVOC] s3 [ENV3 ]                                  *)
      (* POST = ENV3                                         *)
      (*******************************************************)
      method! try_catch _loc (stmt : (L.t, L.t) Ast.Statement.Try.t) =
        this#expecting_abrupt_completions (fun () ->
            let open Ast.Statement.Try in
            let { block = (loc, block); handler; finalizer; comments = _ } = stmt in
            let try_completion_state =
              this#run_to_completion (fun () -> ignore @@ this#block loc block)
            in
            let env1 = this#ssa_env in
            let (catch_completion_state_opt, env2) =
              match handler with
              | Some (loc, clause) ->
                (* NOTE: Havoc-ing the state when entering the handler is probably
                   overkill. We can be more precise but still correct by collecting all
                   possible writes in the try-block and merging them with the state when
                   entering the try-block. *)
                this#havoc_current_ssa_env;
                let catch_completion_state =
                  this#run_to_completion (fun () -> ignore @@ this#catch_clause loc clause)
                in
                ([catch_completion_state], this#ssa_env)
              | None -> ([], this#empty_ssa_env)
            in
            this#merge_ssa_env env1 env2;
            let try_catch_completion_states = (try_completion_state, catch_completion_state_opt) in
            let completion_state =
              this#run_to_completion (fun () ->
                  this#merge_completion_states try_catch_completion_states)
            in
            this#commit_abrupt_completion_matching AbruptCompletion.all completion_state;
            begin
              match finalizer with
              | Some (_loc, block) ->
                (* NOTE: Havoc-ing the state when entering the finalizer is probably
                   overkill. We can be more precise but still correct by collecting
                   all possible writes in the handler and merging them with the state
                   when entering the handler (which in turn should already account for
                   any contributions by the try-block). *)
                this#havoc_current_ssa_env;
                ignore @@ this#block loc block
              | None -> ()
            end;
            this#from_completion completion_state);
        stmt

      (* branching expressions *)
      method! logical _loc (expr : (L.t, L.t) Ast.Expression.Logical.t) =
        let open Ast.Expression.Logical in
        let { operator = _; left; right; comments = _ } = expr in
        ignore @@ this#expression left;
        let env1 = this#ssa_env in
        ignore @@ this#expression right;
        this#merge_self_ssa_env env1;
        expr

      method! conditional _loc (expr : (L.t, L.t) Ast.Expression.Conditional.t) =
        let open Ast.Expression.Conditional in
        let { test; consequent; alternate; comments = _ } = expr in
        ignore @@ this#predicate_expression test;
        let env0 = this#ssa_env in
        ignore @@ this#expression consequent;
        let env1 = this#ssa_env in
        this#reset_ssa_env env0;
        ignore @@ this#expression alternate;
        this#merge_self_ssa_env env1;
        expr

      (* We also havoc state when entering functions and exiting calls. *)
      method! lambda params body =
        this#expecting_abrupt_completions (fun () ->
            let env = this#ssa_env in
            this#run
              (fun () ->
                this#havoc_uninitialized_ssa_env;
                let completion_state =
                  this#run_to_completion (fun () -> super#lambda params body)
                in
                this#commit_abrupt_completion_matching
                  AbruptCompletion.(mem [return; throw])
                  completion_state)
              ~finally:(fun () -> this#reset_ssa_env env))

      method! declare_function loc expr =
        match Declare_function_utils.declare_function_to_function_declaration_simple loc expr with
        | Some stmt ->
          let _ = this#statement (loc, stmt) in
          expr
        | None -> super#declare_function loc expr

      method! call loc (expr : (L.t, L.t) Ast.Expression.Call.t) =
        ignore @@ super#call loc expr;
        this#havoc_current_ssa_env;
        expr

      method! new_ _loc (expr : (L.t, L.t) Ast.Expression.New.t) =
        let open Ast.Expression.New in
        let { callee; targs = _; arguments; comments = _ } = expr in
        ignore @@ this#expression callee;
        ignore @@ Flow_ast_mapper.map_opt this#call_arguments arguments;
        this#havoc_current_ssa_env;
        expr

      method! unary_expression _loc (expr : (L.t, L.t) Ast.Expression.Unary.t) =
        Ast.Expression.Unary.(
          let { argument; operator; comments = _ } = expr in
          ignore @@ this#expression argument;
          begin
            match operator with
            | Await -> this#havoc_current_ssa_env
            | _ -> ()
          end;
          expr)

      method! yield loc (expr : ('loc, 'loc) Ast.Expression.Yield.t) =
        ignore @@ super#yield loc expr;
        this#havoc_current_ssa_env;
        expr

      (* Labeled statements handle labeled breaks, but also push labeled continues
         that are expected to be handled by immediately nested loops. *)
      method! labeled_statement _loc (stmt : (L.t, L.t) Ast.Statement.Labeled.t) =
        this#expecting_abrupt_completions (fun () ->
            let open Ast.Statement.Labeled in
            let { label; body; comments = _ } = stmt in
            possible_labeled_continues <-
              AbruptCompletion.continue (Some label) :: possible_labeled_continues;
            let completion_state =
              this#run_to_completion (fun () -> ignore @@ this#statement body)
            in
            possible_labeled_continues <- [];
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break (Some label)])
              completion_state);
        stmt

      method! statement (stmt : (L.t, L.t) Ast.Statement.t) =
        let open Ast.Statement in
        begin
          match stmt with
          | (_, While _)
          | (_, DoWhile _)
          | (_, For _)
          | (_, ForIn _)
          | (_, ForOf _)
          | (_, Labeled _) ->
            ()
          | _ -> possible_labeled_continues <- []
        end;
        super#statement stmt

      (* Function declarations are hoisted to the top of a block, so that they may be considered
         initialized before they are read. *)
      method! statement_list (stmts : (L.t, L.t) Ast.Statement.t list) =
        let open Ast.Statement in
        let (function_decls, other_stmts) =
          List.partition
            (function
              | (_, FunctionDeclaration _) -> true
              | _ -> false)
            stmts
        in
        ignore @@ super#statement_list (function_decls @ other_stmts);
        stmts
    end

  let program_with_scope ?(flowmin_compatibility = false) program =
    let (loc, _) = program in
    let ssa_walk = new ssa_builder in
    let bindings =
      if flowmin_compatibility then
        let hoist = new lexical_hoister ~flowmin_compatibility in
        hoist#eval hoist#program program
      else
        (* TODO: with_types should probably be false, but this maintains previous behavior *)
        let hoist = new hoister ~flowmin_compatibility ~with_types:true in
        hoist#eval hoist#program program
    in
    let completion_state =
      ssa_walk#run_to_completion (fun () ->
          ignore @@ ssa_walk#with_bindings loc bindings ssa_walk#program program)
    in
    (completion_state, (ssa_walk#acc, ssa_walk#values, ssa_walk#unbound_names))

  let program program =
    let (_, (_, values, _)) = program_with_scope ~flowmin_compatibility:false program in
    values
end

module With_Loc = Make (Loc_sig.LocS) (Ssa_api.With_Loc) (Scope_builder.With_Loc)
module With_ALoc = Make (Loc_sig.ALocS) (Ssa_api.With_ALoc) (Scope_builder.With_ALoc)
include With_Loc
