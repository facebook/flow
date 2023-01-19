(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module is responsible for building a mapping from variable reads to the
 * writes and refinements that reach those reads. It is based on the implementation of the
 * ssa_builder, but with enough divergent behavior that the ssa_builder and name_resolver don't
 * actually share much code. If you're here to add support for a new syntax feature, you'll likely
 * need to modify the ssa_builder as well, but not necessarily with identical changes.*)

let statement_error = ()

let maybe_exhaustively_checked_var_name = "<maybe_exhaustively_checked>"

let next_var_name = "<next>"

open Reason
open Hoister

module type C = Dependency_sigs.C

module type F = Dependency_sigs.F

let error_todo = ()

module type S = sig
  module Env_api : Env_api.S with module L = Loc_sig.ALocS

  type cx

  type abrupt_kind

  exception AbruptCompletionExn of abrupt_kind

  val program_with_scope :
    cx ->
    ?lib:bool ->
    ?exclude_syms:NameUtils.Set.t ->
    (ALoc.t, ALoc.t) Flow_ast.Program.t ->
    abrupt_kind option * Env_api.env_info

  val program :
    cx ->
    ?lib:bool ->
    ?exclude_syms:NameUtils.Set.t ->
    (ALoc.t, ALoc.t) Flow_ast.Program.t ->
    Env_api.values * (int -> Env_api.refinement)
end

module PostInferenceCheck = Env_api

module Make
    (Scope_api : Scope_api_sig.S with module L = Loc_sig.ALocS)
    (Ssa_api : Ssa_api.S with module L = Loc_sig.ALocS)
    (Env_api : Env_api.S
                 with module L = Loc_sig.ALocS
                  and module Scope_api = Scope_api
                  and module Ssa_api = Ssa_api)
    (Context : C)
    (FlowAPIUtils : F with type cx = Context.t) :
  S with module Env_api = Env_api and type cx = Context.t = struct
  module Scope_builder :
    Scope_builder_sig.S with module L = Loc_sig.ALocS and module Api = Scope_api =
    Scope_builder.Make (Loc_sig.ALocS) (Scope_api)

  module Provider_api :
    Provider_api.S with type info = Env_api.Provider_api.info and module L = Loc_sig.ALocS =
    Env_api.Provider_api

  module Ssa_builder = Ssa_builder.Make (Loc_sig.ALocS) (Ssa_api) (Scope_builder)
  module Invalidation_api =
    Invalidation_api.Make (Loc_sig.ALocS) (Scope_api) (Ssa_api) (Provider_api)
  module Env_api = Env_api
  module Eq_test = Eq_test.Make (Scope_api) (Ssa_api) (Env_api)
  module EnvMap = Env_api.EnvMap
  open Scope_builder
  open Env_api.Refi

  type cx = Context.t

  type refinement_chain =
    | BASE of refinement
    | AND of int * int
    | OR of int * int
    | NOT of int

  type cond_context =
    | SwitchTest
    | OtherTest

  (* For every read of a variable x, we are interested in tracking writes to x
     that can reach that read. Ultimately the writes are going to be represented
     as a list of locations, where each location corresponds to a "single static
     assignment" of the variable in the code. But for the purposes of analysis, it
     is useful to represent these writes with a data type that contains either a
     single write, or a "join" of writes (in compiler terminology, a PHI node), or
     a reference to something that is unknown at a particular point in the AST
     during traversal, but will be known by the time traversal is complete. *)
  module Val : sig
    type t = {
      id: int;
      write_state: write_state;
    }

    and write_state

    val empty : unit -> t

    val uninitialized : ALoc.t -> t

    val undefined : L.t virtual_reason -> t

    val number : L.t virtual_reason -> t

    val merge : t -> t -> t

    val empty_array : L.t virtual_reason -> L.LSet.t -> t

    val function_this : ALoc.t virtual_reason -> t

    val global_this : ALoc.t virtual_reason -> t

    val illegal_this : ALoc.t virtual_reason -> t

    val class_instance_this : ALoc.t virtual_reason -> t

    val class_static_this : ALoc.t virtual_reason -> t

    val class_instance_super : ALoc.t virtual_reason -> t

    val class_static_super : ALoc.t virtual_reason -> t

    val exports : t

    val module_scoped : string -> t

    val global : string -> t

    val one : ALoc.t virtual_reason -> t

    val providers : Provider_api.provider list -> t

    val illegal_write : ALoc.t virtual_reason -> t

    val of_write : write_state -> t

    val simplify : ALoc.t option -> Bindings.kind option -> string option -> t -> Env_api.read

    val id_of_val : t -> int

    val base_id_of_val : t -> int

    val refinement : int -> t -> t

    val projection : ALoc.t -> t

    val undeclared : string -> ALoc.t -> t

    val declared_but_skipped : string -> ALoc.t -> t

    val declared_function : ALoc.t -> t

    (* unwraps a Refinement into just the underlying write *)
    val unrefine : int -> t -> t

    val unrefine_deeply : int -> t -> t

    (* Replace the base write of the refinement with the new base.
       If the write is not a refinement, replace the entire write with the base.

       This is useful for attaching a refinement that is known to be associated with a write, but
       is not attached due to syntactic difference.
       e.g. refinements on obj.x should be attached to x in const {x} = obj *)
    val replace_refinement_base_write : base:t -> t -> t

    val writes_of_uninitialized : (int -> bool) -> t -> write_state list

    val is_global_undefined : t -> bool

    val is_global : t -> bool

    val is_undeclared : t -> bool

    val is_undeclared_or_skipped : t -> bool

    val is_declared_function : t -> bool

    val is_projection : t -> bool

    val debug_to_string : (int -> refinement) -> t -> string
  end = struct
    let curr_id = ref 0

    type write_state =
      | Uninitialized of ALoc.t
      | Undeclared of string * ALoc.t
      | DeclaredButSkipped of string * ALoc.t
      | Projection of ALoc.t
      | FunctionThis of ALoc.t virtual_reason
      | GlobalThis of ALoc.t virtual_reason
      | IllegalThis of ALoc.t virtual_reason
      | ClassInstanceThis of ALoc.t virtual_reason
      | ClassStaticThis of ALoc.t virtual_reason
      | ClassInstanceSuper of ALoc.t virtual_reason
      | ClassStaticSuper of ALoc.t virtual_reason
      | Exports
      | ModuleScoped of string
      | Global of string
      | Loc of ALoc.t virtual_reason
      | EmptyArray of {
          reason: ALoc.t virtual_reason;
          arr_providers: L.LSet.t;
        }
      | IllegalWrite of ALoc.t virtual_reason
      | PHI of write_state list
      | Refinement of {
          refinement_id: int;
          val_t: t;
        }
      | Undefined of ALoc.t virtual_reason
      | Number of ALoc.t virtual_reason
      | DeclaredFunction of ALoc.t

    and t = {
      id: int;
      write_state: write_state;
    }

    let rec debug_write_state get_refi = function
      | Uninitialized _ -> "(uninitialized)"
      | Undeclared _ -> "(undeclared)"
      | DeclaredButSkipped _ -> "(declared but skipped)"
      | Projection l -> Utils_js.spf "projection at %s" (L.debug_to_string l)
      | Loc reason ->
        let loc = Reason.poly_loc_of_reason reason in
        Utils_js.spf
          "%s: (%s)"
          (L.debug_to_string loc)
          Reason.(desc_of_reason reason |> string_of_desc)
      | EmptyArray { reason; _ } ->
        let loc = Reason.poly_loc_of_reason reason in
        Utils_js.spf
          "(empty array) %s: (%s)"
          (L.debug_to_string loc)
          Reason.(desc_of_reason reason |> string_of_desc)
      | IllegalWrite reason ->
        let loc = Reason.poly_loc_of_reason reason in
        Utils_js.spf "illegal write at %s" (L.debug_to_string loc)
      | Refinement { refinement_id; val_t } ->
        let refinement_kind =
          refinement_id |> get_refi |> snd |> Env_api.show_refinement_kind_without_locs
        in
        let write_str = debug_to_string get_refi val_t in
        Utils_js.spf "{refinement = %s; write = %s}" refinement_kind write_str
      | FunctionThis _ -> "This(function)"
      | GlobalThis _ -> "This(global)"
      | IllegalThis _ -> "This(illegal)"
      | ClassInstanceThis _ -> "This(instance)"
      | ClassStaticThis _ -> "This(static)"
      | ClassInstanceSuper _ -> "Super(instance)"
      | ClassStaticSuper _ -> "Super(static)"
      | Exports -> "Exports"
      | ModuleScoped name -> "ModuleScoped " ^ name
      | Global name -> "Global " ^ name
      | Undefined _ -> "undefined"
      | Number _ -> "number"
      | DeclaredFunction l -> Utils_js.spf "declared function %s" (L.debug_to_string l)
      | PHI ts ->
        Utils_js.spf "[%s]" (ts |> List.map (debug_write_state get_refi) |> String.concat ",")

    and debug_to_string get_refi { id; write_state } =
      string_of_int id ^ " " ^ debug_write_state get_refi write_state

    (* Ensure we only produce one unique val for the same Loc *)
    let val_one_cache : (Reason.t, t) Hashtbl.t = Hashtbl.create 0

    let is_global_undefined t =
      match t.write_state with
      | Global "undefined" -> true
      | _ -> false

    let is_global t =
      match t.write_state with
      | Global _ -> true
      | _ -> false

    let is_undeclared t =
      match t.write_state with
      | Undeclared _ -> true
      | _ -> false

    let is_undeclared_or_skipped t =
      match t.write_state with
      | Undeclared _ -> true
      | DeclaredButSkipped _ -> true
      | _ -> false

    let is_declared_function t =
      match t.write_state with
      | DeclaredFunction _ -> true
      | _ -> false

    let is_projection t =
      match t.write_state with
      | Projection _ -> true
      | _ -> false

    let new_id () =
      let id = !curr_id in
      curr_id := !curr_id + 1;
      id

    let mk_with_write_state write_state =
      let id = new_id () in
      { id; write_state }

    let of_write = mk_with_write_state

    let empty () = mk_with_write_state @@ PHI []

    let uninitialized r = mk_with_write_state (Uninitialized r)

    let undefined r = mk_with_write_state (Undefined r)

    let empty_array reason arr_providers = mk_with_write_state (EmptyArray { reason; arr_providers })

    let number r = mk_with_write_state (Number r)

    let projection loc = mk_with_write_state @@ Projection loc

    let declared_function loc = mk_with_write_state @@ DeclaredFunction loc

    let refinement refinement_id val_t = mk_with_write_state @@ Refinement { refinement_id; val_t }

    let undeclared name def_loc = mk_with_write_state @@ Undeclared (name, def_loc)

    let declared_but_skipped name def_loc = mk_with_write_state @@ DeclaredButSkipped (name, def_loc)

    let rec unrefine_deeply_write_state id write_state =
      match write_state with
      | Refinement { refinement_id; val_t } when refinement_id = id ->
        unrefine_deeply_write_state id val_t.write_state
      | Refinement { refinement_id; val_t } ->
        let val_t' = mk_with_write_state @@ unrefine_deeply_write_state id val_t.write_state in
        Refinement { refinement_id; val_t = val_t' }
      | PHI ts ->
        let ts' = ListUtils.ident_map (unrefine_deeply_write_state id) ts in
        if ts' == ts then
          write_state
        else
          PHI ts'
      | _ -> write_state

    let rec base_id_of_val { id; write_state } =
      match write_state with
      | Refinement { refinement_id = _; val_t } -> base_id_of_val val_t
      | _ -> id

    let unrefine_deeply id t = mk_with_write_state @@ unrefine_deeply_write_state id t.write_state

    let unrefine id t =
      match t.write_state with
      | Refinement { refinement_id; val_t } when refinement_id = id -> val_t
      | _ -> t

    let replace_refinement_base_write ~base t =
      match t.write_state with
      | Refinement { refinement_id; val_t = _ } -> refinement refinement_id base
      | _ -> base

    let join_write_states = function
      | [] -> PHI []
      | [t] -> t
      | ts -> PHI ts

    let one reason =
      match Hashtbl.find_opt val_one_cache reason with
      | Some v -> v
      | None ->
        let v = mk_with_write_state @@ Loc reason in
        Hashtbl.add val_one_cache reason v;
        v

    let illegal_write reason = mk_with_write_state @@ IllegalWrite reason

    let join write_states =
      match join_write_states write_states with
      | Loc reason -> one reason
      | write_state -> mk_with_write_state write_state

    module WriteSet = Flow_set.Make (struct
      type t = write_state

      let compare = Stdlib.compare
    end)

    let rec normalize (t : write_state) : WriteSet.t =
      match t with
      | Uninitialized _
      | Undefined _
      | Number _
      | DeclaredFunction _
      | Undeclared _
      | DeclaredButSkipped _
      | Projection _
      | FunctionThis _
      | GlobalThis _
      | IllegalThis _
      | ClassInstanceThis _
      | ClassStaticThis _
      | ClassInstanceSuper _
      | ClassStaticSuper _
      | Exports
      | ModuleScoped _
      | Global _
      | Loc _
      | EmptyArray _
      | IllegalWrite _
      | Refinement _ ->
        WriteSet.singleton t
      | PHI ts ->
        List.fold_left
          (fun vals' t ->
            let vals = normalize t in
            WriteSet.union vals' vals)
          WriteSet.empty
          ts

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
        join (WriteSet.elements vals)

    let function_this reason = mk_with_write_state @@ FunctionThis reason

    let global_this reason = mk_with_write_state @@ GlobalThis reason

    let illegal_this reason = mk_with_write_state @@ IllegalThis reason

    let class_instance_this reason = mk_with_write_state @@ ClassInstanceThis reason

    let class_static_this reason = mk_with_write_state @@ ClassStaticThis reason

    let class_instance_super reason = mk_with_write_state @@ ClassInstanceSuper reason

    let class_static_super reason = mk_with_write_state @@ ClassStaticSuper reason

    let exports = mk_with_write_state @@ Exports

    let module_scoped name = mk_with_write_state @@ ModuleScoped name

    let global name = mk_with_write_state @@ Global name

    let providers locs =
      join
        ((Base.List.map ~f:(fun { Provider_api.reason; empty_array_writes } ->
              Base.Option.value_map
                ~f:(fun arr_providers -> EmptyArray { reason; arr_providers })
                ~default:(Loc reason)
                empty_array_writes
          )
         )
           locs
        )

    let rec simplify_val t =
      let vals = normalize t.write_state in
      Base.List.map
        ~f:(function
          | Uninitialized l
            when WriteSet.for_all
                   (function
                     | IllegalWrite _
                     | Uninitialized _ ->
                       true
                     | _ -> false)
                   vals ->
            Env_api.Uninitialized (mk_reason RUninitialized l)
          | Undefined r -> Env_api.Undefined r
          | Number r -> Env_api.Number r
          | DeclaredFunction l -> Env_api.DeclaredFunction l
          | Undeclared (name, loc)
          | DeclaredButSkipped (name, loc) ->
            Env_api.Undeclared (name, loc)
          | Uninitialized l -> Env_api.Uninitialized (mk_reason RPossiblyUninitialized l)
          | Projection loc -> Env_api.Projection loc
          | FunctionThis r -> Env_api.FunctionThis r
          | GlobalThis r -> Env_api.GlobalThis r
          | IllegalThis r -> Env_api.IllegalThis r
          | ClassInstanceThis r -> Env_api.ClassInstanceThis r
          | ClassStaticThis r -> Env_api.ClassStaticThis r
          | ClassInstanceSuper r -> Env_api.ClassInstanceSuper r
          | ClassStaticSuper r -> Env_api.ClassStaticSuper r
          | Loc r -> Env_api.Write r
          | EmptyArray { reason; arr_providers } -> Env_api.EmptyArray { reason; arr_providers }
          | IllegalWrite r -> Env_api.IllegalWrite r
          | Refinement { refinement_id; val_t } ->
            Env_api.Refinement
              { writes = simplify_val val_t; refinement_id; write_id = Some val_t.id }
          | Exports -> Env_api.Exports
          | ModuleScoped name -> Env_api.ModuleScoped name
          | Global name -> Env_api.Global name
          | PHI _ ->
            raise Env_api.(Env_invariant (None, Impossible "A normalized value cannot be a PHI")))
        (WriteSet.elements vals)

    (* Simplification converts a Val.t to a list of locations. *)
    let simplify def_loc binding_kind_opt name value =
      let write_locs = simplify_val value in
      let val_kind =
        match binding_kind_opt with
        | Some (Bindings.Type { imported }) -> Some (Env_api.Type { imported })
        | Some _ -> Some Env_api.Value
        | None -> None
      in
      { Env_api.def_loc; write_locs; val_kind; name; id = Some value.id }

    let id_of_val { id; write_state = _ } = id

    let writes_of_uninitialized refine_to_undefined { write_state; _ } =
      let rec state_is_uninitialized v =
        match v with
        | Undeclared _ -> [v]
        | DeclaredButSkipped _ -> []
        | Undefined _ -> []
        | Number _ -> []
        | DeclaredFunction _ -> []
        | Uninitialized _ -> [v]
        | PHI states -> Base.List.concat_map ~f:state_is_uninitialized states
        | Refinement { refinement_id; val_t = { write_state; _ } } ->
          let states = state_is_uninitialized write_state in
          if List.length states = 0 || (not @@ refine_to_undefined refinement_id) then
            []
          else
            states
        | Loc _ -> []
        | EmptyArray _ -> []
        | IllegalWrite _ -> []
        | FunctionThis _ -> []
        | GlobalThis _ -> []
        | IllegalThis _ -> []
        | ClassInstanceThis _ -> []
        | ClassStaticThis _ -> []
        | ClassInstanceSuper _ -> []
        | ClassStaticSuper _ -> []
        | Exports -> []
        | ModuleScoped _ -> []
        | Global _ -> []
        | Projection _ -> []
      in
      state_is_uninitialized write_state
  end

  module RefinementKey = Refinement_key.Make (L)

  module HeapRefinementMap = WrappedMap.Make (struct
    type t = RefinementKey.proj list

    let compare = Stdlib.compare
  end)

  module LookupMap = WrappedMap.Make (struct
    type t = RefinementKey.lookup

    let compare = Stdlib.compare
  end)

  type heap_refinement_map = Val.t HeapRefinementMap.t

  (* An environment is a map from variables to values. *)
  module Env = struct
    type entry = {
      env_val: Val.t;
      heap_refinements: heap_refinement_map;
      def_loc: ALoc.t option;
    }

    type t = entry SMap.t
  end

  let smap_find x t =
    match SMap.find_opt x t with
    | Some r -> r
    | None -> raise Env_api.(Env_invariant (None, Impossible (Utils_js.spf "%s missing in map" x)))

  let heap_map_find x t =
    match HeapRefinementMap.find_opt x t with
    | Some r -> r
    | None -> raise Env_api.(Env_invariant (None, Impossible "heap entry missing in map"))

  let imap_find x t =
    match IMap.find_opt x t with
    | Some r -> r
    | None -> raise Env_api.(Env_invariant (None, Impossible (Utils_js.spf "%d missing in map" x)))

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

  module SuperCallInDerivedCtorChecker : sig
    val check : cx -> L.t -> (L.t, L.t) Ast.Class.t -> unit
  end = struct
    open Ssa_builder

    class checker cx this_def_loc super_def_loc =
      object (this)
        inherit
          Ssa_builder.ssa_builder
            ~flowmin_compatibility:false
            ~enable_enums:(Context.enable_enums cx) as super

        val mutable this_read_loc_list = []

        val mutable super_read_loc_list = []

        val mutable super_call_loc_list = []

        method private init_internal_name name loc def_loc =
          let reason = mk_reason (RIdentifier (internal_name name)) def_loc in
          let { val_ref; havoc } = smap_find name ssa_env in
          this#any_identifier loc name;
          super_call_loc_list <- loc :: super_call_loc_list;
          val_ref := Val.one reason;
          Havoc.(havoc.locs <- reason :: havoc.locs)

        method add_errors =
          let values = this#values in
          let add_name_already_bound_error_opt loc =
            let write_locs = values |> L.LMap.find_opt loc |> Base.Option.value ~default:[] in
            if
              Base.List.for_all write_locs ~f:(function
                  | Ssa_api.Write _ -> true
                  | Ssa_api.Uninitialized -> false
                  )
            then (
              FlowAPIUtils.add_output
                cx
                Error_message.(
                  EBindingError (ENameAlreadyBound, loc, internal_name "this", this_def_loc)
                );
              FlowAPIUtils.add_output
                cx
                Error_message.(
                  EBindingError (ENameAlreadyBound, loc, internal_name "super", super_def_loc)
                )
            )
          in
          let add_ref_before_decl_error_opt name def_loc loc =
            let write_locs = values |> L.LMap.find_opt loc |> Base.Option.value ~default:[] in
            if
              Base.List.exists write_locs ~f:(function
                  | Ssa_api.Uninitialized -> true
                  | Ssa_api.Write _ -> false
                  )
            then
              FlowAPIUtils.add_output
                cx
                Error_message.(
                  EBindingError (EReferencedBeforeDeclaration, loc, internal_name name, def_loc)
                )
          in
          Base.List.iter super_call_loc_list ~f:add_name_already_bound_error_opt;
          Base.List.iter this_read_loc_list ~f:(add_ref_before_decl_error_opt "this" this_def_loc);
          Base.List.iter super_read_loc_list ~f:(add_ref_before_decl_error_opt "super" super_def_loc)

        method private init_this_super loc =
          this#init_internal_name "this" loc this_def_loc;
          this#init_internal_name "super" loc super_def_loc

        (* We don't want to havoc `this` and `super` initialization states. *)
        method! havoc_current_ssa_env = ()

        method! havoc_uninitialized_ssa_env = ()

        method! this_expression loc this_ =
          this#any_identifier loc "this";
          this_read_loc_list <- loc :: this_read_loc_list;
          this_

        method! super_expression loc super_ =
          this#any_identifier loc "super";
          super_read_loc_list <- loc :: super_read_loc_list;
          super_

        (* We avoid deeper class visit, since it will be handled by name_resolver. *)
        method! class_ _ cls = cls

        method! call loc expr =
          let open Ast.Expression.Call in
          let { callee; targs; arguments; _ } = expr in
          match callee with
          | (_, Ast.Expression.Super _) ->
            (* Do not visit callee so that we can avoid a forward-reference error. *)
            ignore @@ Flow_ast_mapper.map_opt this#call_type_args targs;
            ignore @@ this#call_arguments arguments;
            (* Only after the super call, `this` and `super` are defined. *)
            this#init_this_super loc;
            expr
          | _ -> super#call loc expr
      end

    let check cx loc cls =
      let open Flow_ast.Class in
      let { id; extends; body = (_, { Body.body = members; _ }); _ } = cls in
      let this_def_loc = Base.Option.value_map ~default:loc ~f:fst id in
      match extends with
      | None -> ()
      | Some (super_def_loc, _) ->
        Base.List.iter members ~f:(function
            | Body.Method (_, { Method.kind = Method.Constructor; value = (loc, constructor); _ })
              ->
              let checker = new checker cx this_def_loc super_def_loc in
              let bindings =
                Bindings.(
                  empty
                  |> add ((this_def_loc, { Ast.Identifier.name = "this"; comments = None }), Const)
                  |> add ((super_def_loc, { Ast.Identifier.name = "super"; comments = None }), Const)
                )
              in
              ignore @@ checker#with_bindings loc bindings (checker#function_ loc) constructor;
              checker#add_errors
            | _ -> ()
            )
  end

  let rec list_iter3 f l1 l2 l3 =
    match (l1, l2, l3) with
    | ([], [], []) -> ()
    | (x1 :: l1, x2 :: l2, x3 :: l3) ->
      f x1 x2 x3;
      list_iter3 f l1 l2 l3
    | _ -> raise Env_api.(Env_invariant (None, Impossible "list_iter3 mismatch"))

  type abrupt_kind = AbruptCompletion.t

  exception AbruptCompletionExn = AbruptCompletion.Exn

  type env_val = {
    val_ref: Val.t ref;
    havoc: Val.t;
    writes_by_closure_provider_val: Val.t option;
    def_loc: ALoc.t option;
    heap_refinements: heap_refinement_map ref;
    kind: Bindings.kind;
  }

  type read_entry = {
    def_loc: ALoc.t option;
    binding_kind_opt: Bindings.kind option;
    value: Val.t;
    name: string option;
  }

  type refinement_id = int

  (* Describes a set of projections that arise from refinements and which should be restored to
     the heap refinements after pushing refinement scope,
     e.g.
     ```
     if (x.y) {
       // Has changeset x.y
     } else {
       // During this#negate_new_refinements
       //   1. Initially, heap refinement on x is removed during this#pop_refinement_scope.
       //   2. During this#push_refinement_scope, the base heap_val is restored from changeset,
       //      and the negation of refinement is added,
       //      so we have {refinement=Not Truthy, write=x.y}.
     }
     ```
  *)
  type changeset = Val.t LookupMap.t

  type refinement_prop =
    | Refinements of (RefinementKey.lookup * refinement_id) IMap.t * changeset
    | Not of refinement_prop
    | And of refinement_prop * refinement_prop
    | Or of refinement_prop * refinement_prop

  (* The `applied` and `changeset` elements of this record describe the refinements and
     values that have presently been applied to the environment, when this is on the
     latest_refinement stack (see below). The `total` describes the same changeset and
     refinements, but in a propositional form that can be negated. We must maintain the invariant
     that normalizing the `total` (using the normalize_total_refinements method below) produces the
     applied and the changeset. *)
  type refinement_maps = {
    applied: (RefinementKey.lookup * refinement_id) IMap.t;
    changeset: changeset;
    total: refinement_prop option;
  }

  type name_resolver_state = {
    (* We maintain a map of read locations to raw Val.t and their def locs terms, which are
       simplified to lists of write locations once the analysis is done. *)
    values: read_entry L.LMap.t;
    (* We also maintain a list of all write locations, for use in populating the env with
       types. *)
    write_entries: Env_api.env_entry EnvMap.t;
    predicate_refinement_maps: Env_api.predicate_refinement_maps;
    toplevel_members: Env_api.toplevel_member list;
    module_toplevel_members: Env_api.toplevel_member list L.LMap.t;
    curr_id: int;
    (* Maps refinement ids to refinements. This mapping contains _all_ the refinements reachable at
     * any point in the code. The latest_refinement maps keep track of which entries to read. *)
    refinement_heap: refinement_chain IMap.t;
    latest_refinements: refinement_maps list;
    env: env_val Nel.t SMap.t;
    (* A set of names that have to be excluded from binding.
       This set is always empty when we are checking normal code. It will only be possibly non-empty
       when we are checking libdef code. In libdefs, this set represents a list of names that we
       have already added to the globals. When we read a name in this set, we will ignore the local
       file binding and treat it as a read of global. *)
    exclude_syms: NameUtils.Set.t;
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
    abrupt_completion_envs: AbruptCompletion.env list;
    (* Track the list of labels that might describe a loop. Used to detect which
       labeled continues need to be handled by the loop.

       The idea is that a labeled statement adds its label to the list before
       entering its child, and if the child is not a loop or another labeled
       statement, the list will be cleared. A loop will consume the list, so we
       also clear the list on our way out of any labeled statement. *)
    possible_labeled_continues: AbruptCompletion.t list;
    predicate_scope_names: SSet.t option;
    visiting_hoisted_type: bool;
    jsx_base_name: string option;
  }

  type pattern_write_kind =
    | VarBinding
    | LetBinding
    | ConstBinding
    | FunctionBinding
    | AssignmentWrite

  type this_super_binding_env =
    | FunctionEnv
    | ClassInstanceEnv
    | ClassStaticEnv
    | IllegalThisEnv

  let variable_declaration_binding_kind_to_pattern_write_kind = function
    | None -> AssignmentWrite
    | Some Flow_ast.Statement.VariableDeclaration.Var -> VarBinding
    | Some Flow_ast.Statement.VariableDeclaration.Let -> LetBinding
    | Some Flow_ast.Statement.VariableDeclaration.Const -> ConstBinding

  let error_for_assignment_kind
      cx name assignment_loc def_loc_opt stored_binding_kind pattern_write_kind v =
    match def_loc_opt with
    (* Identifiers with no binding can never reintroduce "cannot reassign binding" errors *)
    | None -> None
    | Some def_loc ->
      (* We use the pattern_write_kind to decide if we should emit an error saying
       * "you cannot re-declare X" vs. "you cannot reassign const X" *)
      (match (stored_binding_kind, pattern_write_kind) with
      | (Bindings.Const, AssignmentWrite) ->
        Some
          Error_message.(
            EBindingError (EConstReassigned, assignment_loc, OrdinaryName name, def_loc)
          )
      | (Bindings.Parameter, AssignmentWrite) when Context.enable_const_params cx ->
        Some
          Error_message.(
            EBindingError (EConstParamReassigned, assignment_loc, OrdinaryName name, def_loc)
          )
      | (Bindings.Class, AssignmentWrite) ->
        let def_reason = mk_reason (RIdentifier (OrdinaryName name)) def_loc in
        Some
          Error_message.(
            EAssignConstLikeBinding
              { loc = assignment_loc; definition = def_reason; binding_kind = ClassNameBinding }
          )
      | (Bindings.Function, AssignmentWrite) ->
        let def_reason = mk_reason (RIdentifier (OrdinaryName name)) def_loc in
        Some
          Error_message.(
            EAssignConstLikeBinding
              { loc = assignment_loc; definition = def_reason; binding_kind = FunctionNameBinding }
          )
      | (Bindings.Import, AssignmentWrite) ->
        Some
          Error_message.(
            EBindingError (EImportReassigned, assignment_loc, OrdinaryName name, def_loc)
          )
      | (Bindings.DeclaredFunction _, AssignmentWrite) ->
        let def_reason = mk_reason (RIdentifier (OrdinaryName name)) def_loc in
        Some
          Error_message.(
            EAssignConstLikeBinding
              {
                loc = assignment_loc;
                definition = def_reason;
                (* The error message is unaffected by the predicate flag *)
                binding_kind = DeclaredFunctionNameBinding;
              }
          )
      | (Bindings.Var, (LetBinding | ConstBinding | FunctionBinding)) ->
        Some
          Error_message.(
            EBindingError (ENameAlreadyBound, assignment_loc, OrdinaryName name, def_loc)
          )
      | (Bindings.Var, VarBinding) when assignment_loc <> def_loc ->
        (* We ban var redeclaration on top of other JS illegal rebinding rules. *)
        Some
          Error_message.(
            EBindingError (EVarRedeclaration, assignment_loc, OrdinaryName name, def_loc)
          )
      | (Bindings.Const, (VarBinding | LetBinding | ConstBinding | FunctionBinding))
      | (Bindings.Let, (VarBinding | LetBinding | ConstBinding | FunctionBinding))
      | (Bindings.Class, (VarBinding | LetBinding | ConstBinding | FunctionBinding))
      | (Bindings.Function, (VarBinding | LetBinding | ConstBinding | FunctionBinding))
      | (Bindings.Import, (VarBinding | LetBinding | ConstBinding | FunctionBinding))
      | (Bindings.Type _, (VarBinding | LetBinding | ConstBinding | FunctionBinding))
        when not (Val.is_undeclared v) ->
        Some
          Error_message.(
            EBindingError (ENameAlreadyBound, assignment_loc, OrdinaryName name, def_loc)
          )
      | (Bindings.DeclaredFunction _, (VarBinding | LetBinding | ConstBinding)) ->
        Some
          Error_message.(
            EBindingError (ENameAlreadyBound, assignment_loc, OrdinaryName name, def_loc)
          )
      | (Bindings.Enum, AssignmentWrite) ->
        Some
          Error_message.(EBindingError (EEnumReassigned, assignment_loc, OrdinaryName name, def_loc))
      | (Bindings.Type { imported }, AssignmentWrite) ->
        Some
          Error_message.(
            EBindingError
              (ETypeInValuePosition { imported; name }, assignment_loc, OrdinaryName name, def_loc)
          )
      | (Bindings.Parameter, (VarBinding | LetBinding | ConstBinding | FunctionBinding))
        when Context.enable_const_params cx && not (Val.is_undeclared v) ->
        Some
          Error_message.(
            EBindingError (ENameAlreadyBound, assignment_loc, OrdinaryName name, def_loc)
          )
      | (Bindings.Parameter, (LetBinding | ConstBinding | FunctionBinding))
        when not (Val.is_undeclared v) ->
        Some
          Error_message.(
            EBindingError (ENameAlreadyBound, assignment_loc, OrdinaryName name, def_loc)
          )
      | _ -> None)

  let module_scoped_vars = ["eval"; "arguments"]

  let initialize_globals ~is_lib exclude_syms unbound_names program_loc =
    (SMap.empty
    |> SSet.fold
         (fun name acc ->
           let global_val = Val.global name in
           let entry =
             {
               val_ref = ref global_val;
               havoc = global_val;
               writes_by_closure_provider_val = None;
               def_loc = None;
               heap_refinements = ref HeapRefinementMap.empty;
               kind = Bindings.Var;
             }
           in
           SMap.add name entry acc)
         unbound_names
    |> NameUtils.Set.fold
         (fun name acc ->
           match name with
           | OrdinaryName name ->
             let global_val = Val.global name in
             let entry =
               {
                 val_ref = ref global_val;
                 havoc = global_val;
                 writes_by_closure_provider_val = None;
                 def_loc = None;
                 heap_refinements = ref HeapRefinementMap.empty;
                 kind = Bindings.Var;
               }
             in
             SMap.add name entry acc
           | InternalName _
           | InternalModuleName _ ->
             acc)
         exclude_syms
    (* this has to come later, since this can be thought to be unbound names in SSA builder when it's used as a type. *)
    |> (let v = program_loc |> mk_reason (RCustom "global object") |> Val.global_this in
        SMap.add
          "this"
          {
            val_ref = ref v;
            havoc = v;
            writes_by_closure_provider_val = None;
            def_loc = None;
            heap_refinements = ref HeapRefinementMap.empty;
            kind = Bindings.Var;
          }
       )
    |> ( if not is_lib then
         SMap.add
           "exports"
           {
             val_ref = ref Val.exports;
             havoc = Val.exports;
             writes_by_closure_provider_val = None;
             def_loc = None;
             heap_refinements = ref HeapRefinementMap.empty;
             kind = Bindings.Var;
           }
       else
         Base.Fn.id
       )
    |>
    if not is_lib then
      SMap.add
        "module"
        {
          val_ref = ref (Val.global "module");
          havoc = Val.global "module";
          writes_by_closure_provider_val = None;
          def_loc = None;
          heap_refinements =
            ref (HeapRefinementMap.singleton [RefinementKey.Prop "exports"] Val.exports);
          kind = Bindings.Var;
        }
    else
      Base.Fn.id
    )
    |> fun init ->
    Base.List.fold
      ~init
      ~f:(fun acc name ->
        let module_scoped_val = Val.module_scoped name in
        let entry =
          {
            val_ref = ref module_scoped_val;
            havoc = module_scoped_val;
            writes_by_closure_provider_val = None;
            def_loc = None;
            heap_refinements = ref HeapRefinementMap.empty;
            kind = Bindings.Var;
          }
        in
        SMap.add name entry acc)
      module_scoped_vars

  (* Statement.ml tries to extract the name and traverse at the location of the
   * jsx element if it's an identifier, otherwise it just traverses the
   * jsx_pragma expression *)
  let extract_jsx_basename =
    let open Flow_ast.Expression in
    function
    | (_, Identifier (_, { Flow_ast.Identifier.name; _ })) -> Some name
    | _ -> None

  let initial_env cx ~is_lib exclude_syms unbound_names program_loc =
    let globals = initialize_globals ~is_lib exclude_syms unbound_names program_loc in
    let globals =
      let exhaustive_entry =
        {
          val_ref = ref (Val.undeclared maybe_exhaustively_checked_var_name L.none);
          havoc = Val.undeclared maybe_exhaustively_checked_var_name L.none;
          writes_by_closure_provider_val = None;
          def_loc = None;
          heap_refinements = ref HeapRefinementMap.empty;
          kind = Bindings.Internal;
        }
      in
      SMap.add maybe_exhaustively_checked_var_name exhaustive_entry globals
    in
    (* We need to make sure that the base name for jsx is always in scope.
     * statement.ml is going to read these identifiers at jsx calls, even if
     * they haven't been declared locally. *)
    let jsx_base_name =
      match Context.jsx cx with
      | Options.Jsx_react -> Some "React"
      | Options.Jsx_pragma (_, ast) -> extract_jsx_basename ast
    in
    let globals = SMap.map Nel.one globals in
    match jsx_base_name with
    | None -> (globals, None)
    | Some jsx_base_name ->
      (* We use a global here so that if the base name is never created locally
       * we first check the globals before emitting an error *)
      let global_val = Val.global jsx_base_name in
      let entry =
        ( {
            val_ref = ref global_val;
            havoc = global_val;
            writes_by_closure_provider_val = None;
            def_loc = None;
            heap_refinements = ref HeapRefinementMap.empty;
            kind = Bindings.Var;
          },
          []
        )
      in
      (SMap.add jsx_base_name entry globals, Some jsx_base_name)

  let conj_total t1 t2 =
    match (t1, t2) with
    | (Some t1, Some t2) -> Some (And (t1, t2))
    | (Some t, _)
    | (_, Some t) ->
      Some t
    | (None, None) -> None

  let empty_refinements = { applied = IMap.empty; changeset = LookupMap.empty; total = None }

  class name_resolver
    cx is_lib exclude_syms (prepass_info, prepass_values, unbound_names) provider_info program_loc =
    let add_output = FlowAPIUtils.add_output cx in

    let rec add_literal_subtype_test refinee_loc literal =
      match literal with
      | SingletonNumR { loc; lit = (num, raw); sense } ->
        Context.add_literal_subtypes
          cx
          (refinee_loc, PostInferenceCheck.SingletonNum (loc, sense, num, raw))
      | SingletonBoolR { loc; lit; sense = _ } ->
        Context.add_literal_subtypes cx (refinee_loc, PostInferenceCheck.SingletonBool (loc, lit))
      | SingletonStrR { loc; lit; sense } ->
        Context.add_literal_subtypes
          cx
          (refinee_loc, PostInferenceCheck.SingletonStr (loc, sense, lit))
      | NotR r -> add_literal_subtype_test refinee_loc r
      | AndR (r1, r2)
      | OrR (r1, r2) ->
        add_literal_subtype_test refinee_loc r1;
        add_literal_subtype_test refinee_loc r2
      | _ -> ()
    in

    let valid_declaration_check name loc =
      let error null_write =
        let null_write =
          Base.Option.map
            ~f:(fun null_loc -> Error_message.{ null_loc; initialized = ALoc.equal loc null_loc })
            null_write
        in
        add_output
          Error_message.(
            EInvalidDeclaration { declaration = mk_reason (RIdentifier name) loc; null_write }
          )
      in
      match Invalidation_api.declaration_validity prepass_info prepass_values provider_info loc with
      | Invalidation_api.Valid -> ()
      | Invalidation_api.NotWritten -> error None
      | Invalidation_api.NullWritten null_loc -> error (Some null_loc)
    in

    let is_def_loc_predicate_function loc =
      let providers = Env_api.Provider_api.providers_of_def provider_info loc in
      match providers with
      | Some { Env_api.Provider_api.state = Find_providers.AnnotatedVar { predicate; _ }; _ } ->
        predicate
      | _ -> false
    in

    (* Sanity check for predicate functions: If there are multiple declare function
     * providers, make sure none of them have a predicate. *)
    let check_predicate_declare_function ~predicate name loc =
      match Env_api.Provider_api.providers_of_def provider_info loc with
      (* This check is only relevant when there are multiple providers *)
      | Some
          {
            Env_api.Provider_api.providers = { Env_api.Provider_api.reason = def_reason; _ } :: _;
            _;
          } ->
        let def_loc = Reason.aloc_of_reason def_reason in
        let def_loc_is_pred = is_def_loc_predicate_function def_loc in
        (* Raise an error for an overload (other than the first one) if:
         * - The first overload is a predicate function (`def_loc_is_pred`), or
         * - The current overload is a predicate function (`predicate`). *)
        if def_loc <> loc && (predicate || def_loc_is_pred) then
          add_output
            (Error_message.EBindingError (Error_message.ENameAlreadyBound, loc, name, def_loc))
      | _ -> ()
    in

    let enable_enums = Context.enable_enums cx in
    object (this)
      inherit
        Scope_builder.scope_builder ~flowmin_compatibility:false ~enable_enums ~with_types:true as super

      method private debug_val = Val.debug_to_string this#refinement_of_id

      val invalidation_caches = Invalidation_api.mk_caches ()

      val mutable env_state : name_resolver_state =
        let (env, jsx_base_name) = initial_env cx ~is_lib exclude_syms unbound_names program_loc in
        let write_entries =
          if is_lib then
            EnvMap.empty
          else
            let filename = Context.file cx in
            let global_exports_loc = Loc.{ none with source = Some filename } |> ALoc.of_loc in
            EnvMap.singleton
              (Env_api.GlobalExportsLoc, global_exports_loc)
              (Env_api.AssigningWrite
                 (mk_reason
                    (RModule (OrdinaryName (File_key.to_string filename)))
                    global_exports_loc
                 )
              )
        in
        {
          values = L.LMap.empty;
          write_entries;
          toplevel_members = [];
          module_toplevel_members = L.LMap.empty;
          predicate_refinement_maps = L.LMap.empty;
          curr_id = 0;
          refinement_heap = IMap.empty;
          latest_refinements = [];
          env;
          exclude_syms;
          abrupt_completion_envs = [];
          possible_labeled_continues = [];
          predicate_scope_names = None;
          visiting_hoisted_type = false;
          jsx_base_name;
        }

      method jsx_base_name = env_state.jsx_base_name

      method values : Env_api.values =
        L.LMap.map
          (fun { def_loc; value; binding_kind_opt; name } ->
            Val.simplify def_loc binding_kind_opt name value)
          env_state.values

      method write_entries : Env_api.env_entry EnvMap.t = env_state.write_entries

      method toplevel_members = env_state.toplevel_members

      method module_toplevel_members = env_state.module_toplevel_members

      method predicate_refinement_maps = env_state.predicate_refinement_maps

      method private is_assigning_write key =
        match EnvMap.find_opt key env_state.write_entries with
        | Some (Env_api.AssigningWrite _) -> true
        | Some (Env_api.GlobalWrite _) -> true
        | Some Env_api.NonAssigningWrite -> false
        | None -> false

      method private merge_vals_with_havoc ~havoc ~def_loc v1 v2 =
        (* It's not safe to reset to havoc when one side can be uninitialized, because the havoc
           val is built from providers which does not include the initial uninitialized state *)
        let can_merge_with_havoc v_havoc v_other =
          Val.id_of_val v_havoc = Val.id_of_val havoc
          && Base.List.is_empty
             @@ Val.writes_of_uninitialized this#refinement_may_be_undefined v_other
        in
        match def_loc with
        | Some loc ->
          (match Env_api.Provider_api.providers_of_def provider_info loc with
          (* We only use havoc as the merge if the providers that generate the havoc val are
             annotated. Only when the havoc val is annotated, we know for certain that it wouldn't
             be widened in the branches we are merging together. *)
          | Some { Env_api.Provider_api.state = Find_providers.AnnotatedVar _; _ }
            when can_merge_with_havoc v1 v2 || can_merge_with_havoc v2 v1 ->
            havoc
          | _ -> Val.merge v1 v2)
        | None -> Val.merge v1 v2

      method private is_excluded name = NameUtils.Set.mem name env_state.exclude_syms

      method private is_excluded_ordinary_name name = this#is_excluded (OrdinaryName name)

      method private new_id () =
        let new_id = env_state.curr_id in
        let curr_id = new_id + 1 in
        env_state <- { env_state with curr_id };
        new_id

      method env_read x =
        match SMap.find_opt x env_state.env with
        | Some (hd, _) -> hd
        | None -> raise Env_api.(Env_invariant (None, MissingEnvEntry x))

      method env_read_opt x = SMap.find_opt x env_state.env |> Base.Option.map ~f:Nel.hd

      method env : Env.t =
        SMap.map
          (fun ({ val_ref; heap_refinements; def_loc; _ }, _) ->
            { Env.env_val = !val_ref; heap_refinements = !heap_refinements; def_loc })
          env_state.env

      (* We often want to merge the refinement scopes and writes of two environments with
       * different strategies, especially in logical refinement scopes. In order to do that, we
       * need to be able to get the writes in our env without the refinement writes. Then we
       * can merge the refinements from two environments using either AND or OR, and then we can
       * merge the writes and reapply the merged refinement if the ssa_id in unchanged.
       *
       * An alternative implementation here might have just used PHI nodes to model disjunctions
       * and successive refinement writes to model conjunctions, but it's not clear that that
       * approach is simpler than this one. *)
      method env_without_latest_refinements : Env.t =
        let refinements_by_key { applied; _ } =
          IMap.fold
            (fun _ (lookup_key, refinement_id) ->
              LookupMap.update lookup_key (function
                  | Some map -> Some (ISet.add refinement_id map)
                  | None -> Some (ISet.singleton refinement_id)
                  ))
            applied
            LookupMap.empty
        in
        let unrefine refinements_by_key lookup_key v =
          LookupMap.find_opt lookup_key refinements_by_key
          |> Base.Option.value_map
               ~f:(fun refinement_ids -> ISet.fold Val.unrefine refinement_ids v)
               ~default:v
        in
        SMap.mapi
          (fun name ({ val_ref; heap_refinements; def_loc; _ }, _) ->
            let head = List.hd env_state.latest_refinements in
            let refinements_by_key = refinements_by_key head in
            let lookup_key = RefinementKey.lookup_of_name name in
            let env_val = unrefine refinements_by_key lookup_key !val_ref in
            let unrefined_heap_refinements =
              HeapRefinementMap.mapi
                (fun projections v ->
                  let lookup_key = RefinementKey.lookup_of_name_with_projections name projections in
                  unrefine refinements_by_key lookup_key v)
                !heap_refinements
            in
            { Env.env_val; heap_refinements = unrefined_heap_refinements; def_loc })
          env_state.env

      method merge_heap_refinements =
        (* When we merge the heap refinements from two branches we cannot include
         * keys that did not appear on both sides. Take this example:
         * let obj = {};
         * if (true) {
         *   obj.foo = 3;
         * } else {
         *   obj.bar = 4;
         * }
         * (obj.foo: 3); // Should fail because the else branch does not add this refinement
         *)
        HeapRefinementMap.merge (fun _ refinement1 refinement2 ->
            match (refinement1, refinement2) with
            | (Some v1, Some v2) -> Some (Val.merge v1 v2)
            | _ -> None
        )

      method merge_remote_env (env : Env.t) : unit =
        (* NOTE: env might have more keys than env_state.env, since the environment it
           describes might be nested inside the current environment *)
        SMap.iter
          (fun x
               ({ val_ref; havoc; heap_refinements = heap_refinements1; def_loc = def_loc_1; _ }, _) ->
            let { Env.env_val; heap_refinements = heap_refinements2; def_loc = def_loc_2 } =
              smap_find x env
            in
            if def_loc_1 = def_loc_2 then (
              val_ref := this#merge_vals_with_havoc ~havoc ~def_loc:def_loc_1 !val_ref env_val;
              heap_refinements1 := this#merge_heap_refinements !heap_refinements1 heap_refinements2
            ))
          env_state.env

      method merge_env (env1 : Env.t) (env2 : Env.t) : unit =
        let env1 = SMap.values env1 in
        let env2 = SMap.values env2 in
        let env = SMap.values env_state.env in
        list_iter3
          (fun ({ val_ref; havoc; heap_refinements; def_loc; _ }, _)
               { Env.env_val = value1; heap_refinements = heap_refinements1; def_loc = _ }
               { Env.env_val = value2; heap_refinements = heap_refinements2; def_loc = _ } ->
            val_ref := this#merge_vals_with_havoc ~havoc ~def_loc value1 value2;
            heap_refinements := this#merge_heap_refinements heap_refinements1 heap_refinements2)
          env
          env1
          env2

      method merge_self_env (other_env : Env.t) : unit =
        let other_env = SMap.values other_env in
        let env = SMap.values env_state.env in
        List.iter2
          (fun ({ val_ref; havoc; heap_refinements; def_loc; _ }, _)
               { Env.env_val = value; heap_refinements = new_heap_refinements; def_loc = _ } ->
            val_ref := this#merge_vals_with_havoc ~havoc ~def_loc !val_ref value;
            heap_refinements := this#merge_heap_refinements !heap_refinements new_heap_refinements)
          env
          other_env

      method reset_env (env0 : Env.t) : unit =
        let env0 = SMap.values env0 in
        let env = SMap.values env_state.env in
        List.iter2
          (fun ({ val_ref; heap_refinements; _ }, _)
               { Env.env_val; heap_refinements = old_heap_refinements; def_loc = _ } ->
            val_ref := env_val;
            heap_refinements := old_heap_refinements)
          env
          env0

      method empty_env : Env.t =
        SMap.map
          (fun _ ->
            {
              Env.env_val = Val.empty ();
              heap_refinements = HeapRefinementMap.empty;
              (* The empty env is always used as a reset value,
                 and the reset_env method (see above) does not mutate def_loc at all.
                 Therefore, the value of def_loc here does not matter. *)
              def_loc = None;
            })
          env_state.env

      (* This method applies a function over the value stored with a refinement key. It is
       * mostly just a convenient helper so that the process of deconstructing the
       * key and finding the appropriate Val.t does not have to be repeated in
       * every method that needs to update an entry. The create_val_for_heap argument can
       * be used to specify what value to apply the function to if the heap entry
       * does not yet exist.

       * In addition to updating the Val.t, if the callback Val.t updating function
       * is called (i.e. if we find a value to update), we'll return any additional
       * data that the function itself returns (as the first element of its return
       * type tuple). In addition, if we end up creating a new val.t as part of a heap
       * refinement, we'll return that val.t as well. In cases where this information
       * isn't necessary, callers can call `map_val_with_lookup` below instead.
       *)
      method map_val_with_lookup_result
          : 'r.
            RefinementKey.lookup ->
            ?create_val_for_heap:Val.t Lazy.t ->
            (Val.t -> 'r * Val.t) ->
            ('r * Val.t option) option =
        fun lookup ?create_val_for_heap f ->
          let { RefinementKey.base; projections } = lookup in
          match this#env_read_opt base with
          | None -> None
          | Some
              {
                val_ref;
                heap_refinements;
                writes_by_closure_provider_val = _;
                havoc = _;
                def_loc = _;
                kind = _;
              } ->
            (match projections with
            | [] ->
              let (res, val_) = f !val_ref in
              val_ref := val_;
              Some (res, None)
            | _ ->
              let (res, new_heap_refinements) =
                match
                  (HeapRefinementMap.find_opt projections !heap_refinements, create_val_for_heap)
                with
                | (Some heap_val, _) ->
                  let (res, val_) = f heap_val in
                  (Some (res, None), HeapRefinementMap.add projections val_ !heap_refinements)
                | (None, Some (lazy default)) ->
                  let (res, val_) = f default in
                  ( Some (res, Some default),
                    HeapRefinementMap.add projections val_ !heap_refinements
                  )
                | (None, None) -> (None, !heap_refinements)
              in
              heap_refinements := new_heap_refinements;
              res)

      method map_val_with_lookup lookup ?create_val_for_heap f =
        let (_ : (unit * Val.t option) option) =
          this#map_val_with_lookup_result lookup ?create_val_for_heap (fun v -> ((), f v))
        in
        ()

      method get_val_of_expression expr =
        match RefinementKey.of_expression expr with
        | None -> None
        | Some { RefinementKey.loc = _; lookup = { RefinementKey.base; projections } } ->
          (match SMap.find_opt base this#env with
          | None -> None
          | Some { Env.env_val; heap_refinements; def_loc = _ } ->
            (match projections with
            | [] -> Some env_val
            | _ ->
              heap_refinements
              |> HeapRefinementMap.find_opt projections
              |> Base.Option.filter ~f:(fun v -> not @@ Val.is_projection v)))

      (* Function calls may introduce refinements if the function called is a
       * predicate function. The EnvBuilder has no idea if a function is a
       * predicate function or not. To handle that, we encode that a variable
       * _might_ be havoced by a function call if that variable is passed
       * as an argument. Variables not passed into the function are havoced if
       * the invalidation api says they can be invalidated.
       *)
      method apply_latent_refinements ((callee_loc, _) as callee) refinement_keys_by_arg =
        List.iteri
          (fun index -> function
            | None -> ()
            | Some key ->
              this#add_single_refinement
                key
                (L.LSet.singleton callee_loc, LatentR { func = callee; index = index + 1 }))
          refinement_keys_by_arg

      method havoc_heap_refinements heap_refinements = heap_refinements := HeapRefinementMap.empty

      method havoc_all_heap_refinements () =
        SMap.iter
          (fun _ elts ->
            Nel.iter
              (fun { heap_refinements; _ } -> this#havoc_heap_refinements heap_refinements)
              elts)
          env_state.env

      method havoc_env ~all ~deep =
        let force_initialization = not deep in
        let havoced_ids =
          SMap.fold
            (fun _x elts acc ->
              let elts =
                if deep then
                  elts
                else
                  Nel.hd elts |> Nel.one
              in
              Nel.fold_left
                (fun acc elt ->
                  match elt with
                  | { kind = Bindings.Internal; _ } -> acc
                  | {
                   val_ref;
                   havoc;
                   writes_by_closure_provider_val;
                   def_loc;
                   heap_refinements;
                   kind = _;
                  } ->
                    this#havoc_heap_refinements heap_refinements;
                    let uninitialized_writes =
                      lazy (Val.writes_of_uninitialized this#refinement_may_be_undefined !val_ref)
                    in
                    let val_is_undeclared_or_skipped = Val.is_undeclared_or_skipped !val_ref in
                    let havoc_ref =
                      if force_initialization then
                        havoc
                      else if val_is_undeclared_or_skipped then
                        !val_ref
                      else
                        let havoc =
                          match (all, writes_by_closure_provider_val) with
                          | (false, Some writes_by_closure_provider_val) ->
                            Val.merge !val_ref writes_by_closure_provider_val
                          | _ -> havoc
                        in
                        Base.List.fold
                          ~init:havoc
                          ~f:(fun acc write -> Val.merge acc (Val.of_write write))
                          (Lazy.force uninitialized_writes)
                    in
                    if
                      Base.Option.is_none def_loc
                      || Invalidation_api.should_invalidate
                           ~all
                           invalidation_caches
                           prepass_info
                           prepass_values
                           (Base.Option.value_exn def_loc (* checked against none above *))
                      || force_initialization
                         && (List.length (Lazy.force uninitialized_writes) > 0
                            || val_is_undeclared_or_skipped
                            )
                    then begin
                      val_ref := havoc_ref;
                      ISet.add (Val.base_id_of_val havoc_ref) acc
                    end else
                      acc)
                acc
                elts)
            env_state.env
            ISet.empty
        in
        let latest_refinements =
          Base.List.map
            ~f:(fun { applied; _ } ->
              let applied =
                IMap.filter
                  (fun ssa_id ({ RefinementKey.base = _; projections }, _) ->
                    Base.List.length projections = 0 && not (ISet.mem ssa_id havoced_ids))
                  applied
              in
              let total =
                if IMap.cardinal applied > 0 then
                  Some (Refinements (applied, LookupMap.empty))
                else
                  None
              in
              { applied; changeset = LookupMap.empty; total })
            env_state.latest_refinements
        in
        env_state <- { env_state with latest_refinements }

      method havoc_current_env ~all = this#havoc_env ~all ~deep:true

      method havoc_uninitialized_env = this#havoc_env ~all:true ~deep:false

      method refinement_may_be_undefined id =
        let rec refine_undefined = function
          | UndefinedR
          | MaybeR ->
            true
          | NotR r -> not @@ refine_undefined r
          | OrR (r1, r2) -> refine_undefined r1 || refine_undefined r2
          | AndR (r1, r2) -> refine_undefined r1 && refine_undefined r2
          | _ -> false
        in
        let (_, id) = this#refinement_of_id id in
        refine_undefined id

      method private providers_of_def_loc def_loc =
        let providers =
          Base.Option.value_map
            ~default:[]
            ~f:(fun { Provider_api.providers; _ } -> providers)
            (Provider_api.providers_of_def provider_info def_loc)
        in
        ( ( if Base.List.is_empty providers then
            Val.uninitialized def_loc
          else
            Val.providers providers
          ),
          providers
        )

      method private mk_env ~this_super_binding_env m =
        SMap.mapi
          (fun name (kind, (loc, _)) ->
            match kind with
            | Bindings.Type _ ->
              let reason = mk_reason (RType (OrdinaryName name)) loc in
              let write_entries =
                EnvMap.add_ordinary loc (Env_api.AssigningWrite reason) env_state.write_entries
              in
              env_state <- { env_state with write_entries };
              {
                val_ref = ref (Val.one reason);
                havoc = Val.one reason;
                writes_by_closure_provider_val = None;
                def_loc = Some loc;
                heap_refinements = ref HeapRefinementMap.empty;
                kind;
              }
            | Bindings.ThisAnnot ->
              let reason = mk_reason RThis loc in
              let write_entries =
                EnvMap.add_ordinary loc (Env_api.AssigningWrite reason) env_state.write_entries
              in
              env_state <- { env_state with write_entries };
              {
                val_ref = ref (Val.one reason);
                havoc = Val.one reason;
                writes_by_closure_provider_val = None;
                def_loc = Some loc;
                heap_refinements = ref HeapRefinementMap.empty;
                kind;
              }
            | Bindings.DeclaredClass ->
              let reason = mk_reason (RIdentifier (OrdinaryName name)) loc in
              let write_entries =
                EnvMap.add_ordinary loc (Env_api.AssigningWrite reason) env_state.write_entries
              in
              env_state <- { env_state with write_entries };
              {
                val_ref = ref (Val.one reason);
                havoc = Val.one reason;
                writes_by_closure_provider_val = None;
                def_loc = Some loc;
                heap_refinements = ref HeapRefinementMap.empty;
                kind;
              }
            | Bindings.(Class | Enum) ->
              let (havoc, providers) = this#providers_of_def_loc loc in
              let write_entries =
                Base.List.fold
                  ~f:(fun acc { Provider_api.reason = r; _ } ->
                    EnvMap.add_ordinary (poly_loc_of_reason r) (Env_api.AssigningWrite r) acc)
                  ~init:env_state.write_entries
                  providers
              in
              env_state <- { env_state with write_entries };
              {
                val_ref = ref (Val.undeclared name loc);
                havoc;
                writes_by_closure_provider_val = None;
                def_loc = Some loc;
                heap_refinements = ref HeapRefinementMap.empty;
                kind;
              }
            | Bindings.DeclaredFunction _ ->
              let (_, providers) = this#providers_of_def_loc loc in
              let write_entries =
                Base.List.fold
                  ~f:(fun acc { Provider_api.reason = r; _ } ->
                    EnvMap.add_ordinary (poly_loc_of_reason r) (Env_api.AssigningWrite r) acc)
                  ~init:env_state.write_entries
                  providers
              in
              env_state <- { env_state with write_entries };
              let declared_function = Val.declared_function loc in
              {
                val_ref = ref declared_function;
                havoc = declared_function;
                writes_by_closure_provider_val = None;
                def_loc = Some loc;
                heap_refinements = ref HeapRefinementMap.empty;
                kind;
              }
            | Bindings.Import ->
              let reason = mk_reason (RIdentifier (OrdinaryName name)) loc in
              {
                val_ref = ref (Val.undeclared name loc);
                havoc = Val.one reason;
                writes_by_closure_provider_val = None;
                def_loc = Some loc;
                heap_refinements = ref HeapRefinementMap.empty;
                kind;
              }
            | Bindings.Internal ->
              {
                val_ref = ref (Val.undeclared name loc);
                havoc = Val.undeclared name loc;
                writes_by_closure_provider_val = None;
                def_loc = None;
                heap_refinements = ref HeapRefinementMap.empty;
                kind;
              }
            | Bindings.GeneratorNext ->
              let reason = mk_reason (RCustom "next") loc in
              let write_entries =
                EnvMap.add_ordinary loc (Env_api.AssigningWrite reason) env_state.write_entries
              in
              env_state <- { env_state with write_entries };
              {
                val_ref = ref (Val.one reason);
                havoc = Val.one reason;
                writes_by_closure_provider_val = None;
                def_loc = Some loc;
                heap_refinements = ref HeapRefinementMap.empty;
                kind;
              }
            | _ ->
              let (initial_val, havoc, writes_by_closure_provider_val) =
                match name with
                | "this" ->
                  let reason = mk_reason (RIdentifier (internal_name "this")) loc in
                  let v =
                    match this_super_binding_env with
                    | FunctionEnv -> Val.function_this reason
                    | ClassStaticEnv -> Val.class_static_this reason
                    | ClassInstanceEnv -> Val.class_instance_this reason
                    | IllegalThisEnv -> Val.illegal_this reason
                  in
                  (v, v, None)
                | "super" ->
                  let reason = mk_reason (RIdentifier (internal_name "super")) loc in
                  let v =
                    match this_super_binding_env with
                    | FunctionEnv ->
                      raise
                        Env_api.(
                          Env_invariant (Some loc, Impossible "Cannot bind super in function env")
                        )
                    | ClassStaticEnv -> Val.class_static_super reason
                    | ClassInstanceEnv -> Val.class_instance_super reason
                    | IllegalThisEnv ->
                      raise
                        Env_api.(
                          Env_invariant
                            ( Some loc,
                              Impossible "It's impossible to bind super under IllegalThisEnv"
                            )
                        )
                  in
                  (v, v, None)
                | _ ->
                  let initial_val =
                    match kind with
                    (* let/const/enum all introduce errors if you try to access or assign them
                     * before syntactically encountering the declaration. All other bindings
                     * do not, so we don't set them to be undeclared *)
                    | Bindings.Let
                    | Bindings.Const
                    | Bindings.Enum
                    | Bindings.Parameter
                    | Bindings.Function ->
                      Val.undeclared name loc
                    | _ -> Val.uninitialized loc
                  in
                  let (havoc, providers) = this#providers_of_def_loc loc in
                  let writes_by_closure =
                    Invalidation_api.written_by_closure prepass_info prepass_values loc
                  in
                  let all_writes_by_closure_are_providers =
                    L.LSet.for_all
                      (Base.List.mem
                         ~equal:ALoc.equal
                         (Base.List.map
                            ~f:(fun { Provider_api.reason; _ } -> poly_loc_of_reason reason)
                            providers
                         )
                      )
                      writes_by_closure
                  in
                  (* Special-cased havoc val of variables whose only closure writes are providers *)
                  let writes_by_closure_provider_val =
                    if all_writes_by_closure_are_providers then
                      let writes_by_closure_providers =
                        Base.List.filter providers ~f:(fun { Provider_api.reason; _ } ->
                            L.LSet.mem (poly_loc_of_reason reason) writes_by_closure
                        )
                      in
                      if not @@ Base.List.is_empty writes_by_closure_providers then
                        (* Now we know a variable needs to be widened at most by these writes
                           by closure providers *)
                        Some (Val.providers writes_by_closure_providers)
                      else
                        None
                    else
                      None
                  in
                  (initial_val, havoc, writes_by_closure_provider_val)
              in
              {
                val_ref = ref initial_val;
                havoc;
                writes_by_closure_provider_val;
                def_loc = Some loc;
                heap_refinements = ref HeapRefinementMap.empty;
                kind;
              })
          m
        |> SMap.map Nel.one

      method private push_env ~this_super_binding_env bindings =
        let old_env = env_state.env in
        let bindings = Bindings.to_map bindings in
        let bindings =
          SMap.filter (fun name _ -> not @@ this#is_excluded_ordinary_name name) bindings
        in
        let env =
          SMap.fold
            (fun x v ->
              SMap.adjust x (function
                  | None -> v
                  | Some y -> Nel.append v y
                  ))
            (this#mk_env ~this_super_binding_env bindings)
            old_env
        in
        env_state <- { env_state with env };
        (bindings, old_env)

      method private pop_env (_, old_env) = env_state <- { env_state with env = old_env }

      method private with_scoped_bindings
          : 'a.
            ?lexical:bool ->
            this_super_binding_env:this_super_binding_env ->
            ALoc.t ->
            ALoc.t Bindings.t ->
            ('a -> 'a) ->
            'a ->
            'a =
        fun ?lexical ~this_super_binding_env loc bindings visit node ->
          let saved_state = this#push_env ~this_super_binding_env bindings in
          this#run
            (fun () -> ignore @@ super#with_bindings ?lexical loc bindings visit node)
            ~finally:(fun () -> this#pop_env saved_state);
          node

      method! with_bindings
          : 'a. ?lexical:bool -> ALoc.t -> ALoc.t Bindings.t -> ('a -> 'a) -> 'a -> 'a =
        this#with_scoped_bindings ~this_super_binding_env:FunctionEnv

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

      method raise_abrupt_completion : 'a. AbruptCompletion.t -> 'a =
        fun abrupt_completion ->
          let env = this#env in
          this#reset_env this#empty_env;
          let abrupt_completion_envs =
            (abrupt_completion, env) :: env_state.abrupt_completion_envs
          in
          env_state <- { env_state with abrupt_completion_envs };
          raise (AbruptCompletion.Exn abrupt_completion)

      method expecting_abrupt_completions f =
        let saved = env_state.abrupt_completion_envs in
        let saved_latest_refinements = env_state.latest_refinements in
        env_state <- { env_state with abrupt_completion_envs = [] };
        this#run f ~finally:(fun () ->
            let abrupt_completion_envs = List.rev_append saved env_state.abrupt_completion_envs in
            env_state <-
              {
                env_state with
                abrupt_completion_envs;
                latest_refinements = saved_latest_refinements;
              }
        )

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
            env_state.abrupt_completion_envs
        in
        if matching <> [] then (
          List.iter (fun (_abrupt_completion, env) -> this#merge_remote_env env) matching;
          env_state <- { env_state with abrupt_completion_envs = non_matching }
        ) else
          match completion_state with
          | Some abrupt_completion when not (filter abrupt_completion) ->
            raise (AbruptCompletion.Exn abrupt_completion)
          | _ -> ()

      method! binding_type_identifier ident =
        let (loc, { Flow_ast.Identifier.name; comments = _ }) = ident in
        ( if this#is_excluded_ordinary_name name then
          ()
        else
          let { kind; def_loc; _ } = this#env_read name in
          let error =
            match def_loc with
            (* Identifiers with no binding can never reintroduce "cannot reassign binding" errors *)
            | None -> None
            | Some def_loc ->
              (match kind with
              | Bindings.Type _ when not (ALoc.equal loc def_loc) ->
                (* Types are already bind in hoister,
                   so we only check for rebind in different locations. *)
                Some
                  Error_message.(EBindingError (ENameAlreadyBound, loc, OrdinaryName name, def_loc))
              | Bindings.Type _ -> None
              | Bindings.Var
              | Bindings.Const
              | Bindings.Let
              | Bindings.Class
              | Bindings.Function
              | Bindings.Parameter
              | Bindings.Import ->
                Some
                  Error_message.(EBindingError (ENameAlreadyBound, loc, OrdinaryName name, def_loc))
              | _ -> None)
          in
          Base.Option.iter error ~f:(fun error ->
              add_output error;
              let write_entries =
                EnvMap.add_ordinary loc Env_api.NonAssigningWrite env_state.write_entries
              in
              env_state <- { env_state with write_entries }
          )
        );
        super#identifier ident

      method! function_identifier ident =
        (* The parent flow_ast_mapper treats functions as Vars, but in Flow
         * (not JS, Flow) they have special behavior with functions. *)
        let (loc, { Flow_ast.Identifier.name = x; comments = _ }) = ident in
        this#bind_pattern_identifier_customized ~kind:FunctionBinding loc x;
        super#identifier ident

      (* We want to translate object pattern destructing {a:{b:{c}}} = o into o.a.b.c,
         so the use of refinement can be recorded as a write.
         We use acc to keep track of the current parent expr *)
      method private binding_pattern_track_object_destructuring ?kind ~acc expr =
        let open Ast.Pattern in
        let (ploc, patt) = expr in
        (match patt with
        | Object { Object.properties; annot; comments = _ } ->
          let write_entries =
            EnvMap.add
              (Env_api.PatternLoc, ploc)
              (Env_api.AssigningWrite (mk_reason RDestructuring ploc))
              env_state.write_entries
          in
          env_state <- { env_state with write_entries };
          Base.List.iter properties ~f:(fun prop ->
              let open Ast.Pattern.Object in
              match prop with
              | RestElement prop -> ignore @@ this#pattern_object_rest_property ?kind prop
              | Property ((_, { Property.key; pattern; default; shorthand = _ }) as prop) ->
                (match key with
                | Property.Identifier (loc, { Ast.Identifier.name = x; comments = _ })
                | Property.Literal (loc, { Ast.Literal.value = Ast.Literal.String x; _ }) ->
                  Base.Option.iter default ~f:(fun default -> ignore @@ this#expression default);
                  let acc =
                    let open Ast.Expression in
                    let property =
                      Member.PropertyIdentifier (loc, { Ast.Identifier.name = x; comments = None })
                    in
                    (loc, Member { Member._object = acc; property; comments = None })
                  in
                  (match pattern with
                  | ( _,
                      Identifier
                        {
                          Identifier.name = (loc, { Ast.Identifier.name = x; comments = _ });
                          annot;
                          optional = _;
                        }
                    ) ->
                    ignore @@ this#recursively_record_member_read acc;
                    (* Leaf of the object pattern *)
                    (match this#get_val_of_expression acc with
                    | None -> ignore @@ this#pattern ?kind pattern
                    | Some refined_v ->
                      ignore @@ this#type_annotation_hint annot;
                      this#bind_pattern_identifier_customized
                        ~kind:(variable_declaration_binding_kind_to_pattern_write_kind kind)
                        ~get_assigned_val:(fun reason ->
                          Val.replace_refinement_base_write ~base:(Val.one reason) refined_v)
                        loc
                        x)
                  | _ ->
                    ignore @@ this#binding_pattern_track_object_destructuring ?kind ~acc pattern)
                | _ -> ignore @@ this#pattern_object_property ?kind prop)
          );
          ignore @@ this#type_annotation_hint annot
        | Array _
        | Identifier _
        | Expression _ ->
          ignore @@ this#pattern ?kind expr);
        expr

      method private record_pattern_loc_writes expr =
        let (ploc, patt) = expr in
        let write_entries =
          EnvMap.add
            (Env_api.PatternLoc, ploc)
            (Env_api.AssigningWrite (mk_reason RDestructuring ploc))
            env_state.write_entries
        in
        env_state <- { env_state with write_entries };
        let open Ast.Pattern in
        match patt with
        | Array { Array.elements; _ } ->
          Base.List.iter elements ~f:(fun element ->
              let open Ast.Pattern.Array in
              match element with
              | Hole _ -> ()
              | Element (_, { Element.argument = p; _ })
              | RestElement (_, { RestElement.argument = p; _ }) ->
                this#record_pattern_loc_writes p
          )
        | Object { Object.properties; _ } ->
          Base.List.iter properties ~f:(fun prop ->
              let open Ast.Pattern.Object in
              match prop with
              | Property (_, { Property.pattern = p; _ })
              | RestElement (_, { RestElement.argument = p; _ }) ->
                this#record_pattern_loc_writes p
          )
        | Identifier _ -> ()
        | Expression _ -> ()

      method! pattern ?kind expr =
        let (ploc, _) = expr in
        let write_entries =
          EnvMap.add
            (Env_api.PatternLoc, ploc)
            (Env_api.AssigningWrite (mk_reason RDestructuring ploc))
            env_state.write_entries
        in
        env_state <- { env_state with write_entries };
        super#pattern ?kind expr

      method! pattern_identifier ?kind ident =
        let (loc, { Flow_ast.Identifier.name = x; comments = _ }) = ident in
        let kind = variable_declaration_binding_kind_to_pattern_write_kind kind in
        this#bind_pattern_identifier_customized ~kind loc x;
        super#identifier ident

      method! pattern_array_element ?kind (elem : ('loc, 'loc) Ast.Pattern.Array.Element.t) =
        let open Ast.Pattern.Array.Element in
        let (_, { argument; default }) = elem in
        (* Flip order compared to base class *)
        let _default' = Flow_ast_mapper.map_opt this#expression default in
        let _argument' = this#pattern_array_element_pattern ?kind argument in
        elem

      method private bind_pattern_identifier_customized ~kind ?(get_assigned_val = Val.one) loc x =
        if this#is_excluded_ordinary_name x then
          ()
        else
          let reason = Reason.(mk_reason (RIdentifier (OrdinaryName x))) loc in
          let {
            val_ref;
            heap_refinements;
            kind = stored_binding_kind;
            def_loc;
            havoc = _;
            writes_by_closure_provider_val = _;
          } =
            this#env_read x
          in
          match kind with
          (* Assignments to undeclared bindings that aren't part of declarations do not
           * initialize those bindings. *)
          | AssignmentWrite when Val.is_undeclared_or_skipped !val_ref ->
            (match def_loc with
            | None ->
              raise
                Env_api.(
                  Env_invariant
                    ( Some loc,
                      Impossible "Cannot have an undeclared or skipped binding without a def loc"
                    )
                )
            | Some def_loc ->
              add_output
                Error_message.(
                  EBindingError (EReferencedBeforeDeclaration, loc, OrdinaryName x, def_loc)
                ))
          | _ ->
            (match error_for_assignment_kind cx x loc def_loc stored_binding_kind kind !val_ref with
            | Some err -> this#error_assignment loc reason stored_binding_kind kind err val_ref
            | _ ->
              this#havoc_heap_refinements heap_refinements;
              let current_val = !val_ref in
              if not (Val.is_declared_function current_val) then val_ref := get_assigned_val reason;
              let write_entries =
                let write_entry =
                  if Val.is_global current_val then
                    Env_api.GlobalWrite reason
                  else
                    Env_api.AssigningWrite reason
                in
                EnvMap.add_ordinary loc write_entry env_state.write_entries
              in
              env_state <- { env_state with write_entries })

      method error_assignment loc reason stored_binding_kind kind err val_ref =
        add_output err;
        let write_entries =
          EnvMap.add_ordinary loc Env_api.NonAssigningWrite env_state.write_entries
        in
        (* Give unsupported var redeclaration a write to avoid spurious errors like use of
            possibly undefined variable. Essentially, we are treating var redeclaration as a
            assignment with an any-typed value. *)
        (match (stored_binding_kind, kind) with
        | (Bindings.Var, VarBinding) -> val_ref := Val.illegal_write reason
        | _ -> ());
        env_state <- { env_state with write_entries }

      (* This method is called during every read of an identifier. We need to ensure that
       * if the identifier is refined that we record the refiner as the write that reaches
       * this read
       *
       * Note that we don't emit EBinding errors for referenced-before-declaration errors here.
       * That is because we may read an UndeclaredTypeAndValue from a type position and the
       * name_resolver doesn't keep track of whether we are in a type context or not.
       *
       * Instead of augmenting the name_resolver with those capabilities, we emit these errors
       * in the new_env, which does know if it's querying a value or a type.
       * *)
      method any_identifier loc name =
        let { val_ref; havoc; def_loc; kind; _ } = this#env_read name in
        let v =
          if env_state.visiting_hoisted_type then
            havoc
          else
            !val_ref
        in
        let values =
          L.LMap.add
            loc
            { def_loc; value = v; binding_kind_opt = Some kind; name = Some name }
            env_state.values
        in
        env_state <- { env_state with values }

      method! this_expression loc this_ =
        this#any_identifier loc "this";
        this_

      method! super_expression loc super_ =
        this#any_identifier loc "super";
        super_

      method! identifier (ident : (ALoc.t, ALoc.t) Ast.Identifier.t) =
        let (loc, { Ast.Identifier.name = x; comments = _ }) = ident in
        this#any_identifier loc x;
        super#identifier ident

      method! generic_identifier_type (git : ('loc, 'loc) Ast.Type.Generic.Identifier.t) =
        let open Ast.Type.Generic.Identifier in
        let rec loop git =
          match git with
          | Unqualified i -> ignore @@ this#type_identifier i
          | Qualified (_, { qualification; _ }) -> loop qualification
        in
        loop git;
        git

      method! jsx_element_name_identifier (ident : (ALoc.t, ALoc.t) Ast.JSX.Identifier.t) =
        let (loc, { Ast.JSX.Identifier.name; comments = _ }) = ident in
        this#any_identifier loc name;
        super#jsx_identifier ident

      method! jsx_element_name_namespaced ns =
        (* TODO: what identifiers does `<foo:bar />` read? *)
        super#jsx_element_name_namespaced ns

      method! jsx_member_expression jsx_mem_expr =
        let rec obj_to_expr = function
          | Ast.JSX.MemberExpression.Identifier (id_loc, { Ast.JSX.Identifier.name; comments }) ->
            (id_loc, Ast.Expression.Identifier (id_loc, { Ast.Identifier.name; comments }))
          | Ast.JSX.MemberExpression.MemberExpression
              ( loc,
                {
                  Ast.JSX.MemberExpression._object;
                  property = (prop_loc, { Ast.JSX.Identifier.name; comments });
                }
              ) ->
            ( loc,
              Ast.Expression.Member
                {
                  Ast.Expression.Member._object = obj_to_expr _object;
                  property =
                    Ast.Expression.Member.PropertyIdentifier
                      (prop_loc, { Ast.Identifier.name; comments });
                  comments = None;
                }
            )
        in
        Ast.JSX.MemberExpression.MemberExpression jsx_mem_expr
        |> obj_to_expr
        |> this#expression
        |> ignore;
        jsx_mem_expr

      method havoc_heap_refinements_using_name ~private_ name =
        SMap.iter
          (fun _ elts ->
            Nel.iter
              (fun { heap_refinements; _ } ->
                heap_refinements :=
                  HeapRefinementMap.filter
                    (fun projections _ ->
                      not (RefinementKey.proj_uses_propname ~private_ name projections))
                    !heap_refinements)
              elts)
          env_state.env

      (* This function should be called _after_ a member expression is assigned a value.
       * It havocs other heap refinements depending on the name of the member and then adds
       * a write to the heap refinement entry for that member expression *)
      method assign_expression lhs rhs =
        match lhs with
        | (loc, Flow_ast.Expression.Member member) ->
          (* Use super member to visit sub-expressions to avoid record a read of the member. *)
          ignore @@ super#member loc member;
          ignore @@ this#expression rhs;
          let reason = mk_reason RSomeProperty loc in
          let assigned_val = Val.one reason in
          this#assign_member ~delete:false member loc assigned_val reason;
          begin
            match rhs with
            | (fun_loc, Ast.Expression.ArrowFunction _) ->
              let reason = mk_reason (RCustom "<<arrow function>>") fun_loc in
              let write_entries =
                EnvMap.add_ordinary fun_loc (Env_api.AssigningWrite reason) env_state.write_entries
              in
              env_state <- { env_state with write_entries }
            | _ -> ()
          end
        | _ -> statement_error

      method assign_member ~delete lhs_member lhs_loc assigned_val val_reason =
        this#post_assignment_heap_refinement_havoc lhs_member;
        (* We pass allow_optional:false, but optional chains can't be in the LHS anyway. *)
        let lookup = RefinementKey.lookup_of_member lhs_member ~allow_optional:false in
        let open Flow_ast.Expression in
        match (lhs_member, lookup) with
        | ( {
              Member.property =
                ( Member.PropertyIdentifier _ | Member.PropertyPrivateName _
                | Member.PropertyExpression
                    (_, Ast.Expression.Literal { Ast.Literal.value = Ast.Literal.String _; _ })
                | Member.PropertyExpression
                    ( _,
                      Ast.Expression.Literal
                        { Ast.Literal.value = Ast.Literal.Number _; raw = _; comments = _ }
                    ) );
              _;
            },
            Some lookup
          ) ->
          this#map_val_with_lookup
            lookup
            (fun _ -> assigned_val)
            ~create_val_for_heap:(lazy assigned_val);
          if not delete then
            let write_entries =
              EnvMap.add_ordinary
                lhs_loc
                (Env_api.AssigningWrite val_reason)
                env_state.write_entries
            in
            env_state <- { env_state with write_entries }
        | _ -> ()

      (* This method is called after assigning a member expression but _before_ the refinement for
       * that assignment is recorded. *)
      method post_assignment_heap_refinement_havoc
          (lhs : (ALoc.t, ALoc.t) Flow_ast.Expression.Member.t) =
        let open Flow_ast.Expression in
        match lhs with
        | {
         Member._object;
         property = Member.PropertyPrivateName (_, { Flow_ast.PrivateName.name; _ });
         _;
        } ->
          (* Yes, we want to havoc using the PROPERTY name here. This is because we
           * do not do any alias tracking, so we want to have the following behavior:
           * let x = {};
           * let y = x;
           * x.foo = 3;
           * y.foo = 4;
           * (x.foo: 3) // MUST error!
           *)
          this#havoc_heap_refinements_using_name name ~private_:true
        | {
         Member._object;
         property = Member.PropertyIdentifier (_, { Flow_ast.Identifier.name; _ });
         _;
        } ->
          (* As in the previous case, we can't know if this object is aliased nor what property
           * is being written. We are forced to conservatively havoc ALL heap refinements in this
           * situation. *)
          this#havoc_heap_refinements_using_name name ~private_:false
        | { Member._object; property = Member.PropertyExpression _; _ } ->
          this#havoc_all_heap_refinements ()

      (* Order of evaluation matters *)
      method! assignment _loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Assignment.t) =
        let open Ast.Expression.Assignment in
        let { operator; left = (left_loc, _) as left; right; comments = _ } = expr in
        begin
          match operator with
          | None ->
            let open Ast.Pattern in
            begin
              match left with
              | (_, (Identifier _ | Object _ | Array _)) ->
                (* given `x = e`, read e then write x *)
                ignore @@ this#expression right;
                ignore @@ this#binding_pattern_track_object_destructuring ?kind:None ~acc:right left
              | (_, Expression e) ->
                (* given `o.x = e`, read o then read e *)
                this#assign_expression e right
            end
          | Some
              ( PlusAssign | MinusAssign | MultAssign | ExpAssign | DivAssign | ModAssign
              | LShiftAssign | RShiftAssign | RShift3Assign | BitOrAssign | BitXorAssign
              | BitAndAssign ) ->
            let open Ast.Pattern in
            begin
              match left with
              | (_, Identifier { Identifier.name; _ }) ->
                (* given `x += e`, read x then read e then write x *)
                ignore @@ this#identifier name;
                ignore @@ this#expression right;
                ignore @@ this#assignment_pattern left
              | (_, Expression e) ->
                (* given `o.x += e`, read o then read e *)
                ignore @@ this#pattern_expression e;
                this#assign_expression e right
              | (_, (Object _ | Array _)) -> statement_error
            end
          | Some ((OrAssign | AndAssign | NullishAssign) as operator) ->
            let left_expr =
              match left with
              | (lhs_loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; _ }) ->
                Some (lhs_loc, Ast.Expression.Identifier name)
              | (lhs_loc, Ast.Pattern.Expression (_, Ast.Expression.Member mem)) ->
                Some (lhs_loc, Ast.Expression.Member mem)
              | _ -> None
            in
            this#push_refinement_scope empty_refinements;
            (match left_expr with
            | None -> statement_error
            | Some left_expr ->
              (* THe LHS is unconditionally evaluated, so we don't run-to-completion and catch the
               * error here *)
              (match operator with
              | OrAssign
              | AndAssign ->
                ignore (this#expression_refinement left_expr)
              | NullishAssign ->
                ignore (this#expression left_expr);
                this#add_refinement_to_expr left_expr (L.LSet.singleton left_loc, NotR MaybeR)
              | _ -> ()));
            let env1 = this#env_without_latest_refinements in
            let env1_with_refinements = this#env in
            (match operator with
            | NullishAssign
            | OrAssign ->
              this#negate_new_refinements ()
            | _ -> ());
            (* The RHS is _only_ evaluated if the LHS fails its check. That means that patterns like
               * x || invariant(false) should propagate the truthy refinement to the next line. We keep track
               * of the completion state on the rhs to do that. If the LHS throws then the entire expression
               * throws, so there's no need to catch the exception from the LHS *)
            let rhs_completion_state =
              this#run_to_completion (fun () -> ignore @@ this#expression right)
            in
            (match rhs_completion_state with
            | Some AbruptCompletion.Throw ->
              this#reset_env env1_with_refinements;
              this#pop_refinement_scope_without_unrefining ()
            | _ ->
              this#pop_refinement_scope ();
              this#merge_self_env env1);
            (match left with
            | (loc, Ast.Pattern.Expression (_, Ast.Expression.Member mem)) ->
              let reason = mk_reason RSomeProperty loc in
              let assigned_val = Val.one reason in
              this#assign_member ~delete:false mem loc assigned_val reason
            | (_, Ast.Pattern.Identifier _) -> ignore @@ this#assignment_pattern left
            | _ -> statement_error)
        end;
        expr

      method! variable_declaration loc decl =
        let open Flow_ast in
        let { Statement.VariableDeclaration.declarations; kind; comments = _ } = decl in
        if kind <> Statement.VariableDeclaration.Const then
          Flow_ast_utils.fold_bindings_of_variable_declarations
            (fun _has_anno () (loc, { Identifier.name; comments = _ }) ->
              valid_declaration_check (OrdinaryName name) loc)
            ()
            declarations;
        super#variable_declaration loc decl

      (* Order of evaluation matters *)
      method! variable_declarator
          ~kind (decl : (ALoc.t, ALoc.t) Ast.Statement.VariableDeclaration.Declarator.t) =
        let open Ast.Statement.VariableDeclaration.Declarator in
        let (_loc, { id; init }) = decl in
        let open Ast.Pattern in
        begin
          match id with
          | ( _,
              ( Identifier { Ast.Pattern.Identifier.annot; _ }
              | Object { Ast.Pattern.Object.annot; _ }
              | Array { Ast.Pattern.Array.annot; _ } )
            ) -> begin
            match (init, id) with
            | ( Some (_, Ast.Expression.Array { Ast.Expression.Array.elements = []; _ }),
                ( _,
                  Identifier
                    {
                      Ast.Pattern.Identifier.annot = Ast.Type.Missing _;
                      name = (name_loc, { Ast.Identifier.name = x; _ });
                      _;
                    }
                )
              )
              when not @@ this#is_excluded_ordinary_name x ->
              let kind = variable_declaration_binding_kind_to_pattern_write_kind (Some kind) in
              let reason = Reason.(mk_reason (RIdentifier (OrdinaryName x))) name_loc in
              let write_kind = Env_api.AssigningWrite reason in
              let assigned_val =
                Base.Option.value_map
                  ~f:(fun { Env_api.Provider_api.array_providers; _ } ->
                    Val.empty_array reason array_providers)
                  ~default:(Val.one reason)
                  (Env_api.Provider_api.providers_of_def provider_info name_loc)
              in

              let {
                val_ref;
                heap_refinements;
                kind = stored_binding_kind;
                def_loc;
                havoc = _;
                writes_by_closure_provider_val = _;
              } =
                this#env_read x
              in

              (match
                 error_for_assignment_kind cx x name_loc def_loc stored_binding_kind kind !val_ref
               with
              | Some err ->
                add_output err;
                let write_entries =
                  EnvMap.add_ordinary name_loc Env_api.NonAssigningWrite env_state.write_entries
                in
                env_state <- { env_state with write_entries }
              | _ ->
                this#record_pattern_loc_writes id;
                this#havoc_heap_refinements heap_refinements;
                let write_entries =
                  if not (Val.is_declared_function !val_ref) then (
                    val_ref := assigned_val;
                    (* Unlike with a typical write, we don't want to create a write_entry here,
                       because the type should be computed from the array providers. *)
                    EnvMap.add_ordinary name_loc write_kind env_state.write_entries
                  ) else
                    (* All of the providers are aleady in the map. We don't want to overwrite them with
                     * a non-assigning write. We _do_ want to enter regular function declarations as
                     * non-assigning writes so that they are not checked against the providers in
                     * Env.set_env_entry *)
                    EnvMap.update_ordinary
                      name_loc
                      (fun x ->
                        match x with
                        | None -> Some Env_api.NonAssigningWrite
                        | _ -> x)
                      env_state.write_entries
                in
                env_state <- { env_state with write_entries })
            | (Some init, _) ->
              (* given `var x = e`, read e then write x *)
              ignore @@ this#expression init;
              ignore @@ this#binding_pattern_track_object_destructuring ~kind ~acc:init id
            | (None, _) ->
              (* No rhs means no write occurs, but the variable moves from undeclared to
               * uninitialized. *)
              this#record_pattern_loc_writes id;
              Flow_ast_utils.fold_bindings_of_pattern
                (fun () (loc, { Flow_ast.Identifier.name; _ }) ->
                  if this#is_excluded_ordinary_name name then
                    ()
                  else
                    let { val_ref; kind = stored_binding_kind; def_loc; _ } = this#env_read name in
                    let kind =
                      variable_declaration_binding_kind_to_pattern_write_kind (Some kind)
                    in
                    let error =
                      error_for_assignment_kind
                        cx
                        name
                        loc
                        def_loc
                        stored_binding_kind
                        kind
                        !val_ref
                    in
                    if Val.is_undeclared !val_ref then val_ref := Val.uninitialized loc;
                    let reason = mk_reason (RIdentifier (OrdinaryName name)) loc in
                    match (error, annot) with
                    | (None, Ast.Type.Available _) ->
                      env_state <-
                        {
                          env_state with
                          write_entries =
                            EnvMap.add_ordinary
                              loc
                              (Env_api.AssigningWrite reason)
                              env_state.write_entries;
                        }
                    | (Some err, _) ->
                      this#error_assignment loc reason stored_binding_kind kind err val_ref
                    | _ -> ())
                ()
                id;
              ignore @@ this#type_annotation_hint annot
          end
          | (_, Expression _) -> statement_error
        end;
        decl

      (* read and write (when the argument is an identifier) *)
      method! update_expression _loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Update.t) =
        let open Ast.Expression.Update in
        let { argument; operator = _; prefix = _; comments = _ } = expr in
        begin
          match argument with
          | (_, Ast.Expression.Identifier x) ->
            (* given `x++`, read x then write x *)
            ignore @@ this#identifier x;
            ignore @@ this#pattern_identifier x
          | (loc, Ast.Expression.Member member) ->
            (* given `o.x++`, read o.x then write o.x *)
            ignore @@ this#expression argument;
            ignore @@ this#pattern_expression argument;
            let val_reason = mk_reason RSomeProperty loc in
            let assigned_val = Val.number val_reason in
            (* We explicitly write a number instead of using the location of the write to avoid
             * location clashes between the read of the member and the write of the member. If
             * we don't do this then expressions like o.x++ will attempt to unify the read of o.x
             * with the write of o.x that happens in that update expression *)
            (match RefinementKey.lookup_of_member member ~allow_optional:false with
            | Some lookup ->
              this#map_val_with_lookup
                lookup
                (fun _ -> assigned_val)
                ~create_val_for_heap:(lazy assigned_val)
            | _ -> ())
          | _ -> (* given 'o()++`, read o *) ignore @@ this#expression argument
        end;
        expr

      (* things that cause abrupt completions *)
      method! break _loc (stmt : ALoc.t Ast.Statement.Break.t) =
        let open Ast.Statement.Break in
        let { label; comments = _ } = stmt in
        this#raise_abrupt_completion (AbruptCompletion.break label)

      method! continue _loc (stmt : ALoc.t Ast.Statement.Continue.t) =
        let open Ast.Statement.Continue in
        let { label; comments = _ } = stmt in
        this#raise_abrupt_completion (AbruptCompletion.continue label)

      method! function_body_any body =
        (match body with
        | Flow_ast.Function.BodyBlock _ -> ()
        | Flow_ast.Function.BodyExpression expr ->
          (match env_state.predicate_scope_names with
          | None -> ()
          | Some names -> this#record_predicate_refinement_maps (fst expr) names expr));
        super#function_body_any body

      method! return loc (stmt : (ALoc.t, ALoc.t) Ast.Statement.Return.t) =
        let open Ast.Statement.Return in
        let { argument; comments = _; return_out = _ } = stmt in
        (match (env_state.predicate_scope_names, argument) with
        | (None, _)
        | (Some _, None) ->
          ignore @@ Flow_ast_mapper.map_opt this#expression argument
        | (Some names, Some argument) -> this#record_predicate_refinement_maps loc names argument);
        this#raise_abrupt_completion AbruptCompletion.return

      method private record_predicate_refinement_maps loc names expr =
        let compute_refi_map () =
          names
          |> SSet.elements
          |> Base.List.map ~f:(fun name -> (name, this#synthesize_read name))
          |> SMap.of_list
        in
        this#push_refinement_scope empty_refinements;
        ignore @@ this#expression_refinement expr;
        let positive_refi_map = compute_refi_map () in
        this#negate_new_refinements ();
        let negative_refi_map = compute_refi_map () in
        this#pop_refinement_scope ();
        env_state <-
          {
            env_state with
            predicate_refinement_maps =
              L.LMap.add
                loc
                (positive_refi_map, negative_refi_map)
                env_state.predicate_refinement_maps;
          }

      method! throw _loc (stmt : (ALoc.t, ALoc.t) Ast.Statement.Throw.t) =
        let open Ast.Statement.Throw in
        let { argument; comments = _ } = stmt in
        ignore @@ this#expression argument;
        this#raise_abrupt_completion AbruptCompletion.throw

      (** Control flow **)
      method! if_statement _loc stmt =
        let open Flow_ast.Statement.If in
        let { test; consequent; alternate; _ } = stmt in
        this#push_refinement_scope empty_refinements;
        ignore @@ this#expression_refinement test;
        let test_refinements = this#peek_new_refinements () in
        let env0 = this#env_without_latest_refinements in
        (* collect completions and environments of every branch *)
        let then_completion_state =
          this#run_to_completion (fun () ->
              ignore @@ this#if_consequent_statement ~has_else:(alternate <> None) consequent
          )
        in
        let then_env_no_refinements = this#env_without_latest_refinements in
        let then_env_with_refinements = this#env in
        this#pop_refinement_scope ();
        this#reset_env env0;
        this#push_refinement_scope test_refinements;
        this#negate_new_refinements ();
        let else_completion_state =
          this#run_to_completion (fun () ->
              ignore
              @@ Flow_ast_mapper.map_opt
                   (fun (loc, { Alternate.body; comments }) ->
                     (loc, { Alternate.body = this#statement body; comments }))
                   alternate
          )
        in
        (* merge environments *)
        let else_env_no_refinements = this#env_without_latest_refinements in
        let else_env_with_refinements = this#env in
        this#pop_refinement_scope ();
        this#reset_env env0;
        this#merge_conditional_branches_with_refinements
          (then_env_no_refinements, then_env_with_refinements, then_completion_state)
          (else_env_no_refinements, else_env_with_refinements, else_completion_state);

        (* merge completions *)
        let if_completion_states = (then_completion_state, [else_completion_state]) in
        this#merge_completion_states if_completion_states;
        stmt

      method! conditional _loc (expr : (ALoc.t, ALoc.t) Flow_ast.Expression.Conditional.t) =
        let open Flow_ast.Expression.Conditional in
        let { test; consequent; alternate; comments = _ } = expr in
        this#push_refinement_scope empty_refinements;
        ignore @@ this#expression_refinement test;
        let test_refinements = this#peek_new_refinements () in
        let env0 = this#env_without_latest_refinements in
        let consequent_completion_state =
          this#run_to_completion (fun () -> ignore @@ this#expression consequent)
        in
        let consequent_env_no_refinements = this#env_without_latest_refinements in
        let consequent_env_with_refinements = this#env in
        this#pop_refinement_scope ();
        this#reset_env env0;
        this#push_refinement_scope test_refinements;
        this#negate_new_refinements ();
        let alternate_completion_state =
          this#run_to_completion (fun () -> ignore @@ this#expression alternate)
        in
        let alternate_env_no_refinements = this#env_without_latest_refinements in
        let alternate_env_with_refinements = this#env in
        this#pop_refinement_scope ();
        this#reset_env env0;
        this#merge_conditional_branches_with_refinements
          ( consequent_env_no_refinements,
            consequent_env_with_refinements,
            consequent_completion_state
          )
          (alternate_env_no_refinements, alternate_env_with_refinements, alternate_completion_state);

        (* merge completions *)
        let conditional_completion_states =
          (consequent_completion_state, [alternate_completion_state])
        in
        this#merge_completion_states conditional_completion_states;
        expr

      method merge_conditional_branches_with_refinements
          (env1, refined_env1, completion_state1) (env2, refined_env2, completion_state2) : unit =
        (* We only want to merge the refined environments from the two branches of an if-statement
         * if there was an assignment in one of the branches. Otherwise, merging the positive and
         * negative branches of the refinement into a union would be unnecessary work to
         * reconstruct the original type.
         *
         * If one of the branches abnormally completes then we can just take the refinements
         * from the other branch. *)
        match (completion_state1, completion_state2) with
        | (None, Some _) -> this#reset_env refined_env1
        | (Some _, None) -> this#reset_env refined_env2
        | _ ->
          SMap.iter
            (fun name ({ val_ref; heap_refinements; havoc; def_loc; _ }, _) ->
              let { Env.env_val = value1; heap_refinements = heap_entries1; def_loc = _ } =
                smap_find name env1
              in
              let { Env.env_val = value2; heap_refinements = heap_entries2; def_loc = _ } =
                smap_find name env2
              in
              let {
                Env.env_val = refined_value1;
                heap_refinements = refined_heap_entries1;
                def_loc = _;
              } =
                smap_find name refined_env1
              in
              let {
                Env.env_val = refined_value2;
                heap_refinements = refined_heap_entries2;
                def_loc = _;
              } =
                smap_find name refined_env2
              in
              (* If the same key exists on both versions of the object then we can
               * merge the two heap refinements, even though the underlying value
               * has changed. This is because the final object does indeed have
               * one of the two refinements at the merge *)
              heap_refinements :=
                HeapRefinementMap.merge
                  (fun key refined_heap_val1 refined_heap_val2 ->
                    match (refined_heap_val1, refined_heap_val2) with
                    | (Some refined_heap_val1, Some refined_heap_val2) ->
                      let heap_val1 = heap_map_find key heap_entries1 in
                      let heap_val2 = heap_map_find key heap_entries2 in
                      if Val.id_of_val heap_val1 = Val.id_of_val heap_val2 then
                        if Val.is_projection heap_val1 then
                          None
                        else
                          Some heap_val1
                      else
                        Some (Val.merge refined_heap_val1 refined_heap_val2)
                    | _ -> None)
                  refined_heap_entries1
                  refined_heap_entries2;
              if Val.id_of_val value1 = Val.id_of_val value2 then
                val_ref := value1
              else
                val_ref := this#merge_vals_with_havoc ~havoc ~def_loc refined_value1 refined_value2)
            env_state.env

      method with_env_state f =
        let pre_state = env_state in
        let pre_env = this#env in
        let result = f () in
        env_state <- pre_state;
        (* It's not enough to just restore the old env_state, since the env itself contains
         * refs. We need to call reset_env to _fully_ reset the env_state *)
        this#reset_env pre_env;
        result

      (* Functions called inside scout_changed_refinement_keys are responsible for popping any refinement
       * scopes they may introduce
       *)
      method scout_changed_refinement_keys ~scout ~continues =
        (* Calling scout may have side effects, like adding new abrupt completions. We
         * need to be sure to restore the old abrupt completion envs after scouting,
         * because a scout should be followed-up by a run that revisits everything visited by
         * the scout. with_env_state will ensure that all mutable state is restored. *)
        this#with_env_state (fun () ->
            let pre_env = this#env in
            let completion_state = this#run_to_completion scout in
            let _ =
              this#run_to_completion (fun () ->
                  this#commit_abrupt_completion_matching
                    (AbruptCompletion.mem continues)
                    completion_state
              )
            in
            let post_env = this#env in
            SMap.fold
              (fun name
                   { Env.env_val = env_val1; heap_refinements = heap_refinements1; def_loc = _ }
                   acc ->
                let { Env.env_val = env_val2; heap_refinements = heap_refinements2; def_loc = _ } =
                  smap_find name pre_env
                in
                let acc =
                  HeapRefinementMap.fold
                    (fun k heap_refinement1 acc ->
                      let heap_refinement2_opt = HeapRefinementMap.find_opt k heap_refinements2 in
                      match heap_refinement2_opt with
                      | None -> RefinementKey.{ base = name; projections = k } :: acc
                      | Some heap_refinement2 ->
                        if Val.id_of_val heap_refinement1 = Val.id_of_val heap_refinement2 then
                          acc
                        else
                          RefinementKey.{ base = name; projections = k } :: acc)
                    heap_refinements1
                    acc
                in
                if Val.id_of_val env_val1 = Val.id_of_val env_val2 then
                  acc
                else
                  RefinementKey.lookup_of_name name :: acc)
              post_env
              []
        )

      method havoc_changed_refinement_keys changed_refinement_keys =
        List.iter
          (fun lookup ->
            let { RefinementKey.base; projections } = lookup in
            let {
              val_ref;
              havoc;
              writes_by_closure_provider_val = _;
              heap_refinements;
              def_loc = _;
              kind;
            } =
              this#env_read base
            in
            (* If a var is changed then all the heap refinements on that var should
             * also be havoced. If only heap refinements are havoced then there's no
             * need to havoc the subject of the projection *)
            match projections with
            | [] ->
              this#havoc_heap_refinements heap_refinements;
              if kind <> Bindings.Const && not (Val.is_undeclared_or_skipped !val_ref) then
                val_ref :=
                  Base.List.fold
                    ~init:havoc
                    ~f:(fun acc write -> Val.merge acc (Val.of_write write))
                    (Val.writes_of_uninitialized this#refinement_may_be_undefined !val_ref)
            | _ -> heap_refinements := HeapRefinementMap.remove projections !heap_refinements)
          changed_refinement_keys

      method handle_continues loop_completion_state continues =
        this#run_to_completion (fun () ->
            this#commit_abrupt_completion_matching
              (AbruptCompletion.mem continues)
              loop_completion_state
        )

      (* After a loop we need to negate the loop guard and apply the refinement. The
       * targets of those refinements may have been changed by the loop, but that
       * doesn't matter. The only way to get out of the loop is for the negation of
       * the refinement to hold, so we apply that negation even though the ssa_id might
       * not match.
       *
       * The exception here is, of course, if we break out of the loop. If we break
       * inside the loop then we should not negate the refinements because it is
       * possible that we just exited the loop by breaking.
       *
       * We don't need to check for continues because they are handled before this point.
       * We don't check for throw/return because then we wouldn't proceed to the line
       * after the loop anyway. *)
      method post_loop_refinements env_before_guard guard =
        if not AbruptCompletion.(mem (List.map fst env_state.abrupt_completion_envs) (break None))
        then (
          this#push_refinement_scope empty_refinements;
          ignore @@ this#expression_refinement guard;
          this#negate_new_refinements ();
          this#pop_refinement_scope_without_unrefining ();
          let final_env = this#env in
          this#reset_env env_before_guard;
          (* This may seem unnecessary, but we need to ensure that when we revisit the guard that
           * the special writes we record for literal subtyping checks are present. This are only
           * recorded in _refinement functions *)
          this#push_refinement_scope empty_refinements;
          ignore @@ this#expression_refinement guard;
          this#pop_refinement_scope ();
          this#reset_env final_env
        )

      (*
       * Unlike the ssa_builder, the name_resolver does not create REF unresolved
       * Val.ts to model the write states of variables in loops. This approach
       * would cause a lot of cycles in the ordering algorithm, which means
       * we'd need to ask for a lot of annotations. Moreover, it's not clear where
       * those annotations should go.
       *
       * Instead, we scout the body of the loop to find which variables are
       * written to. If a variable is written, then we havoc that variable
       * before entering the loop. This does not apply to variables that are
       * only refined.
       *
       * After visiting the body, we reset the state in the ssa environment,
       * havoc any vars that need to be havoced, and then visit the body again.
       * After that we negate the refinements on the loop guard by revisiting the guard
       * with the post-loop state and negating the refinement.
       *
       * This last part that revisits the guard will record writes that we don't want recorded. To
       * restore the original state, we reset the environment to what it was when we originally
       * visited the guard and then visit it again. You can see this in post_loop_refinements.
       *
       * Here's how each param should be used:
       * scout: Visit the guard and any updaters if applicable, then visit the body
       * guard: An optional expression AST that is the predicate that is evaluated before
       *   entering the loop body.
       * visit_guard_and_body: Visit the guard with a refinement scope, any updaters
       *   if applicable, and then visit the body. Return the loop completion state.
       * make_completion_states: given the loop completion state, give the list of
       *   possible completion states for the loop. For do while loops this is different
       *   than regular while loops, so those two implementations may be instructive.
       * auto_handle_continues: Every loop needs to filter out continue completion states.
       *   The default behavior is to do that filtering at the end of the body.
       *   If you need to handle continues before that, like in a do/while loop, then
       *   set this to false. Ensure that you handle continues in both the scouting and
       *   main passes.
       *)
      method env_loop
          ~guard
          ~scout
          ~visit_guard_and_body
          ~make_completion_states
          ~auto_handle_continues
          ~continues =
        this#expecting_abrupt_completions (fun () ->
            (* Scout the body for changed vars *)
            let changed_refinement_keys = this#scout_changed_refinement_keys ~scout ~continues in

            (* We havoc the changed vars in order to prevent loops in the EnvBuilder writes-graph,
             * which would require a fix-point analysis that would not be compatible with
             * local type inference *)
            this#havoc_changed_refinement_keys changed_refinement_keys;

            (* Now we push a refinement scope and visit the guard/body. At the end, we completely
             * get rid of refinements introduced by the guard, even if they occur in a PHI node, to
             * ensure that the refinement does not escape the loop via something like
             * control flow. For example:
             * while (x != null) {
             *   if (x == 3) {
             *     x = 4;
             *   }
             * }
             * x; // Don't want x to be a PHI of x != null and x = 4.
             *)
            this#push_refinement_scope empty_refinements;
            let (loop_completion_state, env_before_guard, env_after_guard_no_refinements) =
              visit_guard_and_body ()
            in
            let loop_completion_state =
              if auto_handle_continues then
                this#handle_continues loop_completion_state continues
              else
                loop_completion_state
            in
            this#pop_refinement_scope_after_loop ();

            (* We either enter the loop body or we don't *)
            (match env_after_guard_no_refinements with
            | None -> ()
            | Some env -> this#merge_self_env env);

            (match guard with
            | None -> ()
            | Some guard -> this#post_loop_refinements env_before_guard guard);

            let completion_states = make_completion_states loop_completion_state in
            let completion_state =
              this#run_to_completion (fun () -> this#merge_completion_states completion_states)
            in
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break None])
              completion_state
        )

      method! while_ _loc (stmt : (ALoc.t, ALoc.t) Flow_ast.Statement.While.t) =
        let open Flow_ast.Statement.While in
        let { test; body; comments = _ } = stmt in
        let scout () =
          ignore @@ this#expression test;
          ignore @@ this#run_to_completion (fun () -> ignore @@ this#statement body)
        in
        let visit_guard_and_body () =
          let env_before_guard = this#env in
          ignore @@ this#expression_refinement test;
          let env = this#env_without_latest_refinements in
          let loop_completion_state =
            this#run_to_completion (fun () -> ignore @@ this#statement body)
          in
          (loop_completion_state, env_before_guard, Some env)
        in
        let make_completion_states loop_completion_state = (None, [loop_completion_state]) in
        let continues = AbruptCompletion.continue None :: env_state.possible_labeled_continues in
        this#env_loop
          ~guard:(Some test)
          ~scout
          ~visit_guard_and_body
          ~make_completion_states
          ~auto_handle_continues:true
          ~continues;
        stmt

      method! do_while _loc stmt =
        let open Flow_ast.Statement.DoWhile in
        let { test; body; comments = _ } = stmt in
        let continues = AbruptCompletion.continue None :: env_state.possible_labeled_continues in
        let scout () =
          let loop_completion_state =
            this#run_to_completion (fun () -> ignore @@ this#statement body)
          in
          ignore @@ this#handle_continues loop_completion_state continues;
          match loop_completion_state with
          | None -> ignore @@ this#expression test
          | Some _ -> ()
        in
        let visit_guard_and_body () =
          let loop_completion_state =
            this#run_to_completion (fun () -> ignore @@ this#statement body)
          in
          let loop_completion_state = this#handle_continues loop_completion_state continues in
          let env_before_guard = this#env in
          (match loop_completion_state with
          | None -> ignore @@ this#expression_refinement test
          | Some _ -> ());
          (loop_completion_state, env_before_guard, None)
        in
        let make_completion_states loop_completion_state = (loop_completion_state, []) in
        this#env_loop
          ~guard:(Some test)
          ~scout
          ~visit_guard_and_body
          ~make_completion_states
          ~auto_handle_continues:false
          ~continues;
        stmt

      method! scoped_for_statement _loc stmt =
        let open Flow_ast.Statement.For in
        let { init; test; update; body; comments = _ } = stmt in
        let continues = AbruptCompletion.continue None :: env_state.possible_labeled_continues in
        let scout () =
          ignore @@ Flow_ast_mapper.map_opt this#for_statement_init init;
          ignore @@ Flow_ast_mapper.map_opt this#expression test;
          let loop_completion_state =
            this#run_to_completion (fun () -> ignore @@ this#statement body)
          in
          let loop_completion_state = this#handle_continues loop_completion_state continues in
          match loop_completion_state with
          | None -> ignore @@ Flow_ast_mapper.map_opt this#expression update
          | Some _ -> ()
        in
        let visit_guard_and_body () =
          ignore @@ Flow_ast_mapper.map_opt this#for_statement_init init;
          let env_before_guard = this#env in
          ignore @@ Flow_ast_mapper.map_opt this#expression_refinement test;
          let env = this#env_without_latest_refinements in
          let loop_completion_state =
            this#run_to_completion (fun () -> ignore @@ this#statement body)
          in
          let loop_completion_state = this#handle_continues loop_completion_state continues in
          (match loop_completion_state with
          | None -> ignore @@ Flow_ast_mapper.map_opt this#expression update
          | Some _ -> ());
          (loop_completion_state, env_before_guard, Some env)
        in
        let make_completion_states loop_completion_state = (None, [loop_completion_state]) in
        this#env_loop
          ~guard:test
          ~scout
          ~visit_guard_and_body
          ~make_completion_states
          ~auto_handle_continues:false
          ~continues;
        stmt

      method for_in_or_of_left_declaration left =
        let (loc, decl) = left in
        let { Flow_ast.Statement.VariableDeclaration.declarations; kind; comments = _ } = decl in
        match declarations with
        | [(_, { Flow_ast.Statement.VariableDeclaration.Declarator.id; init = _ })] ->
          let open Flow_ast.Pattern in
          (match id with
          | (_, (Identifier _ | Object _ | Array _)) ->
            ignore @@ this#variable_declarator_pattern ~kind id
          | (loc, _) -> raise Env_api.(Env_invariant (Some loc, Impossible "unexpected AST node")))
        | _ ->
          raise
            Env_api.(
              Env_invariant
                ( Some loc,
                  Impossible
                    "Syntactically valid for-in loops must have exactly one left declaration"
                )
            )

      method! for_in_left_declaration left =
        this#for_in_or_of_left_declaration left;
        left

      method! for_of_left_declaration left =
        this#for_in_or_of_left_declaration left;
        left

      method scoped_for_in_or_of_statement traverse_left body =
        (* You might be wondering why the lhs has to be scouted-- the LHS can be a pattern that
         * includes a default write with a variable that is written to inside the loop. It's
         * critical that we catch loops in the dependency graph with such variables, since the
         * ordering algorithm will not have a good place to ask for an annotation in that case.
         *)
        let scout () =
          traverse_left ();
          ignore @@ this#run_to_completion (fun () -> ignore @@ this#statement body)
        in
        let visit_guard_and_body () =
          let env = this#env in
          traverse_left ();
          let loop_completion_state =
            this#run_to_completion (fun () -> ignore @@ this#statement body)
          in
          (loop_completion_state, env, Some env)
        in
        let make_completion_states loop_completion_state = (None, [loop_completion_state]) in
        let continues = AbruptCompletion.continue None :: env_state.possible_labeled_continues in
        this#env_loop
          ~guard:None
          ~scout
          ~visit_guard_and_body
          ~make_completion_states
          ~auto_handle_continues:true
          ~continues

      method! scoped_for_in_statement _loc stmt =
        let open Flow_ast.Statement.ForIn in
        let { left; right; body; each = _; comments = _ } = stmt in
        let traverse_left () = ignore (this#for_in_statement_lhs left) in
        this#push_refinement_scope empty_refinements;
        ignore @@ this#expression_refinement right;
        this#scoped_for_in_or_of_statement traverse_left body;
        this#pop_refinement_scope ();
        stmt

      method! scoped_for_of_statement _loc stmt =
        let open Flow_ast.Statement.ForOf in
        let { left; right; body; await = _; comments = _ } = stmt in
        (* This is only evaluated once and so does not need to be scouted *)
        ignore @@ this#expression right;
        let traverse_left () = ignore (this#for_of_statement_lhs left) in
        this#scoped_for_in_or_of_statement traverse_left body;
        stmt

      method private switch_completeness loc =
        let reason = Reason.(mk_reason (RCustom "switch") loc) in
        let { val_ref; _ } = this#env_read maybe_exhaustively_checked_var_name in
        val_ref := Val.one reason

      method! switch loc switch =
        let open Flow_ast.Statement.Switch in
        let incoming_env = this#env in
        let { discriminant; cases; comments = _; exhaustive_out } = switch in
        let _ = this#expression discriminant in
        let lexical_hoist = new lexical_hoister ~flowmin_compatibility:false ~enable_enums in
        let cases_with_lexical_bindings =
          Base.List.map cases ~f:(fun ((_, { Case.consequent; _ }) as case) ->
              let bindings = lexical_hoist#acc |> Bindings.to_map in
              let _ = lexical_hoist#statement_list consequent in
              (case, bindings)
          )
        in
        this#run
          (fun () ->
            ignore
            @@ this#with_bindings
                 ~lexical:true
                 loc
                 lexical_hoist#acc
                 (this#switch_cases_with_lexical_bindings loc exhaustive_out discriminant)
                 cases_with_lexical_bindings)
          ~finally:(fun () ->
            let post_env = this#env in
            (* After all refinements and potential shadowing inside switch,
               we need to re-read the discriminant to restore it. *)
            this#reset_env incoming_env;
            ignore @@ this#expression switch.discriminant;
            this#reset_env post_env);
        switch

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
      method private switch_cases_with_lexical_bindings
          switch_loc exhaustive_out discriminant cases_with_lexical_bindings =
        let incoming_env = this#env in
        this#expecting_abrupt_completions (fun () ->
            let (case_starting_env, case_completion_states, fallthrough_env, has_default) =
              List.fold_left
                (fun acc ((loc, case), bindings) ->
                  this#env_switch_case discriminant acc (loc, case, bindings))
                (incoming_env, [], None, false)
                cases_with_lexical_bindings
            in
            this#reset_env case_starting_env;
            ( if not has_default then
              let discriminant_after_all_negated_refinements =
                this#get_val_of_expression discriminant
              in
              match discriminant_after_all_negated_refinements with
              | Some discriminant ->
                env_state <-
                  {
                    env_state with
                    values =
                      L.LMap.add
                        switch_loc
                        {
                          def_loc = None;
                          value = discriminant;
                          binding_kind_opt = None;
                          name = None;
                        }
                        env_state.values;
                  }
              | None -> ()
            );

            (match fallthrough_env with
            (* If the switch has a default then it is exhaustive. Thus, the post-env can be
             * determined by joining all of the breaks with the last fallthrough env. If there
             * was no fallthrough env, then the we can use empty as the base. *)
            | Some env when has_default -> this#reset_env env
            | None when has_default -> this#reset_env this#empty_env
            (* If the switch wasn't exhaustive then merge with the case_starting_env as a base. If
             * the last case fell out then merge that in too. *)
            | Some fallthrough ->
              this#switch_completeness exhaustive_out;
              this#merge_self_env fallthrough
            | None -> this#switch_completeness exhaustive_out);

            (* In general, cases are non-exhaustive, but if it has a default case then it is! *)
            let completion_state =
              if has_default then
                (* Since there is a default we know there is at least one element in this
                 * list, which means calling List.hd or tail will not fail *)
                let first_state = List.hd case_completion_states in
                let remaining_states = List.tl case_completion_states in
                this#run_to_completion (fun () ->
                    this#merge_completion_states (first_state, remaining_states)
                )
              else
                None
            in

            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break None])
              completion_state
        );
        cases_with_lexical_bindings

      method private env_switch_case
          discriminant
          (case_starting_env, case_completion_states, fallthrough_env, has_default)
          (case_loc, case, lexical_bindings) =
        let open Ast.Statement.Switch.Case in
        let { test; consequent; comments = _ } = case in
        this#reset_env case_starting_env;
        (* Reset discriminant *)
        this#push_refinement_scope empty_refinements;
        let (has_default, latest_refinements, case_starting_env) =
          match test with
          | None -> (true, empty_refinements, this#env)
          | Some test ->
            (* As a convention, locate refined versions of the discriminant on
               case locations, in order to read them from the environment for
               filtered TestProp checks. *)
            begin
              match discriminant with
              | (_, Ast.Expression.Member { Ast.Expression.Member._object; _ }) -> begin
                match this#get_val_of_expression _object with
                | None ->
                  let values = L.LMap.remove case_loc env_state.values in
                  env_state <- { env_state with values }
                | Some refined_v ->
                  let values =
                    L.LMap.add
                      case_loc
                      { def_loc = None; value = refined_v; binding_kind_opt = None; name = None }
                      env_state.values
                  in
                  env_state <- { env_state with values }
              end
              | _ -> ()
            end;
            ignore @@ this#expression test;
            let (loc, _) = test in
            this#eq_test ~strict:true ~sense:true ~cond_context:SwitchTest loc discriminant test;
            (has_default, this#peek_new_refinements (), this#env_without_latest_refinements)
        in
        (match fallthrough_env with
        | None -> ()
        | Some fallthrough -> this#merge_self_env fallthrough);
        let () =
          lexical_bindings
          |> SMap.iter (fun name (kind, (loc, _)) ->
                 if this#is_excluded_ordinary_name name then
                   ()
                 else
                   match kind with
                   | Bindings.Let
                   | Bindings.Const ->
                     let { val_ref; heap_refinements; _ } = this#env_read name in
                     this#havoc_heap_refinements heap_refinements;
                     val_ref := Val.declared_but_skipped name loc
                   | _ -> ()
             )
        in
        let case_completion_state =
          this#run_to_completion (fun () -> ignore @@ this#statement_list consequent)
        in
        let fallthrough_env =
          match case_completion_state with
          | None -> Some this#env
          | Some _ -> None
        in
        this#pop_refinement_scope ();
        this#reset_env case_starting_env;
        let negated_refinements = this#negate_refinements latest_refinements in
        this#push_refinement_scope negated_refinements;
        let case_starting_env = this#env in
        this#pop_refinement_scope ();
        ( case_starting_env,
          case_completion_state :: case_completion_states,
          fallthrough_env,
          has_default
        )

      method! try_catch _loc (stmt : (ALoc.t, ALoc.t) Ast.Statement.Try.t) =
        this#expecting_abrupt_completions (fun () ->
            let open Ast.Statement.Try in
            let { block = (loc, block); handler; finalizer; comments = _ } = stmt in
            let try_entry_env = this#env in
            let try_completion_state =
              this#run_to_completion (fun () -> ignore @@ this#block loc block)
            in
            let try_exit_env = this#env in
            (* The catch entry env must take into account the fact that any line in the try may
             * have thrown. We conservatively approximate this by assuming that the very first
             * line may have thrown, so we merge the entrance env. The other possible states that
             * can bring us to the catch are the envs at exceptions explicitly thrown in try, so
             * we merge those in as well.
             *)
            this#merge_self_env try_entry_env;
            (* Merge in all the throw envs *)
            this#commit_abrupt_completion_matching AbruptCompletion.(mem [throw]) None;
            let catch_completion_state =
              match handler with
              | Some (loc, clause) ->
                this#run_to_completion (fun () -> ignore @@ this#catch_clause loc clause)
              | None ->
                (* No catch is like having a catch that always re-throws the error from the
                 * try block.*)
                this#run_to_completion (fun () ->
                    this#raise_abrupt_completion AbruptCompletion.Throw
                )
            in
            let catch_exit_env = this#env in
            let completion_state =
              match (try_completion_state, catch_completion_state) with
              | (Some AbruptCompletion.Throw, Some AbruptCompletion.Throw)
              | (Some AbruptCompletion.Return, Some _) ->
                try_completion_state
              | (Some AbruptCompletion.Throw, Some AbruptCompletion.Return) ->
                catch_completion_state
              | _ -> None
            in
            (* Finalizers must be checked twice under two environments. We need one to determine
             * what the env should be after the try/catch/finally, which assumes that either try or
             * catch did not throw. This assumption, however, is too optimistic, so checking the
             * finally under this assumption would be unsound. To soundly check the finally, we make
             * no assumptions about throws. In that case, the entry env here is a merge of the try
             * start env, try exit env, and catch exit env, along with every abrupt completion env
             * from both the try and the catch. *)
            let finally_completion_state =
              match finalizer with
              | Some (_loc, block) ->
                (* First we check assuming that either try or catch does not throw so we can
                 * compute the post-state. We check inside expecting_abrupt_completions so that
                 * we do not pollute the abrupt completion states with envs from the this traversal
                 * and we do not call commit_abrupt_completion_matching because we are specifically
                 * trying to look at the case where we exit normally from the finally *)
                this#expecting_abrupt_completions (fun () ->
                    (match catch_completion_state with
                    | None -> this#merge_env try_exit_env catch_exit_env
                    | Some _ -> this#reset_env try_exit_env);
                    ignore @@ this#run_to_completion (fun () -> ignore @@ this#block loc block)
                );
                let exit_env = this#env in
                (* Now check assuming that we may throw anywhere in try or catch so that
                 * we can conservatively check the finally case. The starting env here is modeled as
                 * the merge of the try_entry_env with the catch_exit env, and we include all abrupt
                 * completion envs *)
                this#merge_env try_entry_env try_exit_env;
                this#merge_self_env catch_exit_env;
                this#commit_abrupt_completion_matching AbruptCompletion.all completion_state;
                let completion_state =
                  this#run_to_completion (fun () -> ignore @@ this#block loc block)
                in
                this#reset_env exit_env;
                completion_state
              | None ->
                (match catch_completion_state with
                | None -> this#merge_env try_exit_env catch_exit_env
                | Some _ -> this#reset_env try_exit_env);
                None
            in
            (* If finally has some sort of abnormal completion then we re-raise it. If not, we
             * consider the completion states from try/catch *)
            this#from_completion finally_completion_state;
            this#from_completion completion_state
        );
        stmt

      method! this_binding_function_id_opt ~fun_loc ~has_this_annot id =
        super#this_binding_function_id_opt ~fun_loc ~has_this_annot id;
        let function_write_loc =
          match id with
          | Some (loc, _) -> loc
          | None ->
            let reason = mk_reason (RCustom "<<anonymous function>>") fun_loc in
            let write_entries =
              EnvMap.add_ordinary fun_loc (Env_api.AssigningWrite reason) env_state.write_entries
            in
            env_state <- { env_state with write_entries };
            fun_loc
        in
        if
          this#is_assigning_write (Env_api.OrdinaryNameLoc, function_write_loc)
          && not has_this_annot
        then
          let write_entries =
            EnvMap.add
              (Env_api.FunctionThisLoc, fun_loc)
              (Env_api.AssigningWrite (mk_reason (RIdentifier (internal_name "this")) fun_loc))
              env_state.write_entries
          in
          env_state <- { env_state with write_entries }

      method! function_param_pattern patt = this#visit_function_param_pattern ~is_rest:false patt

      method! function_rest_param expr =
        let open Ast.Function.RestParam in
        let (_, { argument; comments = _ }) = expr in
        ignore @@ this#visit_function_param_pattern ~is_rest:true argument;
        expr

      method private visit_function_param_pattern ~is_rest ((ploc, pattern) as patt) =
        let reason =
          match pattern with
          | Flow_ast.Pattern.Identifier
              {
                Flow_ast.Pattern.Identifier.name = (_, { Flow_ast.Identifier.name; comments = _ });
                annot = _;
                optional = _;
              } ->
            let name = Some name in
            if is_rest then
              mk_reason (RRestParameter name) ploc
            else
              mk_reason (RParameter name) ploc
          | _ -> mk_reason RDestructuring ploc
        in
        let write_entries =
          EnvMap.add
            (Env_api.FunctionParamLoc, ploc)
            (Env_api.AssigningWrite reason)
            env_state.write_entries
        in
        env_state <- { env_state with write_entries };
        super#function_param_pattern patt

      method private predicate_scope_names_of_params predicate params =
        Base.Option.map predicate ~f:(fun _ ->
            let open Flow_ast in
            let (_, { Function.Params.params; rest; _ }) = params in
            let f acc (_, patt) =
              match patt with
              | Pattern.Identifier { Pattern.Identifier.name = (_, { Identifier.name; _ }); _ } ->
                SSet.add name acc
              (* Predicate functions disallow destructuring *)
              | _ -> acc
            in
            let acc =
              Base.List.fold
                params
                ~init:SSet.empty
                ~f:(fun acc (_, { Function.Param.argument; _ }) -> f acc argument
              )
            in
            Base.Option.fold rest ~init:acc ~f:(fun acc (_, { Function.RestParam.argument; _ }) ->
                f acc argument
            )
        )

      (* We also havoc state when entering functions and exiting calls. *)
      method! lambda ~is_arrow ~fun_loc ~generator_return_loc params predicate body =
        this#expecting_abrupt_completions (fun () ->
            let env = this#env in
            this#run
              (fun () ->
                let saved_predicate_scope_names = env_state.predicate_scope_names in
                env_state <-
                  {
                    env_state with
                    predicate_scope_names = this#predicate_scope_names_of_params predicate params;
                  };
                this#havoc_uninitialized_env;
                let completion_state =
                  this#run_to_completion (fun () ->
                      let loc =
                        let open Ast.Function in
                        match body with
                        | BodyBlock (loc, _)
                        | BodyExpression (loc, _) ->
                          loc
                      in
                      let bindings =
                        Bindings.(
                          singleton
                            ( ( loc,
                                {
                                  Ast.Identifier.name = maybe_exhaustively_checked_var_name;
                                  comments = None;
                                }
                              ),
                              Internal
                            )
                        )
                      in
                      let bindings =
                        if not is_arrow then
                          match params with
                          | (_, { Ast.Function.Params.this_ = Some (loc, _); _ }) ->
                            let id name = (loc, { Ast.Identifier.name; comments = None }) in
                            env_state <-
                              {
                                env_state with
                                write_entries =
                                  EnvMap.add_ordinary
                                    loc
                                    (Env_api.AssigningWrite (mk_reason RThis loc))
                                    env_state.write_entries;
                              };
                            Bindings.(add (id "this", ThisAnnot) bindings)
                          | _ ->
                            let id name = (fun_loc, { Ast.Identifier.name; comments = None }) in
                            Bindings.(add (id "this", Const) bindings)
                        else
                          bindings
                      in
                      let bindings =
                        Base.Option.value_map
                          ~f:(fun return_loc ->
                            Bindings.(
                              add
                                ( ( return_loc,
                                    { Ast.Identifier.name = next_var_name; comments = None }
                                  ),
                                  GeneratorNext
                                )
                                bindings
                            ))
                          ~default:bindings
                          generator_return_loc
                      in
                      this#with_bindings
                        fun_loc
                        bindings
                        (fun () ->
                          Context.add_exhaustive_check cx loc ([], false);

                          super#lambda
                            ~is_arrow
                            ~fun_loc
                            ~generator_return_loc
                            params
                            predicate
                            body;

                          let { val_ref; _ } = this#env_read maybe_exhaustively_checked_var_name in
                          let { Env_api.write_locs; _ } = Val.simplify None None None !val_ref in
                          let (locs, undeclared) =
                            Base.List.fold
                              ~init:([], false)
                              ~f:
                                (fun (locs, undeclared) -> function
                                  | Env_api.Undeclared _ -> (locs, true)
                                  | Env_api.Write r ->
                                    (Reason.poly_loc_of_reason r :: locs, undeclared)
                                  | _ ->
                                    raise
                                      Env_api.(
                                        Env_invariant
                                          ( Some fun_loc,
                                            Impossible
                                              "Unexpected env state for maybe_exhaustively_checked_var_name"
                                          )
                                      ))
                              write_locs
                          in
                          Context.add_exhaustive_check cx loc (locs, undeclared))
                        ()
                  )
                in
                env_state <- { env_state with predicate_scope_names = saved_predicate_scope_names };
                this#commit_abrupt_completion_matching
                  AbruptCompletion.(mem [return; throw])
                  completion_state)
              ~finally:(fun () -> this#reset_env env)
        )

      method! class_ loc cls =
        let open Ast.Class in
        SuperCallInDerivedCtorChecker.check cx loc cls;
        (* Give class body the location of the entire class,
           so class body visitor can use it as the def loc of super. *)
        ignore @@ super#class_ loc { cls with body = (loc, snd cls.body) };
        cls

      method! class_identifier_opt ~class_loc:loc id =
        let (class_write_loc, class_self_reason) =
          match id with
          | Some ((name_loc, { Ast.Identifier.name; comments = _ }) as id) ->
            ignore @@ this#pattern_identifier ~kind:Ast.Statement.VariableDeclaration.Let id;
            (name_loc, mk_reason (RType (OrdinaryName name)) name_loc)
          | None ->
            let reason = mk_reason (RType (OrdinaryName "<<anonymous class>>")) loc in
            env_state <-
              {
                env_state with
                write_entries =
                  EnvMap.add_ordinary loc (Env_api.AssigningWrite reason) env_state.write_entries;
              };
            (loc, reason)
        in
        if this#is_assigning_write (Env_api.OrdinaryNameLoc, class_write_loc) then
          let self_write = Env_api.AssigningWrite class_self_reason in
          let this_write =
            Env_api.AssigningWrite (mk_reason (RIdentifier (internal_name "this")) loc)
          in
          let super_write =
            Env_api.AssigningWrite (mk_reason (RIdentifier (internal_name "super")) loc)
          in
          let write_entries =
            env_state.write_entries
            |> EnvMap.add (Env_api.ClassSelfLoc, loc) self_write
            |> EnvMap.add (Env_api.ClassInstanceThisLoc, loc) this_write
            |> EnvMap.add (Env_api.ClassInstanceSuperLoc, loc) super_write
            |> EnvMap.add (Env_api.ClassStaticThisLoc, loc) this_write
            |> EnvMap.add (Env_api.ClassStaticSuperLoc, loc) super_write
          in
          env_state <- { env_state with write_entries }

      method! class_body cls_body =
        let open Ast.Class.Body in
        let (loc, { body; comments }) = cls_body in
        let (static_body, instance_body) =
          let (static_body, instance_body) =
            Base.List.partition_tf body ~f:(function
                | Ast.Class.Body.Method (_, { Ast.Class.Method.static; _ })
                | Ast.Class.Body.Property (_, { Ast.Class.Property.static; _ })
                | Ast.Class.Body.PrivateField (_, { Ast.Class.PrivateField.static; _ })
                -> static
                )
          in
          ( (loc, { Ast.Class.Body.body = static_body; comments }),
            (loc, { Ast.Class.Body.body = instance_body; comments })
          )
        in
        let this_super_bindings =
          Bindings.empty
          |> Bindings.add ((loc, { Ast.Identifier.name = "this"; comments = None }), Bindings.Const)
          |> Bindings.add ((loc, { Ast.Identifier.name = "super"; comments = None }), Bindings.Const)
        in
        ignore
        @@ this#with_scoped_bindings
             ~this_super_binding_env:ClassInstanceEnv
             loc
             this_super_bindings
             super#class_body
             instance_body;
        ignore
        @@ this#with_scoped_bindings
             ~this_super_binding_env:ClassStaticEnv
             loc
             this_super_bindings
             super#class_body
             static_body;
        cls_body

      method private non_this_binding_function loc func = this#arrow_function loc func

      method! class_method _loc meth =
        let { Ast.Class.Method.key; value = (f_loc, f); decorators; _ } = meth in
        ignore @@ this#object_key key;
        (* When there is no `this` annotation, we treat the method like an arrow function:
         * it will now inherit `this` from parent scope.
         *)
        let method_visitor =
          let has_this_param =
            let { Ast.Function.params = (_, { Ast.Function.Params.this_; _ }); _ } = f in
            Base.Option.is_some this_
          in
          if has_this_param then
            this#function_expression_or_method
          else
            this#non_this_binding_function
        in
        (* Use the method key loc as the loc of the function. *)
        ignore @@ method_visitor f_loc f;
        Base.List.iter ~f:(fun d -> ignore @@ this#class_decorator d) decorators;
        meth

      method! class_property loc prop =
        let open Ast.Class.Property in
        let { static; _ } = prop in
        if static then
          super#class_property loc prop
        else
          let env = this#env in
          this#run
            (fun () ->
              this#havoc_uninitialized_env;
              ignore @@ super#class_property loc prop)
            ~finally:(fun () -> this#reset_env env);
          prop

      method! class_private_field loc field =
        let open Ast.Class.PrivateField in
        let { static; _ } = field in
        if static then
          super#class_private_field loc field
        else
          let env = this#env in
          this#run
            (fun () ->
              this#havoc_uninitialized_env;
              ignore @@ super#class_private_field loc field)
            ~finally:(fun () -> this#reset_env env);
          field

      method! object_ loc expr =
        let open Ast.Expression.Object in
        let { properties; comments = _ } = expr in
        Eq_test.object_properties_possible_sentinel_refinements properties
        |> this#add_sentinel_check_writes;
        super#object_ loc expr

      method private add_sentinel_check_writes =
        SMap.iter (fun _ -> function
          | Hint_api.Member reason ->
            let write_entries =
              EnvMap.add
                (Env_api.ExpressionLoc, aloc_of_reason reason)
                (Env_api.AssigningWrite reason)
                env_state.write_entries
            in
            env_state <- { env_state with write_entries }
          | _ -> ()
        )

      method! object_property prop =
        let open Ast.Expression.Object.Property in
        match prop with
        | (_, Init _) -> super#object_property prop
        | (_, Method { key; value = (loc, fn) })
        | (_, Get { key; value = (loc, fn); comments = _ })
        | (_, Set { key; value = (loc, fn); comments = _ }) ->
          ignore @@ this#object_key key;
          let illegal_this_binding =
            Bindings.singleton
              ((loc, { Ast.Identifier.name = "this"; comments = None }), Bindings.Const)
          in
          (* Do not bind this to a function-level this as usual. We use arrow function visitor
             so that we purposely skip this binding, and instead we bind this under a special
             IllegalThisEnv. *)
          ignore
          @@ this#with_scoped_bindings
               ~this_super_binding_env:IllegalThisEnv
               loc
               illegal_this_binding
               (this#non_this_binding_function loc)
               fn;
          prop

      method! declare_function loc expr =
        let {
          Flow_ast.Statement.DeclareFunction.id =
            (id_loc, { Flow_ast.Identifier.name; comments = _ });
          _;
        } =
          expr
        in
        match Declare_function_utils.declare_function_to_function_declaration_simple loc expr with
        | Some stmt ->
          check_predicate_declare_function ~predicate:true (OrdinaryName name) id_loc;
          let _ = this#statement (loc, stmt) in
          expr
        | None ->
          check_predicate_declare_function ~predicate:false (OrdinaryName name) id_loc;
          super#declare_function loc expr

      method! call loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Call.t) =
        (* Traverse everything up front. Now we don't need to worry about missing any reads
         * of identifiers in sub-expressions *)
        ignore @@ super#call loc expr;

        let open Ast.Expression.Call in
        let { callee; targs; arguments; _ } = expr in
        if Flow_ast_utils.is_call_to_invariant callee then
          match (targs, arguments) with
          (* invariant() and invariant(false, ...) are treated like throw *)
          | (None, (_, { Ast.Expression.ArgList.arguments = []; comments = _ })) ->
            this#raise_abrupt_completion AbruptCompletion.throw
          | ( None,
              ( _,
                {
                  Ast.Expression.ArgList.arguments =
                    Ast.Expression.Expression
                      ( _,
                        Ast.Expression.Literal { Ast.Literal.value = Ast.Literal.Boolean false; _ }
                      )
                    :: other_args;
                  comments = _;
                }
              )
            ) ->
            let _ = List.map this#expression_or_spread other_args in
            this#raise_abrupt_completion AbruptCompletion.throw
          | ( None,
              ( _,
                {
                  Ast.Expression.ArgList.arguments = Ast.Expression.Expression cond :: other_args;
                  comments = _;
                }
              )
            ) ->
            this#push_refinement_scope empty_refinements;
            ignore @@ this#expression_refinement cond;
            let _ = List.map this#expression_or_spread other_args in
            this#pop_refinement_scope_without_unrefining ()
          | ( _,
              (_, { Ast.Expression.ArgList.arguments = Ast.Expression.Spread _ :: _; comments = _ })
            ) ->
            error_todo
          | (Some _, _) -> error_todo
        else
          this#havoc_current_env ~all:false;
        expr

      method! call_arguments arg_list =
        ( if Context.lti cx then
          let (_, { Ast.Expression.ArgList.arguments; comments = _ }) = arg_list in
          Base.List.iter arguments ~f:(fun arg ->
              let ((loc, _) as expr) =
                match arg with
                | Ast.Expression.Expression expr -> expr
                | Ast.Expression.Spread (_, spread) -> spread.Ast.Expression.SpreadElement.argument
              in
              if not @@ Provider_api.is_array_provider provider_info loc then
                let write_entries =
                  EnvMap.add
                    (Env_api.ExpressionLoc, loc)
                    (Env_api.AssigningWrite (Reason.mk_expression_reason expr))
                    env_state.write_entries
                in
                env_state <- { env_state with write_entries }
          )
        );
        super#call_arguments arg_list

      method! new_ loc (expr : (ALoc.t, ALoc.t) Ast.Expression.New.t) =
        ignore @@ super#new_ loc expr;
        this#havoc_current_env ~all:false;
        expr

      method private delete loc argument =
        let undefined_reason = mk_reason RVoid loc in
        let undefined = Val.undefined undefined_reason in
        match argument with
        | (_, Flow_ast.Expression.Identifier (id_loc, { Flow_ast.Identifier.name; _ }))
          when not @@ this#is_excluded_ordinary_name name ->
          let { kind; def_loc; val_ref; _ } = this#env_read name in
          (match error_for_assignment_kind cx name id_loc def_loc kind AssignmentWrite !val_ref with
          | None -> val_ref := undefined
          | Some err ->
            env_state <-
              {
                env_state with
                write_entries =
                  EnvMap.add_ordinary
                    (fst argument)
                    Env_api.NonAssigningWrite
                    env_state.write_entries;
              };
            add_output err)
        | (_, Flow_ast.Expression.Member member) ->
          this#assign_member ~delete:true member loc undefined undefined_reason
        | _ -> ()

      method! unary_expression loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Unary.t) =
        Ast.Expression.Unary.(
          let { argument; operator; comments = _ } = expr in
          ignore @@ this#expression argument;
          match operator with
          | Await -> this#havoc_current_env ~all:false
          | Delete -> this#delete loc argument
          | _ -> ()
        );
        expr

      method! yield loc (expr : ('loc, 'loc) Ast.Expression.Yield.t) =
        this#any_identifier loc next_var_name;
        ignore @@ super#yield loc expr;
        this#havoc_current_env ~all:true;
        expr

      method! object_key_computed (key : ('loc, 'loc) Ast.ComputedKey.t) =
        let open Ast.ComputedKey in
        let (_, { expression; comments = _ }) = key in

        let (expression_loc, _) = expression in
        let reason = Reason.mk_expression_reason expression in
        let write_entries =
          EnvMap.add
            (Env_api.ExpressionLoc, expression_loc)
            (Env_api.AssigningWrite reason)
            env_state.write_entries
        in
        env_state <- { env_state with write_entries };

        let _expression : (_, _) Ast.Expression.t = this#expression expression in
        key

      (* Labeled statements handle labeled breaks, but also push labeled continues
         that are expected to be handled by immediately nested loops. *)
      method! labeled_statement _loc (stmt : (ALoc.t, ALoc.t) Ast.Statement.Labeled.t) =
        this#expecting_abrupt_completions (fun () ->
            let open Ast.Statement.Labeled in
            let { label; body; comments = _ } = stmt in
            env_state <-
              {
                env_state with
                possible_labeled_continues =
                  AbruptCompletion.continue (Some label) :: env_state.possible_labeled_continues;
              };
            let completion_state =
              this#run_to_completion (fun () -> ignore @@ this#statement body)
            in
            env_state <- { env_state with possible_labeled_continues = [] };
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break (Some label)])
              completion_state
        );
        stmt

      method! statement (stmt : (ALoc.t, ALoc.t) Ast.Statement.t) =
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
          | _ -> env_state <- { env_state with possible_labeled_continues = [] }
        end;
        super#statement stmt

      (* Function declarations are hoisted to the top of a block, so that they may be considered
         initialized before they are read. *)
      method! statement_list (stmts : (ALoc.t, ALoc.t) Ast.Statement.t list) =
        ignore @@ super#statement_list (Flow_ast_utils.hoist_function_declarations stmts);
        stmts

      (* WHen the refinement scope we push is non-empty we want to make sure that the variables
       * that scope refines are given their new refinement writes in the environment *)
      method private push_refinement_scope
          ({ applied; changeset; total = _ } as new_latest_refinements) =
        env_state <-
          {
            env_state with
            latest_refinements = new_latest_refinements :: env_state.latest_refinements;
          };
        IMap.iter
          (fun ssa_id (lookup, refinement_id) ->
            let refine_val v =
              if Val.base_id_of_val v = ssa_id then
                Val.refinement refinement_id v
              else
                v
            in
            let create_val_for_heap =
              LookupMap.find_opt lookup changeset |> Base.Option.map ~f:Lazy.from_val
            in
            this#map_val_with_lookup lookup ?create_val_for_heap refine_val)
          applied

      (* See pop_refinement_scope. The only difference here is that we unrefine values deeply
       * instead of just at the top level. The reason for this is that intermediate control-flow
       * can introduce refinement writes into phi nodes, and we don't want those refinements to
       * escape the scope of the loop. You may find it instructive to change the calls to
       * just pop_refinement_scope to see the behavioral differences *)
      method private pop_refinement_scope_after_loop () =
        let { applied; _ } = List.hd env_state.latest_refinements in
        env_state <- { env_state with latest_refinements = List.tl env_state.latest_refinements };
        applied
        |> IMap.iter (fun _ (lookup, refinement_id) ->
               let unrefine_deeply x = Val.unrefine_deeply refinement_id x in
               this#map_val_with_lookup lookup unrefine_deeply
           )

      (* Invariant refinement scopes can be popped, but the refinement should continue living on.
       * To model that, we pop the refinement scope but do not unrefine the refinements. The
       * refinements live on in the Refinement writes in the env. *)
      method private pop_refinement_scope_without_unrefining () =
        env_state <- { env_state with latest_refinements = List.tl env_state.latest_refinements }

      (* When a refinement scope ends, we need to undo the refinement applied to the
       * variables mentioned in the latest_refinements head. Some of these values may no
       * longer be the refined value, in which case Val.unrefine will be a no-op. Otherwise,
       * the Refinement Val.t is replaced with the original Val.t that was being refined, with
       * the same original ssa_id. That means that if for some reason you needed to push the refinement
       * scope again that you would re-refine the unrefined variables, which is desirable in cases
       * where we juggle refinement scopes like we do for nullish coalescing *)
      method private pop_refinement_scope () =
        let { applied; _ } = List.hd env_state.latest_refinements in
        env_state <- { env_state with latest_refinements = List.tl env_state.latest_refinements };
        applied
        |> IMap.iter (fun _ (lookup, refinement_id) ->
               let unrefine x = Val.unrefine refinement_id x in
               this#map_val_with_lookup lookup unrefine
           )

      method private peek_new_refinements () = List.hd env_state.latest_refinements

      method private negate_refinements { applied = _; changeset = _; total } =
        match total with
        | None -> empty_refinements
        | Some total ->
          let negated = Not total in
          let (applied, changeset) = this#normalize_total_refinements negated in
          { applied; changeset; total = Some negated }

      method private conjunct_all_refinements refinement_scopes =
        match refinement_scopes with
        | [] -> empty_refinements
        | [x] -> x
        | _ -> List.fold_left (this#merge ~conjunction:true) empty_refinements refinement_scopes

      method private negate_new_refinements () =
        let head = List.hd env_state.latest_refinements in
        let new_latest_refinements = this#negate_refinements head in
        this#pop_refinement_scope ();
        this#push_refinement_scope new_latest_refinements

      method private merge_self_refinement_scope { applied = _; changeset = _; total } =
        let { applied = _; changeset = _; total = head_total } =
          List.hd env_state.latest_refinements
        in
        let merged = conj_total head_total total in
        let (applied, changeset) =
          Base.Option.value_map
            ~f:this#normalize_total_refinements
            ~default:(IMap.empty, LookupMap.empty)
            merged
        in
        let refis = { applied; changeset; total = merged } in
        this#pop_refinement_scope ();
        this#push_refinement_scope refis

      method private normalize_total_refinements total =
        let rec nnf total =
          match total with
          | Not (And (t1, t2)) -> Or (nnf (Not t1), nnf (Not t2))
          | Not (Or (t1, t2)) -> And (nnf (Not t1), nnf (Not t2))
          | Not (Not t) -> nnf t
          | And (t1, t2) -> And (nnf t1, nnf t2)
          | Or (t1, t2) -> Or (nnf t1, nnf t2)
          | Refinements _
          | Not (Refinements _) ->
            total
        in
        let rec recur total =
          match total with
          | Refinements (r, c) -> (r, c)
          | Not (Refinements (r, c)) ->
            ( IMap.map
                (fun (lookup, refinement_id) ->
                  let new_refinement_id = this#new_id () in
                  env_state <-
                    {
                      env_state with
                      refinement_heap =
                        IMap.add new_refinement_id (NOT refinement_id) env_state.refinement_heap;
                    };
                  (lookup, new_refinement_id))
                r,
              c
            )
          | Not _ -> raise Env_api.(Env_invariant (None, Impossible "Negations not resolved"))
          | And (t1, t2) ->
            let (r1, c1) = recur t1 in
            let (r2, c2) = recur t2 in
            let r =
              IMap.union
                ~combine:(fun _ (lookup1, rid1) (lookup2, rid2) ->
                  if lookup1 = lookup2 then begin
                    let new_refinement_id = this#new_id () in
                    env_state <-
                      {
                        env_state with
                        refinement_heap =
                          IMap.add new_refinement_id (AND (rid1, rid2)) env_state.refinement_heap;
                      };
                    Some (lookup1, new_refinement_id)
                  end else
                    None)
                r1
                r2
            in
            let c = LookupMap.union c1 c2 in
            (r, c)
          | Or (t1, t2) ->
            let (r1, c1) = recur t1 in
            let (r2, c2) = recur t2 in
            let r =
              IMap.merge
                (fun _ r1 r2 ->
                  match (r1, r2) with
                  | (None, _)
                  | (_, None) ->
                    None
                  | (Some (lookup1, rid1), Some (lookup2, rid2)) ->
                    if lookup1 = lookup2 then begin
                      let new_refinement_id = this#new_id () in
                      env_state <-
                        {
                          env_state with
                          refinement_heap =
                            IMap.add new_refinement_id (OR (rid1, rid2)) env_state.refinement_heap;
                        };
                      Some (lookup1, new_refinement_id)
                    end else
                      None)
                r1
                r2
            in
            let c = LookupMap.union c1 c2 in
            (r, c)
        in
        let nnf = nnf total in
        recur nnf

      (* Commit a set of refinements, all of which are linked biconditionally. If these refinements are negated,
         then the negation of each refinement should hold; separate calls to this function in the same
         refinement scope produce separate maps that are connected with an AND--and if they are negated, will produce
         an OR of negations of the maps.

         To see how this works, consider two cases:

          First,
            if (x && y) { } else { }
          In this case, we will call commit_refinement twice, each time with a Truthy refinement on either x or y.
          Inside the then case, x and y are both Truthy, but within the else case, we'll end up with a proposition
          like Or (x Falsey, y Falsey). This means that within the else case we don't actually know anything concrete
          about x or y.

          Compare that to
            if {x.y} { } else { }
          For this case, we will call commit_refinement only once, with a map that contains both the refinement x HasProp(y) and
          x.y Truthy. When we negate this, because we only called commit_refinement once and therefore our refinement propositon
          contains just a single set of refinements (not multiple ones conjuncted together), we'll end up with
          x NotHasProp(y) and x.y Falsey--which is what we want.
      *)
      method private commit_refinement (refinements : (ALoc.t * refinement) LookupMap.t) =
        let { applied; changeset = old_changeset; total } = List.hd env_state.latest_refinements in

        let (applied, changeset, map) =
          LookupMap.fold
            (fun key (loc, refinement) (applied, changeset, map) ->
              (* Prevent refinement on undeclared const/let.
                 We should only error if the undeclared const/let is in the same activation scope
                 as the current one. Although we have no information about the current scope, this
                 is not a problem. We will force initialization for all bindings before we visit a
                 lambda. *)
              let should_not_refine =
                let { RefinementKey.base; projections } = key in
                if projections = [] then
                  match this#env_read base with
                  | { val_ref; kind = Bindings.Const | Bindings.Let; def_loc = Some def_loc; _ }
                    when Val.is_undeclared_or_skipped !val_ref ->
                    refinement
                    |> fst
                    |> L.LSet.iter (fun loc ->
                           add_output
                             Error_message.(
                               EBindingError
                                 (EReferencedBeforeDeclaration, loc, OrdinaryName base, def_loc)
                             )
                       );
                    true
                  | _ -> false
                else
                  false
              in
              if should_not_refine then
                (applied, changeset, map)
              else
                let add_refinements v =
                  let ssa_id = Val.base_id_of_val v in
                  let refinement_id = this#new_id () in
                  env_state <-
                    {
                      env_state with
                      refinement_heap =
                        IMap.add refinement_id (BASE refinement) env_state.refinement_heap;
                    };
                  let latest_refinement_opt = IMap.find_opt ssa_id applied in
                  let (final_refinement_id, unrefined_v) =
                    match latest_refinement_opt with
                    | Some (_, existing_refinement_id) ->
                      let unrefined_v = Val.unrefine existing_refinement_id v in
                      let new_refinement_id = this#new_id () in
                      let new_chain = AND (existing_refinement_id, refinement_id) in
                      env_state <-
                        {
                          env_state with
                          refinement_heap =
                            IMap.add new_refinement_id new_chain env_state.refinement_heap;
                        };

                      (new_refinement_id, unrefined_v)
                    | None -> (refinement_id, v)
                  in
                  ( (ssa_id, refinement_id, final_refinement_id),
                    Val.refinement final_refinement_id unrefined_v
                  )
                in
                match
                  this#map_val_with_lookup_result
                    key
                    ~create_val_for_heap:
                      ( lazy
                        (let reason =
                           mk_reason
                             (RefinementKey.reason_desc { RefinementKey.lookup = key; loc })
                             loc
                         in
                         let write_entries =
                           EnvMap.add_ordinary
                             loc
                             (Env_api.AssigningWrite reason)
                             env_state.write_entries
                         in
                         env_state <- { env_state with write_entries };
                         Val.projection loc
                        )
                        )
                    add_refinements
                with
                | Some ((ssa_id, base_refinement_id, final_refinement_id), change) ->
                  ( IMap.add ssa_id (key, final_refinement_id) applied,
                    Base.Option.value_map
                      ~f:(fun change -> LookupMap.add key change changeset)
                      ~default:changeset
                      change,
                    IMap.add ssa_id (key, base_refinement_id) map
                  )
                | None -> (applied, changeset, map))
            refinements
            (applied, LookupMap.empty, IMap.empty)
        in

        env_state <-
          {
            env_state with
            latest_refinements =
              {
                applied;
                changeset = LookupMap.union changeset old_changeset;
                total = conj_total total (Some (Refinements (map, changeset)));
              }
              :: List.tl env_state.latest_refinements;
          }

      method add_single_refinement key refi = this#commit_refinement (this#start_refinement key refi)

      method start_refinement { RefinementKey.loc; lookup } refi =
        LookupMap.singleton lookup (loc, refi)

      method extend_refinement { RefinementKey.loc; lookup } refi refis =
        LookupMap.union
          ~combine:(fun _ (loc1, (locs1, refi1)) (loc2, (locs2, refi2)) ->
            if loc1 <> loc2 then raise Env_api.(Env_invariant (Some loc, Impossible "Loc mismatch"));
            Some (loc1, (L.LSet.union locs1 locs2, AndR (refi1, refi2))))
          refis
          (LookupMap.singleton lookup (loc, refi))

      method identifier_refinement ((loc, ident) as identifier) =
        ignore @@ this#identifier identifier;
        let { Flow_ast.Identifier.name; _ } = ident in
        let { val_ref; _ } = this#env_read name in
        if not (Val.is_undeclared_or_skipped !val_ref) then
          this#add_single_refinement (RefinementKey.of_name name loc) (L.LSet.singleton loc, TruthyR)

      method assignment_refinement loc assignment =
        ignore @@ this#assignment loc assignment;
        let open Flow_ast.Expression.Assignment in
        match assignment.left with
        | ( id_loc,
            Flow_ast.Pattern.Identifier
              { Flow_ast.Pattern.Identifier.name = (_, { Flow_ast.Identifier.name; _ }); _ }
          ) ->
          this#add_single_refinement
            (RefinementKey.of_name name id_loc)
            (L.LSet.singleton loc, TruthyR)
        | _ -> ()

      method private merge
          ~conjunction
          { applied = _; changeset = _; total = total1 }
          { applied = _; changeset = _; total = total2 } =
        let total =
          if conjunction then
            match (total1, total2) with
            | (Some total1, Some total2) -> Some (And (total1, total2))
            | (Some total, _)
            | (_, Some total) ->
              Some (And (total, Refinements (IMap.empty, LookupMap.empty)))
            | (None, None) -> None
          else
            match (total1, total2) with
            | (Some total1, Some total2) -> Some (Or (total1, total2))
            | (Some total, _)
            | (_, Some total) ->
              Some (Or (total, Refinements (IMap.empty, LookupMap.empty)))
            | (None, None) -> None
        in
        let (applied, changeset) =
          Base.Option.value_map
            ~f:this#normalize_total_refinements
            ~default:(IMap.empty, LookupMap.empty)
            total
        in
        { applied; changeset; total }

      method private merge_refinement_scopes
          ~conjunction lhs_latest_refinements rhs_latest_refinements =
        let new_latest_refinements =
          this#merge ~conjunction lhs_latest_refinements rhs_latest_refinements
        in
        this#merge_self_refinement_scope new_latest_refinements

      (* Refines an expr if that expr has a refinement key, othewise does nothing *)
      method add_refinement_to_expr expr refinement =
        match RefinementKey.of_expression expr with
        | None -> ()
        | Some refinement_key -> this#add_single_refinement refinement_key refinement

      method logical_refinement expr =
        let { Flow_ast.Expression.Logical.operator; left = (loc, _) as left; right; comments = _ } =
          expr
        in
        this#push_refinement_scope empty_refinements;
        (* The RHS is _only_ evaluated if the LHS fails its check. That means that patterns like
         * x || invariant(false) should propagate the truthy refinement to the next line. We keep track
         * of the completion state on the rhs to do that. If the LHS throws then the entire expression
         * throws, so there's no need to catch the exception from the LHS *)
        let (lhs_latest_refinements, rhs_latest_refinements, env1, rhs_completion_state) =
          match operator with
          | Flow_ast.Expression.Logical.Or
          | Flow_ast.Expression.Logical.And ->
            ignore @@ this#expression_refinement left;
            let lhs_latest_refinements_or = this#peek_new_refinements () in
            let env1 = this#env_without_latest_refinements in
            (match operator with
            | Flow_ast.Expression.Logical.Or -> this#negate_new_refinements ()
            | _ -> ());
            this#push_refinement_scope empty_refinements;
            let rhs_completion_state =
              this#run_to_completion (fun () -> ignore @@ this#expression_refinement right)
            in
            let rhs_latest_refinements = this#peek_new_refinements () in
            (* Pop RHS refinement scope *)
            this#pop_refinement_scope ();
            let lhs_latest_refinements =
              (* If this is `and`, we want to save the LHS refinements that may have been havoced by the RHS.
                 If this is `or`, the RHS did not fire if the LHS was truthy, so we want the original, un-havoced
                 refinements *)
              match operator with
              | Flow_ast.Expression.Logical.Or -> lhs_latest_refinements_or
              | _ -> this#peek_new_refinements ()
            in
            (* Pop LHS refinement scope *)
            this#pop_refinement_scope ();
            (lhs_latest_refinements, rhs_latest_refinements, env1, rhs_completion_state)
          | Flow_ast.Expression.Logical.NullishCoalesce ->
            (* If this overall expression is truthy, then either the LHS or the RHS has to be truthy.
               If it's because the LHS is truthy, then the LHS also has to be non-maybe (this is of course
               true by definition, but it's also true because of the nature of ??).
               But if we're evaluating the RHS, the LHS doesn't have to be truthy, it just has to be
               non-maybe. As a result, we do this weird dance of refinements so that when we traverse the
               RHS we have done the null-test but the overall result of this expression includes both the
               truthy and non-maybe qualities.

               We can't use null_test here because null_test requires some actual null for the
               sentinel refinement it can create. We can add some complexity here to introduce a
               synthetic null Val.t and get sentinel refinements against null here, but that seems
               like an unlikely way for nullish coalescing to be used. Instead, we simply add a
               NotR MaybeR refinement to the left *)
            ignore (this#expression left);
            this#add_refinement_to_expr left (L.LSet.singleton loc, NotR MaybeR);
            let nullish = this#peek_new_refinements () in
            let env1 = this#env_without_latest_refinements in
            this#negate_new_refinements ();
            this#push_refinement_scope empty_refinements;
            let rhs_completion_state =
              this#run_to_completion (fun () -> ignore (this#expression_refinement right))
            in
            let rhs_latest_refinements = this#peek_new_refinements () in
            this#pop_refinement_scope ();
            this#pop_refinement_scope ();
            this#push_refinement_scope empty_refinements;
            this#add_refinement_to_expr left (L.LSet.singleton loc, TruthyR);
            let truthy_refinements = this#peek_new_refinements () in
            this#pop_refinement_scope ();
            this#push_refinement_scope empty_refinements;
            this#merge_refinement_scopes ~conjunction:true nullish truthy_refinements;
            let lhs_latest_refinements = this#peek_new_refinements () in
            this#pop_refinement_scope ();
            (lhs_latest_refinements, rhs_latest_refinements, env1, rhs_completion_state)
        in
        let conjunction =
          match operator with
          | Flow_ast.Expression.Logical.Or
          | Flow_ast.Expression.Logical.NullishCoalesce ->
            false
          | Flow_ast.Expression.Logical.And -> true
        in
        match rhs_completion_state with
        | Some AbruptCompletion.Throw ->
          let env2 = this#env in
          this#reset_env env1;
          this#push_refinement_scope lhs_latest_refinements;
          this#pop_refinement_scope_without_unrefining ();
          this#merge_self_env env2
        | _ ->
          this#merge_self_env env1;
          this#merge_refinement_scopes ~conjunction lhs_latest_refinements rhs_latest_refinements

      method null_test ~sense ~strict loc expr other =
        (* Negating if sense is false is handled by negate_new_refinements. *)
        let refis = this#maybe_sentinel ~sense:true ~strict loc expr other in
        let refis =
          match RefinementKey.of_expression expr with
          | None -> refis
          | Some key ->
            let refinement =
              if strict then
                NullR
              else
                NotR MaybeR
            in
            this#extend_refinement key (L.LSet.singleton loc, refinement) refis
        in
        ignore @@ this#optional_chain expr;
        this#commit_refinement refis;
        if strict <> sense then this#negate_new_refinements ()

      method void_test ~sense ~strict ~check_for_bound_undefined loc expr other =
        ignore @@ this#expression other;
        (* Negating if sense is true is handled by negate_new_refinements. *)
        let refis = this#maybe_sentinel ~sense:false ~strict loc expr other in
        let is_global_undefined () =
          match this#env_read_opt "undefined" with
          | None -> false
          | Some { val_ref = v; _ } -> Val.is_global_undefined !v
        in
        if (not check_for_bound_undefined) || is_global_undefined () then begin
          let refis =
            match RefinementKey.of_expression expr with
            | None -> refis
            | Some key ->
              let refinement =
                if strict then
                  NotR UndefinedR
                else
                  NotR MaybeR
              in
              this#extend_refinement key (L.LSet.singleton loc, refinement) refis
          in
          ignore @@ this#optional_chain expr;
          this#commit_refinement refis;
          if sense then this#negate_new_refinements ()
        end else begin
          ignore @@ this#optional_chain expr;
          this#commit_refinement refis
        end

      method typeof_test loc arg (str_loc, _) typename sense =
        let (refinement, undef) =
          match typename with
          | "boolean" -> (Some (BoolR loc), false)
          | "function" -> (Some FunctionR, false)
          | "number" -> (Some (NumberR loc), false)
          | "object" -> (Some ObjectR, false)
          | "string" -> (Some (StringR loc), false)
          | "symbol" -> (Some (SymbolR loc), false)
          | "undefined" -> (Some UndefinedR, true)
          | "bigint" -> (Some (BigIntR loc), false)
          | _ ->
            add_output Error_message.(EInvalidTypeof (str_loc, typename));
            (None, false)
        in
        match (refinement, RefinementKey.of_expression arg) with
        | (Some ref, Some refinement_key) ->
          ignore @@ this#optional_chain arg;
          let refinement =
            if sense && not undef then
              ref
            else
              NotR ref
          in
          this#add_single_refinement refinement_key (L.LSet.singleton loc, refinement);
          if sense && undef then this#negate_new_refinements ()
        | _ -> ignore @@ this#optional_chain arg

      method literal_test ~strict ~sense loc expr refinement other =
        (* Negating if sense is false is handled by negate_new_refinements. *)
        let refis = this#maybe_sentinel ~sense:true ~strict loc expr other in
        let refis =
          match RefinementKey.of_expression expr with
          | Some ({ RefinementKey.lookup; loc = _ } as key) when strict ->
            (match lookup with
            | { RefinementKey.base = "this" | "super"; projections = [] } ->
              (* This preserves the old env behavior that it does not perform literal subtyping check
                 against this and super.
                 TODO: error on this in the new-env. *)
              ()
            | { RefinementKey.base; projections = [] } ->
              let { val_ref = _; def_loc; _ } = this#env_read base in
              (match def_loc with
              | None -> ()
              | Some def_loc -> add_literal_subtype_test def_loc refinement)
            | _ -> ());
            this#extend_refinement key (L.LSet.singleton loc, refinement) refis
          | _ -> refis
        in
        ignore @@ this#optional_chain expr;
        this#commit_refinement refis;
        if not sense then this#negate_new_refinements ()

      method maybe_sentinel ~sense ~strict loc expr (other_loc, _) =
        let open Flow_ast in
        let expr' =
          match expr with
          | (loc, Expression.OptionalMember { Expression.OptionalMember.member; _ }) ->
            (loc, Expression.Member member)
          | _ -> expr
        in
        let refis =
          match (strict, expr') with
          | ( true,
              ( _,
                Expression.Member
                  {
                    Expression.Member._object;
                    property =
                      ( Expression.Member.PropertyIdentifier
                          (ploc, { Identifier.name = prop_name; _ })
                      | Expression.Member.PropertyExpression
                          (ploc, Expression.Literal { Literal.value = Literal.String prop_name; _ })
                        );
                    _;
                  }
              )
            ) ->
            (match RefinementKey.of_expression _object with
            | Some refinement_key ->
              let reason = mk_reason (RProperty (Some (OrdinaryName prop_name))) ploc in
              ( if RefinementKey.(refinement_key.lookup.projections) = [] then
                let { val_ref = _; def_loc; _ } =
                  this#env_read RefinementKey.(refinement_key.lookup.base)
                in
                Base.Option.iter def_loc ~f:(fun def_loc ->
                    Context.add_matching_props cx (prop_name, other_loc, def_loc)
                )
              );
              let write_entries =
                EnvMap.add
                  (Env_api.ExpressionLoc, other_loc)
                  (Env_api.AssigningWrite reason)
                  env_state.write_entries
              in
              env_state <- { env_state with write_entries };
              let refinement =
                if sense then
                  SentinelR (prop_name, other_loc)
                else
                  NotR (SentinelR (prop_name, other_loc))
              in
              this#start_refinement refinement_key (L.LSet.singleton loc, refinement)
            | None -> LookupMap.empty)
          | _ -> LookupMap.empty
        in
        refis

      method eq_test ~strict ~sense ~cond_context loc left right =
        Eq_test.visit_eq_test
          ~on_type_of_test:this#typeof_test
          ~on_literal_test:this#literal_test
          ~on_null_test:this#null_test
          ~on_void_test:this#void_test
            (* Member expressions compared against non-literals that include
               * an optional chain cannot refine like we do in literal cases. The
               * non-literal value we are comparing against may be null or undefined,
               * in which case we'd need to use the special case behavior. Since we can't
               * know at this point, we conservatively do not refine at all based on optional
               * chains by ignoring the output of maybe_sentinel.
               *
               * NOTE: Switch statements do not introduce sentinel refinements *)
          ~on_member_eq_other:(fun expr other ->
            ignore @@ this#expression expr;
            let refis = this#maybe_sentinel ~sense ~strict loc expr other in
            ignore @@ this#expression other;
            this#commit_refinement refis)
          ~on_other_eq_member:(fun other expr ->
            ignore @@ this#expression other;
            ignore @@ this#expression expr;
            let refis = this#maybe_sentinel ~sense ~strict loc expr other in
            this#commit_refinement refis)
          ~is_switch_cond_context:(cond_context = SwitchTest)
          ~on_other_eq_test:(fun left right ->
            ignore @@ this#expression left;
            ignore @@ this#expression right)
          ~strict
          ~sense
          loc
          left
          right

      method instance_test loc expr instance =
        ignore @@ this#optional_chain expr;
        ignore @@ this#expression instance;
        match RefinementKey.of_expression expr with
        | None -> ()
        | Some refinement_key ->
          let (instance_loc, _) = instance in
          (* instance is not something the name_resolver can reason about.
             However, we still need to has a read and write entry, so we can
             record it in statement.ml and use it in new-env. *)
          let reason = mk_reason (RefinementKey.reason_desc refinement_key) instance_loc in
          let write_entries =
            EnvMap.add
              (Env_api.ExpressionLoc, instance_loc)
              (Env_api.AssigningWrite reason)
              env_state.write_entries
          in
          env_state <- { env_state with write_entries };
          this#add_single_refinement refinement_key (L.LSet.singleton loc, InstanceOfR instance)

      method binary_refinement loc expr =
        let open Flow_ast.Expression.Binary in
        let { operator; left; right; comments = _ } = expr in
        let eq_test = this#eq_test ~cond_context:OtherTest in
        match operator with
        (* == and != refine if lhs or rhs is an ident and other side is null *)
        | Equal -> eq_test ~strict:false ~sense:true loc left right
        | NotEqual -> eq_test ~strict:false ~sense:false loc left right
        | StrictEqual -> eq_test ~strict:true ~sense:true loc left right
        | StrictNotEqual -> eq_test ~strict:true ~sense:false loc left right
        | Instanceof -> this#instance_test loc left right
        | LessThan
        | LessThanEqual
        | GreaterThan
        | GreaterThanEqual
        | In
        | LShift
        | RShift
        | RShift3
        | Plus
        | Minus
        | Mult
        | Exp
        | Div
        | Mod
        | BitOr
        | Xor
        | BitAnd ->
          ignore @@ this#binary loc expr

      method call_refinement loc call =
        match call with
        | {
         Flow_ast.Expression.Call.callee;
         targs = _;
         arguments =
           ( _,
             {
               Flow_ast.Expression.ArgList.arguments = [Flow_ast.Expression.Expression arg];
               comments = _;
             }
           );
         comments = _;
        }
          when Flow_ast_utils.is_call_to_is_array callee ->
          let refi =
            match RefinementKey.of_expression arg with
            | None -> LookupMap.empty
            | Some refinement_key ->
              this#start_refinement refinement_key (L.LSet.singleton loc, IsArrayR)
          in

          ignore @@ this#expression callee;
          ignore @@ this#optional_chain arg;
          this#commit_refinement refi
        (* Latent refinements are only applied on function calls where the function call is an identifier *)
        | {
         Flow_ast.Expression.Call.callee = (_, Flow_ast.Expression.Identifier _) as callee;
         arguments;
         _;
        }
          when not (Flow_ast_utils.is_call_to_invariant callee) ->
          (* This case handles predicate functions. We ensure that this
           * is not a call to invariant and that the callee is an identifier.
           * The only other criterion that must be met for this call to produce
           * a refinement is that the arguments cannot contain a spread.
           *
           * Assuming there are no spreads we create a mapping from each argument
           * index to the refinement key at that index.
           *
           * The semantics for passing the same argument multiple times to predicate
           * function are sketchy. Pre-LTI Flow allows you to do this but it is buggy. See
           * https://fburl.com/vf52s7rb on v0.155.0
           *
           * We should strongly consider disallowing the same refinement key to
           * appear multiple times in the arguments. *)
          let { Flow_ast.Expression.ArgList.arguments = arglist; _ } = snd arguments in
          let is_spread = function
            | Flow_ast.Expression.Spread _ -> true
            | _ -> false
          in
          let refinement_keys =
            if List.exists is_spread arglist then
              []
            else
              List.map (fun arg -> RefinementKey.of_argument arg) arglist
          in
          ignore @@ this#expression callee;
          ignore @@ this#call_arguments arguments;
          this#havoc_current_env ~all:false;
          this#apply_latent_refinements callee refinement_keys
        | _ -> ignore @@ this#call loc call

      method unary_refinement
          loc ({ Flow_ast.Expression.Unary.operator; argument; comments = _ } as unary) =
        match operator with
        | Flow_ast.Expression.Unary.Not ->
          this#push_refinement_scope empty_refinements;
          ignore @@ this#expression_refinement argument;
          this#negate_new_refinements ();
          let negated_refinements = this#peek_new_refinements () in
          this#pop_refinement_scope ();
          this#merge_self_refinement_scope negated_refinements
        | _ -> ignore @@ this#unary_expression loc unary

      method private record_member_read (loc, expr) =
        let open Ast.Expression in
        match expr with
        | OptionalMember _
        | Member _ ->
          (match this#get_val_of_expression (loc, expr) with
          | None ->
            (* In some cases, we may re-visit the same expression multiple times via different
               environments--for example, visiting the discriminant of a switch statement. In
               these cases, it's possible that there was a val for an expression in a previous
               environment which is no longer available when seen through a subsequent. In order
               to prevent old environment values from "leaking" through, we need to actively remove
               values that may have previously existed but no longer do. *)
            let values = L.LMap.remove loc env_state.values in
            env_state <- { env_state with values }
          | Some refined_v ->
            (* We model a heap refinement as a separate const binding. We prefer this over using
             * None so that we can report errors when using this value in a type position *)
            let values =
              L.LMap.add
                loc
                {
                  def_loc = None;
                  value = refined_v;
                  binding_kind_opt = Some Bindings.Const;
                  name = None;
                }
                env_state.values
            in
            env_state <- { env_state with values })
        | _ -> ()

      method private recursively_record_member_read expr =
        let open Ast.Expression in
        (match expr with
        | (_, OptionalMember { OptionalMember.member = { Member._object; _ }; _ })
        | (_, Member { Member._object; _ }) ->
          this#recursively_record_member_read _object
        | _ -> ());
        this#record_member_read expr

      method optional_chain (loc, expr) =
        let open Ast.Expression in
        this#record_member_read (loc, expr);
        let () =
          match expr with
          | OptionalMember _ -> this#member_expression_refinement loc expr LookupMap.empty
          | OptionalCall
              {
                OptionalCall.call = { Call.callee; targs; arguments; comments = _ };
                optional = _;
                filtered_out = _;
              } ->
            (* TODO: Currently, optional call foo?.(...) is not modeled as NotR (MaybeR) on foo and
               then call foo(...) at all. Before MethodT is un-entangled, let's not add the
               non-maybe refinement on callee for now. *)
            let refi = LookupMap.empty in

            ignore @@ this#optional_chain callee;
            this#commit_refinement refi;
            let _targs' = Base.Option.map ~f:this#call_type_args targs in
            let _arguments' = this#call_arguments arguments in
            this#havoc_current_env ~all:false
          | Member mem -> ignore @@ this#member loc mem
          | Call call -> ignore @@ this#call loc call
          | _ -> ignore @@ this#expression (loc, expr)
        in
        (loc, expr)

      method member_expression_refinement loc expr refis =
        let open Flow_ast.Expression in
        let open Flow_ast.Expression.Member in
        let optional =
          match expr with
          | OptionalMember { Ast.Expression.OptionalMember.optional; _ } -> optional
          | _ -> false
        in
        match expr with
        | OptionalMember OptionalMember.{ member = { Member._object; property; _ }; _ }
        | Member Member.{ _object; property; _ } ->
          ignore @@ this#optional_chain _object;
          let propname =
            match property with
            | PropertyIdentifier (_, { Flow_ast.Identifier.name; _ })
            | PropertyExpression (_, Literal { Flow_ast.Literal.value = Ast.Literal.String name; _ })
            | PropertyExpression
                (_, Literal { Flow_ast.Literal.value = Ast.Literal.Number _; raw = name; _ }) ->
              Some name
            | _ -> None
          in
          (match RefinementKey.of_expression _object with
          | None -> ignore @@ this#member_property property
          | Some refinement_key_obj ->
            if optional then
              this#add_single_refinement refinement_key_obj (L.LSet.singleton loc, NotR MaybeR);
            ignore @@ this#member_property property;
            let refis =
              Base.Option.value_map
                ~f:(fun propname ->
                  this#extend_refinement
                    refinement_key_obj
                    (L.LSet.singleton loc, PropExistsR { propname; loc })
                    refis)
                ~default:refis
                propname
            in

            this#commit_refinement refis)
        | _ ->
          raise
            Env_api.(
              Env_invariant
                ( Some loc,
                  Impossible
                    "member_expression_refinement can only be called on OptionalMember or Member"
                )
            )

      method expression_refinement ((loc, expr) as expression) =
        this#handle_array_providers expression;
        let open Flow_ast.Expression in
        match expr with
        | Identifier ident ->
          this#identifier_refinement ident;
          expression
        | Logical logical ->
          this#logical_refinement logical;
          expression
        | Assignment assignment ->
          this#assignment_refinement loc assignment;
          expression
        | Binary binary ->
          this#binary_refinement loc binary;
          expression
        | Call call ->
          this#call_refinement loc call;
          expression
        | Unary unary ->
          this#unary_refinement loc unary;
          expression
        | Member _
        | OptionalMember _ ->
          (* Add a PropExists refinement to the object and a
           * Truthy heap refinement to the access *)
          let refis =
            match RefinementKey.of_expression (loc, expr) with
            | None -> LookupMap.empty
            | Some key -> this#start_refinement key (L.LSet.singleton loc, TruthyR)
          in
          ignore @@ this#record_member_read expression;
          this#member_expression_refinement loc expr refis;
          expression
        | Array _
        | ArrowFunction _
        | Class _
        | Comprehension _
        | Conditional _
        | Function _
        | Generator _
        | Import _
        | JSXElement _
        | JSXFragment _
        | Literal _
        | MetaProperty _
        | New _
        | Object _
        | OptionalCall _
        | Sequence _
        | Super _
        | TaggedTemplate _
        | TemplateLiteral _
        | TypeCast _
        | TSTypeCast _
        | This _
        | Update _
        | Yield _ ->
          this#expression expression

      method! logical _loc (expr : (ALoc.t, ALoc.t) Flow_ast.Expression.Logical.t) =
        let open Flow_ast.Expression.Logical in
        let { operator; left = (loc, _) as left; right; comments = _ } = expr in
        this#push_refinement_scope empty_refinements;
        (* THe LHS is unconditionally evaluated, so we don't run-to-completion and catch the
         * error here *)
        (match operator with
        | Flow_ast.Expression.Logical.Or
        | Flow_ast.Expression.Logical.And ->
          ignore (this#expression_refinement left)
        | Flow_ast.Expression.Logical.NullishCoalesce ->
          ignore (this#expression left);
          this#add_refinement_to_expr left (L.LSet.singleton loc, NotR MaybeR));
        let env1 = this#env_without_latest_refinements in
        let env1_with_refinements = this#env in
        (match operator with
        | Flow_ast.Expression.Logical.NullishCoalesce
        | Flow_ast.Expression.Logical.Or ->
          this#negate_new_refinements ()
        | Flow_ast.Expression.Logical.And -> ());
        (* The RHS is _only_ evaluated if the LHS fails its check. That means that patterns like
         * x || invariant(false) should propagate the truthy refinement to the next line. We keep track
         * of the completion state on the rhs to do that. If the LHS throws then the entire expression
         * throws, so there's no need to catch the exception from the LHS *)
        let rhs_completion_state =
          this#run_to_completion (fun () -> ignore @@ this#expression right)
        in
        (match rhs_completion_state with
        | Some AbruptCompletion.Throw ->
          let env2 = this#env in
          this#reset_env env1_with_refinements;
          this#pop_refinement_scope_without_unrefining ();
          this#merge_self_env env2
        | _ ->
          this#pop_refinement_scope ();
          this#merge_self_env env1);
        expr

      method private chain_to_refinement =
        function
        | BASE refinement -> refinement
        | AND (id1, id2) ->
          let (locs1, ref1) = this#chain_to_refinement (imap_find id1 env_state.refinement_heap) in
          let (locs2, ref2) = this#chain_to_refinement (imap_find id2 env_state.refinement_heap) in
          (L.LSet.union locs1 locs2, AndR (ref1, ref2))
        | OR (id1, id2) ->
          let (locs1, ref1) = this#chain_to_refinement (imap_find id1 env_state.refinement_heap) in
          let (locs2, ref2) = this#chain_to_refinement (imap_find id2 env_state.refinement_heap) in
          (L.LSet.union locs1 locs2, OrR (ref1, ref2))
        | NOT id ->
          let (locs, ref) = this#chain_to_refinement (imap_find id env_state.refinement_heap) in
          (locs, NotR ref)

      method refinement_of_id id =
        let chain = imap_find id env_state.refinement_heap in
        this#chain_to_refinement chain

      method private handle_array_providers ((loc, _) as exp) =
        if Provider_api.is_array_provider provider_info loc then
          let reason = Reason.mk_expression_reason exp in
          env_state <-
            {
              env_state with
              write_entries =
                EnvMap.add
                  (Env_api.ArrayProviderLoc, loc)
                  (Env_api.AssigningWrite reason)
                  env_state.write_entries;
            }

      method! expression expr =
        this#handle_array_providers expr;
        match expr with
        | (_, Flow_ast.Expression.Call _)
        | (_, Flow_ast.Expression.OptionalCall _)
        | (_, Flow_ast.Expression.Member _)
        | (_, Flow_ast.Expression.OptionalMember _) ->
          this#push_refinement_scope empty_refinements;
          let res = this#optional_chain expr in
          this#pop_refinement_scope ();
          res
        | _ -> super#expression expr

      method! private hoist_annotations f =
        let visiting_hoisted_type = env_state.visiting_hoisted_type in
        env_state <- { env_state with visiting_hoisted_type = true };
        f ();
        env_state <- { env_state with visiting_hoisted_type }

      method jsx_function_call loc =
        match (Context.react_runtime cx, env_state.jsx_base_name, Context.jsx cx) with
        | (Options.ReactRuntimeClassic, Some name, Options.Jsx_react) ->
          this#any_identifier loc name
        | (_, Some name, Options.Jsx_pragma _) -> this#any_identifier loc name
        | (Options.ReactRuntimeClassic, None, Options.Jsx_pragma (_, ast)) ->
          ignore @@ this#expression ast
        | _ -> ()

      method! jsx_element loc expr =
        let open Ast.JSX in
        let { opening_element = (_, { Opening.attributes = opening_attributes; _ }); _ } = expr in
        Eq_test.jsx_attributes_possible_sentinel_refinements opening_attributes
        |> this#add_sentinel_check_writes;
        this#jsx_function_call loc;
        super#jsx_element loc expr

      method! jsx_fragment loc expr =
        this#jsx_function_call loc;
        super#jsx_fragment loc expr

      method private statements_with_bindings loc bindings statements ~finally =
        this#with_bindings
          loc
          bindings
          (fun _ ->
            let completion_state =
              this#run_to_completion (fun () -> ignore @@ this#statement_list statements)
            in
            finally ();
            completion_state)
          None

      method private synthesize_read name =
        let { val_ref; havoc; def_loc; kind; _ } = this#env_read name in
        let v =
          if env_state.visiting_hoisted_type then
            havoc
          else
            !val_ref
        in
        let v = Val.simplify def_loc (Some kind) (Some name) v in
        v

      method! declare_module loc ({ Ast.Statement.DeclareModule.id; body; _ } as m) =
        let (name_loc, name) =
          match id with
          | Ast.Statement.DeclareModule.Identifier (loc, { Ast.Identifier.name; _ }) -> (loc, name)
          | Ast.Statement.DeclareModule.Literal (loc, { Ast.StringLiteral.value; _ }) -> (loc, value)
        in
        let (block_loc, { Ast.Statement.Block.body = statements; comments = _ }) = body in
        let reason = mk_reason (RModule (OrdinaryName name)) name_loc in
        let write_entries =
          env_state.write_entries
          |> EnvMap.add_ordinary name_loc (Env_api.AssigningWrite reason)
          |> EnvMap.add (Env_api.DeclareModuleExportsLoc, loc) (Env_api.AssigningWrite reason)
        in
        let values =
          L.LMap.add
            name_loc
            {
              def_loc = Some name_loc;
              value = Val.one reason;
              binding_kind_opt = None;
              name = Some name;
            }
            env_state.values
        in
        env_state <- { env_state with values; write_entries };
        let bindings =
          let hoist =
            new Hoister.hoister ~flowmin_compatibility:false ~enable_enums ~with_types:true
          in
          hoist#eval hoist#statement_list statements
        in
        let saved_exclude_syms = env_state.exclude_syms in
        env_state <- { env_state with exclude_syms = NameUtils.Set.empty };
        ignore
        @@ this#statements_with_bindings block_loc bindings statements ~finally:(fun () ->
               let members =
                 bindings
                 |> Bindings.to_map
                 |> SMap.keys
                 |> Base.List.map ~f:(fun name -> (OrdinaryName name, this#synthesize_read name))
               in
               env_state <-
                 {
                   env_state with
                   module_toplevel_members =
                     L.LMap.add loc members env_state.module_toplevel_members;
                 }
           );
        env_state <- { env_state with exclude_syms = saved_exclude_syms };
        m

      method visit_program program =
        let (loc, { Flow_ast.Program.statements; _ }) = program in
        let bindings =
          let hoist = new hoister ~flowmin_compatibility:false ~enable_enums ~with_types:true in
          hoist#eval hoist#program program
        in
        this#statements_with_bindings loc bindings statements ~finally:(fun () ->
            let rev_bindings =
              bindings
              |> Bindings.to_map
              |> SMap.keys
              |> Base.List.rev_filter_map ~f:(fun name ->
                     if not (this#is_excluded_ordinary_name name) then
                       Some (OrdinaryName name, this#synthesize_read name)
                     else
                       None
                 )
            in
            let statements =
              Base.List.filter_map statements ~f:(function
                  | ( _,
                      Ast.Statement.DeclareModule
                        {
                          Ast.Statement.DeclareModule.id =
                            ( Ast.Statement.DeclareModule.Identifier
                                (loc, { Ast.Identifier.name; _ })
                            | Ast.Statement.DeclareModule.Literal
                                (loc, { Ast.StringLiteral.value = name; _ }) );
                          _;
                        }
                    )
                    when not (this#is_excluded (internal_module_name name)) ->
                    let reason = mk_reason (RModule (OrdinaryName name)) loc in
                    let v = Val.one reason |> Val.simplify (Some loc) None (Some name) in
                    Some (internal_module_name name, v)
                  | _ -> None
                  )
            in
            let toplevel_members = Base.List.rev_append rev_bindings statements in
            env_state <- { env_state with toplevel_members }
        )
    end

  (* The EnvBuilder does not traverse dead code, but statement.ml does. Dead code
   * is an error in Flow, so type checking after that point is not very meaningful.
   * In order to support statement.ml's queries, we must ensure that the value map we
   * send to it has the dead code reads filled in. An alternative approach to this visitor
   * would be to assume that if the entry does not exist in the map then it is unreachable,
   * but that assumes that the EnvBuilder is 100% correct. This approach lets us discriminate
   * between real dead code and issues with the EnvBuilder, which seems far better than
   * the alternative *)
  class dead_code_marker cx jsx_base_name env_values write_entries =
    object (this)
      inherit
        Scope_builder.scope_builder
          ~flowmin_compatibility:false
          ~enable_enums:(Context.enable_enums cx)
          ~with_types:true as super

      val mutable values = env_values

      val mutable write_entries = write_entries

      method values = values

      method write_entries = write_entries

      method any_identifier loc name =
        values <-
          L.LMap.update
            loc
            (function
              | None ->
                Some
                  {
                    Env_api.def_loc = None;
                    write_locs = [Env_api.Unreachable loc];
                    val_kind = None;
                    name = Some name;
                    id = None;
                  }
              | x -> x)
            values

      method! this_expression loc this_ =
        this#any_identifier loc "this";
        this_

      method! super_expression loc super_ =
        this#any_identifier loc "super";
        super_

      method! binding_type_identifier ident = super#identifier ident

      method! identifier (ident : (ALoc.t, ALoc.t) Ast.Identifier.t) =
        let (loc, { Flow_ast.Identifier.name; _ }) = ident in
        this#any_identifier loc name;
        super#identifier ident

      method! jsx_element_name_identifier (ident : (ALoc.t, ALoc.t) Ast.JSX.Identifier.t) =
        let (loc, { Ast.JSX.Identifier.name; comments = _ }) = ident in
        this#any_identifier loc name;
        super#jsx_identifier ident

      method private jsx_function_call loc =
        match (Context.react_runtime cx, jsx_base_name, Context.jsx cx) with
        | (Options.ReactRuntimeClassic, Some name, Options.Jsx_react) ->
          this#any_identifier loc name
        | (_, Some name, Options.Jsx_pragma _) -> this#any_identifier loc name
        | (Options.ReactRuntimeClassic, None, Options.Jsx_pragma (_, ast)) ->
          ignore @@ this#expression ast
        | _ -> ()

      method! jsx_element loc expr =
        let open Ast.JSX in
        let { opening_element; closing_element; _ } = expr in
        let loc =
          match closing_element with
          | None -> fst opening_element
          | _ -> loc
        in
        this#jsx_function_call loc;
        super#jsx_element loc expr

      method! jsx_fragment loc expr =
        this#jsx_function_call loc;
        super#jsx_fragment loc expr

      method! pattern_identifier ?kind e =
        ignore kind;
        let (loc, _) = e in
        write_entries <-
          EnvMap.update_ordinary
            loc
            (function
              | None -> Some Env_api.NonAssigningWrite
              | x -> x)
            write_entries;
        e

      method! assignment loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Assignment.t) =
        let open Ast.Expression.Assignment in
        let { operator; left; _ } = expr in
        let () =
          match (operator, left) with
          | (Some _, (_, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; _ })) ->
            ignore @@ this#identifier name
          | _ -> ()
        in
        super#assignment loc expr

      method! update_expression _ (expr : (ALoc.t, ALoc.t) Ast.Expression.Update.t) =
        let open Ast.Expression.Update in
        let { argument; operator = _; prefix = _; comments = _ } = expr in
        (match argument with
        | (_, Ast.Expression.Identifier x) ->
          (* given `x++`, read x then write x *)
          ignore @@ this#identifier x;
          ignore @@ this#pattern_identifier x
        | _ -> ignore @@ super#expression argument);
        expr

      method! variable_declarator
          ~kind (decl : (ALoc.t, ALoc.t) Ast.Statement.VariableDeclaration.Declarator.t) =
        let open Ast.Statement.VariableDeclaration.Declarator in
        let (_, { id = _; init }) = decl in
        match init with
        | Some _ -> super#variable_declarator ~kind decl
        (* When there is no init, we should avoid calls to pattern_identifier so that we won't
           mark normal declaration without initialization as non-assigning writes. *)
        | None -> decl

      method! yield loc yield =
        this#any_identifier loc next_var_name;
        super#yield loc yield

      method private mark_dead_write key =
        write_entries <-
          EnvMap.update
            key
            (function
              | None -> Some Env_api.NonAssigningWrite
              | x -> x)
            write_entries

      method! class_ loc expr =
        let { Flow_ast.Class.id; _ } = expr in
        if Base.Option.is_none id then this#mark_dead_write (Env_api.OrdinaryNameLoc, loc);
        this#mark_dead_write (Env_api.ClassSelfLoc, loc);
        this#mark_dead_write (Env_api.ClassInstanceThisLoc, loc);
        this#mark_dead_write (Env_api.ClassInstanceSuperLoc, loc);
        this#mark_dead_write (Env_api.ClassStaticThisLoc, loc);
        this#mark_dead_write (Env_api.ClassStaticSuperLoc, loc);
        super#class_ loc expr

      method! function_ loc expr =
        let { Flow_ast.Function.id; _ } = expr in
        if Base.Option.is_none id then this#mark_dead_write (Env_api.OrdinaryNameLoc, loc);
        this#mark_dead_write (Env_api.FunctionThisLoc, loc);
        super#function_ loc expr

      method! function_declaration loc expr =
        this#mark_dead_write (Env_api.FunctionThisLoc, loc);
        super#function_declaration loc expr

      method! function_param_pattern ((loc, _) as patt) =
        write_entries <-
          EnvMap.update
            (Env_api.FunctionParamLoc, loc)
            (function
              | None -> Some Env_api.NonAssigningWrite
              | x -> x)
            write_entries;
        super#function_param_pattern patt

      method! lambda ~is_arrow ~fun_loc ~generator_return_loc params predicate body =
        let loc =
          let open Ast.Function in
          match body with
          | BodyBlock (loc, _)
          | BodyExpression (loc, _) ->
            loc
        in
        Base.Option.iter generator_return_loc ~f:(fun return_loc ->
            write_entries <-
              EnvMap.update_ordinary
                return_loc
                (function
                  | None -> Some Env_api.NonAssigningWrite
                  | x -> x)
                write_entries
        );
        begin
          try ignore (Context.exhaustive_check cx loc : _ * _) with
          | Not_found -> Context.add_exhaustive_check cx loc ([], false)
        end;
        super#lambda ~is_arrow ~fun_loc ~generator_return_loc params predicate body
    end

  let program_with_scope cx ?(lib = false) ?(exclude_syms = NameUtils.Set.empty) program =
    let (loc, _) = program in
    let jsx_ast =
      match Context.jsx cx with
      | Options.Jsx_react -> None
      | Options.Jsx_pragma (_, ast) -> Some ast
    in
    let enable_enums = Context.enable_enums cx in
    let (_ssa_completion_state, ((scopes, ssa_values, _) as prepass)) =
      Ssa_builder.program_with_scope_and_jsx_pragma
        ~flowmin_compatibility:false
        ~enable_enums
        ~jsx_ast
        program
    in
    let providers =
      try Provider_api.find_providers program with
      | Find_providers.ImpossibleState s -> raise Env_api.(Env_invariant (None, Impossible s))
    in

    let env_walk = new name_resolver cx lib exclude_syms prepass providers loc in
    let completion_state = env_walk#visit_program program in
    (* Fill in dead code reads *)
    let dead_code_marker =
      new dead_code_marker cx env_walk#jsx_base_name env_walk#values env_walk#write_entries
    in
    let _ = dead_code_marker#program program in
    ( completion_state,
      {
        Env_api.scopes;
        ssa_values;
        env_values = dead_code_marker#values;
        env_entries = dead_code_marker#write_entries;
        providers;
        toplevel_members = env_walk#toplevel_members;
        module_toplevel_members = env_walk#module_toplevel_members;
        predicate_refinement_maps = env_walk#predicate_refinement_maps;
        refinement_of_id = env_walk#refinement_of_id;
      }
    )

  let program cx ?lib ?exclude_syms program =
    let (_, { Env_api.env_values; refinement_of_id; _ }) =
      program_with_scope cx ?lib ?exclude_syms program
    in
    (env_values, refinement_of_id)
end

module DummyFlow (Context : C) = struct
  type cx = Context.t

  let add_output _ ?trace _ = ignore trace
end

module Make_Test_With_Cx (Context : C) =
  Make (Scope_api.With_ALoc) (Ssa_api.With_ALoc) (Env_api.With_ALoc) (Context) (DummyFlow (Context))
module Make_of_flow = Make (Scope_api.With_ALoc) (Ssa_api.With_ALoc) (Env_api.With_ALoc)
