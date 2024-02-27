(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason

let curr_id = ref 0

type write_state =
  | Uninitialized of ALoc.t
  | Undeclared of string * ALoc.t
  | DeclaredButSkipped of string * ALoc.t
  | Projection of ALoc.t
  | FunctionThis of reason
  | GlobalThis of reason
  | IllegalThis of reason
  | ClassInstanceThis of reason
  | ClassStaticThis of reason
  | ClassInstanceSuper of reason
  | ClassStaticSuper of reason
  | ModuleScoped of string
  | Global of string
  | Loc of reason
  | EmptyArray of {
      reason: reason;
      arr_providers: Loc_collections.ALocSet.t;
    }
  | IllegalWrite of reason
  | PHI of write_state list
  | Refinement of {
      refinement_id: int;
      val_t: t;
    }
  | Undefined of reason
  | Number of reason
  | DeclaredFunction of ALoc.t

and t = {
  id: int;
  write_state: write_state;
}

let rec debug_write_state get_refi = function
  | Uninitialized _ -> "(uninitialized)"
  | Undeclared _ -> "(undeclared)"
  | DeclaredButSkipped _ -> "(declared but skipped)"
  | Projection l -> Utils_js.spf "projection at %s" (ALoc.debug_to_string l)
  | Loc reason ->
    let loc = Reason.loc_of_reason reason in
    Utils_js.spf
      "%s: (%s)"
      (ALoc.debug_to_string loc)
      Reason.(desc_of_reason reason |> string_of_desc)
  | EmptyArray { reason; _ } ->
    let loc = Reason.loc_of_reason reason in
    Utils_js.spf
      "(empty array) %s: (%s)"
      (ALoc.debug_to_string loc)
      Reason.(desc_of_reason reason |> string_of_desc)
  | IllegalWrite reason ->
    let loc = Reason.loc_of_reason reason in
    Utils_js.spf "illegal write at %s" (ALoc.debug_to_string loc)
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
  | ModuleScoped name -> "ModuleScoped " ^ name
  | Global name -> "Global " ^ name
  | Undefined _ -> "undefined"
  | Number _ -> "number"
  | DeclaredFunction l -> Utils_js.spf "declared function %s" (ALoc.debug_to_string l)
  | PHI ts -> Utils_js.spf "[%s]" (ts |> List.map (debug_write_state get_refi) |> String.concat ",")

and debug_to_string get_refi { id; write_state } =
  string_of_int id ^ " " ^ debug_write_state get_refi write_state

(* Ensure we only produce one unique val for the same Loc *)
let val_one_cache : (Reason.t, t) Hashtbl.t = Hashtbl.create 0

let clear () = Hashtbl.reset val_one_cache

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
        Env_api.Refinement { writes = simplify_val val_t; refinement_id; write_id = Some val_t.id }
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
    | Some (Bindings.Type { imported; type_only_namespace }) ->
      Some (Env_api.Type { imported; type_only_namespace })
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
    | ModuleScoped _ -> []
    | Global _ -> []
    | Projection _ -> []
  in
  state_is_uninitialized write_state
