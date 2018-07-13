(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*
Implements a basic dependence tracker for both values and types (tvars).
It collects, for all the variable declarations, the assignments
to the variable.  For tvar dependence, if the variable declaration has
an annotation, then we do not need to track any assignments to it.
Also, we collect dependences for each AST node, which we can think of
as an implicit temporary variable.

Value flows through heap and function parameters are ignored for now, though
we do introduce HeapLocs for future extension.
*)

(* TODO Ensure physical equality optimization? *)

module LocMap = Utils_js.LocMap

module DepKey = struct
  type t =
  | Id of Loc.t (* uniquely resolved name def_loc, id *)
  | Temp of Loc.t (* expression node in AST *)
  | HeapLoc of Loc.t * string (* locations in a heap object,
                                  loc=allocation site, string=prop *)

  let same_file_loc_compare =
    let open Loc in
    fun loc1 loc2 ->
    let k = Loc.pos_cmp loc1.start loc2.start in
    if k = 0 then Loc.pos_cmp loc1._end loc2._end
    else k

  (* ID < Temp < HeapLoc, and within those based on sub-structure comparison *)
  let compare =
    fun k1 k2 ->
      (match k1, k2 with
      | Id l1, Id l2 -> same_file_loc_compare l1 l2
      | Temp l1, Temp l2 -> same_file_loc_compare l1 l2
      | Id _, Temp _ -> -1
      | Temp _, Id _ -> 1
      | Id _, HeapLoc _ -> -1
      | HeapLoc _, Id _ -> 1
      | Temp _, HeapLoc _ -> -1
      | HeapLoc _, Temp _ -> 1
      | HeapLoc (l1, s1), HeapLoc (l2, s2) ->
          let lcmp = (same_file_loc_compare l1 l2) in
          if (lcmp = 0) then
            String.compare s1 s2
          else lcmp
      )
end

module DepMap = MyMap.Make (DepKey)

module Dep = struct
  type depkind =
    | Annotation of Loc.t Ast.Type.annotation * string list
    | Primitive (* of Ast.Literal.t *)
    | Object (* | Function *)
    | Depends of DepKey.t list (* TODO set, not list *)
    | Incomplete
    | NoInfo
    | Destructure of DepKey.t * string
  type t = {
    typeDep: depkind; (* for tvar dependence - not all cases of depkind apply! *)
    valDep: depkind; (* for value dependence - not all cases of depkind apply! *)
  }

  let merge_dep =
   let merge_typeDep key type_dep_cur type_dep_new =
     match type_dep_cur, type_dep_new with
     | _, Incomplete -> Incomplete
     | Annotation _, _ -> type_dep_cur
     | NoInfo, _ -> type_dep_new
     | Incomplete, _ -> type_dep_cur
     | Depends tlist, _ -> Depends (List.cons key tlist)
     | Primitive, Primitive -> Primitive
     | Primitive, _ -> Incomplete
     | _,_ -> type_dep_cur
  in
  let merge_valDep key val_dep_cur val_dep_new =
    match val_dep_cur, val_dep_new with
    | _, Incomplete -> Incomplete
    | Incomplete, _ -> val_dep_cur
    | NoInfo, NoInfo -> NoInfo
    | NoInfo, Destructure (k,s) -> Destructure (k,s)
    | NoInfo, Primitive -> Depends [key]
    | NoInfo, _ -> Depends [key]
    | Destructure _, _ -> Incomplete
    | Primitive, _ -> Incomplete
    | Depends tlist, _ -> Depends (List.cons key tlist)
    | _, _ -> val_dep_cur
  in
  fun key cur_t new_t ->
    let { typeDep=type_dep_new; valDep=val_dep_new} = new_t in
    let { typeDep=type_dep_cur; valDep=val_dep_cur} = cur_t in
    { typeDep = merge_typeDep key type_dep_cur type_dep_new;
      valDep = merge_valDep key val_dep_cur val_dep_new }

  let print_dep =
   let key_to_string key =
       match key with
       | DepKey.Id d ->
           String.concat " "
             ["{"; "DEFLOC"; Loc.to_string d; "}"]
       | DepKey.Temp l ->
           String.concat " "
             ["{"; "LOC"; Loc.to_string l; "}"]
       | DepKey.HeapLoc (l, s) ->
       String.concat " "
         ["{"; "HEAPLOC"; Loc.to_string l; s; "}"]
   in
   let kind_to_string kind =
     match kind with
     | NoInfo -> "noinfo"
     | Depends slist ->
       String.concat " "
         ["depends";  String.concat ", " (List.map key_to_string slist)]
     | Annotation _ -> "annot"
     | Incomplete -> "incomplete"
     | Object -> "object"
     | Primitive -> "prim"
     | Destructure (k, _) ->
       String.concat " "
         ["destruct"; (key_to_string k)]
   in
   fun key dep ->
     let { typeDep; valDep } = dep in
     String.concat " "
       [key_to_string key;
       "->";
       "T ="; kind_to_string typeDep;
       "V =" ; kind_to_string valDep]
end

class mapper = object(this)
  inherit Flow_ast_mapper.mapper as super

  val mutable use_def_map = LocMap.empty
  method use_def_map = use_def_map

  val mutable dep_map = DepMap.empty
  method dep_map = dep_map

  (* TODO: Move this out of the class, and pass the relevant maps to it? *)
  method has_single_value_dep (loc: Loc.t) =
    let open Dep in
    try
      let d = LocMap.find loc use_def_map in
      let { typeDep = _; valDep } =
        DepMap.find (DepKey.Id d) dep_map in
        match valDep with
        | Depends l -> (List.length l) = 1
        | Primitive -> true
        | Destructure _ -> true
        | _ -> false
     with _ -> false

  method has_no_open_type_dep (loc: Loc.t) =
   let open Dep in
   try
    let d = LocMap.find loc use_def_map in
    let { typeDep; valDep = _ } =
      DepMap.find (DepKey.Id d) dep_map in
      match typeDep with
      | Annotation _ -> true
      | Primitive -> true
      | Object -> true
      | _ -> false
      (* TODO: recurse in the Depends list *)
   with _ -> false

  val merge_dep = Dep.merge_dep

  method! program (program: Loc.t Ast.program) =
    let { Scope_api.scopes; max_distinct=_ } =
    Scope_builder.program ~ignore_toplevel:true program in
    use_def_map <- IMap.fold (fun _ scope acc ->
      LocMap.fold (fun loc { Scope_api.Def.locs; _ } acc ->
        (* TODO: investigate whether picking the first location where there could
          be multiple is fine in principle *)
        LocMap.add loc (Nel.hd locs) acc
      ) scope.Scope_api.Scope.locals acc
    ) scopes LocMap.empty;
    LocMap.iter
      (fun _ def_loc ->
        let open Dep in
        let dep = { typeDep = NoInfo;
                    valDep = NoInfo } in
        dep_map <- DepMap.add (DepKey.Id def_loc) dep dep_map)
      use_def_map;
    super#program program

  method! function_param_pattern (expr: Loc.t Ast.Pattern.t) =
    let open Dep in
    (match expr with
    | _, Ast.Pattern.Identifier id ->
        let open Ast.Pattern.Identifier in
        let { name = (loc, _); annot; _ } = id in
        (try
           let d = LocMap.find loc use_def_map in
           let key = DepKey.Id d in
           match annot with
             | None ->
               (* Currently, we pretend we don't know anything about caller/callee *)
               let dep = { typeDep = Incomplete;
                           valDep = Incomplete } in
               dep_map <-
                 DepMap.add ~combine:(merge_dep key) (* update_dep *)
                 key dep dep_map
             | Some some_annot ->
               let dep = { typeDep = Annotation (some_annot, []);
                           valDep = Incomplete } in
               dep_map <-
                 DepMap.add ~combine:(merge_dep key) (* update_dep *)
                 key dep dep_map
         with _ -> ())
    | _, _ -> ());  (* Other interesting cases in Pattern applicable here? *)
    super#function_param_pattern expr

  method! variable_declarator_pattern ~kind (expr: Loc.t Ast.Pattern.t) =
    let open Dep in
    (match expr with
    | _, Ast.Pattern.Identifier id ->
      let open Ast.Pattern.Identifier in
      let { name = (loc, _); annot; _ } = id in
      (try
         let d = LocMap.find loc use_def_map in
         let key = DepKey.Id d in
         match annot with
           | None ->
             let dep = { typeDep = NoInfo;
                         valDep = NoInfo } in
             (* different from fun param!! *)
             dep_map <-
               DepMap.add ~combine:(merge_dep key) (* update_dep *)
               key dep dep_map
           | Some some_annot -> (* annotation *)
             let dep = { typeDep = Annotation (some_annot, []);
                         valDep = NoInfo } in
             dep_map <-
               DepMap.add ~combine:(merge_dep key) (* update_dep *)
               key dep dep_map
       with _ -> ())
    | _, _ -> ())
    ;
    super#variable_declarator_pattern ~kind expr

  (* In DepMap, map id @ loc to Incomplete.*)
  method map_id_to_incomplete
    (loc: Loc.t) =
    let open Dep in
    try
      let d = LocMap.find loc use_def_map in
      let key = DepKey.Id d in
      let dep_right = { typeDep = Dep.Incomplete;
                        valDep = Dep.Incomplete } in
      dep_map <-
        DepMap.add ~combine:(merge_dep key)
        key dep_right dep_map
    with _ -> ()
    (* Non-renamable vars such as globals do not exist in the scope builder *)

  (* In DepMap, map id @ Loc to Destructure (Temp expr's loc), key *)
  method map_id_to_destructure
    (loc: Loc.t)
    (key: Loc.t Ast.Pattern.Object.Property.key)
    (expr: Loc.t Ast.Expression.t option) =
    let open Dep in
    try
      let d = LocMap.find loc use_def_map in
      (* syntax guarantees that in destructuring, rhs is not optional *)
      let (loc_e,_) = (match expr with | Some e -> e | None -> raise Not_found) in
      match key with
        | Ast.Pattern.Object.Property.Identifier iden ->
          let _, real_name = iden in
          let key = DepKey.Id d in
          let dep_right = { typeDep = Incomplete;
                            valDep = Destructure (DepKey.Temp loc_e, real_name) } in
          dep_map <-
            DepMap.add ~combine:(merge_dep key)
            key dep_right dep_map
        (* Remark: we cannot really pretend rhs is a Destructure expr and
           'depend' on that Destructure. Here is no syntactic representation
           such as *s or s->p.  Also, we destructure into multiple names at
           once, var {a:c,b:d} = s; So, we just say c |-> Destructure s,a, and d
           |-> Destructure s,b.  *)
        | _ ->
          (* If the key is not an identifier, then we cannot do a Destructure kind *)
          this#map_id_to_incomplete loc
    with _ -> ()

  method assign_to_variable_declarator_pattern
    (pat: Loc.t Ast.Pattern.t)
    (expr: Loc.t Ast.Expression.t option) =
    match pat with
      | _, Ast.Pattern.Identifier id ->
        let open Ast.Pattern.Identifier in
        let open Dep in
        let { name = (loc, _); annot = _; _ } = id in
        (try
           let d = LocMap.find loc use_def_map in
           let key = DepKey.Id d in
           (match expr with
             | Some expr' ->
               let (loc_e, _) = expr' in
               let dep_right = DepMap.find (DepKey.Temp loc_e) dep_map in
               dep_map <-
                 DepMap.add ~combine:(merge_dep key)
                 key dep_right dep_map
             | None ->
               (* treat no-rhs-expression as uninitialized *)
               let dep_right = { typeDep = Dep.NoInfo;
                                 valDep = Dep.NoInfo } in
               dep_map <-
                 DepMap.add ~combine:(merge_dep key)
                 key dep_right dep_map
           )
         with _ ->
           ())
      | _, Ast.Pattern.Object o->
        (* Dealing with real destructing depends on actual heap analysis *)
        (* For now, we can just map each of these properties to Destructure *)
        let open Ast.Pattern.Object in
        let { properties; annot = _} = o in
        let process_prop = fun p ->
          (match p with
            | Property (loc,{Property.key=key; pattern; Property.shorthand=shorthand}) ->
              (* Note that if pattern is present then shorthand=false, and we
                 use the pattern as the "name" being declared, or otherwise use
                 the key.  *)
              if shorthand then
                (* The key could be complex, but we only care if it is in the
                   use_def_map table.  If it is not an identifier, it will not be in
                   the use_def_map table anyway.  The loc here is the location of the
                   key.  *)
                this#map_id_to_destructure loc key expr
              else
                (match pattern with
                  | loc, Ast.Pattern.Identifier _ -> this#map_id_to_destructure loc key expr
                  | _, _ -> ())
            | RestProperty (_,_) -> ())
        in
        List.iter process_prop properties
      | _, Ast.Pattern.Array a ->
        let open Ast.Pattern.Array in
        let { elements; annot = _} = a in
        let process_elem = fun e ->
          (match e with
            | Element (loc, _) -> this#map_id_to_incomplete loc
            | RestElement (_,_) -> ())
        in
        let process_elem_opt = fun e_opt ->
          (match e_opt with
            | Some e -> process_elem e
            | None -> ()) in
        List.iter process_elem_opt elements
      | _, _ -> () (* Deal with other names getting values? *)

  method! variable_declarator ~kind
    (decl: Loc.t Ast.Statement.VariableDeclaration.Declarator.t) =
    let open Ast.Statement.VariableDeclaration.Declarator in
    let decl' = super#variable_declarator ~kind decl in (* calls var_decl_pattern *)
    let (_, { id = patt ; init = e }) = decl' in
    this#assign_to_variable_declarator_pattern patt e;
    decl'

  method! expression (expr: Loc.t Ast.Expression.t) =
    let open Ast.Expression in
    let open Dep in
    match expr with

    | loc, Identifier id ->
      let open Dep in
      let id' = this#identifier id in
      (try
         let d = LocMap.find loc use_def_map in
         let dep = { typeDep = Depends [DepKey.Id d];
                     valDep =  Depends [DepKey.Id d] } in
         dep_map <- DepMap.add (DepKey.Temp loc) dep dep_map
       with _ ->
         let dep = { typeDep = Incomplete ;
                     valDep = Incomplete } in
         dep_map <- DepMap.add (DepKey.Temp loc) dep dep_map
      )
      ;
      if id == id' then expr else loc, Identifier id'

    | loc, Literal l ->
      let l' = this#literal l in
      let dep = { typeDep = Dep.Primitive;
                  valDep = Dep.Primitive } in
      dep_map <- DepMap.add (DepKey.Temp loc) dep dep_map
      ;
      loc, Literal l'

    | loc, Ast.Expression.Object o ->
      let open Dep in
      let o' = super#object_ o in
      (* Initialize dependence info for the AST node *)
      let dep = { typeDep = Dep.Object;
                  valDep = Dep.Object } in
      dep_map <- DepMap.add (DepKey.Temp loc) dep dep_map
      ;
      (* Initialize dependence info for HeapLocs implied by the obj literal *)
      let open Ast.Expression.Object in
      let { properties=properties } = o' in
      List.iter (function
        | Property (_, Property.Init {
            key = Property.Identifier (_, name);
            value = (eloc, _);
            shorthand = _;
          }) ->
            let dkey = DepKey.HeapLoc (loc, name) in
            let dep = {
              typeDep = Dep.Depends [DepKey.Temp eloc];
              valDep = Dep.Depends [DepKey.Temp eloc]
            } in
            dep_map <- DepMap.add dkey dep dep_map
        (* TODO *)
        | Property (_, Property.Init {
            key = Property.Literal _ | Property.PrivateName _ | Property.Computed _;
            _;
          })
        | Property (_, Property.Method _)
        | Property (_, Property.Get _)
        | Property (_, Property.Set _) -> ()
        | SpreadProperty _ -> ()
      ) properties;
      loc, Ast.Expression.Object o'

    | loc, Assignment a ->
      let a' = this#assignment a in
      loc, Assignment a'

    | loc, Binary b ->
      let open Ast.Expression.Binary in
      let open Dep in
      let { operator = o; left; right } = b in
      let left' = this#expression left in
      let right' = this#expression right in
      let left_loc, _ = left' in
      let right_loc, _ = right' in
      let dep = { typeDep = Depends [DepKey.Temp left_loc; DepKey.Temp right_loc];
                  valDep = Depends [DepKey.Temp left_loc; DepKey.Temp right_loc] } in
      dep_map <- DepMap.add (DepKey.Temp loc) dep dep_map
      ;
      if left == left' && right == right' then expr
      else loc, Binary { operator = o; left = left'; right = right' }

    | loc, TypeCast x ->
      let open Ast.Expression.TypeCast in
      let open Dep in
      let { expression=e; annot } = x in
      let e' = this#expression e in
      let loc_e',_ = e' in
      let dep = { typeDep = Annotation (annot, []);
                  valDep = Depends [DepKey.Temp loc_e'] } in
      dep_map <- DepMap.add (DepKey.Temp loc) dep dep_map
      ;
      if e' == e then expr
      else loc, TypeCast { expression = e'; annot }

    (* TODO Member: in the best case, we can retrieve the right HeapLocs *)

    | loc, Update x ->
      let open Ast.Expression.Update in
      let x' = this#update_expression x in
      let { argument; operator = _; prefix = _ } = x' in
      (match argument with
        | loc_a, Ast.Expression.Identifier _ ->
          (try
             let d = LocMap.find loc_a use_def_map in
             let key = DepKey.Temp loc in
             let dep_expr = { valDep = Depends [DepKey.Id d];
                              typeDep = Depends [DepKey.Id d] } in (* number? *)
             (* v++ augments the dependence of v onto itself as expr *)
             dep_map <-
               DepMap.add ~combine:(merge_dep key)
               key dep_expr dep_map;
             (* the expression v++ depends on v *)
             dep_map <- DepMap.add (DepKey.Temp loc) dep_expr dep_map
           with _ -> ())
        (* TODO: deal with heap locations: they should not become incomplete *)
        | loc_e, _ ->
          let open Dep in
          let dep = { valDep = Incomplete;
                  typeDep = Incomplete } in
          dep_map <- DepMap.add (DepKey.Temp loc_e) dep dep_map
      )
      ;
      loc, Update x'

    | loc,_ ->
      let open Dep in
      let dep = { valDep = Incomplete;
                  typeDep = Incomplete } in
      dep_map <- DepMap.add (DepKey.Temp loc) dep dep_map;
      super#expression expr

  method assign_to_assignment_pattern
    (pat: Loc.t Ast.Pattern.t)
    (expr: Loc.t Ast.Expression.t)
    (op: Ast.Expression.Assignment.operator) =
    match pat with
    | _, Ast.Pattern.Identifier id ->
      (* Similar, but not identical to the corresponding case in
       * assign_to_variable_declarator_pattern. 1. The rhs expression is not
       * optional. 2. The += syntax can occur here. 3. We ignore the type
       * annotation here. *)
      let open Ast.Pattern.Identifier in
      let { name = (loc, _); annot = _; _} = id in
      (try
         let d = LocMap.find loc use_def_map in
         let key = DepKey.Id d in
         let dep_left =
           DepMap.find (DepKey.Id d) dep_map in
         let (loc_e, _) = expr in
         let dep_right =
           DepMap.find (DepKey.Temp loc_e) dep_map in
         match op with
           | Ast.Expression.Assignment.Assign ->
             (* treat as = assignment *)
             dep_map <-
               DepMap.add ~combine:(merge_dep key)
               key dep_right dep_map
           | _ ->
             (* treat as += assignment *)
             dep_map <-
               DepMap.add ~combine:(merge_dep key)
               key dep_left dep_map;
             dep_map <-
               DepMap.add ~combine:(merge_dep key)
               key dep_right dep_map
       with _ -> ())

    | _, Ast.Pattern.Array a ->
      (* This is identical to the corresponding case in
       * assign_to_variable_declarator_pattern.  TODO - refactor.  *)
      let open Ast.Pattern.Array in
      let { elements; annot = _} = a in
      let process_elem = fun e ->
        (match e with
          | Element (loc, _) -> this#map_id_to_incomplete loc
          | RestElement (_,_) -> ())
      in
      let process_elem_opt = fun e_opt ->
        (match e_opt with
          | Some e -> process_elem e
          | None -> ()) in
      List.iter process_elem_opt elements

    | _, _ -> ()  (* TODO deal with the case e.p = e'. Update or havoc *)

  method! assignment (expr: Loc.t Ast.Expression.Assignment.t) =
    let open Ast.Expression.Assignment in
    let { operator = op; left; right } = expr in
    let left' = this#assignment_pattern left in
    let right' = this#expression right in
    this#assign_to_assignment_pattern left' right' op
    ;
    if left == left' && right == right' then expr
    else { expr with left = left'; right = right' }

  method! for_of_statement_lhs (left: Loc.t Ast.Statement.ForOf.left) =
    let open Ast.Statement.ForOf in
    let open Ast.Statement.VariableDeclaration in
    match left with
    | LeftDeclaration (loc, decl) ->
      let decl' = super#variable_declaration decl in
      (* Even though variable_declarator is handled elsewhere, For the For-of
         case, it does not see the rhs assignment.  So we need to havoc that
         declarator here. *)
      (try
         let {declarations; _} = decl' in
         let var_locs = List.map (fun (l,_) -> l) declarations in
         (* For this decl, let the variable become Incomplete *)
         List.iter this#map_id_to_incomplete var_locs
       with _ -> ())
      ;
      if decl == decl'
        then left
        else LeftDeclaration (loc, decl')
    | LeftPattern patt ->
      let patt' = this#for_of_assignment_pattern patt in
      (match patt' with
        | loc, Ast.Pattern.Identifier _ -> this#map_id_to_incomplete loc
        | _ -> ()) (* TODO: object and array patterns can happen here *)
      ;
      if patt == patt'
      then left
      else LeftPattern patt'

  method! for_in_statement_lhs (left: Loc.t Ast.Statement.ForIn.left) =
    (* Almost identical to the for-of case *)
    let open Ast.Statement.ForIn in
    let open Ast.Statement.VariableDeclaration in
    match left with
    | LeftDeclaration (loc, decl) ->
      let decl' = super#variable_declaration decl in
      (* Even though variable_declarator is handled elsewhere, For the For-In
         case, it does not see the rhs assignment.  So we need to havoc that
         declarator here. *)
      (try
         let {declarations; _} = decl' in
         let var_locs = List.map (fun (l,_) -> l) declarations in
         (* For this decl, let the variable become Incomplete *)
         List.iter this#map_id_to_incomplete var_locs
       with _ -> ())
      ;
      if decl == decl'
        then left
        else LeftDeclaration (loc, decl')
    | LeftPattern patt ->
      let patt' = this#for_in_assignment_pattern patt in
      (match patt' with
        | loc, Ast.Pattern.Identifier _ -> this#map_id_to_incomplete loc
        | _ -> ()) (* TODO: object and array patterns can happen here *)
      ;
      if patt == patt'
      then left
      else LeftPattern patt'
end
