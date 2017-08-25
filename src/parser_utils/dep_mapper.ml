(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
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
  | Id of Loc.t * int (* uniquely resolved name def_loc, id *)
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
      | Id (l1,i1), Id (l2,i2) ->
          if (i1 < i2) then -1
            else (if (i1 > i2) then 1 else same_file_loc_compare l1 l2)
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
    | Annotation of Ast.Type.annotation * string list
    | Primitive (* of Ast.Literal.t *)
    | Object (* | Function *)
    | Depends of DepKey.t list (* TODO set, not list *)
    | Incomplete
    | NoInfo
    | Destructure of DepKey.t * string
  type t = {
    key: DepKey.t; (* to simplify printing, join_tvar, join *)
    typeDep: depkind; (* for tvar dependence - not all cases of depkind apply! *)
    valDep: depkind; (* for value dependence - not all cases of depkind apply! *)
  }

  let merge_dep =
   fun cur_t new_t ->
   let merge_typeDep =
     fun type_dep_cur key_new type_dep_new ->
     match type_dep_cur, type_dep_new with
     | _, Incomplete -> Incomplete
     | Annotation _, _ -> type_dep_cur
     | NoInfo, _ -> type_dep_new
     | Incomplete, _ -> type_dep_cur
     | Depends tlist, _ -> Depends (List.cons key_new tlist)
     | Primitive, Primitive -> Primitive
     | Primitive, _ -> Incomplete
     | _,_ -> type_dep_cur
  in
  let merge_valDep =
    fun val_dep_cur key_new val_dep_new ->
    match val_dep_cur, val_dep_new with
    | _, Incomplete -> Incomplete
    | Incomplete, _ -> val_dep_cur
    | NoInfo, NoInfo -> NoInfo
    | NoInfo, Destructure (k,s) -> Destructure (k,s)
    | NoInfo, Primitive -> Depends [key_new]
    | NoInfo, _ -> Depends [key_new]
    | Destructure _, _ -> Incomplete
    | Primitive, _ -> Incomplete
    | Depends tlist, _ -> Depends (List.cons key_new tlist)
    | _, _ -> val_dep_cur
  in
   let { key=key_new; typeDep=type_dep_new; valDep=val_dep_new} = new_t in
   let { key=key_cur; typeDep=type_dep_cur; valDep=val_dep_cur} = cur_t in
   { key = key_cur;
     typeDep = merge_typeDep type_dep_cur key_new type_dep_new;
     valDep = merge_valDep val_dep_cur key_new val_dep_new }

  let print_dep =
   fun dep ->
   let {key;typeDep;valDep}=dep in
   let key_to_string =
     fun key ->
       match key with
       | DepKey.Id (d,i) ->
           String.concat " "
             ["{"; "DEFLOC"; Loc.to_string d; "ID"; string_of_int i; "}"]
       | DepKey.Temp l ->
           String.concat " "
             ["{"; "LOC"; Loc.to_string l; "}"]
       | DepKey.HeapLoc (l, s) ->
       String.concat " "
         ["{"; "HEAPLOC"; Loc.to_string l; s; "}"]
   in
   let kind_to_string =
     fun kind ->
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
     String.concat " "
       [key_to_string key;
       "->";
       "T ="; kind_to_string typeDep;
       "V =" ; kind_to_string valDep]
end

class mapper = object(this)
  inherit Flow_ast_mapper.mapper as super

  val mutable renamings = LocMap.empty

  val mutable depMap = DepMap.empty

  method depMap = depMap

  method renamings = renamings

  (* TODO: Move this out of the class, and pass the relevant maps to it? *)
  method has_single_value_dep (loc: Loc.t) =
    let open Dep in
    try
      let (d,i) = LocMap.find loc renamings in
      let { key=_; typeDep=_; valDep=valDep} =
        DepMap.find (DepKey.Id (d,i)) depMap in
        match valDep with
        | Depends l -> (List.length l) = 1
        | Primitive -> true
        | Destructure _ -> true
        | _ -> false
     with _ -> false

  method has_no_open_type_dep (loc: Loc.t) =
   let open Dep in
   try
    let (d,i) = LocMap.find loc renamings in
    let { key=_; typeDep=typeDep; valDep=_} =
      DepMap.find (DepKey.Id (d,i)) depMap in
      match typeDep with
      | Annotation _ -> true
      | Primitive -> true
      | Object -> true
      | _ -> false
      (* TODO: recurse in the Depends list *)
   with _ -> false

  val merge_dep = Dep.merge_dep

  method! program (program: Ast.program) =
    let { Scope_builder.locals; globals=_; max_distinct=_; scopes=_ } =
    Scope_builder.program ~ignore_toplevel:true program in
    renamings <-
      LocMap.map (fun ({ Scope_builder.Def.loc; name; _ }, _) -> loc, name) locals;
    LocMap.iter
      (fun _ (def_loc,id) ->
        let open Dep in
          depMap <- DepMap.add (DepKey.Id (def_loc,id))
                     { key = (DepKey.Id (def_loc,id));
                       typeDep = NoInfo;
                       valDep = NoInfo } depMap)
      renamings;
    super#program program

  method! function_param_pattern (expr: Ast.Pattern.t) =
    (* Utils_js.print_endlinef "%s" "function_pattern"; *)
    let open Dep in
    (match expr with
    | _, Ast.Pattern.Identifier id ->
        let open Ast.Pattern.Identifier in
        let { name = (loc, _); typeAnnotation = ta; _ } = id in
        (try
          let (d,i) = LocMap.find loc renamings in
          match ta with
          | None ->
            (* Currently, we pretend we don't know anything about caller/callee *)
            let dep = { key=(DepKey.Id (d,i));
                         typeDep= Incomplete;
                         valDep= Incomplete} in
            depMap <-
              DepMap.add ~combine:merge_dep (* update_dep *)
                (DepKey.Id (d,i)) dep depMap
          | Some some_ta ->
            let dep = { key=(DepKey.Id (d,i));
                         typeDep=( Annotation (some_ta, [] ));
                         valDep= Incomplete } in
            depMap <-
              DepMap.add ~combine:merge_dep (* update_dep *)
                (DepKey.Id (d,i)) dep depMap
         with _ -> ())
    | _, _ -> ());  (* Other interesting cases in Pattern applicable here? *)
    super#function_param_pattern expr

  method! variable_declarator_pattern ~kind (expr: Ast.Pattern.t) =
    (* Utils_js.print_endlinef "%s" "vardecl_pattern"; *)
    let open Dep in
    (match expr with
    | _, Ast.Pattern.Identifier id ->
      let open Ast.Pattern.Identifier in
      let { name = (loc, _); typeAnnotation = ta; _ } = id in
      (try
        let (d,i) = LocMap.find loc renamings in
        match ta with
        | None ->
        let dep = { key=(DepKey.Id (d,i));
                     typeDep= NoInfo;
                     valDep =  NoInfo} in
        (* different from fun param!! *)
        depMap <-
          DepMap.add ~combine:merge_dep (* update_dep *)
            (DepKey.Id (d,i)) dep depMap
        | Some some_ta -> (* annotation *)
        let dep = { key=(DepKey.Id (d,i));
                     typeDep=(Annotation (some_ta, [] ));
                     valDep = NoInfo} in
        depMap <-
          DepMap.add ~combine:merge_dep (* update_dep *)
            (DepKey.Id (d,i)) dep depMap
      with _ -> ())
    | _, _ -> ())
    ;
    super#variable_declarator_pattern ~kind expr

 (* In DepMap, map id @ loc to Incomplete.*)
  method map_id_to_incomplete =
  let open Dep in
  fun loc ->
  try
      let (d,i) = LocMap.find loc renamings in
      let dep_right = { key = (DepKey.Id (d,i));
          typeDep = Dep.Incomplete;
          valDep = Dep.Incomplete} in
        depMap <- DepMap.add ~combine:merge_dep
          (DepKey.Id (d,i)) dep_right depMap
   with _ -> ()
     (* Non-renamable vars such as globals do not exist in the scope builder *)
     (* Utils_js.prerr_endlinef "map_id_to_incomplete: %s" "id not recognized"*)

  (* In DepMap, map id @ Loc to Destructure (Temp expr's loc), key *)
  method map_id_to_destructure
    (loc: Loc.t)
    (key: Ast.Pattern.Object.Property.key)
    (expr: Ast.Expression.t option) =
  let open Dep in
  try
    let (d,i) = LocMap.find loc renamings in
    (* syntax guarantees that in destructuring, rhs is not optional *)
    let (loc_e,_) = (match expr with | Some e -> e | None -> raise Not_found) in
    match key with
    | Ast.Pattern.Object.Property.Identifier iden ->
      let _, real_name = iden in
      let dep_right = { key = (DepKey.Id (d,i));
        typeDep = Incomplete;
        valDep = Destructure (DepKey.Temp loc_e, real_name) }
      in
        depMap <- DepMap.add ~combine:merge_dep
          (DepKey.Id (d,i)) dep_right depMap
      (* Remark: we cannot really pretend rhs is a Destructure expr and 'depend'
      on that Destructure. Here is no syntactic representation such as *s or s->p.
      Also, we destructure into multiple names at once, var {a:c,b:d} = s; So, we
      just say c |-> Destructure s,a, and d |-> Destructure s,b.
      *)
    | _ ->
    (* If the key is not an identifier, then we cannot do a Destructure kind *)
      this#map_id_to_incomplete loc
  with _ -> ()
    (*Utils_js.prerr_endlinef "map_id_to_destructure: %s" "id not recognized"*)

  method assign_to_variable_declarator_pattern
    (pat: Ast.Pattern.t)
    (expr: Ast.Expression.t option) =
  (* Utils_js.print_endlinef "%s" "assign_to_vardecl_pat"; *)
  match pat with
  | _, Ast.Pattern.Identifier id ->
    let open Ast.Pattern.Identifier in
    let open Dep in
    let { name = (loc, _); typeAnnotation = _; _ } = id in
    (try
      let (d,i) = LocMap.find loc renamings in
      (match expr with
      | Some expr' ->
      let (loc_e, _) = expr' in
      let dep_right = DepMap.find (DepKey.Temp loc_e) depMap in
        depMap <- DepMap.add ~combine:merge_dep
          (DepKey.Id (d,i)) dep_right depMap
      | None ->
      let dep_right = {  (* treat no-rhs-expression as uninitialized *)
          key = (DepKey.Id (d,i));
          typeDep = Dep.NoInfo;
          valDep = Dep.NoInfo
        } in
        depMap <- DepMap.add ~combine:merge_dep
         (DepKey.Id (d,i)) dep_right depMap
      )
    with _ ->
    ())
    (*  Utils_js.prerr_endlinef
    "assign_to_variable_declarator_pattern:Identifier %s" "id not recognized"
    *)
  | _, Ast.Pattern.Object o->
    (* Dealing with real destructing depends on actual heap analysis *)
    (* For now, we can just map each of these properties to Destructure *)
    let open Ast.Pattern.Object in
    let { properties; typeAnnotation=_} = o in
    let process_prop = fun p ->
      (match p with
      | Property (loc,{Property.key=key; pattern; Property.shorthand=shorthand}) ->
        (* Note that if pattern is present then shorthand=false, and we use the
        pattern as the "name" being declared, or otherwise use the key.
        *)
        if shorthand then
          (* The key could be complex, but we only care if it is in the renamings table.
          If it is not an identifier, it will not be in the renamings table anyway.
          The loc here is the location of the key.
          *)
          this#map_id_to_destructure loc key expr
        else
          (match pattern with
            | loc, Ast.Pattern.Identifier _ -> this#map_id_to_destructure loc key expr
            | _, _ -> ())
      | RestProperty (_,_) -> ())
        (* Utils_js.prerr_endlinef "%s" "Cannot handle rest properties" *)
    in
      List.iter process_prop properties
  | _, Ast.Pattern.Array a ->
    let open Ast.Pattern.Array in
    let { elements; typeAnnotation=_} = a in
    let process_elem = fun e ->
     (match e with
     | Element (loc, _) -> this#map_id_to_incomplete loc
     | RestElement (_,_) -> ())
       (* Utils_js.prerr_endlinef "%s" "Cannot handle rest elements" *)
    in
    let process_elem_opt = fun e_opt ->
     (match e_opt with
       | Some e -> process_elem e
       | None -> ()) in
    List.iter process_elem_opt elements
  | _, _ -> () (* Deal with other names getting values? *)

  method! variable_declarator ~kind
    (decl: Ast.Statement.VariableDeclaration.Declarator.t) =
    (* Utils_js.print_endlinef "%s" "vardecl"; *)
    let open Ast.Statement.VariableDeclaration.Declarator in
    let decl' = super#variable_declarator ~kind decl in (* calls var_decl_pattern *)
    let (_, { id = patt ; init = e }) = decl' in
    this#assign_to_variable_declarator_pattern patt e;
    decl'

  method! expression (expr: Ast.Expression.t) =
    let open Ast.Expression in
    let open Dep in
    match expr with

    | loc, Identifier id ->
      (* Utils_js.print_endlinef "%s" "expression-identifier"; *)
      let open Dep in
      let id' = this#identifier id in
      (try
        let (d,i) = LocMap.find loc renamings in
        let dep = {key = (DepKey.Temp loc);
                    typeDep = Depends [DepKey.Id (d,i)];
                    valDep =  Depends [DepKey.Id (d,i)]} in
        depMap <- DepMap.add (DepKey.Temp loc) dep depMap
       with _ ->
        let dep = { key = (DepKey.Temp loc) ;
                     typeDep = Incomplete ;
                     valDep = Incomplete } in
        depMap <- DepMap.add (DepKey.Temp loc) dep depMap
       )
       ; if (id == id') then expr else loc, Identifier id'

    | loc, Literal l ->
      (* Utils_js.print_endlinef "%s" "expression-literal"; *)
      let l' = this#literal l in
      let dep = { key = DepKey.Temp loc;
                  typeDep = Dep.Primitive;
                  valDep = Dep.Primitive} in
      depMap <- DepMap.add (DepKey.Temp loc) dep depMap
      ; loc, Literal l'

    | loc, Ast.Expression.Object o ->
      (* Utils_js.print_endlinef "%s" "expression-object"; *)
      let open Dep in
      let o' = super#object_ o in
      (* Initialize dependence info for the AST node *)
      let dep = { key = DepKey.Temp loc;
                  typeDep = Dep.Object;
                  valDep = Dep.Object } in
      depMap <- DepMap.add (DepKey.Temp loc) dep depMap
      ;
      (* Initialize dependence info for HeapLocs implied by the obj literal *)
      let open Ast.Expression.Object in
      let { properties=properties } = o' in
      List.iter
        (fun prop ->
          match prop with
          | Property
            (_, { Property.key=key;
                  value=value;
                  _method=_;
                  shorthand=_ }) ->
            (match value, key with
            | Property.Init (eloc, _),
              Property.Identifier (_, name) ->
              let dkey = DepKey.HeapLoc (loc, name) in
              let dep =
                { Dep.key = dkey;
                  typeDep = Dep.Depends [DepKey.Temp eloc];
                  valDep = Dep.Depends [DepKey.Temp eloc] } in
                  depMap <- DepMap.add dkey dep depMap
            | _, _ -> ())
              (* Utils_js.prerr_endlinef "%s" "Only handled Ident : Init" *)
          | SpreadProperty _ -> ())
              (* Utils_js.prerr_endlinef "%s" "Not handled SpreadProperty" *)
      properties
      ;
      loc, Ast.Expression.Object o'

    | loc, Assignment a ->
      (* Utils_js.print_endlinef "%s" "expression-assignment"; *)
      let a' = this#assignment a in
      loc, Assignment a'

    | loc, Binary b ->
      (* Utils_js.print_endlinef "%s" "expression-binary"; *)
      let open Ast.Expression.Binary in
      let open Dep in
      let { operator = o; left; right } = b in
      let left' = this#expression left in
      let right' = this#expression right in
      let left_loc, _ = left' in
      let right_loc, _ = right' in
      let dep =
        {key = (DepKey.Temp loc);
         typeDep =
           Depends [DepKey.Temp left_loc; DepKey.Temp right_loc];
         valDep =
           Depends [DepKey.Temp left_loc; DepKey.Temp right_loc] }
      in
      depMap <- DepMap.add (DepKey.Temp loc) dep depMap
      ;
      if left == left' && right == right' then expr
      else loc, Binary { operator = o; left = left'; right = right' }

    | loc, TypeCast x ->
      (* Utils_js.print_endlinef "%s" "expression-typecast"; *)
      let open Ast.Expression.TypeCast in
      let open Dep in
      let { expression=e; typeAnnotation=ta } = x in
      let e' = this#expression e in
      let loc_e',_ = e' in
      let dep = { key = DepKey.Temp loc;
        typeDep = Annotation (ta, []);
        valDep = Depends [DepKey.Temp loc_e']} in
      depMap <- DepMap.add (DepKey.Temp loc) dep depMap
      ;
      if e' = e then expr
      else loc, TypeCast { expression = e'; typeAnnotation= ta }

    (* TODO Member: in the best case, we can retrieve the right HeapLocs
     *)

    | loc, Update x ->
    (* Utils_js.print_endlinef "%s" "expression-update"; *)
    let open Ast.Expression.Update in
    let x' = this#update_expression x in
    let { argument; operator = _; prefix = _ } = x' in
    (match argument with
    | loc_a, Ast.Expression.Identifier _ ->
      (try
        let (d,i) = LocMap.find loc_a renamings in
        (* let dep_id = DepMap.find (DepKey.Id (d,i)) depMap in *)
        let dep_expr = { key = DepKey.Temp loc;
                         valDep = Depends [DepKey.Id (d,i)];
                         typeDep = Depends [DepKey.Id (d,i)]} in (* number? *)
          (* v++ augments the dependence of v onto itself as expr *)
          depMap <- DepMap.add ~combine:merge_dep
            (DepKey.Id (d,i)) dep_expr depMap;
          (* the expression v++ depends on v *)
          depMap <- DepMap.add (DepKey.Temp loc) dep_expr depMap
      with _ -> ())
      (* Utils_js.prerr_endlinef "expression: Update %s" "id not recognized" *)
    (* TODO: deal with heap locations: they should not become incomplete *)
    | loc_e, _ ->
      let open Dep in
      let dep = { key = DepKey.Temp loc;
                valDep = Incomplete;
                typeDep = Incomplete } in
      depMap <- DepMap.add (DepKey.Temp loc_e) dep depMap
    )
    ;
    loc, Update x'

    | loc,_ ->
      (* Utils_js.print_endlinef "%s" "expression-other"; *)
      let open Dep in
      let dep = { key = DepKey.Temp loc;
                  valDep = Incomplete;
                  typeDep = Incomplete } in
      depMap <- DepMap.add (DepKey.Temp loc) dep depMap;
      super#expression expr

  method assign_to_assignment_pattern
    (pat: Ast.Pattern.t)
    (expr: Ast.Expression.t)
    (op : Ast.Expression.Assignment.operator) =
    (* Utils_js.print_endlinef "%s" "assign_to_assignment"; *)
    match pat with
    | _, Ast.Pattern.Identifier id ->
      (* Similar, but not identical to the corresponding case in
       * assign_to_variable_declarator_pattern. 1. The rhs expression
       * is not optional. 2. The += syntax can occur here. 3. We
       * ignore the type annotation here. *)
      let open Ast.Pattern.Identifier in
      let { name = (loc, _); typeAnnotation = _; _} = id in
      (try
        let (d,i) =
          LocMap.find loc renamings in
        let dep_left =
          DepMap.find (DepKey.Id (d,i)) depMap in
        let (loc_e, _) = expr in
        let dep_right =
          DepMap.find (DepKey.Temp loc_e) depMap in
        match op with
        | Ast.Expression.Assignment.Assign ->
          (* treat as = assignment *)
          depMap <-
            DepMap.add ~combine:merge_dep
              (DepKey.Id (d,i)) dep_right depMap
        | _ ->
          (* treat as += assignment *)
          depMap <-
            DepMap.add ~combine:merge_dep
              (DepKey.Id (d,i)) dep_left depMap;
          depMap <-
            DepMap.add ~combine:merge_dep
              (DepKey.Id (d,i)) dep_right depMap
        with _ -> ())
        (* Utils_js.prerr_endlinef
          "assign_to_assignment_pattern:Identifier %s" "id not recognized" *)

    | _, Ast.Pattern.Array a ->
    (* This is identical to the corresponding case in
     * assign_to_variable_declarator_pattern.  TODO - refactor.
     *)
      let open Ast.Pattern.Array in
      let { elements; typeAnnotation=_} = a in
      let process_elem = fun e ->
       (match e with
       | Element (loc, _) -> this#map_id_to_incomplete loc
       | RestElement (_,_) -> ())
        (* Utils_js.prerr_endlinef "%s" "Cannot handle rest elements" *)
      in
      let process_elem_opt = fun e_opt ->
       (match e_opt with
         | Some e -> process_elem e
         | None -> ()) in
      List.iter process_elem_opt elements

    | _, _ -> ()  (* TODO deal with the case e.p = e'. Update or havoc *)

  method! assignment (expr: Ast.Expression.Assignment.t) =
    (* Utils_js.print_endlinef "%s" "assignment"; *)
    let open Ast.Expression.Assignment in
    let { operator = op; left; right } = expr in
    let left' = this#assignment_pattern left in
    let right' = this#expression right in
    this#assign_to_assignment_pattern left' right' op
    ;
    if left == left' && right == right' then expr
    else { expr with left = left'; right = right' }

  method! for_of_statement_lhs (left: Ast.Statement.ForOf.left) =
    let open Ast.Statement.ForOf in
    let open Ast.Statement.VariableDeclaration in
    match left with
    | LeftDeclaration (loc, decl) ->
      let decl' = super#variable_declaration decl in
      (* Even though variable_declarator is handled elsewhere,
        For the For-of case, it does not see the rhs assignment.
        So we need to havoc that declarator here. *)
      (try
        let {declarations; _} = decl' in
        let var_locs = List.map (fun (l,_) -> l) declarations in
        (* For this decl, let the variable become Incomplete *)
        List.iter this#map_id_to_incomplete var_locs
      with _ -> ())
      (*  Utils_js.prerr_endlinef "%s" "Could not find var loc in for-of" *)
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

  method! for_in_statement_lhs (left: Ast.Statement.ForIn.left) =
    (* Almost identical to the for-of case *)
    let open Ast.Statement.ForIn in
    let open Ast.Statement.VariableDeclaration in
    match left with
    | LeftDeclaration (loc, decl) ->
      let decl' = super#variable_declaration decl in
      (* Even though variable_declarator is handled elsewhere,
        For the For-In case, it does not see the rhs assignment.
        So we need to havoc that declarator here. *)
      (try
        let {declarations; _} = decl' in
        let var_locs = List.map (fun (l,_) -> l) declarations in
        (* For this decl, let the variable become Incomplete *)
        List.iter this#map_id_to_incomplete var_locs
      with _ -> ())
      (*  Utils_js.prerr_endlinef "%s" "Could not find var loc in for-in" *)
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
