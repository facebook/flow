(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module is the entry point of the typechecker. It sets up subtyping
   constraints for every expression, statement, and declaration form in a
   JavaScript AST; the subtyping constraints are themselves solved in module
   Flow_js. It also manages environments, including not only the maintenance of
   scope information for every function (pushing/popping scopes, looking up
   variables) but also flow-sensitive information about local variables at every
   point inside a function (and when to narrow or widen their types). *)

open Utils

module Ast = Spider_monkey_ast

open Modes_js
open Reason_js
open Constraint_js
open Type

module Abnormal : sig

  type abnormal =
    | Return
    | Break of string option
    | Continue of string option
  exception Exn of abnormal
  val swap: abnormal -> bool -> bool
  val set: abnormal -> unit
  val raise_exn: abnormal -> 'a
  val exception_handler: (unit -> 'a) -> (abnormal -> 'a) -> 'a

end = struct

  type abnormal =
    | Return
    | Break of string option
    | Continue of string option

  exception Exn of abnormal

  let raise_exn abnormal =
    raise (Exn abnormal)

  let exception_handler main handler =
    try main ()
    with
    | Exn abnormal -> handler abnormal
    | exn -> raise exn

  (* thread-local global to detect abnormal control flows *)
  let abnormals = Hashtbl.create 0

  let check abnormal =
    Hashtbl.find abnormals abnormal

  let set abnormal =
    Hashtbl.replace abnormals abnormal true;
    raise_exn abnormal

  let swap abnormal newv =
    let oldv = try check abnormal with _ -> false in
    Hashtbl.replace abnormals abnormal newv;
    oldv

end

(**** types ****)

(*************)
(* Utilities *)
(*************)

(* composition *)
let (>>) f g = fun x -> g (f (x))

let mk_object cx reason =
  Flow_js.mk_object_with_proto cx reason (MixedT reason)

let extend_object cx reason o = function
  | None -> o
  | Some other ->
      Flow_js.unit_flow cx (other, ObjAssignT (reason, o, AnyT.t));
      o

let summarize cx t = match t with
  | OpenT _ ->
      let reason = reason_of_t t in
      Flow_js.mk_tvar_where cx reason (fun tvar ->
        Flow_js.unit_flow cx (t, SummarizeT (reason, tvar))
      )
  (* These remaining cases simulate SummarizeT semantics, and are a slight
     optimization in that they avoid creation of fresh type
     variables. Semantically, we could have the above case fire unconditionally,
     since the fresh type variable is unified in all cases. *)
  | StrT (reason, Some _) -> StrT.why reason
  | NumT (reason, Some _) -> NumT.why reason
  | _ -> t

(* given a module name, return associated tvar if already
 * present in module map, or create and add *)
let module_t cx m reason =
  match SMap.get m cx.modulemap with
  | Some t -> t
  | None ->
      Flow_js.mk_tvar_where cx reason (fun t ->
        cx.modulemap <- cx.modulemap |> SMap.add m t;
      )

let require cx m m_name loc =
  cx.required <- SSet.add m cx.required;
  cx.require_loc <- SMap.add m loc cx.require_loc;
  module_t cx m (mk_reason m_name loc)

let exports cx m =
  module_t cx m (Reason_js.new_reason "exports" (Pos.make_from
    (Relative_path.create Relative_path.Dummy cx.file)))

let lookup_module cx m = SMap.find_unsafe m cx.modulemap

(* Destructuring visitor for tree-shaped patterns, parameteric over an action f
   to perform at the leaves. A type for the pattern is passed, which is taken
   apart as the visitor goes deeper. *)

let rec destructuring cx t f = Ast.Pattern.(function
  | loc, Array { Array.elements; _; } -> Array.(
      let reason = mk_reason "array pattern" loc in
      elements |> List.iteri (fun i -> function
        | Some (Element p) ->
            let i = NumT (mk_reason "number" loc, Some (string_of_int i)) in
            let tvar = Flow_js.mk_tvar cx reason in
            Flow_js.unit_flow cx (t, GetElemT(reason,i,tvar));
            destructuring cx tvar f p
        | Some (Spread (loc, { SpreadElement.argument })) ->
            error_destructuring cx loc
        | None ->
            ()
      )
    )

  | loc, Object { Object.properties; _; } -> Object.(
      let reason = mk_reason "object pattern" loc in
      let xs = ref [] in
      properties |> List.iter (function
        | Property (loc, prop) -> Property.(
            match prop with
            | { key = Identifier (_, id); pattern = p; } ->
                let x = id.Ast.Identifier.name in
                xs := x :: !xs;
                let tvar = Flow_js.mk_tvar cx reason in
                Flow_js.unit_flow cx (t, GetT(reason,x,tvar));
                destructuring cx tvar f p
            | _ ->
              error_destructuring cx loc
          )
        | SpreadProperty (loc, { SpreadProperty.argument }) ->
            let tvar = Flow_js.mk_tvar cx reason in
            Flow_js.unit_flow cx (t, ObjRestT(reason,!xs,tvar));
            destructuring cx tvar f argument
      )
    )

  | loc, Identifier (_, { Ast.Identifier.name; _ }) ->
      let type_ =
        if Type_inference_hooks_js.dispatch_id_hook cx name loc
        then AnyT.at loc
        else t in
      f cx loc name type_

  | loc, _ -> error_destructuring cx loc
)

and error_destructuring cx loc =
  let msg = "unsupported destructuring" in
  Flow_js.add_error cx [mk_reason "" loc, msg]

and type_of_pattern = Ast.Pattern.(function
  | loc, Array { Array.typeAnnotation; _; } -> typeAnnotation

  | loc, Object { Object.typeAnnotation; _; } -> typeAnnotation

  | loc, Identifier (_, { Ast.Identifier.typeAnnotation; _; }) -> typeAnnotation

  | loc, _ -> None
)

(* instantiate pattern visitor for assignments *)
let destructuring_assignment cx t =
  destructuring cx t (fun cx loc name t ->
    let reason = mk_reason (spf "assignment of identifier %s" name) loc in
    Env_js.set_var cx name t reason
  )

(* instantiate pattern visitor for parameters *)
let destructuring_map cx t p =
  let tmap, lmap = ref SMap.empty, ref SMap.empty in
  p |> destructuring cx t (fun _ loc name t ->
    tmap := !tmap |> SMap.add name t;
    lmap := !lmap |> SMap.add name loc
  );
  !tmap, !lmap

let pattern_decl cx t =
  destructuring cx t (fun cx loc name t ->
    Hashtbl.replace cx.type_table loc t;
    Env_js.init_env cx name (create_env_entry t t (Some loc))
  )

(**************)
(* Query/Fill *)
(**************)

(* These computations should trigger ground_type calls on the types returned by
   query_type/fill_types: in general those types may not be ground (the only
   non-ground parts should be strict_requires).

   1. Look up InfoHeap(cx.file) to get strict_reqs.

   2. Look up ContextHeap(NameHeap(strict_req)) to get strict_cxs that cx
   depends on, and so on.

   3. Next, look up their exported types via recursive calls to
   lookup_type(lookup_module(strict_cx,strict_cx._module)).

   In fact, 2. and 3. could be optimized: we could store exported types
   (possibly not ground) in InfoHeap, so that they are cached. This means that
   we could look up InfoHeap(NameHeap(strict_req)) to get strict_req_types
   directly, instead of going through ContextHeap.

   Note that exported types do not need to be blown away unless their files
   change, since they are locally determined; instead, we ground them as
   necessary.
*)

(*
let rec ground_export exports r =
  if not (Hashtbl.mem exports r) then (
    let file = Module_js.get_file(r) in
    let _, _, strict_reqs, exported_type =
      Module_js.get_module_info(file) in
    let ground_exported_type =
      ground_exports strict_reqs exports exported_type in
    Hashtbl.add exports r ground_exported_type
  )

and ground_exports rs exports t =
  SSet.iter (ground_export exports) rs;
  ground_exported_type exports t
*)

let query_type cx pos =
  let result = ref (Pos.none, None, []) in
  let diff = ref (max_int, max_int) in
  Hashtbl.iter (fun range t ->
    if in_range pos range
    then (
      let d = diff_range range in
      if d < !diff then (
        diff := d;
        Flow_js.suggested_type_cache := IMap.empty;
        let ground_t = Flow_js.ground_type cx ISet.empty t in
        let possible_ts = Flow_js.possible_types_of_type cx t in
        result := if is_printed_type_parsable cx ground_t
          then (Reason_js.pos_of_loc range, Some ground_t, possible_ts)
          else (Reason_js.pos_of_loc range, None, possible_ts)
      )
    )
  ) cx.type_table;
  !result

(********)
(* Fill *)
(********)

let fill_types cx =
  Flow_js.suggested_type_cache := IMap.empty;
  Hashtbl.fold (fun pos t list ->
    let line, start, end_ = Pos.info_pos pos in
    let t = Flow_js.ground_type cx ISet.empty t in
    if is_printed_type_parsable cx t then
      (line, end_, spf ": %s" (string_of_t cx t))::list
    else list
  ) cx.annot_table []

(* AST helpers *)

let error_type cx loc msg =
  let reason = mk_reason "" loc in
  Flow_js.add_error cx [reason, msg];
  AnyT reason

let check_type_param_arity cx loc params n f =
  if List.length params = n
  then f ()
  else
    let msg = spf "Incorrect number of type parameters (expected %n)" n in
    error_type cx loc msg

(**********************************)
(* Transform annotations to types *)
(**********************************)

(* converter *)
let rec convert cx map = Ast.Type.(function

  | loc, Any -> AnyT.at loc

  | loc, Void -> void_ loc

  | loc, Number -> NumT.at loc

  | loc, String -> StrT.at loc

  | loc, Boolean -> BoolT.at loc

  | loc, Nullable t -> MaybeT (convert cx map t)

  | loc, Union ts ->
      let ts = List.map (convert cx map) ts in
      (* "Flatten" out any unions in this union, like
       * var a: number | (string | bool) *)
      let ts = List.map (function
        | UnionT (r, ts) -> ts
        | t -> [t]) ts in
      let ts = List.flatten ts in
      UnionT (mk_reason "union type" loc, ts)

  | loc, Intersection ts ->
      let ts = List.map (convert cx map) ts in
      IntersectionT (mk_reason "intersection type" loc, ts)

  | loc, Typeof x ->
      (match x with
      | (loc, Generic {
          Generic.id = qualification;
          typeParameters = None
        }) ->
          convert_qualification cx qualification
      | _ ->
        error_type cx loc "Unexpected typeof expression")

  | loc, Tuple ts ->
      let elts = List.map (convert cx map) ts in
      let reason = mk_reason "tuple type" loc in
      let element_reason = mk_reason "tuple element" loc in
      let tx =
        if ts = []
        then Flow_js.mk_tvar cx element_reason
        else UnionT (element_reason, elts) in
      ArrT (reason, tx, elts)

  | loc, Array t ->
      let r = mk_reason "array type" loc in
      let t = convert cx map t in
      ArrT (r, t, [])

  | loc, StringLiteral { StringLiteral.value; _ }  ->
    let reason = mk_reason "string literal type" loc in
      EnumT
        (reason,
         Flow_js.mk_object_with_map_proto cx reason
           (SMap.singleton value AnyT.t) (MixedT reason))

  (* TODO *)
  | loc, Generic { Generic.id = Generic.Identifier.Qualified (_,
         { Generic.Identifier.qualification; id; }); typeParameters } ->

    let m = convert_qualification cx qualification in
    let _, { Ast.Identifier.name; _ } = id in
    let reason = mk_reason name loc in
    let t = Flow_js.mk_tvar_where cx reason (fun t ->
      Flow_js.unit_flow cx (m, GetT (reason, name, t));
    ) in
    let typeParameters = extract_type_param_instantiations typeParameters in
    let params = if typeParameters = []
      then None else Some typeParameters in
    mk_nominal_type_ cx reason map (t, params)

  (* type applications: name < params > *)
  | loc, Generic {
      Generic.id = Generic.Identifier.Unqualified (id);
      typeParameters
    } ->
    let _, { Ast.Identifier.name; _ } = id in
    let typeParameters = extract_type_param_instantiations typeParameters in

    (match name with

      (* TODO Type.Mixed *)
      | "mixed" ->
        check_type_param_arity cx loc typeParameters 0 (fun () ->
          MixedT.at loc
        )

      (* Array<T> *)
      | "Array" ->
        check_type_param_arity cx loc typeParameters 1 (fun () ->
          let t = convert cx map (List.hd typeParameters) in
          ArrT (mk_reason "array type" loc, t, [])
        )

      (* $Either<...T> is the union of types ...T *)
      | "$Either" ->
        let ts = List.map (convert cx map) typeParameters in
        UnionT (mk_reason "union type" loc, ts)

      (* $All<...T> is the intersection of types ...T *)
      | "$All" ->
        let ts = List.map (convert cx map) typeParameters in
        IntersectionT (mk_reason "intersection type" loc, ts)

      (* $Tuple<...T> is the tuple of types ...T *)
      | "$Tuple" ->
        let ts = List.map (convert cx map) typeParameters in
        ArrT (mk_reason "tuple type" loc, AnyT.t, ts)

      (* $Supertype<T> acts as any over supertypes of T *)
      | "$Supertype" ->
        check_type_param_arity cx loc typeParameters 1 (fun () ->
          UpperBoundT (convert cx map (List.hd typeParameters))
        )

      (* $Subtype<T> acts as any over subtypes of T *)
      | "$Subtype" ->
        check_type_param_arity cx loc typeParameters 1 (fun () ->
          LowerBoundT (convert cx map (List.hd typeParameters))
        )

      (* $Enum<T> is the set of keys of T *)
      | "$Enum" ->
        check_type_param_arity cx loc typeParameters 1 (fun () ->
          let t = convert cx map (List.hd typeParameters) in
          EnumT (mk_reason "enum type" loc, t)
        )

      (* $Record<T> is the type of objects whose keys are those of T *)
      | "$Record" ->
        check_type_param_arity cx loc typeParameters 1 (fun () ->
          let t = convert cx map (List.hd typeParameters) in
          RecordT (mk_reason "record type" loc, t)
        )

      | "Function" | "function" ->
        let reason = mk_reason "function type" loc in
        FunT (
          reason,
          AnyT.at loc,
          AnyT.at loc,
          {
            this_t = AnyT.at loc;
            params_tlist = [RestT (AnyT.at loc)];
            params_names = None;
            return_t = AnyT.at loc;
            closure_t = 0
          })

      | "Object" ->
        let reason = mk_reason "object type" loc in
        Flow_js.mk_object_with_proto cx reason (AnyT.at loc)

      (* Custom classes *)
      | "ReactClass" ->
        let reason = mk_reason "ReactClass" loc in
        CustomClassT
          ("ReactClass",
           (List.map (convert cx map) typeParameters),
           AnyT.why reason)

      (* in-scope type vars *)
      | _ when SMap.mem name map ->
        check_type_param_arity cx loc typeParameters 0 (fun () ->
          SMap.find_unsafe name map
        )

      (* other applications with id as head expr *)
      | _ ->
        let reason = mk_reason name loc in
        let id_expr = fst id, Ast.Expression.Identifier id in
        let params = if typeParameters = []
          then None else Some typeParameters in
        mk_nominal_type cx reason map (id_expr, params)
    )

  (* TODO: unsupported generators *)
  | loc, Function { Function.params; returnType; rest; typeParameters } ->
    let typeparams, _, map_ = mk_type_param_declarations cx typeParameters in
    let map = SMap.fold SMap.add map_ map in

    let rev_params_tlist, rev_params_names =
      (let rev_tlist, rev_pnames =
        List.fold_left (fun (tlist, pnames) param ->
        match param with
        | _, { Function.Param.name;
               Function.Param.typeAnnotation; optional = false; _ } ->
            (convert cx map typeAnnotation) :: tlist,
            ((snd name).Ast.Identifier.name) :: pnames
        | _, { Function.Param.name;
               Function.Param.typeAnnotation; optional = true; _ } ->
            (OptionalT (convert cx map typeAnnotation)) :: tlist,
            ((snd name).Ast.Identifier.name) :: pnames
      ) ([], []) params in
      match rest with
        | Some (_, { Function.Param.name;
                     Function.Param.typeAnnotation; _ }) ->
            let rest = mk_rest cx (convert cx map typeAnnotation) in
            rest :: rev_tlist,
            ((snd name).Ast.Identifier.name) :: rev_pnames
        | None -> rev_tlist, rev_pnames
      ) in
    let ft =
      FunT (
        mk_reason "function type" loc,
        Flow_js.dummy_static,
        Flow_js.mk_tvar cx (mk_reason "prototype" loc),
        {
          this_t = Flow_js.mk_tvar cx (mk_reason "this" loc);
          params_tlist = (List.rev rev_params_tlist);
          params_names = Some (List.rev rev_params_names);
          return_t = convert cx map returnType;
          closure_t = 0
        })
    in
    if (typeparams = []) then ft else PolyT(typeparams, ft)

  | loc, Object { Object.properties; indexers; callProperties; } ->
    let map_ = List.fold_left (fun map_ ->
      Object.Property.(fun (loc, { key; value; optional; static; }) ->
        (match key with
          | Ast.Expression.Object.Property.Identifier
              (_, { Ast.Identifier.name; _ }) when not optional ->
            let t = convert cx map value in
            SMap.add name t map_
          | _ ->
            let msg = "Unsupported key in object type" in
            Flow_js.add_error cx [mk_reason "" loc, msg];
            map_
        )
      )
    ) SMap.empty properties
    in
    let map_ = match callProperties with
      | [] -> map_
      | [loc, { Object.CallProperty.value = (_, ft); _; }] -> SMap.add "$call" (convert cx map (loc, Ast.Type.Function ft)) map_
      | fts ->
          let fts = List.map
            (fun (loc, { Object.CallProperty.value = (_, ft); _; }) ->
                convert cx map (loc, Ast.Type.Function ft))
            fts in
        SMap.add "$call" (IntersectionT (mk_reason "object type" loc, fts)) map_
    in
    (* Seal an object type unless it specifies an indexer. *)
    let sealed, key, value = Object.Indexer.(
      match indexers with
      | [(_, { key; value; _; })] ->
          let keyt = convert cx map key in
          let valuet = convert cx map value in
          false, keyt, valuet
      | [] ->
          true, MixedT (mk_reason "key" loc), MixedT (mk_reason "value" loc)
      (* TODO *)
      | _ -> failwith "Unimplemented: multiple indexers"
    ) in
    let pmap = Flow_js.mk_propmap cx map_ in
    let proto = MixedT (reason_of_string "Object") in
    ObjT (mk_reason "object type" loc,
      Flow_js.mk_objecttype ~sealed (key, value) pmap proto)
  )

and convert_qualification cx = Ast.Type.Generic.Identifier.(function
  | Qualified (loc, { qualification; id; }) ->

    let m = convert_qualification cx qualification in
    let _, { Ast.Identifier.name; _ } = id in
    let reason = mk_reason name loc in
    Flow_js.mk_tvar_where cx reason (fun t ->
      Flow_js.unit_flow cx (m, GetT (reason, name, t));
    )

  | Unqualified (id) ->

    let loc, { Ast.Identifier.name; _ } = id in
    let reason = mk_reason name loc in
    Env_js.get_var cx name reason
)

and mk_rest cx = function
  | ArrT(_, t, []) -> RestT t
  | t ->
      (* unify t with Array<e>, return (RestT e) *)
      let reason = prefix_reason "element of " (reason_of_t t) in
      let tvar = Flow_js.mk_tvar cx reason in
      let arrt = ArrT(reason, tvar, []) in
      Flow_js.unify cx t arrt;
      RestT tvar

and mk_type cx reason = mk_type_ cx SMap.empty reason

and mk_type_ cx map reason = function
  | None ->
      let t =
        if cx.weak
        then AnyT.why reason
        else Flow_js.mk_tvar cx reason
      in
      Hashtbl.replace cx.annot_table (pos_of_reason reason) t;
      t

  | Some annot ->
      convert cx map annot

and mk_type_annotation cx reason = mk_type_annotation_ cx SMap.empty reason

and mk_type_annotation_ cx map reason = function
  | None -> mk_type_ cx map reason None
  | Some (loc, typeAnnotation) -> mk_type_ cx map reason (Some typeAnnotation)

(************)
(* Visitors *)
(************)

(* TODO: detect structural misuses abnormal control flow constructs *)
and statement_decl cx = Ast.Statement.(

  (* helpers *)
  let var_declarator cx (loc, { VariableDeclaration.Declarator.id; init }) =
    Ast.(match id with
    | (loc, Pattern.Identifier (_, { Identifier.name; typeAnnotation; _ })) ->
        let r = mk_reason (spf "var %s" name) loc in
        let t = mk_type_annotation cx r typeAnnotation in
        Hashtbl.replace cx.type_table loc t;
        Env_js.init_env cx name (create_env_entry t t (Some loc))
    | p ->
        let r = mk_reason "var _" loc in
        let t = type_of_pattern p |> mk_type_annotation cx r in
        pattern_decl cx t p
    )
  in

  (* TODO const, let? *)
  let variable_declaration cx loc { VariableDeclaration.declarations; kind } =
    match kind with
    | VariableDeclaration.Const ->
        let msg = "Unsupported variable declaration: const" in
        Flow_js.add_error cx [mk_reason "" loc, msg]
    | VariableDeclaration.Let ->
        let msg = "Unsupported variable declaration: let" in
        Flow_js.add_error cx [mk_reason "" loc, msg]
    | VariableDeclaration.Var ->
        List.iter (var_declarator cx) declarations
  in

  let block_body cx { Block.body } =
    List.iter (statement_decl cx) body
  in

  let catch_clause cx { Try.CatchClause.body = (_, b); _ } =
    block_body cx b
  in

  function

  | (loc, Empty) -> ()

  | (loc, Block b) ->
      block_body cx b

  | (loc, Expression _) -> ()

  | (loc, If { If.test; consequent; alternate }) ->
      statement_decl cx consequent;
      (match alternate with
        | None -> ()
        | Some st -> statement_decl cx st
      )

  | (loc, Labeled { Labeled.label; body }) ->
      statement_decl cx body

  | (loc, Break _) -> ()

  | (loc, Continue _) -> ()

  | (loc, With _) ->
      (* TODO disallow or push vars into env? *)
      ()

  | (loc, TypeAlias { TypeAlias.id; typeParameters; right; } ) ->
      let _, { Ast.Identifier.name; _ } = id in
      let r = mk_reason (spf "type %s" name) loc in
      let tvar = Flow_js.mk_tvar cx r in
      Env_js.init_env cx name (create_env_entry tvar tvar (Some loc))

  | (loc, Switch { Switch.discriminant; cases; lexical }) ->
      (* TODO: ensure that default is last *)
      List.iter (fun (loc, { Switch.Case.test; consequent }) ->
        List.iter (statement_decl cx) consequent
      ) cases

  | (loc, Return _) -> ()

  | (loc, Throw _) -> ()

  | (loc, Try { Try.block = (_, b); handler; guardedHandlers; finalizer }) ->
      block_body cx b;

      (match handler with
        | None -> ()
        | Some (loc, h) -> catch_clause cx h
      );

      List.iter (fun (loc, h) ->
        catch_clause cx h
      ) guardedHandlers;

      (match finalizer with
        | None -> ()
        | Some (_, b) -> block_body cx b
      )

  | (loc, While { While.test; body }) ->
      statement_decl cx body

  | (loc, DoWhile { DoWhile.body; test }) ->
      statement_decl cx body

  | (loc, For { For.init; test; update; body }) ->
      (match init with
        | Some (For.InitDeclaration (loc, decl)) ->
            variable_declaration cx loc decl
        | _ -> ()
      );
      statement_decl cx body

  | (loc, ForIn { ForIn.left; right; body; each }) ->
      (match left with
        | ForIn.LeftDeclaration (loc, decl) ->
            variable_declaration cx loc decl
        | _ -> ()
      );
      statement_decl cx body

  | (loc, ForOf _) ->
      (* TODO? *)
      ()

  | (loc, Let _) ->
      (* TODO *)
      ()

  | (loc, Debugger) -> ()

  | (loc, FunctionDeclaration { FunctionDeclaration.id; _ }) ->
      let _, { Ast.Identifier.name; _ } = id in
      let r = mk_reason (spf "function %s" name) loc in
      let tvar = Flow_js.mk_tvar cx r in
      Env_js.init_env cx name (create_env_entry tvar tvar (Some loc))

  | (loc, DeclareVariable { DeclareVariable.id; })
  | (loc, DeclareFunction { DeclareFunction.id; }) ->
      let _, { Ast.Identifier.name; typeAnnotation; _; } = id in
      let r = mk_reason (spf "declare %s" name) loc in
      let t = mk_type_annotation cx r typeAnnotation in
      Hashtbl.replace cx.type_table loc t;
      Env_js.init_env cx name (create_env_entry t t (Some loc))

  | (loc, VariableDeclaration decl) ->
      variable_declaration cx loc decl

  | (loc, ClassDeclaration { Class.id; _ }) ->
      let _, { Ast.Identifier.name; _ } = id in
      let r = mk_reason (spf "class %s" name) loc in
      let tvar = Flow_js.mk_tvar cx r in
      Env_js.init_env cx name (create_env_entry tvar tvar (Some loc))

  | (loc, DeclareClass { Interface.id; _ })
  | (loc, InterfaceDeclaration { Interface.id; _ }) ->
      let _, { Ast.Identifier.name; _ } = id in
      let r = mk_reason (spf "class %s" name) loc in
      let tvar = Flow_js.mk_tvar cx r in
      Env_js.init_env cx name (create_env_entry tvar tvar (Some loc))
  | (loc, DeclareModule { DeclareModule.id; _ }) ->
      let name = match id with
      | DeclareModule.Identifier (_, id) -> id.Ast.Identifier.name
      | DeclareModule.Literal (_, { Ast.Literal.value = Ast.Literal.String str; _; }) ->
          str
      | _ ->
          (* The only literals that we should see as module names are strings *)
          assert false in
      let r = mk_reason (spf "module %s" name) loc in
      let t = mk_object cx r in
      Hashtbl.replace cx.type_table loc t;
      Env_js.init_env cx (spf "$module__%s" name) (create_env_entry t t (Some loc))
  | (_, ExportDeclaration _) ->
      (* TODO *)
      failwith "Unimplemented: ExportDeclaration"
  | (_, ImportDeclaration _) ->
      (* TODO *)
      failwith "Unimplemented: ImportDeclaration"
)

and toplevels cx stmts =
  let n = ref 0 in
  let stmts = List.filter Ast.Statement.(function
    | (loc, Empty) -> false
    | _ -> true
  ) stmts
  in
  Abnormal.exception_handler (fun () ->
    stmts |> List.iter (fun stmt ->
      statement cx stmt;
      incr n
    )
  )
    (fun exn ->
      if !n < List.length stmts - 1
      then (
        (* !n+1 is the location of unreachable code *)
        let msg = "unreachable code" in
        let (loc, _) = List.nth stmts !n in
        Flow_js.add_warning cx [mk_reason "" loc, msg]
      );
      Abnormal.raise_exn exn
    )

and statement cx = Ast.Statement.(

  let variables cx loc { VariableDeclaration.declarations; kind } =
    List.iter (variable cx) declarations
  in

  let catch_clause cx { Try.CatchClause.param; guard; body = (_, b) } =
    Ast.Pattern.(match param with
      | loc, Identifier (_, {
          Ast.Identifier.name; typeAnnotation = None; _
        }) ->
          let t = Flow_js.mk_tvar cx (mk_reason "catch" loc) in
          Env_js.let_env
            name
            (create_env_entry t t (Some loc))
            (fun () -> toplevels cx b.Block.body)

      | loc, Identifier (_, { Ast.Identifier.name; _ }) ->
          let msg = "type annotations for catch params not yet supported" in
          Flow_js.add_error cx [mk_reason "" loc, msg]

      | loc, _ ->
          let msg = "unsupported catch parameter declaration" in
          Flow_js.add_error cx [mk_reason "" loc, msg]
    )
  in

  function

  | (loc, Empty) -> ()

  | (loc, Block { Block.body }) ->
      toplevels cx body

  | (loc, Expression { Expression.expression = e }) ->
      ignore (expression cx e)

  (* Refinements for `if` are derived by the following Hoare logic rule:

     [Pre & c] S1 [Post1]
     [Pre & ~c] S2 [Post2]
     Post = Post1 | Post2
     ----------------------------
     [Pre] if c S1 else S2 [Post]
  *)
  | (loc, If { If.test; consequent; alternate }) ->
      let reason = mk_reason "if" loc in
      let _, pred, not_pred, xts = predicate_of_condition cx test in
      let ctx = !Env_js.env in
      let oldset = Env_js.swap_changeset (fun _ -> SSet.empty) in

      let exception_then, exception_else = ref None, ref None in

      let then_ctx = Env_js.clone_env ctx in
      Env_js.update_frame cx then_ctx;
      Env_js.refine_with_pred cx reason pred xts;

      mark_exception_handler
        (fun () -> statement cx consequent)
        exception_then;

      let else_ctx = Env_js.clone_env ctx in
      Env_js.update_frame cx else_ctx;
      Env_js.refine_with_pred cx reason not_pred xts;
      (match alternate with
        | None -> ()
        | Some st ->
            mark_exception_handler
              (fun () -> statement cx st)
              exception_else;
      );

      let newset = Env_js.swap_changeset (SSet.union oldset) in
      Env_js.merge_env cx reason (ctx, then_ctx, else_ctx) newset;
      Env_js.update_frame cx ctx;

      raise_and_exception !exception_then !exception_else;


  | (loc, Labeled { Labeled.label = _, { Ast.Identifier.name; _ }; body }) ->
      (match body with
      | (loc, While _)
      | (loc, DoWhile _)
      | (loc, For _)
      | (loc, ForIn _)
        ->
        let reason = mk_reason "label" loc in
        let oldset = Env_js.swap_changeset (fun _ -> SSet.empty) in
        let label = Some name in
        let save_break_exn = Abnormal.swap (Abnormal.Break label) false in
        let save_continue_exn = Abnormal.swap (Abnormal.Continue label) false in

        let ctx = !Env_js.env in
        Env_js.widen_env cx reason;

        let loop_ctx = Env_js.clone_env ctx in
        Env_js.update_frame cx loop_ctx;

        ignore_break_continue_exception_handler
          (fun () -> statement cx body)
          label
          Abnormal.raise_exn;
        let newset = Env_js.swap_changeset (SSet.union oldset) in
        if (Abnormal.swap (Abnormal.Continue label) save_continue_exn)
        then Env_js.havoc_env2 newset;
        Env_js.copy_env cx reason (ctx,loop_ctx) newset;

        if (Abnormal.swap (Abnormal.Break label) save_break_exn)
        then Env_js.havoc_env2 newset

      | _ ->
        let oldset = Env_js.swap_changeset (fun _ -> SSet.empty) in
        let label = Some name in
        let save_break_exn = Abnormal.swap (Abnormal.Break label) false in
        ignore_break_exception_handler
          (fun () -> statement cx body)
          label;
        let newset = Env_js.swap_changeset (SSet.union oldset) in
        if (Abnormal.swap (Abnormal.Break label) save_break_exn)
        then Env_js.havoc_env2 newset
      )

  | (loc, Break { Break.label }) ->
      let label_opt = match label with
        | None -> None
        | Some (_, { Ast.Identifier.name; _ }) -> Some name
      in
      Env_js.clear_env (mk_reason "break" loc);
      Abnormal.set (Abnormal.Break label_opt)

  | (loc, Continue { Continue.label }) ->
      let label_opt = match label with
        | None -> None
        | Some (_, { Ast.Identifier.name; _ }) -> Some name
      in
      Env_js.clear_env (mk_reason "continue" loc);
      Abnormal.set (Abnormal.Continue label_opt)

  | (loc, With _) ->
      (* TODO or disallow? *)
      ()

  | (loc, TypeAlias { TypeAlias.id; typeParameters; right; } ) ->
      let _, { Ast.Identifier.name; _ } = id in
      let r = mk_reason (spf "type %s" name) loc in
      let typeparams, _, type_params_map =
        mk_type_param_declarations cx typeParameters in
      let t = convert cx type_params_map right in

      (* ClassT acts like the kind of whatever type it "wraps." The
         corresponding "unwrap" operation is `mk_instance`. Both these should
         probably be renamed. *)
      let type_ =
        if typeparams = []
        then TypeT (r, t)
        else PolyT(typeparams, TypeT (r, t))
      in
      Hashtbl.replace cx.type_table loc type_;
      Env_js.set_var cx name type_ r

  | (loc, Switch { Switch.discriminant; cases; lexical }) ->

      ignore (expression cx discriminant);
      let save_break_exn = Abnormal.swap (Abnormal.Break None) false in

      let default = ref false in
      let ctx = !Env_js.env in
      let last_ctx = ref ctx in
      let oldset = Env_js.swap_changeset (fun _ -> SSet.empty) in

      let exceptions = List.rev_map (fun (loc, case) ->
        if !default then None (* TODO: error when case follows default *)
        else (
          let case_ctx = Env_js.clone_env ctx in
          let reason = mk_reason "case" loc in
          Env_js.update_frame cx case_ctx;
          Env_js.merge_env cx reason (case_ctx,ctx,!last_ctx) !Env_js.changeset;

          let exception_ = ref None in
          mark_exception_handler (fun () ->
            ignore_break_exception_handler Ast.Statement.Switch.Case.(fun () ->
              match case.test with
              | None ->
                  default := true;
                  toplevels cx case.consequent
              | Some expr ->
                  ignore (expression cx expr);
                  toplevels cx case.consequent
            ) None
          ) exception_;
          last_ctx := case_ctx;
          !exception_
        )
      ) cases
      in

      let newset = Env_js.swap_changeset (SSet.union oldset) in
      if not !default
      then (
        let default_ctx = Env_js.clone_env ctx in
        let reason = mk_reason "default" loc in
        Env_js.update_frame cx default_ctx;
        Env_js.merge_env cx reason (default_ctx,ctx,!last_ctx) newset
      );

      if Abnormal.swap (Abnormal.Break None) save_break_exn
      then Env_js.havoc_env2 newset;

      if !default
      then (
        match List.hd exceptions with
        | Some exn
            when List.for_all (fun exc -> exc = Some exn) (List.tl exceptions)
            -> Abnormal.raise_exn exn
        | _
            -> ()
      )

  | (loc, Return { Return.argument }) ->
      let reason = mk_reason "return" loc in
      let ret =
        Env_js.get_var cx (internal_name "return") reason
      in
      let t = match argument with
        | None -> void_ loc
        | Some expr -> expression cx expr
      in
      Flow_js.unit_flow cx (t, ret);
      Env_js.clear_env reason;
      Abnormal.set Abnormal.Return

  | (loc, Throw { Throw.argument }) ->
      ignore (expression cx argument)

  | (loc, Try { Try.block = (_, b); handler; guardedHandlers; finalizer }) ->
      let oldset = Env_js.swap_changeset (fun _ -> SSet.empty) in
      let exception_try, exception_finally = ref None, ref None in

      mark_exception_handler
        (fun () -> toplevels cx b.Block.body)
        exception_try;
      (* havoc environment, since the try block may exit anywhere *)
      Env_js.havoc_env2 !Env_js.changeset;

      (match handler with
        | None -> ()
        | Some (loc, h) ->
            ignore_exception_handler (fun () -> catch_clause cx h);
            (* havoc environment, since the catch block may exit anywhere *)
            Env_js.havoc_env2 !Env_js.changeset
      );

      assert (guardedHandlers = []); (* remove from AST *)

      (match finalizer with
        | None -> ()
        | Some (_, { Block.body }) ->
            mark_exception_handler
              (fun () -> toplevels cx body)
              exception_finally;
      );

      let newset = Env_js.swap_changeset (SSet.union oldset) in
      ignore newset;

      raise_exception !exception_finally

  (***************************************************************************)
  (* Refinements for `while` are derived by the following Hoare logic rule:

     [Pre' & c] S [Post']
     Pre' = Pre | Post'
     Post = Pre' & ~c
     ----------------------
     [Pre] while c S [Post]
  *)
  (***************************************************************************)
  | (loc, While { While.test; body }) ->
      let reason = mk_reason "while" loc in
      let save_break_exn = Abnormal.swap (Abnormal.Break None) false in
      let save_continue_exn = Abnormal.swap (Abnormal.Continue None) false in
      let ctx = !Env_js.env in
      let oldset = Env_js.swap_changeset (fun _ -> SSet.empty) in
      (* ctx = Pre *)
      (* ENV = [ctx] *)

      Env_js.widen_env cx reason;
      (* ctx = Pre', Pre' > Pre *)

      let do_ctx = Env_js.clone_env ctx in
      Env_js.update_frame cx do_ctx;
      (* do_ctx = Pre' *)
      (* ENV = [do_ctx] *)
      let _, pred, not_pred, xtypes =
        predicate_of_condition cx test in

      let body_ctx = Env_js.clone_env do_ctx in
      Env_js.update_frame cx body_ctx;
      (* body_ctx = Pre' *)
      (* ENV = [body_ctx] *)

      Env_js.refine_with_pred cx reason pred xtypes;
      (* body_ctx = Pre' & c *)

      let exception_ = ref None in
      ignore_break_continue_exception_handler
        (fun () -> statement cx body)
        None
        (save_handler exception_);
      (* body_ctx = Post' *)

      let newset = Env_js.swap_changeset (SSet.union oldset) in

      if Abnormal.swap (Abnormal.Continue None) save_continue_exn
      then Env_js.havoc_env2 newset;
      Env_js.copy_env cx reason (ctx,body_ctx) newset;
      (* Pre' > Post' *)

      Env_js.update_frame cx do_ctx;
      Env_js.refine_with_pred cx reason not_pred xtypes;
      if Abnormal.swap (Abnormal.Break None) save_break_exn
      then Env_js.havoc_env2 newset;
      (* ENV = [ctx] *)
      (* ctx = Pre' * ~c *)

      raise_exception !exception_

  (***************************************************************************)
  (* Refinements for `do-while` are derived by the following Hoare logic rule:

     [Pre'] S [Post']
     Pre' = Pre | (Post' & c)
     Post = Post' & ~c
     -------------------------
     [Pre] do S while c [Post]
  *)
  (***************************************************************************)
  | (loc, DoWhile { DoWhile.body; test }) ->
      let reason = mk_reason "do-while" loc in
      let save_break_exn = Abnormal.swap (Abnormal.Break None) false in
      let save_continue_exn = Abnormal.swap (Abnormal.Continue None) false in
      let ctx = !Env_js.env in
      let oldset = Env_js.swap_changeset (fun _ -> SSet.empty) in
      (* ctx = Pre *)
      (* ENV = [ctx] *)

      Env_js.widen_env cx reason;
      (* ctx = Pre', Pre' > Pre *)

      let body_ctx = Env_js.clone_env ctx in
      Env_js.update_frame cx body_ctx;
      (* body_ctx = Pre' *)
      (* ENV = [body_ctx] *)

      let exception_ = ref None in
      ignore_break_continue_exception_handler
        (fun () -> statement cx body)
        None
        (save_handler exception_);

      if Abnormal.swap (Abnormal.Continue None) save_continue_exn
      then Env_js.havoc_env2 !Env_js.changeset;

      let _, pred, not_pred, xtypes =
        predicate_of_condition cx test in
      (* body_ctx = Post' *)

      let done_ctx = Env_js.clone_env body_ctx in
      (* done_ctx = Post' *)

      Env_js.refine_with_pred cx reason pred xtypes;
      (* body_ctx = Post' & c *)

      let newset = Env_js.swap_changeset (SSet.union oldset) in
      Env_js.copy_env cx reason (ctx,body_ctx) newset;
      (* Pre' > Post' & c *)

      Env_js.update_frame cx done_ctx;
      Env_js.refine_with_pred cx reason not_pred xtypes;
      if Abnormal.swap (Abnormal.Break None) save_break_exn
      then Env_js.havoc_env2 newset;
      (* ENV = [done_ctx] *)
      (* done_ctx = Post' & ~c *)

      raise_exception !exception_

  (***************************************************************************)
  (* Refinements for `for` are derived by the following Hoare logic rule:

     [Pre] i [Init]
     [Pre' & c] S;u [Post']
     Pre' = Init | Post'
     Post = Pre' & ~c
     --------------------------
     [Pre] for (i;c;u) S [Post]

     NOTE: This rule is similar to that for `while`.
  *)
  (***************************************************************************)
  | (loc, For { For.init; test; update; body }) ->
      let reason = mk_reason "for" loc in
      let save_break_exn = Abnormal.swap (Abnormal.Break None) false in
      let save_continue_exn = Abnormal.swap (Abnormal.Continue None) false in
      (match init with
        | None -> ()
        | Some (For.InitDeclaration (loc, decl)) ->
            variables cx loc decl
        | Some (For.InitExpression expr) ->
            ignore (expression cx expr)
      );

      let ctx = !Env_js.env in
      let oldset = Env_js.swap_changeset (fun _ -> SSet.empty) in
      Env_js.widen_env cx reason;

      let do_ctx = Env_js.clone_env ctx in
      Env_js.update_frame cx do_ctx;

      let _, pred, not_pred, xtypes = match test with
        | None ->
            UndefT.t, SMap.empty, SMap.empty, SMap.empty (* TODO: prune the "not" case *)
        | Some expr ->
            predicate_of_condition cx expr
      in

      let body_ctx = Env_js.clone_env do_ctx in
      Env_js.update_frame cx body_ctx;
      Env_js.refine_with_pred cx reason pred xtypes;

      let exception_ = ref None in
      ignore_break_continue_exception_handler
        (fun () -> statement cx body)
        None
        (save_handler exception_);

      if Abnormal.swap (Abnormal.Continue None) save_continue_exn
      then Env_js.havoc_env2 !Env_js.changeset;

      (match update with
        | None -> ()
        | Some expr ->
            ignore (expression cx expr)
      );

      let newset = Env_js.swap_changeset (SSet.union oldset) in
      Env_js.copy_env cx reason (ctx,body_ctx) newset;

      Env_js.update_frame cx do_ctx;
      Env_js.refine_with_pred cx reason not_pred xtypes;
      if Abnormal.swap (Abnormal.Break None) save_break_exn
      then Env_js.havoc_env2 newset;

      raise_exception !exception_

  (***************************************************************************)
  (* Refinements for `for-in` are derived by the following Hoare logic rule:

     [Pre] o [Init]
     [Pre'] S [Post']
     Pre' = Init | Post'
     Post = Pre'
     --------------------------
     [Pre] for (i in o) S [Post]
  *)
  (***************************************************************************)
  | (loc, ForIn { ForIn.left; right; body; each }) ->
      let reason = mk_reason "for-in" loc in
      let save_break_exn = Abnormal.swap (Abnormal.Break None) false in
      let save_continue_exn = Abnormal.swap (Abnormal.Continue None) false in
      let t = expression cx right in
      let o = mk_object cx (mk_reason "iteration expected on object" loc) in
      Flow_js.unit_flow cx (t,o);

      let ctx = !Env_js.env in
      let oldset = Env_js.swap_changeset (fun _ -> SSet.empty) in
      Env_js.widen_env cx reason;

      let body_ctx = Env_js.clone_env ctx in
      Env_js.update_frame cx body_ctx;

      (match left with
        | ForIn.LeftDeclaration (_, {
            VariableDeclaration.declarations = [
              (loc, {
                VariableDeclaration.Declarator.id =
                  (_, Ast.Pattern.Identifier (_, id));
                _;
              })
            ];
            _;
          })
        | ForIn.LeftExpression (loc, Ast.Expression.Identifier (_, id)) ->
            let name = id.Ast.Identifier.name in
            let reason = mk_reason (spf "for..in %s" name) loc in
            Env_js.set_var cx name (StrT.at loc) reason

        | _ ->
            let msg = "unexpected LHS in for...in" in
            Flow_js.add_error cx [mk_reason "" loc, msg]
      );

      let exception_ = ref None in
      ignore_break_continue_exception_handler
        (fun () -> statement cx body)
        None
        (save_handler exception_);

      let newset = Env_js.swap_changeset (SSet.union oldset) in

      if Abnormal.swap (Abnormal.Continue None) save_continue_exn
      then Env_js.havoc_env2 newset;
      Env_js.copy_env cx reason (ctx,body_ctx) newset;

      Env_js.update_frame cx ctx;
      if Abnormal.swap (Abnormal.Break None) save_break_exn
      then Env_js.havoc_env2 newset;

      raise_exception !exception_

  | (loc, ForOf _) ->
      (* TODO? *)
      ()

  | (loc, Let _) ->
      (* TODO *)
      ()

  | (loc, Debugger) ->
      ()

  (* TODO: unsupported generators *)
  | (loc, FunctionDeclaration {
      FunctionDeclaration.id = _, { Ast.Identifier.name; typeAnnotation; _ };
      params; defaults; rest;
      body;
      returnType;
      typeParameters;
      _
    }) ->
      let reason = mk_reason "function" loc in
      let this = Flow_js.mk_tvar cx (replace_reason "this" reason) in
      let fn_type = mk_function None cx reason typeParameters
        (params, defaults, rest) returnType body this
      in
      Hashtbl.replace cx.type_table loc fn_type;
      Env_js.set_var cx name fn_type reason

  | (loc, DeclareVariable { DeclareVariable.id; })
  | (loc, DeclareFunction { DeclareFunction.id; }) -> ()

  | (loc, VariableDeclaration decl) ->
      variables cx loc decl

  | (loc, ClassDeclaration { Class.id; body; superClass;
      typeParameters; superTypeParameters; implements }) ->
      if implements <> [] then
        let msg = "implements not supported" in
        Flow_js.add_error cx [mk_reason "" loc, msg]
      else ();
      let nloc, { Ast.Identifier.name; _ } = id in
      let reason = mk_reason name nloc in
      let extends = superClass, superTypeParameters in
      let cls_type = mk_class cx reason typeParameters extends body in
      Hashtbl.replace cx.type_table loc cls_type;
      Env_js.set_var cx name cls_type reason

  | (loc, DeclareClass {
      Interface.id;
      typeParameters;
      body = (_, { Ast.Type.Object.properties; indexers; callProperties });
      extends;
    })
  | (loc, InterfaceDeclaration {
      Interface.id;
      typeParameters;
      body = (_, { Ast.Type.Object.properties; indexers; callProperties });
      extends;
    }) ->
    let _, { Ast.Identifier.name = iname; _ } = id in
    let reason = mk_reason iname loc in
    let typeparams, imap, map = mk_type_param_declarations cx typeParameters in
    let sfmap, smmap, fmap, mmap = List.fold_left Ast.Type.Object.Property.(
      fun (sfmap_, smmap_, fmap_, mmap_) (loc, { key; value; static; _ }) ->
        Ast.Expression.Object.Property.(match key with
        | Literal (loc, _)
        | Computed (loc, _) ->
            let msg = "illegal name" in
            Flow_js.add_error cx [Reason_js.mk_reason "" loc, msg];
            (sfmap_, smmap_, fmap_, mmap_)

        | Identifier (loc, { Ast.Identifier.name; _ }) ->
          if static
          then
            let t = convert cx map value in
            let t = match SMap.get name mmap_ with
              | None -> t
              | Some (IntersectionT (reason, ts)) ->
                  IntersectionT (reason, ts @ [t])
              | Some t0 ->
                  IntersectionT (Reason_js.mk_reason iname loc, [t; t0])
            in
            match value with
              | _, Ast.Type.Function _ -> (sfmap_, SMap.add name t smmap_,
          fmap_, mmap_)
              | _ -> (SMap.add name t sfmap_, smmap_, fmap_, mmap_)
          else
            let t = convert cx map value in
            let t = match SMap.get name mmap_ with
              | None -> t
              | Some (IntersectionT (reason, ts)) ->
                  IntersectionT (reason, ts @ [t])
              | Some t0 ->
                  IntersectionT (Reason_js.mk_reason iname loc, [t; t0])
            in
            match value with
              | _, Ast.Type.Function _ -> (sfmap_, smmap_, fmap_, SMap.add name t mmap_)
              | _ -> (sfmap_, smmap_, SMap.add name t fmap_, mmap_)
        )
    ) (SMap.empty, SMap.empty, SMap.empty, SMap.empty) properties
    in
    let fmap = Ast.Type.Object.Indexer.(match indexers with
    | [] -> fmap
    | [(_, { key; value; _; })] ->
        let keyt = convert cx map key in
        let valuet = convert cx map value in
        fmap |> SMap.add "$key" keyt |> SMap.add "$value" valuet
    (* TODO *)
    | _ -> failwith "Unimplemented: multiple indexers")
    in
    let calls = callProperties |> List.map (function
      | loc, { Ast.Type.Object.CallProperty.value = (_, ft); static; } ->
        (static, convert cx map (loc, Ast.Type.Function ft))
    ) in
    let scalls, calls = List.partition fst calls in
    let smmap = match scalls with
      | [] -> smmap
      | [_,t] -> SMap.add "$call" t smmap
      | _ -> let scalls = List.map snd scalls in
             SMap.add "$call" (IntersectionT (mk_reason iname loc,
                                              scalls)) smmap
    in
    let mmap = match calls with
      | [] -> mmap
      | [_,t] -> SMap.add "$call" t mmap
      | _ -> let calls = List.map snd calls in
             SMap.add "$call" (IntersectionT (mk_reason iname loc,
                                              calls)) mmap
    in
    let mmap = match SMap.get "constructor" mmap with
      | None ->
        let constructor_funtype =
          Flow_js.mk_functiontype2 [] (Some []) VoidT.t 0 in
        let funt = (FunT (Reason_js.mk_reason "constructor" loc,
          Flow_js.dummy_static, Flow_js.dummy_prototype, constructor_funtype))
        in
        SMap.add "constructor" funt mmap
      | Some _ ->
        mmap
    in
    let i = mk_interface cx reason typeparams imap map (sfmap, smmap, fmap, mmap) extends in
    Hashtbl.replace cx.type_table loc i;
    Env_js.set_var cx iname i reason
  | (loc, DeclareModule { DeclareModule.id; body; }) ->
    let name = match id with
    | DeclareModule.Identifier (_, id) -> id.Ast.Identifier.name
    | DeclareModule.Literal (_, { Ast.Literal.value = Ast.Literal.String str; _; }) ->
        str
    | _ ->
        (* The only literals that we should see as module names are strings *)
        assert false in
    let _, { Ast.Statement.Block.body = elements } = body in

    let reason = mk_reason (spf "module %s" name) loc in
    let o = Env_js.get_var_in_scope cx (spf "$module__%s" name) reason in
    let block = ref SMap.empty in
    Env_js.push_env block;

    List.iter (statement_decl cx) elements;
    toplevels cx elements;

    Env_js.pop_env();

    !block |> SMap.iter (fun x {specific=t;_} ->
      Flow_js.unit_flow cx (o, SetT(replace_reason (spf "%s.%s" name x) reason, x, t)
    ));
  | (_, ExportDeclaration _) ->
      (* TODO *)
      failwith "Unimplemented: ExportDeclaration"
  | (_, ImportDeclaration _) ->
      (* TODO *)
      failwith "Unimplemented: ImportDeclaration"
)

and save_handler mark exn = mark := Some exn

and mark_exception_handler main exception_ =
  Abnormal.exception_handler main
    (save_handler exception_)

and ignore_exception_handler main =
  Abnormal.exception_handler main (fun exn -> ())

and ignore_return_exception_handler main =
  Abnormal.exception_handler main (function
    | Abnormal.Return -> ()
    | exn -> Abnormal.raise_exn exn
  )

and ignore_break_exception_handler main l1 =
  Abnormal.exception_handler main (function
    | Abnormal.Break l2 when l1 = l2 -> ()
    | exn -> Abnormal.raise_exn exn
  )

and ignore_break_continue_exception_handler main l1 handler =
  Abnormal.exception_handler main (function
    | (Abnormal.Break l2 | Abnormal.Continue l2) when l1 = l2 -> ()
    | exn -> handler exn
  )

and raise_and_exception exn1 exn2 = match (exn1,exn2) with
  | (Some exn, Some _) when exn1 = exn2 -> Abnormal.raise_exn exn
  | _ -> ()

and raise_exception exn_ = match exn_ with
  | Some exn -> Abnormal.raise_exn exn
  | _ -> ()

and object_ cx props = Ast.Expression.Object.(
  let spread = ref None in
  List.fold_left (fun map -> function

    (* name = function expr *)
    | Property (loc, { Property.kind = Property.Init;
                       key = Property.Identifier (_, {
                         Ast.Identifier.name; typeAnnotation; _ });
                       value = (vloc, Ast.Expression.Function func);
                       _ }) ->
        Ast.Expression.Function.(
          let { params; defaults; rest; body;
                returnType; typeParameters; id; _ } = func
          in
          let reason = mk_reason "function" vloc in
          let this = Flow_js.mk_tvar cx (replace_reason "this" reason) in
          let ft = mk_function id cx reason typeParameters
            (params, defaults, rest) returnType body this
          in
          Hashtbl.replace cx.type_table vloc ft;
          SMap.add name ft map
        )

    (* name = non-function expr *)
    | Property (loc, { Property.kind = Property.Init;
        key =
          Property.Identifier (_, { Ast.Identifier.name; _ }) |
          Property.Literal (_, {
            Ast.Literal.value = Ast.Literal.String name;
            _;
          });
                     value = v;
                     _ }) ->
      let t = expression cx v in
      SMap.add name t map

    (* literal LHS *)
    | Property (loc, { Property.key = Property.Literal _; _ }) ->
      let msg = "non-string literal property keys not supported" in
      Flow_js.add_error cx [mk_reason "" loc, msg];
      map

    (* get/set kind *)
    | Property (loc, { Property.kind = Property.Get | Property.Set; _ }) ->
      let msg = "get/set properties not yet supported" in
      Flow_js.add_error cx [mk_reason "" loc, msg];
      map

    (* computed LHS *)
    | Property (loc, { Property.key = Property.Computed _; _ }) ->
      let msg = "computed property keys not supported" in
      Flow_js.add_error cx [mk_reason "" loc, msg];
      map

    (* spread prop *)
    | SpreadProperty (loc, { SpreadProperty.argument }) ->
      spread := Some (expression cx argument);
      map

  ) SMap.empty props,
  !spread
)

and variable cx (loc, vdecl) = Ast.(
  let { Statement.VariableDeclaration.Declarator.id; init } = vdecl in
  match id with
    | (loc, Pattern.Identifier (_, { Identifier.
          name; typeAnnotation; optional
        })) ->
        let reason = mk_reason (spf "var %s" name) loc in
        (match init with
        | Some expr ->
            Env_js.set_var cx name (expression cx expr) reason
        | None ->
            if (not optional)
            then Env_js.set_var cx name (void_ loc) reason
        );
        (* Adjust state based on whether there is an annotation.

           This reveals an interesting tension between annotations and
           flow-sensitivity. Consider:

           var x:A = new B(); // B is a subclass of A
           // ^ should we have x:A or x:B after this initialization?
           x = new B();
           // ^ how about after this assignment?
           if (x instanceof B) {
           // ^ and how about after this dynamic check?
           }

           In general, it seems desirable to narrow down the type
           of a variable when possible (indeed, that's the whole point
           of flow-sensitivity) but it is unclear what to do when there
           are annotations. It might be argued that annotations override
           flow-sensitivity, but that is not very nice, because it would
           force patterns such as:

           var y = x;
           if (y instanceof B) {
           // ^ we have y:B after this dynamic check, yet x:A
           }

           Below, we do a compromise; we remain flow-sensitive in general
           but treat an annotation as if it were part of initialization.
           This seems to be a nice solution, because (e.g.) it would ban
           code such as:

           var y:string; // initialize with a string!

           as well as:

           function foo(x:B) { }
           var x:A = new B();
           foo(x);
        *)
        (match typeAnnotation with
          | None -> ()
          | Some _ ->
              let t = Env_js.get_var_in_scope cx name reason in
              Env_js.set_var cx name t reason
        )

    | loc, _ ->
        (match init with
          | Some expr ->
              let reason = mk_reason "var _" loc in
              let t_ = type_of_pattern id |> mk_type_annotation cx reason in
              let t = expression cx expr in
              Flow_js.unit_flow cx (t, t_);
              destructuring_assignment cx t_ id
          | None ->
              ()
        )
)

and array_element cx undef_loc el = Ast.Expression.(
  match el with
  | Some (Expression e) -> expression cx e
  | Some (Spread (_, { SpreadElement.argument })) -> spread cx argument
  | None -> UndefT.at undef_loc
)

and expression_or_spread cx = Ast.Expression.(function
  | Expression e -> expression cx e
  | Spread (_, { SpreadElement.argument }) -> spread cx argument
)

and spread cx (loc, e) =
  let arr = expression cx (loc, e) in
  let reason = mk_reason "spread operand" loc in
  let tvar = Flow_js.mk_tvar cx reason in
  Flow_js.unit_flow cx (arr, ArrT (reason, tvar, []));
  RestT tvar

and expression cx (loc, e) =
  let t = expression_ cx loc e in
  Hashtbl.replace cx.type_table loc t;
  t

and this_ cx reason =
  Env_js.get_var cx (internal_name "this") reason

and super_ cx reason =
  Env_js.get_var cx (internal_name "super") reason

(* Module exports are treated differently than `exports`. The latter is a
   variable that is implicitly set to the empty object at the top of a
   module. As such, properties can be added to it throughout the module,
   corresponding to the pattern exports.foo = ... for exporting foo. As it turns
   out, module.exports is the same object. A different pattern is to reset
   module.exports at the end of the module, like module.exports = { foo: ... }
   to export foo. This makes any properties added to the exports object
   redundant. Both these patterns are modeled by storing module.exports as an
   internal variable, initially set to the empty object, doing inference on a
   module, and then flowing module.exports to exports, so that whatever its
   final value is (initial object or otherwise) is checked against the type
   declared for exports or any other use of exports. *)
and get_module_exports cx reason =
  Env_js.get_var cx (internal_name "exports") reason

and set_module_exports cx reason t =
  Env_js.set_var cx (internal_name "exports") t reason

and void_ loc =
  VoidT.at loc

and null_ loc =
  NullT.at loc

and expression_ cx loc = Ast.Expression.(function

  | Literal lit ->
      literal cx loc lit

  | Identifier (_, { Ast.Identifier.name; _ }) ->
      if Type_inference_hooks_js.dispatch_id_hook cx name loc
      then AnyT.at loc
      else (
        if name = "undefined"
        then
          let void = void_ loc in
          let reason = reason_of_t void in
          UnionT (reason, [void; Flow_js.mk_tvar cx reason])
        else (
          let reason = mk_reason (spf "identifier %s" name) loc in
          let t = Env_js.var_ref cx name reason in
          t
        )
      )

  | This ->
      this_ cx (mk_reason "this" loc)

  | Unary u ->
      unary cx loc u

  | Update u ->
      update cx loc u

  | Binary b ->
      binary cx loc b

  | Logical l ->
      logical cx loc l

  | Member {
      Member._object;
      property = Member.PropertyExpression index;
      _
    } ->
      let reason = mk_reason "access of computed property/element" loc in
      let tobj = expression cx _object in
      let tind = expression cx index in
      Flow_js.mk_tvar_where cx reason (fun t ->
        Flow_js.unit_flow cx (tobj, GetElemT(reason, tind, t))
      )

  | Member {
      Member._object = _, Identifier (_,
        { Ast.Identifier.name = "module"; _ });
      property = Member.PropertyIdentifier (_,
        { Ast.Identifier.name = "exports"; _ });
      _
    } ->
      let reason = mk_reason "module.exports" loc in
      get_module_exports cx reason

  | Member {
      Member._object = _, Identifier (_,
        { Ast.Identifier.name = "ReactGraphQL"; _ });
      property = Member.PropertyIdentifier (_,
        { Ast.Identifier.name = "Mixin"; _ });
      _
    } ->
      let reason = mk_reason "ReactGraphQLMixin" loc in
      Flow_js.get_builtin cx "ReactGraphQLMixin" reason

  | Member {
      Member._object = _, Identifier (_,
        { Ast.Identifier.name = "super"; _ });
      property = Member.PropertyIdentifier (ploc,
        { Ast.Identifier.name; _ });
      _
    } ->
      let reason = mk_reason (spf "property %s" name) ploc in
      let super = super_ cx reason in
      if Type_inference_hooks_js.dispatch_member_hook cx name ploc super
      then AnyT.at ploc
      else (
        Flow_js.mk_tvar_where cx reason (fun tvar ->
          Flow_js.unit_flow cx (super, GetT(reason, name, tvar)))
      )

  | Member {
      Member._object;
      property = Member.PropertyIdentifier (ploc, { Ast.Identifier.name; _ });
      _
    } -> (
      let reason = mk_reason (spf "property %s" name) loc in
      let refined = match _object with
      | _, Identifier (_, { Ast.Identifier.name = oname; _ }) ->
          Env_js.get_lookup_refinement cx oname name reason
      | _ -> None
      in match refined with
      | Some t -> t
      | None ->
        let tobj = expression cx _object in
        if Type_inference_hooks_js.dispatch_member_hook cx name ploc tobj
        then AnyT.at ploc
        else (
          Flow_js.mk_tvar_where cx reason (fun t ->
            Flow_js.unit_flow cx (tobj, GetT (reason, name, t)))
        )
      )

  | Object { Object.properties } ->
    let reason = mk_reason "object literal" loc in
    let map, spread = object_ cx properties in
    extend_object
      cx reason
      (Flow_js.mk_object_with_map_proto cx reason map (MixedT reason))
      spread

  | Array { Array.elements } ->
    let reason = mk_reason "array literal" loc in
    if elements = [] then
      (* empty array, analogous to object with implicit properties *)
      let element_reason = mk_reason "array element" loc in
      let tx = Flow_js.mk_tvar cx element_reason in
      ArrT (reason, tx, [])

    else if List.length elements >= 8 then (
      (* big array literal: we assume / assert that it must be homogenous *)
        let t = summarize cx (array_element cx loc (List.hd elements)) in
        List.iter (fun e ->
          Flow_js.unify cx t (array_element cx loc e)
        ) (List.tl elements);
        ArrT (reason, t, [])
    )
    else (
      (* small array literal: if no spreads, we model it as a tuple with a
         backing array *)
        let tup, elts = List.fold_left (fun (tup, elts) elem ->
          match elem with
          | Some (Expression e) -> (tup, expression cx e :: elts)
          | Some (Spread (_, { SpreadElement.argument })) ->
              (false, spread cx argument :: elts)
          | None -> (tup, (UndefT.at loc) :: elts)
        ) (true, []) elements in
        let elts = List.rev elts in

        if tup then
          (* NOTE: We still pin down the element type rather than
             leave it open, just like we do for object literals with
             explicit properties.
             Indeed, tuples may still be subject to massive unification,
             e.g. whenthey are part of big array literals, which may
             blow up constraints by copying them over and over again *)
          let element_reason = mk_reason "array element" loc in
          let elts_ = List.map (summarize cx) elts in
          let tx = match elts_ with
            | [elt] -> elt
            | _ -> UnionT (element_reason, elts_)
          in
          ArrT (reason, tx, elts)

        else
          (* spreads wreck positions, treat as homogenous *)
          let t = summarize cx (List.hd elts) in
          List.iter (fun tx -> Flow_js.unit_flow cx (tx, t)) (List.tl elts);
          ArrT (reason, t, [])
    )

  | Call {
      Call.callee = _, Identifier (_, { Ast.Identifier.name = "require"; _ });
      arguments
    } -> (
      match arguments with
      | [ Expression (_, Literal {
          Ast.Literal.value = Ast.Literal.String m_name; _;
        }) ] ->
        let m = Module_js.imported_module cx.file m_name in
        require cx m m_name loc
      | _ ->
        (*
        let msg = "require(...) supported only on strings" in
        Flow_js.add_error cx [mk_reason "" loc, msg];
        *)
        AnyT.at loc
    )

  | New {
      New.callee = _, Identifier (_, { Ast.Identifier.name = "Function"; _ });
      arguments
    } -> (
      let argts = List.map (expression_or_spread cx) arguments in
      List.iter (fun t ->
        Flow_js.unit_flow cx (t, StrT.at loc)
      ) argts;
      let reason = mk_reason "new Function(..)" loc in
      FunT (reason, Flow_js.dummy_static, Flow_js.dummy_prototype,
        Flow_js.mk_functiontype [] (Some []) (MixedT reason))
    )

  | New {
      New.callee = _, Identifier (_, { Ast.Identifier.name = "Array"; _ });
      arguments
    } -> (
      let argts = List.map (expression_or_spread cx) arguments in
      (match argts with
      | [argt] ->
        let reason = mk_reason "new Array(..)" loc in
        Flow_js.unit_flow cx
          (argt, NumT (replace_reason "array length" reason, None));
        let element_reason = replace_reason "array element" reason in
        let t = Flow_js.mk_tvar cx element_reason in
        ArrT (reason, t, [])
      | _ ->
        let msg = "Use array literal instead of new Array(..)" in
        Flow_js.add_error cx [mk_reason "" loc, msg];
        UndefT.at loc
      )
    )

  | New { New.callee; arguments } ->
      let class_ = expression cx callee in
      let argts = List.map (expression_or_spread cx) arguments in
      new_call cx loc class_ argts

  | Call {
      Call.callee = _, Member {
        Member._object = _, Identifier (_,
          { Ast.Identifier.name = "Object"; _ });
        property = Member.PropertyIdentifier (_,
          { Ast.Identifier.name; _ });
        _
      };
      arguments
    } ->
      static_method_call_Object cx loc name arguments

  | Call {
      Call.callee = _, Member {
        Member._object = _, Identifier (_,
          { Ast.Identifier.name = "React"; _ });
        property = Member.PropertyIdentifier (_,
          { Ast.Identifier.name; _ });
        _
      };
      arguments
    } ->
      static_method_call_React cx loc name arguments

  | Call {
      Call.callee = _, Member {
        Member._object = _, Identifier (_,
          { Ast.Identifier.name = "super"; _ });
        property = Member.PropertyIdentifier (ploc,
          { Ast.Identifier.name; _ });
        _
      };
      arguments
    } ->
      let argts = List.map (expression_or_spread cx) arguments in
      let reason = mk_reason (spf "super.%s(...)" name) loc in
      let super = super_ cx reason in
      Type_inference_hooks_js.dispatch_call_hook cx name ploc super;
      Flow_js.mk_tvar_where cx reason (fun t ->
        Flow_js.unit_flow cx (super,
          MethodT (reason, name, super, argts, t, 0));
      )

  | Call {
      Call.callee = _, Member {
        Member._object;
        property = Member.PropertyIdentifier (ploc,
          { Ast.Identifier.name; _ });
        _
      };
      arguments
    } ->
      let ot = expression cx _object in
      let argts = List.map (expression_or_spread cx) arguments in
      let reason = mk_reason (spf "call of method %s" name) loc in
      Type_inference_hooks_js.dispatch_call_hook cx name ploc ot;
      Env_js.havoc_heap_refinements ();
      Flow_js.mk_tvar_where cx reason (fun t ->
        Flow_js.unit_flow cx
          (ot, MethodT(reason, name, ot, argts, t, List.hd !Flow_js.frames))
      )

  | Call {
      Call.callee = _, Identifier (_, { Ast.Identifier.name = "super"; _ });
      arguments
    } ->
      let argts = List.map (expression_or_spread cx) arguments in
      let reason = mk_reason "super(...)" loc in
      let super = super_ cx reason in
      let t = VoidT reason in
      Flow_js.unit_flow cx
        (super, MethodT(reason, "constructor", super, argts, t, 0));
      t

  (******************************************)
  (* See ~/www/static_upstream/core/ *)

  | Call {
      Call.callee = _, Identifier (_,
        { Ast.Identifier.name = "invariant"; _ });
      arguments
    } ->
      (* TODO: require *)
      let reason = mk_reason "invariant" loc in
      (match arguments with
      | [ Expression cond ] ->
        let _, pred, not_pred, xtypes = predicate_of_condition cx cond in
        Env_js.refine_with_pred cx reason pred xtypes
      | _ ->
        let msg = "unsupported arguments in call to invariant()" in
        Flow_js.add_error cx [mk_reason "" loc, msg]
      );
      void_ loc

  | Call {
      Call.callee = _, Identifier (_,
        { Ast.Identifier.name = "copyProperties"; _ });
      arguments
    } ->
      (* TODO: require *)
      let argts = List.map (expression_or_spread cx) arguments in
      let reason = mk_reason "object extension" loc in
      chain_objects cx reason (List.hd argts) (List.tl argts)

  | Call {
      Call.callee = _, Identifier (_,
        { Ast.Identifier.name = "mergeInto"; _ });
      arguments
    } ->
      (* TODO: require *)
      let argts = List.map (expression_or_spread cx) arguments in
      let reason = mk_reason "object extension" loc in
      ignore (chain_objects cx reason (List.hd argts) (List.tl argts));
      void_ loc

  | Call {
      Call.callee = _, Identifier (_,
        { Ast.Identifier.name = "mergeDeepInto"; _ });
      arguments
    } ->
      (* TODO: require *)
      let argts = List.map (expression_or_spread cx) arguments in
      ignore argts; (* TODO *)
      void_ loc

  | Call {
      Call.callee = _, Identifier (_, { Ast.Identifier.name = "merge"; _ });
      arguments
    }
      (* TODO: require *)
  | Call {
      Call.callee = _, Identifier (_,
        { Ast.Identifier.name = "mergeObjects"; _ });
      arguments
    } ->
      (* TODO: require *)
      let argts = List.map (expression_or_spread cx) arguments in
      let reason = mk_reason "object" loc in
      spread_objects cx reason argts

  | Call {
      Call.callee = _, Identifier (_, { Ast.Identifier.name = "mixin"; _ });
      arguments
    } ->
      (* TODO: require *)
      let argts = List.map (expression_or_spread cx) arguments in
      let reason = mk_reason "object" loc in
      ClassT (spread_objects cx reason argts)

  | Call {
      Call.callee = _, Identifier (_,
        { Ast.Identifier.name = "classWithMixins"; _ });
      arguments
    } ->
      (* TODO: require *)
      let argts = List.map (expression_or_spread cx) arguments in
      Flow_js.mk_tvar_where cx (mk_reason "class" loc) (fun t ->
        List.iter (fun argt -> Flow_js.unit_flow cx (argt, t)) argts
      )

  | Call { Call.callee; arguments } ->
      let f = expression cx callee in
      let reason = mk_reason "function call" loc in
      let argts = List.map (expression_or_spread cx) arguments in
      Env_js.havoc_heap_refinements ();
      Flow_js.mk_tvar_where cx reason (fun t ->
        let app =
          Flow_js.mk_functiontype2 argts None t (List.hd !Flow_js.frames) in
        Flow_js.unit_flow cx (f, CallT (reason, app));
      )

  | Conditional { Conditional.test; consequent; alternate } ->
      let reason = mk_reason "conditional" loc in
      let _, pred, not_pred, xtypes = predicate_of_condition cx test in
      let ctx = !Env_js.env in
      let oldset = Env_js.swap_changeset (fun _ -> SSet.empty) in

      let then_ctx = Env_js.clone_env ctx in
      Env_js.update_frame cx then_ctx;
      Env_js.refine_with_pred cx reason pred xtypes;
      let t1 = expression cx consequent in

      let else_ctx = Env_js.clone_env ctx in
      Env_js.update_frame cx else_ctx;
      Env_js.refine_with_pred cx reason not_pred xtypes;
      let t2 = expression cx alternate in

      let newset = Env_js.swap_changeset (SSet.union oldset) in
      Env_js.merge_env cx reason (ctx, then_ctx, else_ctx) newset;
      Env_js.update_frame cx ctx;
      (* TODO call pos_of_predicate on some pred? t1 is wrong but hopefully close *)
      Flow_js.mk_tvar_where cx reason (fun t ->
        Flow_js.unit_flow cx (t1, t);
        Flow_js.unit_flow cx (t2, t);
      )

  | Assignment { Assignment.operator; left; right } ->
      assignment cx loc (left, operator, right)

  | Sequence { Sequence.expressions } ->
      List.fold_left (fun t e -> expression cx e) (void_ loc) expressions

  | Function {
      Function.id;
      params; defaults; rest;
      body;
      returnType;
      typeParameters;
      _
    } ->
      let reason = mk_reason "function" loc in
      let this = Flow_js.mk_tvar cx (replace_reason "this" reason) in
      mk_function id cx reason
        typeParameters (params, defaults, rest) returnType body this

  (* TODO: unsupported generators *)
  | ArrowFunction {
      ArrowFunction.id;
      params; defaults; rest;
      body;
      _
    } ->
      let reason = mk_reason "arrow" loc in
      let this = this_ cx reason in
      let super = super_ cx reason in
      mk_method cx reason (params, defaults, rest) None body this super

  | TaggedTemplate {
      TaggedTemplate.tag = _, Identifier (_,
        { Ast.Identifier.name = "query"; _ });
      quasi = _, { TemplateLiteral.quasis; expressions }
    } ->
    List.iter (fun e -> ignore (expression cx e)) expressions;
    (*parse_graphql cx encaps;*)
    void_ loc

  | TaggedTemplate {
      TaggedTemplate.tag;
      quasi = _, { TemplateLiteral.quasis; expressions }
    } ->
      List.iter (fun e -> ignore (expression cx e)) expressions;
      let t = expression cx tag in
      let reason = mk_reason "encaps tag" loc in
      let reason_array = replace_reason "array" reason in
      let ft = Flow_js.mk_functiontype
        [ ArrT (reason_array, StrT.why reason, []);
          RestT (AnyT.why reason) ]
        None (* TODO: ? *)
        (StrT.why reason)
      in
      Flow_js.unit_flow cx (t, CallT (reason, ft));
      StrT.at loc

  | TemplateLiteral {
      TemplateLiteral.quasis;
      expressions
    } ->
      List.iter (fun e -> ignore (expression cx e)) expressions;
      StrT.at loc

  | XJSElement e ->
      jsx cx e

  (* TODO *)
  | Yield _
  | Comprehension _
  | Generator _
  | Let _
  | Class _ ->
    Flow_js.add_error cx [mk_reason "" loc, "not (sup)ported"];
    UndefT.at loc
)

(* We assume that constructor functions return void
   and constructions return objects.
   TODO: This assumption does not always hold.
   If construction functions return non-void values (e.g., functions),
   then those values are returned by constructions.
*)
and new_call cx tok class_ argts =
  let reason = mk_reason "constructor call" tok in
  Flow_js.mk_tvar_where cx reason (fun t ->
    Flow_js.unit_flow cx (class_, ConstructorT (reason, argts, t));
  )

and reflective s =
  (* could be stricter *)
  List.mem s [
    "propertyIsEnumerable";
    "length";
    "isPrototypeOf";
    "hasOwnProperty";
    "constructor";
    "valueOf";
    "toString";
    "toLocaleString";
  ]

and non_identifier s =
  (* should be stricter *)
  Str.string_match (Str.regexp ".* ") s 0

and literal cx loc lit = Ast.Literal.(match lit.value with
  | String s ->
    let lit =
      if reflective s || non_identifier s
      then None
      else Some s
    in
      StrT (mk_reason "string" loc, lit)

  | Boolean b ->
      BoolT.at loc

  | Null ->
      let null = null_ loc in
      let reason = reason_of_t null in
      UnionT (reason, [null; Flow_js.mk_tvar cx reason])

  | Number f ->
      NumT (mk_reason "number" loc, Some lit.raw)

  | RegExp rx ->
      Flow_js.get_builtin_type cx (mk_reason "regexp" loc) "RegExp"
)

and unary cx loc = Ast.Expression.Unary.(function
  | { operator = Not; argument; _ } ->
      let t = BoolT.at loc in
      Flow_js.unit_flow cx (expression cx argument, t);
      t

  | { operator = Plus; argument; _ } ->
      ignore (expression cx argument);
      AnyT.at loc

  | { operator = Minus; argument; _ }
  | { operator = BitNot; argument; _ } ->
      let t = NumT.at loc in
      Flow_js.unit_flow cx (expression cx argument, t);
      t

  | { operator = Typeof; argument; _ } ->
      ignore (expression cx argument);
      StrT.at loc

  | { operator = Void; argument; _ } ->
      ignore (expression cx argument);
      void_ loc

  | { operator = Delete; argument; _ } ->
      ignore (expression cx argument);
      BoolT.at loc
)

and update cx loc = Ast.Expression.Update.(function
  | { operator; argument; _ } ->
      let t = NumT.at loc in
      Flow_js.unit_flow cx (expression cx argument, t);
      t
)

(* TODO verify that these cases are properly covered
   by the new parser:

  | ((U_new,tok),e) ->
      let class_ = expression cx e in
      new_call cx tok class_ []

  | ((U_spread,tok),e) ->
      (* TODO: tuples *)
      let arr = expression cx e in
      let reason_array_arg = mk_reason ("array argument of operator") tok in
      let tvar = Flow_js.mk_tvar cx reason_array_arg in
      Flow_js.unit_flow cx (arr, ArrT (reason_array_arg,tvar,[]));
      RestT(tvar)
*)


and binary cx loc = Ast.Expression.Binary.(function
  | { operator = Equal; left; right }
  | { operator = NotEqual; left; right } ->
      let reason = mk_reason "non-strict equality comparison" loc in
      let t1 = expression cx left in
      let t2 = expression cx right in
      Flow_js.unit_flow cx (t1, EqT (reason,t2));
      BoolT.at loc

  | { operator = StrictEqual; left; right }
  | { operator = StrictNotEqual; left; right }
  | { operator = In; left; right }
  | { operator = Instanceof; left; right } ->
      ignore (expression cx left);
      ignore (expression cx right);
      BoolT.at loc

  | { operator = LessThan; left; right }
  | { operator = LessThanEqual; left; right }
  | { operator = GreaterThan; left; right }
  | { operator = GreaterThanEqual; left; right } ->
      let reason = mk_reason "relational comparison" loc in
      let t1 = expression cx left in
      let t2 = expression cx right in
      Flow_js.unit_flow cx (t1, ComparatorT (reason,t2));
      BoolT.at loc

  | { operator = LShift; left; right }
  | { operator = RShift; left; right }
  | { operator = RShift3; left; right }
  | { operator = Minus; left; right }
  | { operator = Mult; left; right }
  | { operator = Div; left; right }
  | { operator = Mod; left; right }
  | { operator = BitOr; left; right }
  | { operator = Xor; left; right }
  | { operator = BitAnd; left; right } ->
      let t = NumT.at loc in
      Flow_js.unit_flow cx (expression cx left, t);
      Flow_js.unit_flow cx (expression cx right, t);
      t

  | { operator = Plus; left; right } ->
      let reason = mk_reason "add" loc in
      let t1 = expression cx left in
      let t2 = expression cx right in
      Flow_js.mk_tvar_where cx reason (fun t ->
        Flow_js.unit_flow cx (t1, AdderT (reason, t2, t));
      )
)

and refine_type cx reason t p =
  Flow_js.mk_tvar_where cx reason (fun tvar ->
    Flow_js.unit_flow cx (t, mk_predicate (p, tvar))
  )

and logical cx loc = Ast.Expression.Logical.(function
  | { operator = Or; left; right } ->
      let t1, _, not_map, xtypes = predicate_of_condition cx left in
      let reason = mk_reason "||" loc in
      let t = refine_type cx reason (expression cx left) ExistsP in
      let t2 = Env_js.refine_env cx reason not_map xtypes
        (fun () -> expression cx right)
      in
      Flow_js.unit_flow cx (t2, t);
      t

  | { operator = And; left; right } ->
      let t1, map, _, xtypes = predicate_of_condition cx left in
      let reason = mk_reason "&&" loc in
      let t = refine_type cx reason (expression cx left) (NotP ExistsP) in
      let t2 = Env_js.refine_env cx reason map xtypes
        (fun () -> expression cx right)
      in
      Flow_js.unit_flow cx (t2, t);
      t
)

and assignment_lhs cx = Ast.Pattern.(function
  | loc, Object _
  | loc, Array _ ->
      error_destructuring cx loc;
      AnyT.at loc

  | loc, Identifier i ->
      expression cx (loc, Ast.Expression.Identifier i)

  | loc, Expression e ->
      expression cx e
)

and assignment cx loc = Ast.Expression.(function

  | (r, Assignment.Assign, e) ->
      let t = expression cx e in
      (match r with
        | _, Ast.Pattern.Expression (_, Member {
            Member._object = _, Ast.Expression.Identifier (_,
              { Ast.Identifier.name = "module"; _ });
            property = Member.PropertyIdentifier (_,
              { Ast.Identifier.name = "exports"; _ });
            _
          }) ->
            let reason = mk_reason "assignment of module.exports" loc in
            set_module_exports cx reason t

        | _, Ast.Pattern.Expression (_, Member {
            Member._object = _, Identifier (_,
              { Ast.Identifier.name = "super"; _ });
            property = Member.PropertyIdentifier (_,
              { Ast.Identifier.name; _ });
            _
          }) ->
            let reason = mk_reason
              (spf "assignment of property %s" name) loc in
            let super = super_ cx reason in
            Flow_js.unit_flow cx (super, SetT(reason, name, t))

        | _, Ast.Pattern.Expression (_, Member {
            Member._object;
            property = Member.PropertyIdentifier (ploc,
              { Ast.Identifier.name; _ });
            _
          }) ->
            let reason = mk_reason
              (spf "assignment of property %s" name) loc in
            let o = expression cx _object in
            if Type_inference_hooks_js.dispatch_member_hook cx name ploc o
            then ()
            else Flow_js.unit_flow cx (o, SetT (reason, name, t))

        | _, Ast.Pattern.Expression (_, Member {
            Member._object;
            property = Member.PropertyExpression index;
            _
          }) ->
            let reason = mk_reason "assignment of computed property/element" loc in
            let a = expression cx _object in
            let i = expression cx index in
            Flow_js.unit_flow cx (a, SetElemT (reason, i, t))

        | _ ->
            destructuring_assignment cx t r
      );
      t

  | (r, Assignment.PlusAssign, e) ->
      let reason = mk_reason "+=" loc in
      let rt = assignment_lhs cx r in
      let et = expression cx e in
      let t = Flow_js.mk_tvar cx reason in
      Flow_js.unit_flow cx (rt, AdderT (reason, et, t));
      Flow_js.unit_flow cx (et, AdderT (reason, rt, t));
      Flow_js.unit_flow cx (t, rt);
      rt

  | (r, Assignment.MinusAssign, e)
  | (r, Assignment.MultAssign, e)
  | (r, Assignment.DivAssign, e)
  | (r, Assignment.ModAssign, e)
  | (r, Assignment.LShiftAssign, e)
  | (r, Assignment.RShiftAssign, e)
  | (r, Assignment.RShift3Assign, e)
  | (r, Assignment.BitOrAssign, e)
  | (r, Assignment.BitXorAssign, e)
  | (r, Assignment.BitAndAssign, e)
    ->
      let t = NumT.at loc in
      let rt = assignment_lhs cx r in
      let et = expression cx e in
      Flow_js.unit_flow cx (rt, t);
      Flow_js.unit_flow cx (et, t);
      Flow_js.unit_flow cx (t, rt);
      rt
)

(* Object assignment patterns. In the `copyProperties` model (chain_objects), an
   existing object receives properties from other objects. In the
   `mergeProperties` model (spread_objects), a new object receives properties
   from other objects and is returned. Both these patterns suffer from "races"
   in the type checker, since the object supposed to receive properties is
   available even when the other objects supplying the properties are not yet
   available. In constrast, clone_object makes the receiving object available
   only when the properties have actually been received. This is useful when
   merging properties across modules, e.g., and should eventually replace the
   other patterns wherever they are potentially racy. *)

and chain_objects cx reason this those =
  those |> List.iter (fun that ->
    Flow_js.unit_flow cx
      (that, ObjAssignT(reason, this, AnyT.t))
  );
  this

and spread_objects cx reason those =
  chain_objects cx reason (mk_object cx reason) those

and clone_object cx reason this that =
  Flow_js.mk_tvar_where cx reason (fun tvar ->
    Flow_js.unit_flow cx
      (that, ObjAssignT(reason, this, ObjRestT(reason, [], tvar)))
  )

and chain_objects_lazy cx reason this those =
  let result = ref this in
  List.iter (fun that ->
    result := Flow_js.mk_tvar_where cx reason (fun t ->
      Flow_js.unit_flow cx (that, ObjAssignT(reason, !result, t));
    )
  ) those;
  !result

and jsx cx = Ast.XJS.(function { openingElement; closingElement; children } ->
  jsx_title cx openingElement (List.map (jsx_body cx) children)
)

and jsx_title cx openingElement children = Ast.XJS.(
  let eloc, { Opening.name; attributes; _ } = openingElement in
  match name with

  | Identifier (_, { Identifier.name }) when name = String.capitalize name ->
      let reason = mk_reason (spf "React element: %s" name) eloc in
      let c = Env_js.get_var cx name reason in
      let reason_props = prefix_reason "props of " reason in
      let o = Flow_js.mk_object_with_proto cx reason_props (MixedT reason_props) in

      attributes |> List.iter (function
        | Opening.Attribute (aloc, { Attribute.
              name = Attribute.Identifier (_, { Identifier.name = aname });
              value
            }) ->
            let atype = (match value with
              | Some (Attribute.Literal (loc, lit)) ->
                  literal cx loc lit
              | Some (Attribute.ExpressionContainer (_, {
                  ExpressionContainer.expression = Some (loc, e)
                })) ->
                  expression cx (loc, e)
              | _ ->
                  (* empty or nonexistent attribute values *)
                  UndefT.at aloc
            ) in
            let desc = spf "property %s of " aname in
            let reason_prop = prefix_reason desc reason_props in
            Flow_js.unit_flow cx (o, SetT(reason_prop, aname, atype))

        | Opening.Attribute _ ->
            () (* TODO: attributes with namespaced names *)

        | Opening.SpreadAttribute (aloc, { SpreadAttribute.argument }) ->
            () (* TODO: spread attributes *)
      );

      let reason_children = prefix_reason "children of " reason_props in
      let t = Flow_js.mk_tvar cx reason_children in
      children |> List.iter (
        fun child -> Flow_js.unit_flow cx (child, t)
      );
      let children =
        match List.length children with
        | 0 -> UndefT.t
        | 1 -> t
        | _ -> ArrT(reason_children, t, []) (* TODO: tuples *)
      in
      Flow_js.unit_flow cx (o, SetT(reason_children, "children", children));
      Flow_js.mk_tvar_where cx reason (fun tvar ->
        Flow_js.unit_flow cx (c, MarkupT(reason, o, tvar));
      )

  | Identifier (_, { Identifier.name }) ->
      attributes |> List.iter (function
        | Opening.Attribute (aloc, { Attribute.
              name = Attribute.Identifier (_, { Identifier.name = aname });
              value
            }) ->
            ignore (match value with
              | Some (Attribute.Literal (loc, lit)) ->
                  literal cx loc lit
              | Some (Attribute.ExpressionContainer (_, {
                  ExpressionContainer.expression = Some (loc, e)
                })) ->
                  expression cx (loc, e)
              | _ ->
                  (* empty or nonexistent attribute values *)
                  UndefT.at aloc
            )

        | Opening.Attribute _ ->
            () (* TODO: attributes with namespaced names *)

        | Opening.SpreadAttribute (aloc, { SpreadAttribute.argument }) ->
            () (* TODO: spread attributes *)
      );

      AnyT.t

  | _ ->
      (* TODO? covers namespaced names, member expressions as element names *)
      AnyT.at eloc
)

and jsx_body cx = Ast.XJS.(function
  | loc, Element e -> jsx cx e
  | loc, ExpressionContainer ec -> (
      let { ExpressionContainer.expression = ex } = ec in
      match ex with
        | Some (loc, e) -> expression cx (loc, e)
        | None -> UndefT (mk_reason "empty jsx body" loc)
    )
  | loc, Text s -> StrT.at loc
)

(* Native support for React.PropTypes validation functions, which are
   interpreted as type annotations for React props. This strategy is reasonable
   because the validation functions enforce types at run time (during
   development), and we can always insist on them because they are turned off in
   production. *)

and mk_proptype cx = Ast.Expression.(function
  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "isRequired"; _ });
      _object = e;
      _
    } ->
      mk_proptype cx e

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "number"; _ });
      _
    } ->
      NumT.at vloc

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "string"; _ });
      _
    } ->
      StrT.at vloc

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "bool"; _ });
      _
    } ->
      BoolT.at vloc

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "array"; _ });
      _
    } ->
      ArrT (mk_reason "array" vloc, AnyT.at vloc, [])

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "func"; _ });
      _
    } ->
      AnyT.at vloc (* TODO *)

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "object"; _ });
      _
    } ->
      AnyT.at vloc (* TODO *)

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "node"; _ });
      _
    } ->
      AnyT.at vloc (* TODO *)

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "element"; _ });
      _
    } ->
      AnyT.at vloc (* TODO *)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "arrayOf"; _ });
         _
      };
      arguments = [Expression e];
    } ->
      ArrT (mk_reason "arrayOf" vloc, mk_proptype cx e, [])

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "instanceOf"; _ });
         _
      };
      arguments = [Expression e];
    } ->
      Flow_js.mk_annot cx (mk_reason "instanceOf" vloc)
        (expression cx e)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "objectOf"; _ });
         _
      };
      arguments = [Expression e];
    } ->
      AnyT.at vloc (* TODO *)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "oneOf"; _ });
         _
      };
      arguments = es;
    } ->
      AnyT.at vloc (* TODO *)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "oneOfType"; _ });
         _
      };
      arguments = es;
    } ->
      AnyT.at vloc (* TODO *)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "shape"; _ });
         _
      };
      arguments = [Expression (_, Object { Object.properties })];
    } ->
      let reason = mk_reason "shape" vloc in
      let amap, omap = mk_proptypes cx properties in
      Flow_js.mk_object_with_map_proto cx reason
        (SMap.union amap omap) (MixedT reason)

  | vloc, _ -> AnyT.at vloc
)

and mk_proptypes cx props = Ast.Expression.Object.(
  List.fold_left (fun (amap, omap) -> function

    (* required prop *)
    | Property (loc, { Property.
        kind = Property.Init;
        key = Property.Identifier (_, {
          Ast.Identifier.name; _ });
        value = (vloc, Ast.Expression.Member {
          Ast.Expression.Member.
          property = Ast.Expression.Member.PropertyIdentifier (_, {
            Ast.Identifier.name = "isRequired"; _ });
          _object = e;
          _
        });
        _ }) ->
        let tvar = mk_proptype cx e in
        SMap.add name tvar amap,
        omap

    (* other prop *)
    | Property (loc, { Property.kind = Property.Init;
        key =
          Property.Identifier (_, { Ast.Identifier.name; _ }) |
          Property.Literal (_, {
            Ast.Literal.value = Ast.Literal.String name;
            _;
          });
        value = v;
        _ }) ->
        let tvar = mk_proptype cx v in
        amap,
        SMap.add name tvar omap

    (* literal LHS *)
    | Property (loc, { Property.key = Property.Literal _; _ }) ->
      let msg = "non-string literal property keys not supported" in
      Flow_js.add_error cx [mk_reason "" loc, msg];
      amap, omap

    (* get/set kind *)
    | Property (loc, { Property.kind = Property.Get | Property.Set; _ }) ->
      let msg = "get/set properties not yet supported" in
      Flow_js.add_error cx [mk_reason "" loc, msg];
      amap, omap

    (* computed LHS *)
    | Property (loc, { Property.key = Property.Computed _; _ }) ->
      let msg = "computed property keys not supported" in
      Flow_js.add_error cx [mk_reason "" loc, msg];
      amap, omap

    (* spread prop *)
    | SpreadProperty (loc, { SpreadProperty.argument }) ->
      let msg = "spread property not supported" in
      Flow_js.add_error cx [mk_reason "" loc, msg];
      amap, omap

  ) (SMap.empty, SMap.empty) props
)

and static_method_call_React cx loc m args_ = Ast.Expression.(
  match (m, args_) with

  | "createClass", [ Expression (_,
        Object { Object.properties = class_props }
      ) ] ->
      let reason_class = mk_reason "React class" loc in
      let reason_component = mk_reason "React component" loc in
      let this = Flow_js.mk_tvar cx reason_component in
      let mixins = ref [] in
      let static_reason = prefix_reason "statics of " reason_class in
      let static = ref (mk_object cx static_reason) in
      let attributes_reason = prefix_reason "required props of " reason_component in
      let attributes = ref (mk_object cx attributes_reason) in
      let default_reason = prefix_reason "default props of " reason_component in
      let default = ref (mk_object cx default_reason) in
      let ref_omap = ref SMap.empty in
      let reason_state = prefix_reason "state of " reason_component in
      let state = ref (mk_object cx reason_state) in
      let (fmap, mmap) =
        List.fold_left Ast.Expression.Object.(fun (fmap, mmap) -> function

          (* mixins *)
          | Property (loc, { Property.kind = Property.Init;
              key =
                Property.Identifier (_, { Ast.Identifier.name = "mixins"; _ });
              value = aloc, Array { Array.elements };
              _ }) ->
            mixins := List.map (array_element cx aloc) elements;
            fmap, mmap

          (* statics *)
          | Property (loc, { Property.kind = Property.Init;
               key = Property.Identifier (nloc, {
                Ast.Identifier.name = "statics"; _ });
              value = _, Object { Object.properties };
              _ }) ->
            let reason = mk_reason "statics" nloc in
            let map, spread = object_ cx properties in
            static :=
              extend_object cx reason
              (Flow_js.mk_object_with_map_proto cx reason map (MixedT reason))
              spread;
            fmap, mmap

          (* propTypes *)
          | Property (loc, { Property.kind = Property.Init;
              key = Property.Identifier (nloc, {
                Ast.Identifier.name = "propTypes"; _ });
              value = _, Object { Object.properties };
              _ }) ->
            let reason = mk_reason "propTypes" nloc in
            let amap, omap = mk_proptypes cx properties in
            ref_omap := omap;
            attributes :=
              Flow_js.mk_object_with_map_proto cx reason amap (MixedT reason);
            fmap, mmap

          (* getDefaultProps *)
          | Property (loc, { Property.kind = Property.Init;
              key = Property.Identifier (_, {
                Ast.Identifier.name = "getDefaultProps"; _ });
              value = (vloc, Ast.Expression.Function func);
              _ }) ->
            Ast.Expression.Function.(
              let { params; defaults; rest; body;
                returnType; typeParameters; _ } = func
              in
              let reason = mk_reason "defaultProps" vloc in
              let t = mk_method cx reason (params, defaults, rest)
                returnType body this (MixedT reason)
              in
              let override_default =
                ObjAssignT(default_reason, !default, AnyT.t)
              in
              Flow_js.unit_flow cx (t,
                CallT (reason,
                  Flow_js.mk_functiontype [] None override_default));
              fmap, mmap
            )

          (* getInitialState *)
          | Property (loc, { Property.kind = Property.Init;
              key = Property.Identifier (_, {
                Ast.Identifier.name = "getInitialState"; _ });
              value = (vloc, Ast.Expression.Function func);
              _ }) ->
            Ast.Expression.Function.(
              let { params; defaults; rest; body;
                returnType; typeParameters; _ } = func
              in
              let reason = mk_reason "initialState" vloc in
              let t = mk_method cx reason (params, defaults, rest)
                returnType body this (MixedT reason)
              in
              let override_state =
                ObjAssignT(reason_state, !state, AnyT.t)
              in
              Flow_js.unit_flow cx (t,
                CallT (reason,
                  Flow_js.mk_functiontype [] None override_state));
              fmap, mmap
            )

          (* name = function expr *)
          | Property (loc, { Property.kind = Property.Init;
              key = Property.Identifier (_, {
                Ast.Identifier.name; _ });
              value = (vloc, Ast.Expression.Function func);
              _ }) ->
            Ast.Expression.Function.(
              let { params; defaults; rest; body;
                returnType; typeParameters; _ } = func
              in
              let reason = mk_reason "function" vloc in
              let t = mk_method cx reason (params, defaults, rest)
                returnType body this (MixedT reason)
              in
              fmap, SMap.add name t mmap
            )

          (* name = non-function expr *)
          | Property (loc, { Property.kind = Property.Init;
              key =
                Property.Identifier (_, { Ast.Identifier.name; _ }) |
                Property.Literal (_, {
                  Ast.Literal.value = Ast.Literal.String name; _;
                });
              value = v;
              _ }) ->
            let t = expression cx v in
            SMap.add name t fmap, mmap

          | _ ->
            let msg = "unsupported property specification in createClass" in
            Flow_js.add_error cx [mk_reason "" loc, msg];
            fmap, mmap

        ) (SMap.empty, SMap.empty) class_props in


      let default_tvar = Flow_js.mk_tvar_where cx default_reason (fun t ->
        Flow_js.unit_flow cx
          (!default, ObjExtendT(default_reason, !ref_omap, t))
      ) in
      let props_reason = prefix_reason "props of " reason_component in
      let props = mk_object cx props_reason in
      Flow_js.unit_flow cx
        (!attributes, ObjAssignT(props_reason, props, AnyT.t));
      Flow_js.unit_flow cx
        (default_tvar, ObjAssignT(props_reason, props, AnyT.t));

      let type_args = [!attributes; props; !state] in
      let super_reason = prefix_reason "super of " reason_component in
      let super =
        Flow_js.mk_typeapp_instance cx super_reason
          "ReactComponent" type_args
      in

      let extract_map (from_map,to_map) name =
        match SMap.get name from_map with
        | Some t -> SMap.remove name from_map, SMap.add name t to_map
        | None -> from_map, to_map
      in
      let fmap, smap =
        List.fold_left extract_map (fmap, SMap.empty)
          ["contextTypes";"childContextTypes";"displayName"]
      in
      let override_statics =
        Flow_js.mk_object_with_map_proto cx
          static_reason smap (MixedT static_reason)
      in

      let react_class = Flow_js.mk_typeapp_instance cx reason_class
        "ReactClass" type_args
      in
      Flow_js.unit_flow cx (react_class, override_statics);
      static := clone_object cx static_reason !static react_class;

      (* TODO: Mixins are assumed to be classes. Instead, they should simply be
         objects containing fields/methods, with an optional `statics` property
         which is an object containing fields/methods, and an optional `mixins`
         property which is an array containing mixins. *)

      (* mixins' statics are copied into static to inherit static properties *)
      !mixins |> List.iter (fun mixin ->
        static := clone_object cx static_reason !static mixin;
      );
      let id = Flow_js.mk_nominal cx in
      let itype = {
        class_id = id;
        type_args = IMap.empty;
        fields_tmap = fmap;
        methods_tmap = mmap
      } in
      Flow_js.unit_flow cx (super, SuperT (super_reason, itype));
      Flow_js.unit_flow cx (super, ParentT (super_reason, itype));

      let mixin_reason = prefix_reason "mixins of " reason_class in
      (* mixins are added to super to inherit instance properties *)
      let super =
        if !mixins = [] then super
        else
          let imixins =
            !mixins |> List.map (Flow_js.mk_annot cx mixin_reason)
          in
          imixins |> List.iter (fun imixin ->
            Flow_js.unit_flow cx (imixin, SuperT (mixin_reason, itype))
          );
          IntersectionT (super_reason, super::imixins)
      in
      let instance = InstanceT (reason_component,!static,super,itype) in
      Flow_js.unit_flow cx (instance, this);

      CustomClassT("ReactClass", type_args, instance)

  | _, args ->
      let argts = List.map (expression_or_spread cx) args in
      Flow_js.static_method_call cx "React"
        (mk_reason (spf "React.%s" m) loc) m argts
)

and predicate_of_condition cx e = Ast.Expression.(match e with

  (* x instanceof t *)
  | _, Binary { Binary.operator = Binary.Instanceof;
      left = _, Identifier (_, { Ast.Identifier.name; _ });
      right
    } ->
      let t = expression cx right in
      (
        BoolT.t,
        SMap.singleton name (InstanceofP t),
        SMap.singleton name (NotP (InstanceofP t)),
        SMap.empty
      )

  (* o.p instanceof t *)
  | _, Binary { Binary.operator = Binary.Instanceof;
     left = _, Member {
         Member._object = _, Identifier (_,
           { Ast.Identifier.name = oname; _ });
         property = Member.PropertyIdentifier (ploc,
           { Ast.Identifier.name = pname; _ });
         _ } as left;
      right
    } ->
      let name = Env_js.prop_lookup_name oname pname in
      let lt = expression cx left in
      let rt = expression cx right in
      (
        BoolT.t,
        SMap.singleton name (InstanceofP rt),
        SMap.singleton name (NotP (InstanceofP rt)),
        SMap.singleton name lt
      )

  (* x == null *)
  | _, Binary { Binary.operator = Binary.Equal;
      left = _, Identifier (_, { Ast.Identifier.name; _ });
      right = _, Literal { Ast.Literal.value = Ast.Literal.Null; _; }
    } ->
      (
        BoolT.t,
        SMap.singleton name (IsP "maybe"),
        SMap.singleton name (NotP (IsP "maybe")),
        SMap.empty
      )

  (* o.p == null *)
  | _, Binary { Binary.operator = Binary.Equal;
     left = _, Member {
         Member._object = _, Identifier (_,
           { Ast.Identifier.name = oname; _ });
         property = Member.PropertyIdentifier (ploc,
           { Ast.Identifier.name = pname; _ });
         _ } as left;
     right = _, Literal { Ast.Literal.value = Ast.Literal.Null; _; }
   } ->
      let name = Env_js.prop_lookup_name oname pname in
      let t = expression cx left in
      (
        BoolT.t,
        SMap.singleton name (IsP "maybe"),
        SMap.singleton name (NotP (IsP "maybe")),
        SMap.singleton name t
      )

  (* x != null *)
  | _, Binary { Binary.operator = Binary.NotEqual;
      left = _, Identifier (_, { Ast.Identifier.name; _ });
      right = _, Literal { Ast.Literal.value = Ast.Literal.Null; _; }
    } ->
      (
        BoolT.t,
        SMap.singleton name (NotP (IsP "maybe")),
        SMap.singleton name (IsP "maybe"),
        SMap.empty
      )

  (* o.p != null *)
  | _, Binary { Binary.operator = Binary.NotEqual;
     left = _, Member {
         Member._object = _, Identifier (_,
           { Ast.Identifier.name = oname; _ });
         property = Member.PropertyIdentifier (ploc,
           { Ast.Identifier.name = pname; _ });
         _ } as left;
     right = _, Literal { Ast.Literal.value = Ast.Literal.Null; _; }
   } ->
     let name = Env_js.prop_lookup_name oname pname in
     let t = expression cx left in
     (
       BoolT.t,
       SMap.singleton name (NotP (IsP "maybe")),
       SMap.singleton name (IsP "maybe"),
       SMap.singleton name t
     )

  (* x === null *)
  | _, Binary { Binary.operator = Binary.StrictEqual;
      left = _, Identifier (_, { Ast.Identifier.name; _ });
      right = _, Literal { Ast.Literal.value = Ast.Literal.Null; _; }
    } ->
      (
        BoolT.t,
        SMap.singleton name (IsP "null"),
        SMap.singleton name (NotP (IsP "null")),
        SMap.empty
      )

  (* o.p === null *)
  | _, Binary { Binary.operator = Binary.StrictEqual;
     left = _, Member {
         Member._object = _, Identifier (_,
           { Ast.Identifier.name = oname; _ });
         property = Member.PropertyIdentifier (ploc,
           { Ast.Identifier.name = pname; _ });
         _ } as left;
     right = _, Literal { Ast.Literal.value = Ast.Literal.Null; _; }
   } ->
      let name = Env_js.prop_lookup_name oname pname in
      let t = expression cx left in
      (
        BoolT.t,
        SMap.singleton name (IsP "null"),
        SMap.singleton name (NotP (IsP "null")),
        SMap.singleton name t
      )

  | _, Binary { Binary.operator = Binary.StrictNotEqual;
      left = _, Identifier (_, { Ast.Identifier.name; _ });
      right = _, Literal { Ast.Literal.value = Ast.Literal.Null; _; }
    } ->
      (
        BoolT.t,
        SMap.singleton name (NotP (IsP "null")),
        SMap.singleton name (IsP "null"),
        SMap.empty
      )

  (* o.p !== null *)
  | _, Binary { Binary.operator = Binary.StrictNotEqual;
     left = _, Member {
         Member._object = _, Identifier (_,
           { Ast.Identifier.name = oname; _ });
         property = Member.PropertyIdentifier (ploc,
           { Ast.Identifier.name = pname; _ });
         _ } as left;
     right = _, Literal { Ast.Literal.value = Ast.Literal.Null; _; }
   } ->
      let name = Env_js.prop_lookup_name oname pname in
      let t = expression cx left in
      (
        BoolT.t,
        SMap.singleton name (NotP (IsP "null")),
        SMap.singleton name (IsP "null"),
        SMap.singleton name t
      )

  (* TODO: (strict) equality of undefined *)

  (* x *)
  | loc, Identifier (_, { Ast.Identifier.name; _ }) ->
      let reason = mk_reason name loc in
      (
        Env_js.get_var cx name reason,
        SMap.singleton name ExistsP,
        SMap.singleton name (NotP ExistsP),
        SMap.empty
      )

  (* o.p *)
  | loc, Member {
         Member._object = _, Identifier (_,
           { Ast.Identifier.name = oname; _ });
         property = Member.PropertyIdentifier (ploc,
           { Ast.Identifier.name = pname; _ });
         _ } as expr ->
      let name = Env_js.prop_lookup_name oname pname in
      let t = expression cx expr in
      (
        t,
        SMap.singleton name ExistsP,
        SMap.singleton name (NotP ExistsP),
        SMap.singleton name t
      )

  | _, Binary { Binary.operator = Binary.Equal | Binary.StrictEqual;
      left = _, Unary { Unary.operator = Unary.Typeof;
        argument = _, Identifier (_, { Ast.Identifier.name; _ }); _ };
      right = _, Literal { Ast.Literal.value = Ast.Literal.String s; _; }
    } ->
      (
        BoolT.t,
        SMap.singleton name (IsP s),
        SMap.singleton name (NotP (IsP s)),
        SMap.empty
      )

  (* Array.isArray(x) *)
  | _, Call {
      Call.callee = _, Member {
        Member._object = _, Identifier (_,
          { Ast.Identifier.name = "Array"; _ });
        property = Member.PropertyIdentifier (_,
          { Ast.Identifier.name = "isArray"; _ });
        _
      };
      arguments = [Expression (_, Identifier (_,
          { Ast.Identifier.name; _ }))]
    } ->
      (
        BoolT.t,
        SMap.singleton name (IsP "array"),
        SMap.singleton name (NotP (IsP "array")),
        SMap.empty
      )

  (* Array.isArray(o.p) *)
  | _, Call {
      Call.callee = _, Member {
        Member._object = _, Identifier (_,
          { Ast.Identifier.name = "Array"; _ });
        property = Member.PropertyIdentifier (_,
          { Ast.Identifier.name = "isArray"; _ });
        _
      };
      arguments = [Expression (_, Member {
         Member._object = _, Identifier (_,
           { Ast.Identifier.name = oname; _ });
         property = Member.PropertyIdentifier (ploc,
           { Ast.Identifier.name = pname; _ });
         _ } as expr
      )]
    } ->
      let name = Env_js.prop_lookup_name oname pname in
      let t = expression cx expr in
      (
        BoolT.t,
        SMap.singleton name (IsP "array"),
        SMap.singleton name (NotP (IsP "array")),
        SMap.singleton name t
      )

  | loc, Logical { Logical.operator = Logical.And; left; right } ->
      let reason = mk_reason "&&" loc in
      let t1, map1, not_map1, xts1 = predicate_of_condition cx left in
      let t = refine_type cx reason t1 (NotP ExistsP) in
      let t2, map2, not_map2, xts2 = Env_js.refine_env cx reason map1 xts1
        (fun () -> predicate_of_condition cx right)
      in
      Flow_js.unit_flow cx (t2, t);
      (
        t,
        mk_and map1 map2,
        mk_or not_map1 not_map2,
        SMap.union xts1 xts2
      )

  | loc, Logical { Logical.operator = Logical.Or; left; right } ->
      let reason = mk_reason "||" loc in
      let t1, map1, not_map1, xts1 = predicate_of_condition cx left in
      let t = refine_type cx reason t1 ExistsP in
      let t2, map2, not_map2, xts2 = Env_js.refine_env cx reason not_map1 xts1
        (fun () -> predicate_of_condition cx right)
      in
      Flow_js.unit_flow cx (t2,t);
      (
        t,
        mk_or map1 map2,
        mk_and not_map1 not_map2,
        SMap.union xts1 xts2
      )

  | _, Unary { Unary.operator = Unary.Not; argument; _ } ->
      let (t, map, not_map, xts) = predicate_of_condition cx argument in
      (t, not_map, map, xts)

  | e ->
      (expression cx e, SMap.empty, SMap.empty, SMap.empty)
)

and mk_and map1 map2 = SMap.merge
  (fun x -> fun p1 p2 -> match (p1,p2) with
    | (None, None) -> None
    | (Some p, None)
    | (None, Some p) -> Some p
    | (Some p1, Some p2) -> Some (AndP(p1,p2))
  )
  map1 map2

and mk_or map1 map2 = SMap.merge
  (fun x -> fun p1 p2 -> match (p1,p2) with
    | (None, None) -> None
    | (Some p, None)
    | (None, Some p) -> None
    | (Some p1, Some p2) -> Some (OrP(p1,p2))
  )
  map1 map2

(* TODO: switch to TypeScript specification of Object *)
and static_method_call_Object cx loc m args_ = Ast.Expression.(
  let reason = mk_reason (spf "Object.%s" m) loc in
  match (m, args_) with
  | ("create", [ Expression e ]) ->
    let proto = expression cx e in
    Flow_js.mk_object_with_proto cx reason proto

  | ("create", [ Expression e;
                 Expression (_, Object { Object.properties }) ]) ->
    let proto = expression cx e in
    let pmap, _ = object_ cx properties in
    let map = pmap |> SMap.mapi (fun x spec ->
      let reason = prefix_reason (spf ".%s of " x) reason in
      Flow_js.mk_tvar_where cx reason (fun tvar ->
        Flow_js.unit_flow cx (spec, GetT(reason, "value", tvar));
      )
    ) in
    Flow_js.mk_object_with_map_proto cx reason map proto

  | ("getPrototypeOf", [ Expression e ]) ->
    let o = expression cx e in
    Flow_js.mk_tvar_where cx reason (fun tvar ->
      Flow_js.unit_flow cx (o, GetT(reason, "__proto__", tvar));
    )

  | (("getOwnPropertyNames" | "keys"), [ Expression e ]) ->
    let o = expression cx e in
    ArrT (reason,
      Flow_js.mk_tvar_where cx reason (fun tvar ->
        let reason = prefix_reason "element of " reason in
        Flow_js.unit_flow cx (o, KeyT(reason, tvar));
      ),
          [])

  | ("defineProperty", [ Expression e;
                         Expression (_, Literal
                           { Ast.Literal.value = Ast.Literal.String x; _ });
                         Expression config ]) ->
    let o = expression cx e in
    let spec = expression cx config in
    let tvar = Flow_js.mk_tvar cx reason in
    Flow_js.unit_flow cx (spec, GetT(reason, "value", tvar));
    Flow_js.unit_flow cx (o, SetT (reason, x, tvar));
    o

  | ("defineProperties", [ Expression e;
                         Expression (_, Object { Object.properties }) ]) ->
    let o = expression cx e in
    let pmap, _ = object_ cx properties in
    pmap |> SMap.iter (fun x spec ->
      let reason = prefix_reason (spf ".%s of " x) reason in
      let tvar = Flow_js.mk_tvar cx reason in
      Flow_js.unit_flow cx (spec, GetT(reason, "value", tvar));
      Flow_js.unit_flow cx (o, SetT (reason, x, tvar));
    );
    o

  | ("assign", (Expression e)::others) ->
    let this = expression cx e in
    let those = List.map (expression_or_spread cx) others in
    chain_objects_lazy cx reason this those

  (* TODO *)
  | (("seal" | "preventExtensions"), args)
  | ("freeze", args)

  | (_, args) ->
      let argts = List.map (expression_or_spread cx) args in
      Flow_js.static_method_call cx "Object" reason m argts
)

and static_method_call cx name tok m argts =
  let reason = mk_reason (spf "%s.%s" name m) tok in
  let cls = Flow_js.get_builtin cx name reason in
  Flow_js.mk_tvar_where cx reason (fun tvar ->
    Flow_js.unit_flow cx (cls, MethodT(reason, m, cls, argts, tvar, 0));
  )

(* TODO: reason_c could be replaced by c *)
and mk_extends cx reason_c map = function
  | (None, None) ->
      let root = MixedT (reason_of_string "Object") in
      root
  | (None, _) ->
      assert false (* type args with no head expr *)
  | (Some e, targs) ->
      let loc, _ = e in
      let reason = mk_reason (desc_of_reason reason_c) loc in
      let params = match targs with
      | None -> None
      | Some (_, { Ast.Type.ParameterInstantiation.params; }) -> Some params in
      mk_nominal_type cx reason map (e, params)

and mk_nominal_type cx reason map (e, targs) =
  let c = expression cx e in
  mk_nominal_type_ cx reason map (c, targs)

and mk_nominal_type_ cx reason map (c, targs) =
  match targs with
  | Some ts ->
      let tparams = List.map (convert cx map) ts in
      TypeAppT (c, tparams)
  | None ->
      Flow_js.mk_annot cx reason c

and body_loc = Ast.Statement.FunctionDeclaration.(function
  | BodyBlock (loc, _) -> loc
  | BodyExpression (loc, _) -> loc
)

and mk_signature cx c_type_params_map body = Ast.Statement.Class.(
  let _, { Body.body = elements } = body in

  let fields = List.fold_left (fun fields -> function

    | Body.Property (loc, {
        Property.key = Ast.Expression.Object.Property.Identifier
          (_, { Ast.Identifier.name; _ });
        typeAnnotation = (_, typeAnnotation);
        static = false;
      }) ->
        let t = convert cx c_type_params_map typeAnnotation in
        SMap.add name t fields

    | _ -> fields

  ) SMap.empty elements
  in

  let methods =
    SMap.singleton "constructor"
      ([], SMap.empty, ([], [], VoidT.t, SMap.empty, SMap.empty))
  in

  List.fold_left (fun (fields, methods) -> function
    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Identifier (_,
          { Ast.Identifier.name; _ });
        value = _, { Ast.Expression.Function.params; defaults; rest;
          returnType; typeParameters; body; _ };
        kind = Ast.Expression.Object.Property.Init;
        static = false
      }) ->
      let fields = if name = "constructor"
        then mine_fields cx body fields
        else fields in
      let typeparams, imap, f_type_params_map =
        mk_type_param_declarations cx typeParameters in
      let map = SMap.fold SMap.add f_type_params_map c_type_params_map in
      let params_ret = mk_params_ret cx map
        (params, defaults, rest) (body_loc body, returnType) in
      let params_ret = if name = "constructor"
        then (
          let params, pnames, ret, params_map, params_loc = params_ret in
          let return_void = VoidT (mk_reason "return undefined" loc) in
          Flow_js.unit_flow cx (ret, return_void);
          params, pnames, return_void, params_map, params_loc
        )
        else params_ret
      in
      (fields,
       SMap.add name (typeparams, f_type_params_map, params_ret) methods
      )

    | _ -> (fields, methods)
  ) (fields, methods) elements
)

and mk_class_elements cx this super method_sigs body = Ast.Statement.Class.(
  let _, { Body.body = elements } = body in
  List.iter (function

    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Identifier (_,
          { Ast.Identifier.name; _ });
        value = _, { Ast.Expression.Function.params; defaults; rest;
          returnType; typeParameters; body; _ };
        kind = Ast.Expression.Object.Property.Init;
        static = false
      }) ->

      let (typeparams, type_params_map,
           (_, _, ret, param_types_map, param_loc_map)) =
        SMap.find_unsafe name method_sigs in

      let reason = mk_reason (spf "method %s" name) loc in
      let save_return_exn = Abnormal.swap Abnormal.Return false in
      generate_tests reason typeparams (fun map_ ->
        let param_types_map =
          param_types_map |> SMap.map (Flow_js.subst cx map_) in
        let ret = Flow_js.subst cx map_ ret in

        mk_body None cx param_types_map param_loc_map ret body this super;
      );
      if not (Abnormal.swap Abnormal.Return save_return_exn)
      then Flow_js.unit_flow cx (VoidT (mk_reason "return undefined" loc), ret)

    | _ -> ()
  ) elements
)

and mk_static_fields_methods cx super_static static body = Ast.Statement.Class.(
  let _, { Body.body = elements } = body in

  let map = List.fold_left (fun map -> function

    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Identifier (_,
          { Ast.Identifier.name; _ });
        value = _, { Ast.Expression.Function.params; defaults; rest;
          returnType; typeParameters; body; _ };
        kind = Ast.Expression.Object.Property.Init;
        static = true
      }) ->
      let reason = mk_reason (spf "function %s" name) loc in
      let this = Flow_js.mk_tvar cx (replace_reason "this" reason) in
      let meth = mk_function None cx reason
        typeParameters (params, defaults, rest) returnType body this
      in
      SMap.add name meth map

    | Body.Property (loc, {
        Property.key = Ast.Expression.Object.Property.Identifier
          (_, { Ast.Identifier.name; _ });
        typeAnnotation = (_, typeAnnotation);
        static = true;
      }) ->
        let t = convert cx SMap.empty typeAnnotation in
        SMap.add name t map

    | _ -> map
  ) SMap.empty elements in

  Flow_js.unit_flow cx (
    Flow_js.mk_object_with_map_proto cx (reason_of_t static) map super_static,
    static
  )
)

(* TODO: why reason_c? *)
and mk_methodtype reason_c (typeparams,_,(params,pnames,ret,_,_)) =
  let ft = FunT (reason_c, Flow_js.dummy_static, Flow_js.dummy_prototype,
                 Flow_js.mk_functiontype2 params pnames ret 0) in
  if (typeparams = [])
  then
    ft
  else
    PolyT (typeparams, ft)

and mk_class cx reason_c type_params extends body =
  let typeparams, type_args_map, type_params_map =
    mk_type_param_declarations cx type_params in

  let (fields, methods_) = mk_signature cx type_params_map body in

  let id = Flow_js.mk_nominal cx in

  let super = mk_extends cx reason_c type_params_map extends in

  let static_reason = prefix_reason "statics of " reason_c in
  let super_static = Flow_js.mk_tvar_where cx static_reason (fun t ->
    Flow_js.unit_flow cx (super, GetT(static_reason, "statics", t));
  ) in
  let static = Flow_js.mk_tvar cx static_reason in

  generate_tests reason_c typeparams (fun map_ ->
    let super = Flow_js.subst cx map_ super in

    let fields = fields |> SMap.map (Flow_js.subst cx map_) in

    let methods_ = methods_ |> SMap.map
        (fun (typeparams,type_params_map,
             (params,pnames,ret,param_types_map,param_loc_map)) ->

          let params = List.map (Flow_js.subst cx map_) params in
          let ret = Flow_js.subst cx map_ ret in
          let param_types_map =
            SMap.map (Flow_js.subst cx map_) param_types_map in
          (typeparams,type_params_map,
           (params, Some pnames, ret, param_types_map, param_loc_map))
        )
    in
    let methods = methods_ |> SMap.map (mk_methodtype reason_c) in
    let instance = {
      class_id = id;
      type_args = type_args_map |> IMap.map (Flow_js.subst cx map_);
      fields_tmap = fields;
      methods_tmap = methods;
    } in
    let super_reason = prefix_reason "super of " reason_c in
    Flow_js.unit_flow cx (super, SuperT(super_reason, instance));

    let this = InstanceT (reason_c,static,super,instance) in
    mk_class_elements cx this super methods_ body;
  );

  mk_static_fields_methods cx super_static static body;

  let methods = methods_ |> SMap.map (
    fun (typeparams, type_params_map, (params,pnames,ret,params_map,params_loc)) ->
      let ret =
        if (is_void cx ret)
        then (VoidT.at (loc_of_t ret))
        else ret
      in
      mk_methodtype reason_c
        (typeparams, type_params_map,
         (params,Some pnames,ret,params_map,params_loc))
  ) in

  let instance = {
    class_id = id;
    type_args = type_args_map;
    fields_tmap = fields;
    methods_tmap = methods;
  } in
  let super_reason = prefix_reason "super of " reason_c in
  let this = InstanceT (reason_c, static, super, instance) in
  Flow_js.unit_flow cx (super, ParentT(super_reason, instance));

  if (typeparams = [])
  then
    ClassT this
  else
    PolyT(typeparams, ClassT this)

and mk_interface cx reason typeparams imap map (sfmap, smmap, fmap, mmap) extends =
  let id = Flow_js.mk_nominal cx in
  let extends =
    match extends with
    | [] -> (None,None)
    | (loc,{Ast.Statement.Interface.Extends.id; typeParameters})::_ ->
        (* TODO: multiple extends *)
        Some (loc,Ast.Expression.Identifier id), typeParameters
  in
  let super_reason = prefix_reason "super of " reason in
  let super = mk_extends cx super_reason map extends in

  let static_reason = prefix_reason "statics of " reason in
  let static, fmap =
    match SMap.get "statics" fmap with
    | None ->
      Flow_js.mk_object_with_map_proto cx static_reason
        (SMap.union sfmap smmap) (MixedT (reason_of_string "Object")),
      fmap
    | Some t -> t, SMap.remove "statics" fmap
  in

  let (imixins,fmap) =
    match SMap.get "mixins" fmap with
    | None -> ([], fmap)
    | Some (ArrT(_,_,ts)) -> (ts, SMap.remove "mixins" fmap)
    | _ -> assert false
  in

  let instance = {
    class_id = id;
    type_args = imap;
    fields_tmap = fmap;
    methods_tmap = mmap;
  } in
  Flow_js.unit_flow cx (super, ParentT(super_reason, instance));
  Flow_js.unit_flow cx (super, SuperT(super_reason, instance));

  (* mixins' statics are copied into static to inherit static properties *)

  imixins |> List.iter (fun imixin ->
    Flow_js.unit_flow cx (ClassT(imixin), ObjAssignT(static_reason, static, AnyT.t));
    Flow_js.unit_flow cx (imixin, SuperT(super_reason, instance));
  );

  (* mixins are added to super to inherit instance properties *)
  let super =
    if imixins = [] then super
    else
      IntersectionT (super_reason, super::imixins)
  in
  (* TODO: check that super is SuperT *)
  let c = ClassT(InstanceT (reason, static, super, instance)) in
  if typeparams = []
  then c
  else PolyT (typeparams, c)

and function_decl id cx (reason:reason) type_params params ret body this super =
  let typeparams,_,type_params_map = mk_type_param_declarations cx type_params in

  let (params, pnames, ret, param_types_map, param_types_loc) =
    mk_params_ret cx type_params_map params (body_loc body, ret) in

  let save_return_exn = Abnormal.swap Abnormal.Return false in
  generate_tests reason typeparams (fun map_ ->
    let param_types_map =
      param_types_map |> SMap.map (Flow_js.subst cx map_) in
    let ret = Flow_js.subst cx map_ ret in

    mk_body id cx param_types_map param_types_loc ret body this super;
  );

  if not (Abnormal.swap Abnormal.Return save_return_exn)
  then Flow_js.unit_flow cx (VoidT.why reason, ret);

  let ret =
    if (is_void cx ret)
    then (VoidT.at (loc_of_t ret))
    else ret
  in

  (typeparams,params,pnames,ret)

and is_void cx = function
  | OpenT(_,id) ->
      Flow_js.check_lower_bound cx id (function | VoidT _ -> true | _ -> false)
  | _ -> false

and mk_upper_bound cx locs name t =
  let reason =
    reason_of_t t
    |> prefix_reason "upper bound of "
  in
  let tvar = Flow_js.mk_tvar cx reason in
  Flow_js.unit_flow cx (t,tvar);
  let loc =
    if SMap.mem name locs
    then Some (SMap.find_unsafe name locs)
    else None
  in
  create_env_entry t tvar loc

and mk_body id cx param_types_map param_types_loc ret body this super =
  let ctx = !Env_js.env in
  let new_ctx = Env_js.clone_env ctx in
  Env_js.update_frame cx new_ctx;
  Env_js.havoc_env();
  let add_rec map =
    match id with
    | None -> map
    | Some (loc, { Ast.Identifier.name; _ }) ->
        map |> SMap.add name
          (create_env_entry (AnyT.at loc) (AnyT.at loc) None)
  in
  let block = ref (
      param_types_map
      |> SMap.mapi (mk_upper_bound cx param_types_loc)
      |> add_rec
      |> SMap.add
          (internal_name "super")
          (create_env_entry super super None)
      |> SMap.add
          (internal_name "this")
          (create_env_entry this this None)
      |> SMap.add
          (internal_name "return")
          (create_env_entry ret ret None)
  )
  in
  Env_js.push_env block;
  Flow_js.mk_frame cx !Flow_js.frames !Env_js.env;
  let set = Env_js.swap_changeset (fun _ -> SSet.empty) in

  let stmts = Ast.Statement.(match body with
    | FunctionDeclaration.BodyBlock (_, { Block.body }) ->
        body
    | FunctionDeclaration.BodyExpression expr ->
        [ fst expr, Return { Return.argument = Some expr } ]
  ) in

  List.iter (statement_decl cx) stmts;
  ignore_return_exception_handler (fun () ->
    toplevels cx stmts
  );

  Flow_js.frames := List.tl !Flow_js.frames;
  Env_js.pop_env();
  Env_js.changeset := set;

  Env_js.update_frame cx ctx

and before_pos loc =
  Ast.Loc.(
    let line = loc.start.line in
    let column = loc.start.column in
    { loc with
        start = { line = line; column = column - 1 };
        _end = { line = line; column = column }
    }
  )

and mk_params_ret cx map_ params (body_loc, ret_type_opt) =

  let (params, defaults, rest) = params in
  let defaults = if defaults = [] && params <> []
    then List.map (fun _ -> None) params
    else defaults
  in

  let rev_param_types_list,
      rev_param_names,
      param_types_map,
      param_types_loc =
    List.fold_left2 (fun (tlist, pnames, tmap, lmap) param default ->
      Ast.Pattern.(match param with
        | loc, Identifier (_, {
            Ast.Identifier.name; typeAnnotation; optional
          }) ->
            let reason = mk_reason (spf "parameter %s" name) loc in
            let t = mk_type_annotation_ cx map_ reason typeAnnotation in
            (match default with
              | None ->
                  let external_t, internal_t =
                    if optional
                    then
                      let reason = reason_of_t t in
                      OptionalT t, UnionT (reason, [VoidT.why reason; t])
                    else t, t
                  in
                  external_t :: tlist,
                  name :: pnames,
                  SMap.add name internal_t tmap,
                  SMap.add name loc lmap
              | Some expr ->
                  (* TODO: assert (not optional) *)
                  let te = expression cx expr in
                  Flow_js.unit_flow cx (te, t);
                  (OptionalT t) :: tlist,
                  name :: pnames,
                  SMap.add name t tmap,
                  SMap.add name loc lmap
            )
        | loc, _ ->
            let reason = mk_reason "destructuring" loc in
            let t = type_of_pattern param |> mk_type_annotation_ cx map_ reason in
            let (des_tmap, des_lmap) = destructuring_map cx t param in
            t :: tlist, "_" :: pnames,
            SMap.union tmap des_tmap,
            SMap.union lmap des_lmap
        )
    ) ([], [], SMap.empty, SMap.empty) params defaults
  in

  let rev_param_types_list,
      rev_param_names,
      param_types_map,
      param_types_loc =
    match rest with
      | None -> rev_param_types_list,
                rev_param_names,
                param_types_map,
                param_types_loc
      | Some (loc, { Ast.Identifier.name; typeAnnotation; _ }) ->
          let reason = mk_reason (spf "rest parameter %s" name) loc in
          let t = mk_type_annotation_ cx map_ reason typeAnnotation in
          ((mk_rest cx t) :: rev_param_types_list,
            name :: rev_param_names,
            SMap.add name t param_types_map,
            SMap.add name loc param_types_loc)
  in

  let phantom_return_loc = before_pos body_loc in
  let return_type =
    mk_type_annotation_ cx map_ (mk_reason "return" phantom_return_loc) ret_type_opt in

  (List.rev rev_param_types_list,
   List.rev rev_param_names,
   return_type,
   param_types_map,
   param_types_loc)

(* Generate for every type parameter a pair of tests, instantiating that type
   parameter with Top and Bottom. Run a closure that takes these instantiations,
   each one in turn, and does something with it. We modify the salt for every
   instantiation so that re-analyzing the same AST with different instantiations
   causes different reasons to be generated. *)

and generate_tests reason typeparams each =
  typeparams
  |> List.fold_left (fun list {id; name; _ } ->
    let xreason = replace_reason name reason in
    let top = MixedT (
      prefix_reason "some instantiation (e.g., mixed) of " xreason
    ) in
    let bot = UndefT (
      prefix_reason "another instantiation (e.g., undefined) of " xreason
    ) in
    List.rev_append
      (list |> List.map (IMap.add id top))
      (list |> List.map (IMap.add id bot))
  ) [IMap.empty]
  |> List.iteri (fun i map_ ->
       each map_;
     )

(* take a list of types appearing in AST as type params,
   do semantic checking and create tvars for them. *)
and mk_type_params cx (types: Ast.Identifier.t list) =
  let mk_type_param (typeparams, imap, smap) (loc, t) =
    let name = t.Ast.Identifier.name in
    let reason = mk_reason name loc in

    (* TODO: Can we get rid of typeparam.id?  What will be the implications on
       shadowing? Investigate! *)
    let id = Flow_js.mk_var cx in

    let typeparam = { reason; id; name; } in
    (typeparam :: typeparams,
     IMap.add id (BoundT typeparam) imap,
     SMap.add name (BoundT typeparam) smap)
  in
  let typeparams, imap, map =
    List.fold_left mk_type_param ([], IMap.empty, SMap.empty) types
  in
  List.rev typeparams, imap, map

and mk_type_param_declarations cx typeParameters =
  mk_type_params cx (extract_type_param_declarations typeParameters)

and extract_type_param_declarations = function
  | None -> []
  | Some (_, typeParameters) -> typeParameters.Ast.Type.ParameterDeclaration.params

and extract_type_param_instantiations = function
  | None -> []
  | Some (_, typeParameters) -> typeParameters.Ast.Type.ParameterInstantiation.params

and mk_function id cx reason type_params params ret body this =

  let (typeparams,params,pnames,ret) =
    function_decl id cx reason type_params params ret
      body this (MixedT (replace_reason "empty super object" reason))
  in

  (* prepare type *)
  let proto_reason = replace_reason "prototype" reason in
  let prototype = mk_object cx proto_reason in
  let static = mk_object cx (prefix_reason "statics of " reason) in

  let funtype = {
    this_t = this;
    params_tlist = params;
    params_names = Some pnames;
    return_t = ret;
    closure_t = List.hd !Flow_js.frames
  } in

  if (typeparams = [])
  then
    FunT (reason, static,prototype,funtype)
  else
    PolyT (typeparams, FunT(reason, static,prototype,funtype))

and mk_method cx reason params ret body this super =
  let (_,params,pnames,ret) =
    function_decl None cx reason None params ret body this super
  in
  FunT (reason, Flow_js.dummy_static, Flow_js.dummy_prototype,
        Flow_js.mk_functiontype2
          params (Some pnames) ret (List.hd !Flow_js.frames))

(* scrape top-level, unconditional field assignments from constructor code *)
(** TODO: use a visitor **)
and mine_fields cx body fields =

  let scrape_field_assign_expr map = Ast.Expression.(function
    | _, Assignment {
        Assignment.left = _, Ast.Pattern.Expression (_, Member {
          Member._object = _, This;
          property = Member.PropertyIdentifier (
            loc, { Ast.Identifier.name = name; _ }
          );
          _
        });
        _
      } ->
        if (SMap.mem name map)
        then map
        else
          let desc = (spf "field %s constructor init" name) in
          let t = mk_type cx (mk_reason desc loc) None in
          SMap.add name t map
    | _ ->
        map
  ) in

  Ast.Statement.FunctionDeclaration.(match body with
    | BodyExpression expr ->
        scrape_field_assign_expr fields expr

    | BodyBlock (_, { Ast.Statement.Block.body }) ->
        List.fold_left Ast.Statement.(fun acc -> function
          | _, Expression { Expression.expression } ->
              scrape_field_assign_expr acc expression
          | _ -> acc
        ) fields body
  )


(**********)
(* Driver *)
(**********)

(* seed new graph with builtins *)
let add_builtins cx =
  let reason, id = open_tvar Flow_js.builtins in
  cx.graph <- cx.graph |>
      IMap.add id (new_bounds id reason)

(* cross-module GC toggle *)
let xmgc_enabled = ref true

(**************************************)

let cross_module_gc =

  let require_set = ref SSet.empty in

  fun cx ms reqs ->
    if !xmgc_enabled then (
      reqs |> SSet.iter (fun r -> require_set := SSet.add r !require_set);
      ms |> SSet.iter (fun m -> require_set := SSet.remove m !require_set);

      let ins = (Flow_js.builtins)::(
        SSet.fold (fun r list -> (lookup_module cx r)::list) !require_set []
      ) in
      Flow_js.do_gc cx ins []
    )

let force_annotations cx =
  let tvar = lookup_module cx cx._module in
  let reason, id = open_tvar tvar in
  let bounds = Flow_js.find_graph cx id in
  let before = Errors_js.ErrorSet.cardinal cx.errors in
  Flow_js.enforce_strict cx id bounds;
  let after = Errors_js.ErrorSet.cardinal cx.errors in
  let ground_bounds = new_bounds id reason in
  if (after = before)
  then ground_bounds.lower <- bounds.lower;
  cx.graph <- IMap.add id ground_bounds cx.graph

(* core inference, assuming setup and teardown happens elsewhere *)
let infer_core cx statements =
  try
    statements |> List.iter (statement_decl cx);
    statements |> toplevels cx;
  with
    | Abnormal.Exn _ ->
        let msg = "abnormal control flow" in
        Flow_js.add_warning cx [new_reason "" (Pos.make_from
          (Relative_path.create Relative_path.Dummy cx.file)), msg]
    | exc ->
        let msg = Printexc.to_string exc in
        Flow_js.add_warning cx [new_reason "" (Pos.make_from
          (Relative_path.create Relative_path.Dummy cx.file)), msg]

(* build module graph *)
let infer_ast ast file m force_check =
  Env_js.global_block := SMap.empty;
  Flow_js.Cache.clear();

  let (loc, statements, comments) = ast in

  let mode = Module_js.parse_flow comments in

  let check = (match mode with
    | Module_js.ModuleMode_Unchecked -> force_check
    | _ -> true
  ) in

  let weak = (match mode with
    | Module_js.ModuleMode_Weak -> true
    | _ -> modes.weak_by_default
  ) in

  let cx = new_context file m in

  (* add types for pervasive builtins *)
  add_builtins cx;

  cx.weak <- weak;

  let reason_exports_module = reason_of_string (spf "exports of module %s" m) in
  let block = ref
    (SMap.singleton "exports"
       (create_env_entry
          (UndefT (reason_of_string "undefined exports"))
          (Flow_js.mk_tvar cx reason_exports_module)
          None
       ) |>
     SMap.add (internal_name "exports")
       (create_env_entry
          (UndefT (reason_of_string "undefined exports"))
          (AnyT reason_exports_module)
          None
       )
    )
  in
  Env_js.env := [block];
  Flow_js.mk_frame cx [] !Env_js.env;
  Env_js.changeset := SSet.empty;

  let reason = new_reason "exports" (Pos.make_from
    (Relative_path.create Relative_path.Dummy cx.file)) in

  if check then (
    set_module_exports cx reason (mk_object cx reason);
    infer_core cx statements;
  );

  cx.checked <- check;

  let exports_ = Env_js.get_var_in_scope cx "exports" reason in
  Flow_js.unit_flow cx (
    get_module_exports cx reason,
    exports_
  );
  Flow_js.unit_flow cx (
    exports_,
    exports cx m);

  let ins = (Flow_js.builtins)::(
    SSet.fold (fun r list -> (lookup_module cx r)::list) cx.required []
  ) in
  let outs = [lookup_module cx m] in
  Flow_js.do_gc cx ins outs;

  (* insist that whatever type flows into exports is fully annotated *)
  (if modes.strict then force_annotations cx);

  cx

let infer_module file =
  let ast = Parsing_service_js.get_ast_unsafe file in
  let (_, _, comments) = ast in
  let module_name = Module_js.exported_module file comments in
  infer_ast ast file module_name modes.all

(* Map.union: which is faster, union M N or union N M when M > N?
   union X Y = fold add X Y which means iterate over X, adding to Y
   So running time is roughly X * log Y.

   Now, when M > N, we have M * log N > N * log M.
   So do union N M as long as N may override M for overlapping keys.
*)

(* helper for merge_module *)
let aggregate_context_data cx =
  let master_cx = Flow_js.master_cx in
  (* PARENTS *)
  let old_mast = IMap.cardinal master_cx.parents in
  let old_mod = IMap.cardinal cx.parents in
  master_cx.parents <-
    IMap.union cx.parents master_cx.parents;
  let new_mast = IMap.cardinal master_cx.parents in
  assert (old_mast + old_mod = new_mast);
  (* CLOSURES *)
  let old_mast = IMap.cardinal master_cx.closures in
  let old_mod = IMap.cardinal cx.closures in
  master_cx.closures <-
    IMap.union cx.closures master_cx.closures;
  let new_mast = IMap.cardinal master_cx.closures in
  assert (old_mast + old_mod = new_mast);
  (* PROPERTY MAPS *)
  let old_mast = IMap.cardinal master_cx.property_maps in
  let old_mod = IMap.cardinal cx.property_maps in
  master_cx.property_maps <- IMap.union
    cx.property_maps master_cx.property_maps;
  let new_mast = IMap.cardinal master_cx.property_maps in
  assert (old_mast + old_mod = new_mast);
  (* add globals to master cx *)
  master_cx.globals <-
    SSet.union cx.globals master_cx.globals;
  ()

(* update master graph with module graph *)
let update_graph cx =
  let _, builtin_id = open_tvar Flow_js.builtins in
  cx.graph |> IMap.iter (fun id module_bounds ->
    if (id = builtin_id) then (
      (* merge new bounds with old bounds for builtins *)
      let master_bounds = Flow_js.find_graph Flow_js.master_cx id in
      master_bounds.uppertvars <- IMap.union
        module_bounds.uppertvars
        master_bounds.uppertvars;
      master_bounds.upper <- TypeMap.union
        module_bounds.upper
        master_bounds.upper
    )
    else
      (* add all other tvars -> bounds *)
      Flow_js.master_cx.graph <-
        Flow_js.master_cx.graph |> IMap.add id module_bounds;
  )

type direction = Out | In

(* make sure a module typed in the given context also has a type
 * in the master context, and create a flow between the two types
 * in the direction specified *)
let link_module_types dir cx m =
  let glo = exports Flow_js.master_cx m in
  let loc = lookup_module cx m in
  let edge = match dir with Out -> (glo, loc) | In -> (loc, glo) in
  Flow_js.unit_flow Flow_js.master_cx edge

(* map an exported module type from context to master *)
let export_to_master cx m =
  link_module_types In cx m

(* map a required module type from master to context *)
let require_from_master cx m =
  link_module_types Out cx m

(* merge module context into master context *)
let merge_module cx =
  (* aggregate context data before flow calcs *)
  aggregate_context_data cx;
  (* update master graph with module graph *)
  update_graph cx;
  (* link local and global types for our export and our required's *)
  export_to_master cx cx._module;
  SSet.iter (require_from_master cx) cx.required;
  ()

(* helper for merge_module_strict *)
let copy_context_strict cx cx_other =
  (* aggregate context data *)
  cx.parents <-
    IMap.union cx_other.parents cx.parents;
  cx.closures <-
    IMap.union cx_other.closures cx.closures;
  cx.property_maps <- IMap.union
    cx_other.property_maps cx.property_maps;
  cx.globals <-
    SSet.union cx_other.globals cx.globals;
  (* update graph *)
  let _, builtin_id = open_tvar Flow_js.builtins in
  cx_other.graph |> IMap.iter (fun id module_bounds ->
    if (id <> builtin_id) then
      (* TODO: we assume that either cx.graph does not already have id, or it
         does but with the same module_bounds; this assumption may not hold when
         there are cycles? *)
      cx.graph <- cx.graph |> IMap.add id module_bounds
  )

let implicit_require_strict cx master_cx =
  let _, builtin_id = open_tvar Flow_js.builtins in
  let master_bounds = Flow_js.find_graph master_cx builtin_id in
  master_bounds.lower |> TypeMap.iter (fun t _ ->
    Flow_js.unit_flow cx (t, Flow_js.builtins)
  )

(* Connect the export of cx_from to its import in cx_to. This happens in the
   context of some arbitrary cx. *)
let explicit_require_strict cx cx_from cx_to =
  let m = cx_from._module in
  let from_t = lookup_module cx_from m in
  let to_t = lookup_module cx_to m in
  Flow_js.unit_flow cx (from_t, to_t)

let declaration_require_strict cx m cx_to =
  let to_t = lookup_module cx_to m in
  let m_name = reason_of_t to_t |> desc_of_reason in
  let reason = reason_of_string m_name in
  let from_t = Flow_js.mk_tvar cx reason in
  Flow_js.lookup_builtin cx (spf "$module__%s" m_name) reason None from_t;
  Flow_js.unit_flow cx (from_t, to_t)

(* Merge context of module with contexts of its implicit requires and explicit
   requires. The implicit requires are those defined in lib.js (master_cx). For
   the explicit requires, we need to merge the entire dependency graph: this
   includes "nodes" (cxs) and edges (links). Intuitively, the operation we
   really need is "substitution" of known types for unknown type variables. This
   operation is simulated by the more general procedure of copying and linking
   graphs. *)
let merge_module_strict cx cxs links declarations master_cx =
  Flow_js.Cache.clear();

  copy_context_strict cx master_cx;
  implicit_require_strict cx master_cx;

  cxs |> List.iter (fun cx_ ->
    copy_context_strict cx cx_
  );
  links |> List.iter (function cx_from, cx_to ->
    explicit_require_strict cx cx_from cx_to
  );
  declarations |> List.iter (function cx_to, r ->
    declaration_require_strict cx r cx_to
  )

(* merge all modules at a dependency level, then do xmgc *)
let merge_module_list cx_list =
  List.iter merge_module cx_list;
  let (modules,requires) = cx_list |> ((SSet.empty,SSet.empty) |>
      List.fold_left (fun (modules,requires) cx ->
        (SSet.add cx._module modules,
         SSet.union cx.required requires)
      )
  ) in
  (* cross-module garbage collection *)
  cross_module_gc Flow_js.master_cx modules requires;
  ()

(* variation of infer + merge for lib definitions *)
let init file statements =
  Env_js.global_block := SMap.empty;
  Flow_js.Cache.clear();

  let cx = new_context file Files_js.lib_module in

  (* add types for pervasive builtins *)
  add_builtins cx;

  let block = ref SMap.empty in
  Env_js.env := [block];
  Flow_js.mk_frame cx [] !Env_js.env;
  Env_js.changeset := SSet.empty;

  infer_core cx statements;

  !block |> SMap.iter (fun x {specific=t;_} ->
    Flow_js.set_builtin cx x t
  );

  aggregate_context_data cx;
  update_graph cx
