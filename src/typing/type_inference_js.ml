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
    | Throw
    | Break of string option
    | Continue of string option
  exception Exn of abnormal
  val swap: abnormal -> bool -> bool
  val set: abnormal -> unit
  val raise_exn: abnormal -> 'a
  val exception_handler: (unit -> 'a) -> (abnormal -> 'a) -> 'a
  val string: abnormal -> string

end = struct

  type abnormal =
    | Return
    | Throw
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

  let string = function
    | Return -> "return"
    | Throw -> "throw"
    | Break (Some lbl) -> spf "break %s" lbl
    | Break None -> "break"
    | Continue (Some lbl) -> spf "continue %s" lbl
    | Continue None -> "continue"

end

(**** types ****)

(*************)
(* Utilities *)
(*************)

(* type exemplar set - reasons are not considered in compare *)
module TypeExSet = Set.Make(struct
  include Type
  let compare = reasonless_compare
end)

(* composition *)
let (>>) f g = fun x -> g (f (x))

let mk_object cx reason =
  Flow_js.mk_object_with_proto cx reason (MixedT reason)

let extended_object cx reason ~sealed map spread =
  let o =
    Flow_js.mk_object_with_map_proto cx reason ~sealed map (MixedT reason)
  in
  match spread with
  | None -> o
  | Some other ->
      Flow_js.mk_tvar_where cx reason (fun t ->
        Flow_js.flow cx (other, ObjAssignT (reason, o, t, [], false))
      )

let summarize cx t = match t with
  | OpenT _ ->
      let reason = reason_of_t t in
      Flow_js.mk_tvar_where cx reason (fun tvar ->
        Flow_js.flow cx (t, SummarizeT (reason, tvar))
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

let import_ns cx reason module_name loc =
  let module_ = Module_js.imported_module cx.file module_name in
  let module_type = require cx module_ module_name loc in
  Flow_js.mk_tvar_where cx reason (fun t ->
    Flow_js.flow cx (module_type, ImportModuleNsT(reason, t))
  )

let exports cx m =
  module_t cx m (Reason_js.new_reason "exports" (Pos.make_from
    (Relative_path.create Relative_path.Dummy cx.file)))

let lookup_module cx m =
  SMap.find_unsafe m cx.modulemap

(**
 * Given an exported default declaration, identify nameless declarations and
 * name them with a special internal name that can be used to reference them
 * when assigning the export value.
 *)
let nameify_default_export_decl decl = Ast.Statement.(
  match decl with
  | loc, FunctionDeclaration(func_decl) ->
    if func_decl.FunctionDeclaration.id <> None then decl else
      loc, FunctionDeclaration(FunctionDeclaration.({
        func_decl with
          id = Some(Ast.Identifier.(loc, {
            name = internal_name "*default*";
            typeAnnotation = None;
            optional = false;
          }));
      }))

  | loc, ClassDeclaration(class_decl) ->
    if class_decl.Class.id <> None then decl else
      loc, ClassDeclaration(Class.({
        class_decl with
          id = Some(Ast.Identifier.(loc, {
            name = internal_name "*default*";
            typeAnnotation = None;
            optional = false;
          }));
      }))

  | _ -> decl
)

(**
 * Given a LHS destructuring pattern, extract a list of (loc, identifier-name)
 * tuples from the pattern that represent new bindings. This is primarily useful
 * for exporting a destructuring variable declaration.
 *)
let rec extract_destructured_bindings accum pattern = Ast.Pattern.(
  match pattern with
  | Identifier(loc, n) ->
    let name = n.Ast.Identifier.name in
    (loc, name)::accum

  | Object(n) ->
    let props = n.Object.properties in
    List.fold_left extract_obj_prop_pattern_bindings accum props

  | Array(n) ->
    let elems = n.Array.elements in
    List.fold_left extract_arr_elem_pattern_bindings accum elems

  | Expression(_) ->
    failwith "Parser Error: Expression patterns don't exist in JS."
) and extract_obj_prop_pattern_bindings accum = Ast.Pattern.(function
  | Object.Property(_, prop) ->
    let (_, rhs_pattern) = prop.Object.Property.pattern in
    extract_destructured_bindings accum rhs_pattern

  | Object.SpreadProperty(_) ->
    failwith "Unsupported: Destructuring object spread properties"
) and extract_arr_elem_pattern_bindings accum = Ast.Pattern.(function
  | Some(Array.Element(_, pattern)) ->
    extract_destructured_bindings accum pattern

  | Some(Array.Spread(_, {Array.SpreadElement.argument = (_, pattern)})) ->
    extract_destructured_bindings accum pattern

  | None -> accum
)

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
            Flow_js.flow cx (t, GetElemT(reason,i,tvar));
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
                Flow_js.flow cx (t, GetT(reason,x,tvar));
                destructuring cx tvar f p
            | _ ->
              error_destructuring cx loc
          )
        | SpreadProperty (loc, { SpreadProperty.argument }) ->
            let tvar = Flow_js.mk_tvar cx reason in
            Flow_js.flow cx (t, ObjRestT(reason,!xs,tvar));
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

(* type refinements on expressions - wraps Env_js API *)
module Refinement : sig

  (* if expression is syntactically eligible for type refinement,
     return Some (access key), otherwise None.
     Eligible expressions are simple ids and chains of property lookups
     from an id base *)
  val key : Ast.Expression.t -> string option

  (* get type refinement for expression, if it exists *)
  val get : context -> Ast.Expression.t -> reason -> Type.t option

end = struct
  type expr = Id of string | Chain of string list | Other

  (* get id or chain of property names from conforming expressions *)
  let rec prop_chain = Ast.Expression.(function
  | _, This ->
      (* treat this as a property chain, in terms of refinement lifetime *)
      Chain [internal_name "this"]
  | _, Identifier (_, { Ast.Identifier.name; _ })
      when name != "undefined" ->
      (* treat super as a property chain, in terms of refinement lifetime *)
      (match name with
        | "super" -> Chain [internal_name "super"]
        | _ -> Id name
      )
  | _, Member { Member._object;
     property = Member.PropertyIdentifier (_ , { Ast.Identifier.name; _ });
     _ } -> (
      match prop_chain _object with
        | Id base -> Chain [name; base]
        | Chain names -> Chain (name :: names)
        | Other -> Other
      )
  | _ ->
      Other
  )

  let key e =
    match prop_chain e with
      | Id name -> Some name
      | Chain names -> Some (Env_js.refinement_key (List.rev names))
      | Other -> None

  let get cx e r =
    match key e with
    | Some k -> Env_js.get_refinement cx k r
    | None -> None

end

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
        let ground_t = Flow_js.printified_type cx t in
        let possible_ts = Flow_js.possible_types_of_type cx t in
        result := if is_printed_type_parsable cx ground_t
          then (Reason_js.pos_of_loc range, Some ground_t, possible_ts)
          else (Reason_js.pos_of_loc range, None, possible_ts)
      )
    )
  ) cx.type_table;
  !result

let dump_types cx =
  Flow_js.suggested_type_cache := IMap.empty;
  let lst = Hashtbl.fold (fun range t list ->
    let pos = Reason_js.pos_of_loc range in
    let ground_t = Flow_js.printified_type cx t in
    let possible_ts = Flow_js.possible_types_of_type cx t in
    let possible_reasons = possible_ts
      |> List.map Constraint_js.reason_of_t
    in
    (pos, string_of_t cx ground_t, possible_reasons)::list
  ) cx.type_table [] in
  lst |> List.sort (fun
    (a_pos, _, _) (b_pos, _, _) -> Pervasives.compare a_pos b_pos
  )

(********)
(* Fill *)
(********)

let fill_types cx =
  Flow_js.suggested_type_cache := IMap.empty;
  Hashtbl.fold (fun pos t list ->
    let line, start, end_ = Pos.info_pos pos in
    let t = Flow_js.printified_type cx t in
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
          (**
           * TODO(jeffmo): Set ~for_type:false once the export-type hacks are
           *               cleared up using proper `export type` functionality.
           *
           *               Task(6860853)
           *)
          convert_qualification (*~for_type:false*) cx "typeof-annotation" qualification
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
      mk_enum_type cx reason [value]

  (* TODO *)
  | loc, Generic { Generic.id = Generic.Identifier.Qualified (_,
         { Generic.Identifier.qualification; id; }); typeParameters } ->

    let m = convert_qualification cx "type-annotation" qualification in
    let _, { Ast.Identifier.name; _ } = id in
    let reason = mk_reason name loc in
    let t = Flow_js.mk_tvar_where cx reason (fun t ->
      Flow_js.flow cx (m, GetT (reason, name, t));
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

      (* $Shape<T> matches the shape of T *)
      | "$Shape" ->
        check_type_param_arity cx loc typeParameters 1 (fun () ->
          ShapeT (convert cx map (List.hd typeParameters))
        )

      (* $Diff<T,S> *)
      | "$Diff" ->
        check_type_param_arity cx loc typeParameters 2 (fun () ->
          let t1 = typeParameters |> List.hd |> convert cx map in
          let t2 = typeParameters |> List.tl |> List.hd |> convert cx map in
          DiffT (t1, t2)
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

      (* $Exports<'M'> is the type of the exports of module 'M' *)
      (** TODO: use `import typeof` instead when that lands **)
      | "$Exports" ->
        check_type_param_arity cx loc typeParameters 1 (fun () ->
          match List.hd typeParameters with
          | _, StringLiteral { StringLiteral.value; _ } ->
              Env_js.get_var_in_scope cx
                (internal_module_name value)
                (mk_reason (spf "exports of module %s" value) loc)
          | _ -> assert false
        )

      (* $FlowIssue is a synonym for any, used by JS devs to signal
         a potential typechecker bug to the Flow team.
         $FlowFixMe is a synonym for any, used by the Flow team to
         signal a needed mod to JS devs.
       *)
      (* TODO move these to type aliases once optional type args
         work properly in type aliases: #7007731 *)
      | "$FlowIssue" | "$FlowFixMe" ->
        (* Optional type params are info-only, validated then forgotten. *)
        List.iter (fun p -> ignore (convert cx map p)) typeParameters;
        AnyT.at loc

      (* Class<T> is the type of the class whose instances are of type T *)
      | "Class" ->
        check_type_param_arity cx loc typeParameters 1 (fun () ->
          ClassT(convert cx map (List.hd typeParameters))
        )

      | "Function" | "function" ->
        check_type_param_arity cx loc typeParameters 0 (fun () ->
          let reason = mk_reason "function type" loc in
          AnyFunT reason
        )

      | "Object" ->
        check_type_param_arity cx loc typeParameters 0 (fun () ->
          let reason = mk_reason "object type" loc in
          AnyObjT reason
        )
      (* TODO: presumably some existing uses of AnyT can benefit from AnyObjT
         as well: e.g., where AnyT is used to model prototypes and statics we
         don't care about; but then again, some of these uses may be internal,
         so while using AnyObjT may offer some sanity checking it might not
         reveal user-facing errors. *)

      (* in-scope type vars *)
      | _ when SMap.mem name map ->
        check_type_param_arity cx loc typeParameters 0 (fun () ->
          SMap.find_unsafe name map
        )

      (* other applications with id as head expr *)
      | _ ->
        let reason = mk_reason name loc in
        let params = if typeParameters = []
          then None else Some typeParameters in
        let c = identifier ~for_type:true cx name loc in
        mk_nominal_type_ cx reason map (c, params)
    )

  (* TODO: unsupported generators *)
  | loc, Function { Function.params; returnType; rest; typeParameters } ->
    let typeparams, map_ = mk_type_param_declarations cx ~map typeParameters in
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
      Object.Property.(fun (loc, { key; value; optional; _ }) ->
        (match key with
          | Ast.Expression.Object.Property.Literal
              (_, { Ast.Literal.value = Ast.Literal.String name; _ })
          | Ast.Expression.Object.Property.Identifier
              (_, { Ast.Identifier.name; _ }) ->
              let t = convert cx map value in
              if optional
              then
                (* wrap types of optional properties, just like we do for
                   optional parameters *)
                SMap.add name (OptionalT t) map_
              else
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
      | [loc, { Object.CallProperty.value = (_, ft); _; }] ->
          SMap.add "$call" (convert cx map (loc, Ast.Type.Function ft)) map_
      | fts ->
          let fts = List.map
            (fun (loc, { Object.CallProperty.value = (_, ft); _; }) ->
                convert cx map (loc, Ast.Type.Function ft))
            fts in
          let callable_reason = mk_reason "callable object type" loc in
          SMap.add "$call" (IntersectionT (callable_reason, fts)) map_
    in
    (* Seal an object type unless it specifies an indexer. *)
    let sealed, dict = Object.Indexer.(
      match indexers with
      | [(_, { id = (_, { Ast.Identifier.name; _ }); key; value; _; })] ->
          let keyt = convert cx map key in
          let valuet = convert cx map value in
          false,
          Some { Constraint_js.Type.
            dict_name = Some name;
            key = keyt;
            value = valuet
          }
      | [] ->
          true,
          None
      (* TODO *)
      | _ -> failwith "Unimplemented: multiple indexers"
    ) in
    let pmap = Flow_js.mk_propmap cx map_ in
    let proto = MixedT (reason_of_string "Object") in
    let flags = { sealed; exact = not sealed; frozen = false; } in
    ObjT (mk_reason "object type" loc,
      Flow_js.mk_objecttype ~flags dict pmap proto)

  | loc, Exists ->
    (* Do not evaluate existential type variables when map is non-empty. This
       ensures that existential type variables under a polymorphic type remain
       unevaluated until the polymorphic type is applied. *)
    let force = SMap.is_empty map in
    let reason = mk_reason "existential" loc in
    if force then Flow_js.mk_tvar cx reason
    else ExistsT reason
  )

and convert_qualification ?(for_type=true) cx reason_prefix = Ast.Type.Generic.Identifier.(function
  | Qualified (loc, { qualification; id; }) ->
    let m = convert_qualification cx reason_prefix qualification in
    let _, { Ast.Identifier.name; _ } = id in
    let reason = mk_reason (spf "%s '<<object>>.%s')" reason_prefix name) loc in
    Flow_js.mk_tvar_where cx reason (fun t ->
      Flow_js.flow cx (m, GetT (reason, name, t));
    )

  | Unqualified (id) ->
    let loc, { Ast.Identifier.name; _ } = id in
    let reason = mk_reason (spf "%s '%s'" reason_prefix name) loc in
    Env_js.get_var ~for_type cx name reason
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

and mk_enum_type cx reason keys =
  let map = List.fold_left (fun map key ->
    SMap.add key AnyT.t map
  ) SMap.empty keys in
  EnumT (reason, Flow_js.mk_object_with_map_proto cx reason map (MixedT reason))

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
      Env_js.init_env cx name (create_env_entry ~for_type:true tvar tvar (Some loc))

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

  | (loc, FunctionDeclaration { FunctionDeclaration.id; _ }) -> (
      match id with
      | Some(id) ->
        let _, { Ast.Identifier.name; _ } = id in
        let r = mk_reason (spf "function %s" name) loc in
        let tvar = Flow_js.mk_tvar cx r in
        Env_js.init_env cx name (create_env_entry tvar tvar (Some loc))
      | None -> failwith (
          "Flow Error: Nameless function declarations should always be given " ^
          "an implicit name before they get hoisted!"
        )
    )

  | (loc, DeclareVariable { DeclareVariable.id; })
  | (loc, DeclareFunction { DeclareFunction.id; }) ->
      let _, { Ast.Identifier.name; typeAnnotation; _; } = id in
      let r = mk_reason (spf "declare %s" name) loc in
      let t = mk_type_annotation cx r typeAnnotation in
      Hashtbl.replace cx.type_table loc t;
      Env_js.init_env cx name (create_env_entry t t (Some loc))

  | (loc, VariableDeclaration decl) ->
      variable_declaration cx loc decl

  | (loc, ClassDeclaration { Class.id; _ }) -> (
      match id with
      | Some(id) ->
        let _, { Ast.Identifier.name; _ } = id in
        let r = mk_reason (spf "class %s" name) loc in
        let tvar = Flow_js.mk_tvar cx r in
        Env_js.init_env cx name (create_env_entry tvar tvar (Some loc))
      | None -> ()
    )

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
      let t = Flow_js.mk_tvar cx r in
      Hashtbl.replace cx.type_table loc t;
      Env_js.init_env cx
        (internal_module_name name)
        (create_env_entry t t (Some loc))
  | (_, ExportDeclaration {
      ExportDeclaration.default;
      ExportDeclaration.declaration;
      ExportDeclaration.specifiers;
      ExportDeclaration.source;
    }) -> (
      match declaration with
      | Some(ExportDeclaration.Declaration(stmt)) ->
        let stmt = if default then nameify_default_export_decl stmt else stmt in
        statement_decl cx stmt
      | Some(ExportDeclaration.Expression(_)) -> ()
      | None -> if not default then () else failwith (
          "Parser Error: Default exports must always have an associated " ^
          "declaration or expression!"
        )
    )
  | (loc, ImportDeclaration {
      ImportDeclaration.default;
      ImportDeclaration.importKind;
      ImportDeclaration.source;
      ImportDeclaration.specifier;
    }) ->
      let (source_loc, source_literal) = source in
      let module_name = (
        match source_literal.Ast.Literal.value with
        | Ast.Literal.String(value) -> value
        | _ -> failwith  (
            "Parser error: Invalid source type! Must be a string literal."
          )
      ) in
      let (import_str, isType) = (
        match importKind with
        | ImportDeclaration.ImportType -> "import type", true
        | ImportDeclaration.ImportTypeof -> "import typeof", true
        | ImportDeclaration.ImportValue -> "import", false
      ) in
      (
        match default with
        | Some(_, local_ident) ->
          let local_name = local_ident.Ast.Identifier.name in
          let reason_str =
            (spf "%s %s from \"%s\"" import_str local_name module_name)
          in
          let reason = mk_reason reason_str loc in
          let tvar = Flow_js.mk_tvar cx reason in

          let env_entry =
            (create_env_entry ~for_type:isType tvar tvar (Some loc))
          in
          Env_js.init_env cx local_name env_entry
        | None -> (
          match specifier with
          | Some(ImportDeclaration.Named(_, named_specifiers)) ->
            let init_specifier (specifier_loc, specifier) = (
              let (loc, remote_ident) =
                specifier.ImportDeclaration.NamedSpecifier.id
              in
              let remote_name = remote_ident.Ast.Identifier.name in
              let (local_name, reason) = (
                match specifier.ImportDeclaration.NamedSpecifier.name with
                | Some(_, { Ast.Identifier.name = local_name; _; }) ->
                  let reason_str =
                    spf "%s { %s as %s }" import_str remote_name local_name
                  in
                  (local_name, (mk_reason reason_str loc))
                | None ->
                  let reason_str = spf "%s { %s }" import_str remote_name in
                  (remote_name, (mk_reason reason_str loc))
              ) in
              let tvar = Flow_js.mk_tvar cx reason in
              let env_entry =
                create_env_entry ~for_type:isType tvar tvar (Some specifier_loc)
              in
              Env_js.init_env cx local_name env_entry;
            ) in
            List.iter init_specifier named_specifiers
          | Some(ImportDeclaration.NameSpace(_, (loc, local_ident))) ->
            let local_name = local_ident.Ast.Identifier.name in
            let reason =
              mk_reason (spf "%s * as %s" import_str local_name) loc
            in
            let tvar = Flow_js.mk_tvar cx reason in
            let env_entry =
              create_env_entry ~for_type:isType tvar tvar (Some loc)
            in
            Env_js.init_env cx local_name env_entry
          | None -> failwith (
            "Parser error: Non-default imports must always have a " ^
            "specifier!"
          )
        )
      )
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
      incr n (* n is bumped whenever stmt doesn't exit abnormally *)
    )
  )
    (fun exn ->
      (* !n is the index of the statement that exits abnormally, so !n+1 is the
         index of possibly unreachable code. *)
      let uc = !n+1 in
      if uc < List.length stmts
      then (
        let msg = "unreachable code" in
        let (loc, _) = List.nth stmts uc in
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

      (* adjust post-if environment. if we've returned from one arm,
         swap in the env generated by the other, otherwise merge *)
      (match !exception_then, !exception_else with
      | Some Abnormal.Return, None
      | Some Abnormal.Throw, None ->
          Env_js.update_frame cx else_ctx
      | None, Some Abnormal.Return
      | None, Some Abnormal.Throw ->
          Env_js.update_frame cx then_ctx
      | _ ->
          Env_js.merge_env cx reason (ctx, then_ctx, else_ctx) newset;
          Env_js.update_frame cx ctx);

      (match !exception_then, !exception_else with
      | Some Abnormal.Throw, Some Abnormal.Return
      | Some Abnormal.Return, Some Abnormal.Throw ->
          raise_exception (Some Abnormal.Return);
      | _ ->
          raise_and_exception !exception_then !exception_else);


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
      let typeparams, type_params_map =
        mk_type_param_declarations cx typeParameters in
      let t = convert cx type_params_map right in
      let type_ =
        if typeparams = []
        then TypeT (r, t)
        else PolyT(typeparams, TypeT (r, t))
      in
      Hashtbl.replace cx.type_table loc type_;
      Env_js.set_var ~for_type:true cx name type_ r

  | (loc, Switch { Switch.discriminant; cases; lexical }) ->

      ignore (expression cx discriminant);
      let save_break_exn = Abnormal.swap (Abnormal.Break None) false in

      let default = ref false in
      let ctx = !Env_js.env in
      let last_ctx = ref None in
      let fallthrough_ctx = ref ctx in
      let oldset = Env_js.swap_changeset (fun _ -> SSet.empty) in

      let merge_with_last_ctx cx reason ctx changeset =
        match !last_ctx with
        | Some x -> Env_js.merge_env cx reason (ctx, ctx, x) changeset
        | None -> ()
      in

      let exceptions = List.rev_map (fun (loc, {Switch.Case.test; consequent}) ->
        if !default then None (* TODO: error when case follows default *)
        else (
          let reason = mk_reason "case" loc in
          let _, pred, not_pred, xtypes = match test with
          | None ->
              default := true;
              UndefT.t, SMap.empty, SMap.empty, SMap.empty
          | Some expr ->
              let fake_ast = loc, Ast.Expression.(Binary {
                Binary.operator = Binary.StrictEqual;
                left = discriminant;
                right = expr;
              }) in
              predicate_of_condition cx fake_ast
          in

          let case_ctx = Env_js.clone_env !fallthrough_ctx in
          Env_js.update_frame cx case_ctx;
          Env_js.refine_with_pred cx reason pred xtypes;
          merge_with_last_ctx cx reason case_ctx !Env_js.changeset;

          let exception_ = ref None in
          mark_exception_handler (fun () ->
            toplevels cx consequent
          ) exception_;

          Env_js.update_frame cx !fallthrough_ctx;
          Env_js.refine_with_pred cx reason not_pred xtypes;

          last_ctx := Some case_ctx;
          !exception_
        )
      ) cases
      in

      let newset = Env_js.swap_changeset (SSet.union oldset) in

      (* if there's no default clause, pretend there is and it's empty *)
      if not !default
      then (
        let default_ctx = Env_js.clone_env !fallthrough_ctx in
        let reason = mk_reason "default" loc in
        Env_js.update_frame cx default_ctx;
        merge_with_last_ctx cx reason default_ctx newset
      );

      if Abnormal.swap (Abnormal.Break None) save_break_exn
      then Env_js.havoc_env2 newset;

      (* if the default clause exits abnormally and other cases either fall
         through or exit abnormally the same way, then the switch exits
         abnormally that way *)
      if !default
      then (
        match List.hd exceptions with
        | Some (Abnormal.Break None) | None -> ()
        | Some exn ->
            if List.tl exceptions |> List.for_all (function
              | None
              | Some Abnormal.Throw
              | Some Abnormal.Return -> true
              | _ -> false
            )
            then Abnormal.raise_exn exn
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
      Flow_js.flow cx (t, ret);
      Env_js.clear_env reason;
      Abnormal.set Abnormal.Return

  | (loc, Throw { Throw.argument }) ->
      let reason = mk_reason "throw" loc in
      ignore (expression cx argument);
      Env_js.clear_env reason;
      Abnormal.set Abnormal.Throw

  (***************************************************************************)
  (* Try-catch-finally statements have a lot of control flow possibilities. (To
     simplify matters, a missing catch block is considered to to be block that
     throws, and a missing finally block is considered to be an empty block.)

     A try block may either

     * exit normally: in this case, it proceeds to the finally block.

     * exit abnormally: in this case, it proceeds to the catch block.

     A catch block may either:

     * exit normally: in this case, it proceeds to the finally block.

     * exit abnormally: in this case, it proceeds to the finally block and
     throws at the end of the finally block.

     A finally block may either:

     * exit normally: in this case, the try-catch-finally statement exits
     normally. (Note that to be in this case, either the try block exited
     normally, or it didn't and the catch block exited normally.)

     * exit abnormally: in this case, the try-catch-finally statement exits
     abnormally.

     Based on these possibilities, approximations for the local state at various
     points in a try-catch-finally statement can be derived.

     * The start of a catch block is reachable via anywhere in the try
     block. Thus, the local state must be conservative.

     * The start of a finally block is reachable via the end of the try block,
     or anywhere in the catch block. Thus, the local state must be conservative.

     * The end of a try-catch-finally statement is reachable via the end of the
     finally block. However, in this case we can assume that either

     ** the try block exited normally, in which case the local state at the
     start of the finally block is the same as the local state at the end of the
     try block.

     ** the catch block exited normally, in which case the local state at the
     start of the finally block is the same as the local state at the end of
     the catch block.

     Thus, a finally block should be analyzed twice, with each of the following
     assumptions for the local state at its start: (1) conservative (to model
     abnormal exits in the try or catch blocks); (2) whatever is at the end of
     the try block merged with whatever is at the end of the catch block (for
     normal exits in the try and catch blocks). *)
  (***************************************************************************)
  | (loc, Try { Try.block = (_, b); handler; guardedHandlers; finalizer }) ->
      let reason = mk_reason "try" loc in
      let oldset = Env_js.swap_changeset (fun _ -> SSet.empty) in
      let exception_try = ref None in
      let exception_catch = ref None in
      let exception_finally = ref None in

      mark_exception_handler
        (fun () -> toplevels cx b.Block.body)
        exception_try;

      (* clone end of try as start of finally *)
      let finally_ctx = Env_js.clone_env !Env_js.env in

      (match handler with
        | None ->
            (* a missing catch means that if try throws, the end of
               the try-catch-finally is unreachable *)
            exception_catch := Some Abnormal.Throw;

        | Some (_, h) ->
            (* havoc environment, since the try block may exit anywhere *)
            Env_js.havoc_env2 !Env_js.changeset;
            mark_exception_handler
              (fun () -> catch_clause cx h)
              exception_catch;

            (* merge end of catch to start of finally *)
            Env_js.merge_env cx reason
              (finally_ctx, finally_ctx, !Env_js.env) !Env_js.changeset;
      );

      assert (guardedHandlers = []); (* remove from AST *)

      (match finalizer with
        | None ->
            (* update environment to the end of try or catch; otherwise do
               nothing, so that this remains the state reached at the end of the
               try-catch-finally *)
            Env_js.update_frame cx finally_ctx

        | Some (_, { Block.body }) ->
            (* analyze twice, with different start states *)

            (* 1. havoc environment, since the catch block may exit anywhere *)
            Env_js.havoc_env2 !Env_js.changeset;
            mark_exception_handler
              (fun () -> toplevels cx body)
              exception_finally;

            (* 2. update environment to the end of try or catch *)
            Env_js.update_frame cx finally_ctx;
            ignore_exception_handler
              (fun () -> toplevels cx body);
      );

      let newset = Env_js.swap_changeset (SSet.union oldset) in
      ignore newset;

      raise_exception !exception_finally;

      (match !exception_try, !exception_catch with
      | Some Abnormal.Throw, Some Abnormal.Throw
      | Some Abnormal.Return, Some _ ->
          raise_exception !exception_try
      | Some Abnormal.Throw, Some Abnormal.Return ->
          raise_exception !exception_catch
      | _ -> ())


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

      ignore_exception_handler (fun () -> statement cx body);

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
      then Env_js.havoc_env2 newset

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
      Flow_js.flow cx (t, MaybeT o); (* null/undefined are allowed *)

      let ctx = !Env_js.env in
      let oldset = Env_js.swap_changeset (fun _ -> SSet.empty) in
      Env_js.widen_env cx reason;

      let body_ctx = Env_js.clone_env ctx in
      Env_js.update_frame cx body_ctx;

      let _, pred, _, xtypes = predicate_of_condition cx right in
      Env_js.refine_with_pred cx reason pred xtypes;

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
      FunctionDeclaration.id;
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
      (match id with
      | Some(_, {Ast.Identifier.name; _ }) ->
        Env_js.set_var cx name fn_type reason
      | None -> ())

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
      let (nloc, name) = (
        match id with
        | Some(nloc, { Ast.Identifier.name; _ }) -> nloc, name
        | None -> loc, "<<anonymous class>>"
      ) in
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
    }) as stmt ->
    let structural = match stmt with
    | (_, InterfaceDeclaration _) -> true
    | _ -> false in

    let _, { Ast.Identifier.name = iname; _ } = id in
    let reason = mk_reason iname loc in
    let typeparams, map = mk_type_param_declarations cx typeParameters in
    let sfmap, smmap, fmap, mmap = List.fold_left Ast.Type.Object.Property.(
      fun (sfmap_, smmap_, fmap_, mmap_)
        (loc, { key; value; static; _method; _ }) -> (* TODO: optional *)
        Ast.Expression.Object.Property.(match key with
        | Literal (loc, _)
        | Computed (loc, _) ->
            let msg = "illegal name" in
            Flow_js.add_error cx [Reason_js.mk_reason "" loc, msg];
            (sfmap_, smmap_, fmap_, mmap_)

        | Identifier (loc, { Ast.Identifier.name; _ }) ->
            let t = convert cx map value in
            (* check for overloads in static and instance method maps *)
            let map_ = if static then smmap_ else mmap_ in
            let t = match SMap.get name map_ with
              | None -> t
              | Some (IntersectionT (reason, ts)) ->
                  IntersectionT (reason, ts @ [t])
              | Some t0 ->
                  IntersectionT (Reason_js.mk_reason iname loc, [t; t0])
            in
            (* TODO: additionally check that the four maps are disjoint *)
            (match static, _method with
            | true, true ->  (sfmap_, SMap.add name t smmap_, fmap_, mmap_)
            | true, false -> (SMap.add name t sfmap_, smmap_, fmap_, mmap_)
            | false, true -> (sfmap_, smmap_, fmap_, SMap.add name t mmap_)
            | false, false -> (sfmap_, smmap_, SMap.add name t fmap_, mmap_)
            )
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
    let i = mk_interface cx reason typeparams map
      (sfmap, smmap, fmap, mmap) extends structural in
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
    let t = Env_js.get_var_in_scope cx (internal_module_name name) reason in
    let scope = ref SMap.empty in
    Env_js.push_env cx scope;

    List.iter (statement_decl cx) elements;
    toplevels cx elements;

    Env_js.pop_env();

    let exports = match SMap.get "exports" !scope with
      | Some {specific=exports;_} ->
          (* TODO: what happens when other things are also declared? *)
          exports
      | None ->
          let map = SMap.map (fun {specific=export;_} -> export) !scope in
          Flow_js.mk_object_with_map_proto cx reason map (MixedT reason)
    in
    Flow_js.unify cx (CJSExportDefaultT(reason, exports)) t
  | (loc, ExportDeclaration {
      ExportDeclaration.default;
      ExportDeclaration.declaration;
      ExportDeclaration.specifiers;
      ExportDeclaration.source;
    }) -> (
      let extract_export_info_from_decl = (function
        | loc, FunctionDeclaration({FunctionDeclaration.id=None; _;}) ->
          if default then
            [("function() {}", loc, internal_name "*default*")]
          else failwith (
            "Parser Error: Immediate exports of nameless functions can " ^
            "only exist for default exports!"
          )
        | loc, FunctionDeclaration({FunctionDeclaration.id=Some(_, id); _;}) ->
          let name = id.Ast.Identifier.name in
          [(spf "function %s() {}" name, loc, name)]
        | loc, ClassDeclaration({Class.id=None; _;}) ->
          if default then
            [("class {}", loc, internal_name "*default*")]
          else failwith (
            "Parser Error: Immediate exports of nameless classes can " ^
            "only exist for default exports"
          )
        | loc, ClassDeclaration({Class.id=Some(_, id); _;}) ->
          let name = id.Ast.Identifier.name in
          [(spf "class %s() {}" name, loc, name)]
        | _, VariableDeclaration({VariableDeclaration.declarations; _; }) ->
          let decl_to_bindings accum (loc, decl) =
            let id = snd decl.VariableDeclaration.Declarator.id in
            List.rev (extract_destructured_bindings accum id)
          in
          let bound_names = List.fold_left decl_to_bindings [] declarations in
          bound_names |> List.map (fun (loc, name) ->
            (spf "var %s" name, loc, name)
          )
        | _, TypeAlias({TypeAlias.id=(_, id); _;}) ->
          let name = id.Ast.Identifier.name in
          [(spf "type %s = ..." name, loc, name)]
        | _ -> failwith "Parser Error: Invalid export-declaration type!"
      ) in

      let export_reason_start =
        if default then "export default" else "export"
      in

      match declaration with
      | Some(ExportDeclaration.Declaration(decl)) ->
        let decl = if default then nameify_default_export_decl decl else decl in
        statement cx decl;

        let export_from_local (export_reason, loc, local_name) = (
          let reason =
            mk_reason (spf "%s %s" export_reason_start export_reason) loc
          in
          let for_type = match decl with _, TypeAlias _ -> true | _ -> false in
          let local_tvar = Env_js.var_ref ~for_type cx local_name reason in
          let exports_obj = get_module_exports cx reason in
          let relation =
            if default then
              (exports_obj, ExportDefaultT(reason, local_tvar))
            else
              (exports_obj, SetT(reason, local_name, local_tvar))
          in
          Flow_js.flow cx relation
        ) in

        (**
         * Export each declared binding. Some declarations export multiple
         * bindings, like a multi-declarator variable declaration.
         *)
        List.iter export_from_local (extract_export_info_from_decl decl)
      | Some(ExportDeclaration.Expression(expr)) ->
        if default then (
          let expr_t = expression cx expr in
          let reason =
            mk_reason (spf "%s <<expression>>" export_reason_start) loc
          in
          let exports_obj = get_module_exports cx reason in
          let relation = (exports_obj, ExportDefaultT(reason, expr_t)) in
          Flow_js.flow cx relation
        ) else failwith (
          "Parser Error: Exporting an expression is only possible for " ^
          "`export default`!"
        )
      | None ->
        if default then failwith (
          "Parser Error: Default exports must always have an associated " ^
          "declaration or expression!"
        ) else (
          match specifiers with
          (* export {foo, bar} *)
          | Some(ExportDeclaration.ExportSpecifiers(specifiers)) ->
            let export_specifier specifier = ExportDeclaration.Specifier.(
              let (reason, local_name, remote_name) = (
                match specifier with
                | loc, {id = (_, {Ast.Identifier.name=id; _;}); name=None;} ->
                  let reason = mk_reason (spf "export {%s}" id) loc in
                  (reason, id, id)
                | loc, {id=(_, {Ast.Identifier.name=id; _;});
                        name=Some(_, {Ast.Identifier.name; _;})} ->
                  let reason =
                    mk_reason (spf "export {%s as %s}" id name) loc
                  in
                  (reason, id, name)
              ) in

              (**
               * Determine if we're dealing with the `export {} from` form
               * (and if so, retrieve the ModuleNamespaceObject tvar for the
               *  source module)
               *)
              let source_module_tvar = (
                match source with
                | Some(src_loc, {
                    Ast.Literal.value = Ast.Literal.String(module_name);
                    _;
                  }) ->
                    let reason =
                      mk_reason "ModuleNamespace for export {} from" src_loc
                    in
                    Some(import_ns cx reason module_name src_loc)
                | Some(_) -> failwith (
                    "Parser Error: `export ... from` must specify a string " ^
                    "literal for the source module name!"
                  )
                | None -> None
              ) in

              let local_tvar = (
                match source_module_tvar with
                | Some(tvar) ->
                  Flow_js.mk_tvar_where cx reason (fun t ->
                    Flow_js.flow cx (tvar, GetT(reason, local_name, t))
                  )
                | None ->
                  Env_js.var_ref cx local_name reason
              ) in

              Flow_js.flow cx (
                get_module_exports cx reason,
                SetT(reason, remote_name, local_tvar)
              )
            ) in
            List.iter export_specifier specifiers

          (* export * from "Source"; *)
          | Some(ExportDeclaration.ExportBatchSpecifier(_)) ->
              let source_tvar = (
                match source with
                | Some(src_loc, {
                    Ast.Literal.value = Ast.Literal.String(module_name);
                    _;
                  }) ->
                    let reason_str =
                      spf "ModuleNamespaceObject for export * from \"%s\""
                        module_name
                    in
                    let reason = mk_reason reason_str src_loc in
                    import_ns cx reason module_name src_loc
                | Some(_)
                | None
                  -> failwith (
                    "Parser Error: `export * from` must specify a string " ^
                    "literal for the source module name!"
                  )
              ) in

              let reason = mk_reason "export * from \"%s\"" loc in
              set_module_exports cx reason source_tvar

          | None -> failwith (
              "Parser Error: Non-default export must either have associated " ^
              "declarations or a list of specifiers!"
            )
        )
    )
  | (loc, ImportDeclaration({
      ImportDeclaration.default;
      ImportDeclaration.importKind;
      ImportDeclaration.source;
      ImportDeclaration.specifier;
    })) ->
      let (source_loc, source_literal) = source in
      let module_name = (
        match source_literal.Ast.Literal.value with
        | Ast.Literal.String(value) -> value
        | _ -> failwith  (
            "Parser error: Invalid source type! Must be a string literal."
          )
      ) in
      let (import_str, isType) = (
        match importKind with
        | ImportDeclaration.ImportType -> "import type", true
        | ImportDeclaration.ImportTypeof -> "import typeof", true
        | ImportDeclaration.ImportValue -> "import", false
      ) in

      let import_reason = mk_reason
        (spf "exports of \"%s\"" module_name)
        source_loc
      in

      let module_ns_tvar = import_ns cx import_reason module_name source_loc in
      (match default with
        | Some(local_ident_loc, local_ident) ->
          let local_name = local_ident.Ast.Identifier.name in

          let reason = mk_reason
            (spf "\"default\" export of \"%s\"" module_name)
            local_ident_loc
          in
          let local_t = Flow_js.mk_tvar_where cx reason (fun t ->
            let t = if not isType then t else ImportTypeT(reason, t) in
            Flow_js.flow cx (module_ns_tvar, GetT(reason, "default", t));
          ) in

          let reason = mk_reason
            (spf "%s %s from \"%s\"" import_str local_name module_name)
            loc
          in
          let local_t_generic =
            Env_js.get_var_in_scope ~for_type:isType cx local_name reason
          in
          Flow_js.unify cx local_t local_t_generic;
        | None -> (
          match specifier with
          | Some(ImportDeclaration.Named(_, named_specifiers)) ->
            let import_specifier (specifier_loc, specifier) = (
              let (remote_ident_loc, remote_ident) =
                specifier.ImportDeclaration.NamedSpecifier.id
              in
              let remote_name = remote_ident.Ast.Identifier.name in

              let get_reason_str =
                spf "\"%s\" export of \"%s\"" remote_name module_name
              in

              let (local_name, get_reason, set_reason) = (
                match specifier.ImportDeclaration.NamedSpecifier.name with
                | Some(local_ident_loc, { Ast.Identifier.name = local_name; _; }) ->
                  let get_reason = mk_reason get_reason_str remote_ident_loc in
                  let set_reason = mk_reason
                    (spf "%s { %s as %s }" import_str remote_name local_name)
                    specifier_loc
                  in
                  (local_name, get_reason, set_reason)
                | None ->
                  let get_reason = mk_reason get_reason_str specifier_loc in
                  let set_reason = mk_reason
                    (spf "%s { %s }" import_str remote_name)
                    specifier_loc
                  in
                  (remote_name, get_reason, set_reason)
              ) in
              let local_t = Flow_js.mk_tvar_where cx get_reason (fun t ->
                let t = if not isType then t else ImportTypeT(set_reason, t) in
                Flow_js.flow cx (module_ns_tvar, GetT(get_reason, remote_name, t))
              ) in

              let local_t_generic =
                Env_js.get_var_in_scope ~for_type:isType cx local_name get_reason
              in
              Flow_js.unify cx local_t local_t_generic;
            ) in
            List.iter import_specifier named_specifiers
          | Some(ImportDeclaration.NameSpace(_, (ident_loc, local_ident))) ->
            let local_name = local_ident.Ast.Identifier.name in
            let reason = mk_reason
              (spf "%s * as %s" import_str local_name)
              ident_loc
            in
            if isType then (
              (**
               * TODO: `import type * as` really doesn't make much sense with
               *        our current CommonJS interop table. Here we support
               *        this in the same way we treat import-default as a
               *        temporary means of transitioning our old interop table
               *        to the new one. Once the transition is finished, we
               *        should make `import type * as` a hard error.
               *)
              let module_ = Module_js.imported_module cx.file module_name in
              let module_type = require cx module_ module_name source_loc in
              Env_js.set_var ~for_type:true cx local_name module_type reason
            ) else (
              Env_js.set_var cx local_name module_ns_tvar reason
            )
          | None ->
            failwith (
              "Parser error: Non-default imports must always have a " ^
              "specifier!"
            )
        )
      )
)

and save_handler mark exn = mark := Some exn

and mark_exception_handler main exception_ =
  Abnormal.exception_handler main
    (save_handler exception_)

and ignore_exception_handler main =
  Abnormal.exception_handler main (fun exn -> ())

and has_return_exception_handler main =
  let has_return = ref false in
  Abnormal.exception_handler main (function
    | Abnormal.Return
    | Abnormal.Throw -> has_return := true; ()
    | exn -> Abnormal.raise_exn exn
  );
  !has_return

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
  let spread = ref [] in
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
      spread := (expression cx argument)::!spread;
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
              Flow_js.flow cx (t, t_);
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
  Flow_js.flow cx (arr, ArrT (reason, tvar, []));
  RestT tvar

and expression cx (loc, e) =
  let t = expression_ cx loc e in
  Hashtbl.replace cx.type_table loc t;
  t

and this_ cx r = Ast.Expression.(
  match Refinement.get cx (loc_of_reason r, This) r with
  | Some t -> t
  | None -> Env_js.get_var cx (internal_name "this") r
)

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

and identifier ?(for_type=false) cx name loc =
  if Type_inference_hooks_js.dispatch_id_hook cx name loc
  then AnyT.at loc
  else (
    if name = "undefined"
    then void_ loc
    else (
      let reason = mk_reason (spf "identifier %s" name) loc in
      let t = Env_js.var_ref ~for_type cx name reason in
      t
    )
  )

and expression_ cx loc e = Ast.Expression.(match e with

  | Literal lit ->
      literal cx loc lit

  | Identifier (_, { Ast.Identifier.name; _ }) -> identifier cx name loc

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

  | TypeCast {
        TypeCast.expression = eloc, expr;
        typeAnnotation } ->
      let r = mk_reason "typecast" loc in
      let t = mk_type_annotation cx r (Some typeAnnotation) in
      Hashtbl.replace cx.type_table loc t;
      let infer_t = expression_ cx eloc expr in
      Flow_js.flow cx (infer_t, t);
      t

  | Member {
      Member._object;
      property = Member.PropertyExpression index;
      _
    } ->
      let reason = mk_reason "access of computed property/element" loc in
      let tobj = expression cx _object in
      let tind = expression cx index in
      Flow_js.mk_tvar_where cx reason (fun t ->
        Flow_js.flow cx (tobj, GetElemT(reason, tind, t))
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
        { Ast.Identifier.name = "ReactGraphQL" | "ReactGraphQLLegacy"; _ });
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
      (match Refinement.get cx (loc, e) reason with
      | Some t -> t
      | None ->
        let super = super_ cx reason in
        if Type_inference_hooks_js.dispatch_member_hook cx name ploc super
        then AnyT.at ploc
        else (
          Flow_js.mk_tvar_where cx reason (fun tvar ->
            Flow_js.flow cx (super, GetT(reason, name, tvar)))
        )
      )

  | Member {
      Member._object;
      property = Member.PropertyIdentifier (ploc, { Ast.Identifier.name; _ });
      _
    } -> (
      let reason = mk_reason (spf "property %s" name) loc in
      match Refinement.get cx (loc, e) reason with
      | Some t -> t
      | None ->
        let tobj = expression cx _object in
        if Type_inference_hooks_js.dispatch_member_hook cx name ploc tobj
        then AnyT.at ploc
        else (
          Flow_js.mk_tvar_where cx reason (fun t ->
            Flow_js.flow cx (tobj, GetT (reason, name, t)))
        )
      )

  | Object { Object.properties } ->
    let reason = mk_reason "object literal" loc in
    let map, spread = object_ cx properties in
    let sealed = (spread = [] && not (SMap.is_empty map)) in
    extended_object cx reason ~sealed map spread

  | Array { Array.elements } -> (
    let reason = mk_reason "array literal" loc in
    let element_reason = mk_reason "array element" loc in
    match elements with
    | [] ->
        (* empty array, analogous to object with implicit properties *)
        let elemt = Flow_js.mk_tvar cx element_reason in
        ArrT (prefix_reason "empty " reason, elemt, [])
    | elems ->
        (* tup is true if no spreads *)
        (* tset is set of distinct (mod reason) elem types *)
        (* tlist is reverse list of element types if tup, else [] *)
        let tup, tset, tlist = List.fold_left (fun (tup, tset, tlist) elem ->
          let tup, elemt = match elem with
          | Some (Expression e) ->
              tup, expression cx e
          | Some (Spread (_, { SpreadElement.argument })) ->
              false, spread cx argument
          | None ->
              tup, UndefT.at loc
          in
          let elemt = if tup then elemt else summarize cx elemt in
          tup,
          TypeExSet.add elemt tset,
          if tup then elemt :: tlist else []
        ) (true, TypeExSet.empty, []) elems
        in
        (* composite elem type is union *)
        let elemt = match TypeExSet.elements tset with
        | [t] -> t
        | list -> UnionT (element_reason, list)
        in
        ArrT (reason, elemt, List.rev tlist)
    )

  | Call {
      Call.callee = _, Identifier (_, {
        Ast.Identifier.name = "require";
        _
      });
      arguments
    } -> (
      match arguments with
      | [ Expression (_, Literal {
          Ast.Literal.value = Ast.Literal.String module_name; _;
        }) ] ->
        let m = Module_js.imported_module cx.file module_name in
        require cx m module_name loc
      | _ ->
        (*
        let msg = "require(...) supported only on strings" in
        Flow_js.add_error cx [mk_reason "" loc, msg];
        *)
        AnyT.at loc
    )

  | Call {
      Call.callee = _, Identifier (_, {
        Ast.Identifier.name = "requireLazy";
        _
      });
      arguments
    } -> (
      match arguments with
      | [Expression(_, Array({Array.elements;})); Expression(callback_expr);] ->
        (**
         * From a static perspective (and as long as side-effects aren't
         * considered in Flow), a requireLazy call can be viewed as an immediate
         * call to require() for each of the modules, and then an immediate call
         * to the requireLazy() callback with the results of each of the prior
         * calls to require().
         *
         * TODO: requireLazy() is FB-specific. Let's find a way to either
         *       generalize or toggle this only for the FB environment.
         *)

        let element_to_module_tvar tvars = (function
          | Some(Expression(_, Literal({
              Ast.Literal.value = Ast.Literal.String module_name;
              _;
            }))) ->
              let m = Module_js.imported_module cx.file module_name in
              let module_tvar = require cx m module_name loc in
              module_tvar::tvars
          | _ ->
              let msg =
                "The first arg to requireLazy() must be a literal array of " ^
                "string literals!"
              in
              Flow_js.add_error cx [mk_reason "" loc, msg];
              tvars
        ) in
        let module_tvars = List.fold_left element_to_module_tvar [] elements in
        let module_tvars = List.rev module_tvars in

        let callback_expr_t = expression cx callback_expr in
        let reason = mk_reason "requireLazy() callback" loc in
        let _ = func_call cx reason callback_expr_t module_tvars in

        let null = null_ loc in
        let reason = reason_of_t null in
        UnionT(reason, [null; Flow_js.mk_tvar cx reason])

      | _ ->
        let msg =
          "The first arg to requireLazy() must be a literal array of " ^
          "string literals!"
        in
        Flow_js.add_error cx [mk_reason "" loc, msg];

        AnyT.at loc
    )

  | New {
      New.callee = _, Identifier (_, { Ast.Identifier.name = "Function"; _ });
      arguments
    } -> (
      let argts = List.map (expression_or_spread cx) arguments in
      List.iter (fun t ->
        Flow_js.flow cx (t, StrT.at loc)
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
        Flow_js.flow cx
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
          { Ast.Identifier.name = "createClass"; _ });
        _
      };
      arguments = [ Expression (_, Object { Object.properties = class_props }) ]
    } ->
      react_create_class cx loc class_props

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
        Flow_js.flow cx (super,
          MethodT (reason, name, super, argts, t, 0));
      )

  | Call {
      Call.callee = (_, Member {
        Member._object;
        property = Member.PropertyIdentifier (ploc,
          { Ast.Identifier.name; _ });
        _
      }) as callee;
      arguments
    } ->
      (* method call *)
      let argts = List.map (expression_or_spread cx) arguments in
      let reason = mk_reason (spf "call of method %s" name) loc in
      let ot = expression cx _object in
      Type_inference_hooks_js.dispatch_call_hook cx name ploc ot;
      (match Refinement.get cx callee reason with
      | Some f ->
          (* note: the current state of affairs is that we understand
             member expressions as having refined types, rather than
             understanding receiver objects as carrying refined properties.
             generalizing this properly is a todo, and will deliver goodness.
             meanwhile, here we must hijack the property selection normally
             performed by the flow algorithm itself. *)
          Env_js.havoc_heap_refinements ();
          Flow_js.mk_tvar_where cx reason (fun t ->
            let frame = Env_js.peek_frame () in
            let app = Flow_js.mk_methodtype2 ot argts None t frame in
            Flow_js.flow cx (f, CallT (reason, app));
          )
      | None ->
          Env_js.havoc_heap_refinements ();
          Flow_js.mk_tvar_where cx reason (fun t ->
            Flow_js.flow cx
              (ot, MethodT(reason, name, ot, argts, t, Env_js.peek_frame ()))
          )
      )

  | Call {
      Call.callee = _, Identifier (_, { Ast.Identifier.name = "super"; _ });
      arguments
    } ->
      let argts = List.map (expression_or_spread cx) arguments in
      let reason = mk_reason "super(...)" loc in
      let super = super_ cx reason in
      let t = VoidT reason in
      Flow_js.flow cx
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
      | (Expression (_, Literal {
          Ast.Literal.value = Ast.Literal.Boolean false; _;
        }))::arguments ->
        (* invariant(false, ...) is treated like a throw *)
        ignore (List.map (expression_or_spread cx) arguments);
        Env_js.clear_env reason;
        Abnormal.set Abnormal.Throw
      | (Expression cond)::arguments ->
        ignore (List.map (expression_or_spread cx) arguments);
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
      (* TODO: This needs to be fixed. *)
      let argts = List.map (expression_or_spread cx) arguments in
      Flow_js.mk_tvar_where cx (mk_reason "class" loc) (fun t ->
        List.iter (fun argt -> Flow_js.flow cx (argt, t)) argts
      )

  | Call { Call.callee; arguments } ->
      let f = expression cx callee in
      let reason = mk_reason "function call" loc in
      let argts = List.map (expression_or_spread cx) arguments in
      func_call cx reason f argts

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
        Flow_js.flow cx (t1, t);
        Flow_js.flow cx (t2, t);
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
      returnType;
      typeParameters;
      _
    } ->
      let reason = mk_reason "arrow function" loc in
      let this = this_ cx reason in
      mk_function id cx reason
        typeParameters (params, defaults, rest) returnType body this

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
      Flow_js.flow cx (t, CallT (reason, ft));
      StrT.at loc

  | TemplateLiteral {
      TemplateLiteral.quasis;
      expressions
    } ->
      List.iter (fun e -> ignore (expression cx e)) expressions;
      StrT.at loc

  | JSXElement e ->
      jsx cx e

  | Class { Class.id; body; superClass;
      typeParameters; superTypeParameters; implements } ->
      if implements <> [] then
        let msg = "implements not supported" in
        Flow_js.add_error cx [mk_reason "" loc, msg]
      else ();
      let (nloc, name) = (
        match id with
        | Some(nloc, { Ast.Identifier.name; _ }) -> nloc, name
        | None -> loc, "<<anonymous class>>"
      ) in
      let reason = mk_reason name nloc in
      let extends = superClass, superTypeParameters in
      mk_class cx reason typeParameters extends body

  (* TODO *)
  | Yield _
  | Comprehension _
  | Generator _
  | Let _ ->
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
    Flow_js.flow cx (class_, ConstructorT (reason, argts, t));
  )

and func_call cx reason func_t argts =
  Env_js.havoc_heap_refinements ();
  Flow_js.mk_tvar_where cx reason (fun t ->
    let app =
      Flow_js.mk_functiontype2 argts None t (Env_js.peek_frame ())
    in
    Flow_js.flow cx (func_t, CallT(reason, app))
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
      BoolT (mk_reason "boolean" loc, Some b)

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
      ignore (expression cx argument);
      BoolT.at loc

  | { operator = Plus; argument; _ } ->
      ignore (expression cx argument);
      AnyT.at loc

  | { operator = Minus; argument; _ }
  | { operator = BitNot; argument; _ } ->
      let t = NumT.at loc in
      Flow_js.flow cx (expression cx argument, t);
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
      Flow_js.flow cx (expression cx argument, t);
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
      Flow_js.flow cx (arr, ArrT (reason_array_arg,tvar,[]));
      RestT(tvar)
*)


and binary cx loc = Ast.Expression.Binary.(function
  | { operator = Equal; left; right }
  | { operator = NotEqual; left; right } ->
      let reason = mk_reason "non-strict equality comparison" loc in
      let t1 = expression cx left in
      let t2 = expression cx right in
      Flow_js.flow cx (t1, EqT (reason,t2));
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
      Flow_js.flow cx (t1, ComparatorT (reason,t2));
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
      Flow_js.flow cx (expression cx left, t);
      Flow_js.flow cx (expression cx right, t);
      t

  | { operator = Plus; left; right } ->
      let reason = mk_reason "add" loc in
      let t1 = expression cx left in
      let t2 = expression cx right in
      Flow_js.mk_tvar_where cx reason (fun t ->
        Flow_js.flow cx (t1, AdderT (reason, t2, t));
      )
)

and logical cx loc = Ast.Expression.Logical.(function
  | { operator = Or; left; right } ->
      let t1, _, not_map, xtypes = predicate_of_condition cx left in
      let reason = mk_reason "||" loc in
      let t2 = Env_js.refine_env cx reason not_map xtypes
        (fun () -> expression cx right)
      in
      Flow_js.mk_tvar_where cx reason (fun t ->
        Flow_js.flow cx (t1, OrT (reason, t2, t));
      )

  | { operator = And; left; right } ->
      let t1, map, _, xtypes = predicate_of_condition cx left in
      let reason = mk_reason "&&" loc in
      let t2 = Env_js.refine_env cx reason map xtypes
        (fun () -> expression cx right)
      in
      Flow_js.mk_tvar_where cx reason (fun t ->
        Flow_js.flow cx (t1, AndT (reason, t2, t));
      )
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
            set_module_exports cx reason (CJSExportDefaultT(reason, t));

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
            Flow_js.flow cx (super, SetT(reason, name, t))

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
            else (
              Env_js.havoc_heap_refinements ();
              Flow_js.flow cx (o, SetT (reason, name, t))
            )

        | _, Ast.Pattern.Expression (_, Member {
            Member._object;
            property = Member.PropertyExpression index;
            _
          }) ->
            let reason = mk_reason "assignment of computed property/element" loc in
            let a = expression cx _object in
            let i = expression cx index in
            Flow_js.flow cx (a, SetElemT (reason, i, t))

        | _ ->
            destructuring_assignment cx t r
      );
      t

  | (r, Assignment.PlusAssign, e) ->
      let reason = mk_reason "+=" loc in
      let rt = assignment_lhs cx r in
      let et = expression cx e in
      let t = Flow_js.mk_tvar cx reason in
      Flow_js.flow cx (rt, AdderT (reason, et, t));
      Flow_js.flow cx (et, AdderT (reason, rt, t));
      Flow_js.flow cx (t, rt);
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
      Flow_js.flow cx (rt, t);
      Flow_js.flow cx (et, t);
      Flow_js.flow cx (t, rt);
      rt
)

(* Object assignment patterns. In the `copyProperties` model (chain_objects), an
   existing object receives properties from other objects. This pattern suffers
   from "races" in the type checker, since the object supposed to receive
   properties is available even when the other objects supplying the properties
   are not yet available. In the `mergeProperties` model (spread_objects), a new
   object receives properties from other objects and is returned, but the new
   object is made available only when the properties have actually been
   received. Similarly, clone_object makes the receiving object available only
   when the properties have actually been received. These patterns are useful
   when merging properties across modules, e.g., and should eventually replace
   other patterns wherever they are potentially racy. *)

and spread_objects cx reason those =
  chain_objects cx reason (mk_object cx reason) those

and extended_object cx reason ~sealed map spread =
  let o =
    Flow_js.mk_object_with_map_proto cx reason ~sealed map (MixedT reason)
  in
  chain_objects cx reason o spread

and clone_object_with_excludes cx reason this that excludes =
  Flow_js.mk_tvar_where cx reason (fun tvar ->
    Flow_js.flow cx (
      this,
      ObjAssignT(reason, that, ObjRestT(reason, excludes, tvar), [], true)
    )
  )

and clone_object cx reason this that =
  clone_object_with_excludes cx reason this that []

and chain_objects cx reason this those =
  let result = ref this in
  List.iter (fun that ->
    result := Flow_js.mk_tvar_where cx reason (fun t ->
      Flow_js.flow cx (!result, ObjAssignT(reason, that, t, [], true));
    )
  ) those;
  !result

and react_ignored_attributes = [ "key"; "ref"; ]

and react_ignore_attribute aname =
  List.mem aname react_ignored_attributes

and jsx cx = Ast.JSX.(function { openingElement; closingElement; children } ->
  jsx_title cx openingElement (List.map (jsx_body cx) children)
)

and jsx_title cx openingElement children = Ast.JSX.(
  let eloc, { Opening.name; attributes; _ } = openingElement in
  match name with

  | Identifier (_, { Identifier.name }) when name = String.capitalize name ->
      let reason = mk_reason (spf "React element: %s" name) eloc in
      let c = Env_js.get_var cx name reason in
      let map = ref SMap.empty in
      let spread = ref None in
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

            if not (react_ignore_attribute aname)
            then map := !map |> SMap.add aname atype

        | Opening.Attribute _ ->
            () (* TODO: attributes with namespaced names *)

        | Opening.SpreadAttribute (aloc, { SpreadAttribute.argument }) ->
            let ex_t = expression cx argument in
            spread := Some (ex_t)
      );

      let reason_props = prefix_reason "props of " reason in
      let o = Flow_js.mk_object_with_map_proto cx reason_props
        !map (MixedT reason_props)
      in
      let o = match !spread with
        | None -> o
        | Some ex_t ->
            let reason_prop = prefix_reason "spread of " (reason_of_t ex_t) in
            clone_object_with_excludes cx reason_prop o ex_t react_ignored_attributes
      in
      (* TODO: children *)
      let react = require cx "react" "react" eloc in
      Flow_js.mk_tvar_where cx reason (fun tvar ->
        Flow_js.flow cx (
          react,
          MethodT(reason, "createElement", react, [c;o], tvar, 0)
        );
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

and jsx_body cx = Ast.JSX.(function
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
      AnyFunT (mk_reason "func" vloc)

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "object"; _ });
      _
    } ->
      AnyObjT (mk_reason "object" vloc)

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
      Flow_js.mk_instance cx (mk_reason "instanceOf" vloc)
        (expression cx e)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "objectOf"; _ });
         _
      };
      arguments = [Expression e];
    } ->
      let flags = {
        frozen = false;
        sealed = false;
        exact = true
      } in
      let dict = Some {
        dict_name = None;
        key = AnyT.t;
        value = mk_proptype cx e
      } in
      let pmap = Flow_js.mk_propmap cx SMap.empty in
      let proto = MixedT (reason_of_string "Object") in
      ObjT (mk_reason "objectOf" vloc, Flow_js.mk_objecttype ~flags dict pmap proto)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "oneOf"; _ });
         _
      };
      arguments = [Expression (_, Array { Array.elements })]
    } ->
      let rec string_literals lits es = match (es) with
        | Some (Expression (loc, Literal { Ast.Literal.
            value = Ast.Literal.String lit; _
          })) :: tl ->
            string_literals (lit :: lits) tl
        | [] -> Some lits
        | _  -> None in
      (match string_literals [] elements with
        | Some lits ->
            let reason = mk_reason "oneOf" vloc in
            mk_enum_type cx reason lits
        | None -> AnyT.at vloc)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "oneOfType"; _ });
         _
      };
      arguments = [Expression (_, Array { Array.elements })]
    } ->
      let rec proptype_elements ts es = match es with
        | Some (Expression e) :: tl ->
            proptype_elements (mk_proptype cx e :: ts) tl
        | [] -> Some ts
        | _ -> None in
      let reason = mk_reason "oneOfType" vloc in
      (match proptype_elements [] elements with
        | Some ts -> UnionT (reason, ts)
        | None -> AnyT.at vloc)

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

(* Legacy: generate React class from specification object. *)
and react_create_class cx loc class_props = Ast.Expression.(
  let reason_class = mk_reason "React class" loc in
  let reason_component = mk_reason "React component" loc in
  let this = Flow_js.mk_tvar cx reason_component in
  let mixins = ref [] in
  let static_reason = prefix_reason "statics of " reason_class in
  let static = ref (mk_object cx static_reason) in
  let default_reason = prefix_reason "default props of " reason_component in
  let default = ref (mk_object cx default_reason) in
  let reason_state = prefix_reason "state of " reason_component in
  let state = mk_object cx reason_state in

  let props_reason = prefix_reason "props of " reason_component in
  let props = ref (mk_object cx props_reason) in

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
        static := extended_object cx reason ~sealed:false map spread;
        fmap, mmap

      (* propTypes *)
      | Property (loc, { Property.kind = Property.Init;
          key = Property.Identifier (nloc, {
            Ast.Identifier.name = "propTypes"; _ });
          value = _, Object { Object.properties } as value;
          _ }) ->
        ignore (expression cx value);
        let reason = mk_reason "propTypes" nloc in
        let amap, omap = mk_proptypes cx properties in
        let map = SMap.fold (fun k v map -> SMap.add k (OptionalT v) map) omap amap in
        props :=
          Flow_js.mk_object_with_map_proto cx reason map (MixedT reason);
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
          (match t with
          | FunT (_, _, _, { params_tlist = []; return_t; _ }) ->
              default := return_t
          | _ -> ());
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
            ObjAssignT(reason_state, state, AnyT.t, [], false)
          in
          Flow_js.flow cx (t,
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

  let type_args = [!default; !props; state] in
  let super_reason = prefix_reason "super of " reason_component in
  let super =
    Flow_js.get_builtin_typeapp cx super_reason
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
  let super_static = Flow_js.mk_tvar_where cx static_reason (fun t ->
    Flow_js.flow cx (super, GetT(static_reason, "statics", t));
  ) in
  Flow_js.flow cx (super_static, override_statics);
  static := clone_object cx static_reason !static super_static;

  let id = Flow_js.mk_nominal cx in
  let itype = {
    class_id = id;
    type_args = SMap.empty;
    fields_tmap = Flow_js.mk_propmap cx fmap;
    methods_tmap = Flow_js.mk_propmap cx mmap;
    mixins = !mixins <> [];
    structural = false;
  } in
  Flow_js.flow cx (super, SuperT (super_reason, itype));

  (* TODO: Mixins are handled quite superficially. *)
  (* mixins' statics are copied into static to inherit static properties *)
  (* mixins must be consistent with instance properties *)
  !mixins |> List.iter (fun mixin ->
    static := clone_object cx static_reason !static mixin;
    Flow_js.flow cx (mixin, SuperT (super_reason, itype))
  );

  let instance = InstanceT (reason_component,!static,super,itype) in
  Flow_js.flow cx (instance, this);

  ClassT(instance)
)

(* given an expression found in a test position, notices certain
   type refinements which follow from the test's success or failure,
   and returns a quad:
   - result type of the test (not always bool)
   - map (lookup key -> type) of refinements which hold if
   the test is true
   - map of refinements which hold if the test is false
   - map of unrefined types for lvalues found in refinement maps
 *)
and predicate_of_condition cx e = Ast.(Expression.(

  (* refinement key if expr is eligible, along with unrefined type *)
  let refinable_lvalue e =
    Refinement.key e, expression cx e
  in

  (* package result quad from test type, refi key, unrefined type,
     predicate, and predicate's truth sense *)
  let result test_t key unrefined_t pred sense =
    let p, notp = if sense
      then pred, NotP pred
      else NotP pred, pred
    in
    (test_t,
      SMap.singleton key p,
      SMap.singleton key notp,
      SMap.singleton key unrefined_t)
  in

  (* package empty result (no refinements derived) from test type *)
  let empty_result test_t =
    (test_t, SMap.empty, SMap.empty, SMap.empty)
  in

  (* inspect a null equality test *)
  let null_test loc op e =
    let refinement = match refinable_lvalue e with
    | None, t -> None
    | Some name, t ->
        match op with
        | Binary.Equal | Binary.NotEqual ->
            Some (name, t, IsP "null or undefined", op = Binary.Equal)
        | Binary.StrictEqual | Binary.StrictNotEqual ->
            Some (name, t, IsP "null", op = Binary.StrictEqual)
        | _ -> None
    in
    match refinement with
    | Some (name, t, p, sense) -> result (BoolT.at loc) name t p sense
    | None -> empty_result (BoolT.at loc)
  in

  (* inspect an undefined equality test *)
  let undef_test loc op e =
    let refinement = match refinable_lvalue e with
    | None, t -> None
    | Some name, t ->
        match op with
        | Binary.Equal | Binary.NotEqual ->
            Some (name, t, IsP "null or undefined", op = Binary.Equal)
        | Binary.StrictEqual | Binary.StrictNotEqual ->
            Some (name, t, IsP "undefined", op = Binary.StrictEqual)
        | _ -> None
    in
    match refinement with
    | Some (name, t, p, sense) -> result (BoolT.at loc) name t p sense
    | None -> empty_result (BoolT.at loc)
  in

  let bool_test loc op e right =
    let refinement = match refinable_lvalue e with
    | None, t -> None
    | Some name, t ->
        match op with
        (* TODO support == *)
        | Binary.StrictEqual | Binary.StrictNotEqual ->
            let pred = string_of_bool right in
            Some (name, t, IsP pred, op = Binary.StrictEqual)
        | _ -> None
    in
    match refinement with
    | Some (name, t, p, sense) -> result (BoolT.at loc) name t p sense
    | None -> empty_result (BoolT.at loc)
  in

  (* inspect a typeof equality test *)
  let typeof_test sense arg typename =
    match refinable_lvalue arg with
    | Some name, t -> result BoolT.t name t (IsP typename) sense
    | None, t -> empty_result BoolT.t
  in

  let mk_and map1 map2 = SMap.merge
    (fun x -> fun p1 p2 -> match (p1,p2) with
      | (None, None) -> None
      | (Some p, None)
      | (None, Some p) -> Some p
      | (Some p1, Some p2) -> Some (AndP(p1,p2))
    )
    map1 map2
  in

  let mk_or map1 map2 = SMap.merge
    (fun x -> fun p1 p2 -> match (p1,p2) with
      | (None, None) -> None
      | (Some p, None)
      | (None, Some p) -> None
      | (Some p1, Some p2) -> Some (OrP(p1,p2))
    )
    map1 map2
  in

  (* main *)
  match e with

  (* ids, member expressions *)
  | _, Identifier _
  | _, Member _ -> (
      match refinable_lvalue e with
      | Some name, t -> result t name t ExistsP true
      | None, t -> empty_result t
    )

  (* assignments *)
  | _, Assignment { Assignment.left = loc, Ast.Pattern.Identifier id; _ } -> (
      let expr = expression cx e in
      match refinable_lvalue (loc, Ast.Expression.Identifier id) with
      | Some name, _ -> result expr name expr ExistsP true
      | None, _ -> empty_result expr
    )

  (* expr instanceof t *)
  | _, Binary { Binary.operator = Binary.Instanceof; left; right } -> (
      match refinable_lvalue left with
      | Some name, t ->
          result BoolT.t name t (InstanceofP (expression cx right)) true
      | None, t ->
          empty_result BoolT.t
    )

  (* expr op null *)
  | loc, Binary { Binary.operator;
      left;
      right = _, Literal { Literal.value = Literal.Null; _ }
    } ->
      null_test loc operator left

  (* null op expr *)
  | loc, Binary { Binary.operator;
      left = _, Literal { Literal.value = Literal.Null; _ };
      right
    } ->
      null_test loc operator right

  (* expr op undefined *)
  | loc, Binary { Binary.operator;
      left;
      right = _, Identifier (_, { Identifier.name = "undefined"; _ })
    } ->
      undef_test loc operator left

  (* undefined op expr *)
  | loc, Binary { Binary.operator;
      left = _, Identifier (_, { Identifier.name = "undefined"; _ });
      right
    } ->
      undef_test loc operator right

  (* expr op void(...) *)
  | loc, Binary { Binary.operator;
      left;
      right = _, Unary ({ Unary.operator = Unary.Void; _ }) as void_arg
    } ->
      ignore (expression cx void_arg);
      undef_test loc operator left

  (* void(...) op expr *)
  | loc, Binary { Binary.operator;
      left = _, Unary ({ Unary.operator = Unary.Void; _ }) as void_arg;
      right
    } ->
      ignore (expression cx void_arg);
      undef_test loc operator right

  (* expr op true; expr op false *)
  | loc, Binary { Binary.operator;
      left;
      right = _, Literal { Literal.value = Literal.Boolean value; _ }
    } ->
      bool_test loc operator left value

  (* true op expr; false op expr *)
  | loc, Binary { Binary.operator;
      left = _, Literal { Literal.value = Literal.Boolean value; _ };
      right
    } ->
      bool_test loc operator right value

  (* typeof expr ==/=== string *)
  | _, Binary { Binary.operator = Binary.Equal | Binary.StrictEqual;
      left = _, Unary { Unary.operator = Unary.Typeof; argument; _ };
      right = _, Literal { Literal.value = Literal.String s; _ }
    } ->
      typeof_test true argument s

  (* typeof expr !=/!== string *)
  | _, Binary { Binary.operator = Binary.NotEqual | Binary.StrictNotEqual;
      left = _, Unary { Unary.operator = Unary.Typeof; argument; _ };
      right = _, Literal { Literal.value = Literal.String s; _ }
    } ->
      typeof_test false argument s

  (* string ==/=== typeof expr *)
  | _, Binary { Binary.operator = Binary.Equal | Binary.StrictEqual;
      left = _, Literal { Literal.value = Literal.String s; _ };
      right = _, Unary { Unary.operator = Unary.Typeof; argument; _ }
    } ->
      typeof_test true argument s

  (* string !=/!== typeof expr *)
  | _, Binary { Binary.operator = Binary.NotEqual | Binary.StrictNotEqual;
      left = _, Literal { Literal.value = Literal.String s; _ };
      right = _, Unary { Unary.operator = Unary.Typeof; argument; _ }
    } ->
      typeof_test false argument s

  (* Array.isArray(expr) *)
  | _, Call {
      Call.callee = _, Member {
        Member._object = _, Identifier (_,
          { Identifier.name = "Array"; _ });
        property = Member.PropertyIdentifier (_,
          { Identifier.name = "isArray"; _ });
        _ };
      arguments = [Expression arg]
    } -> (
      match refinable_lvalue arg with
      | Some name, t ->
          result BoolT.t name t (IsP "array") true
      | None, t ->
          empty_result BoolT.t
    )

  (* obj.hasOwnProperty('x') *)
  | loc, Call {
      Call.callee = callee_loc, Member {
        Member._object;
        property = Member.PropertyIdentifier (_,
          { Identifier.name = "hasOwnProperty"; _});
        _ };
      arguments = [Expression (_, Literal
        { Ast.Literal.value = Ast.Literal.String x; _ }
      )]
    } -> (
      (* obj.x *)
      let fake_ast = callee_loc, Ast.Expression.Member {
        Member._object;
        property = Member.PropertyIdentifier (
          Ast.Loc.none, {
            Identifier.name = x;
            typeAnnotation = None;
            optional = false;
          }
        );
        computed = false;
      } in
      match refinable_lvalue fake_ast with
      | Some name, t ->
          result (BoolT.at loc) name t (IsP "undefined") false
      | None, t ->
          empty_result (BoolT.at loc)
    )

  (* test1 && test2 *)
  | loc, Logical { Logical.operator = Logical.And; left; right } ->
      let reason = mk_reason "&&" loc in
      let t1, map1, not_map1, xts1 = predicate_of_condition cx left in
      let t2, map2, not_map2, xts2 = Env_js.refine_env cx reason map1 xts1
        (fun () -> predicate_of_condition cx right)
      in
      (
        Flow_js.mk_tvar_where cx reason (fun t ->
          Flow_js.flow cx (t1, AndT (reason, t2, t));
        ),
        mk_and map1 map2,
        mk_or not_map1 not_map2,
        SMap.union xts1 xts2
      )

  (* test1 || test2 *)
  | loc, Logical { Logical.operator = Logical.Or; left; right } ->
      let reason = mk_reason "||" loc in
      let t1, map1, not_map1, xts1 = predicate_of_condition cx left in
      let t2, map2, not_map2, xts2 = Env_js.refine_env cx reason not_map1 xts1
        (fun () -> predicate_of_condition cx right)
      in
      (
        Flow_js.mk_tvar_where cx reason (fun t ->
          Flow_js.flow cx (t1, OrT (reason, t2, t));
        ),
        mk_or map1 map2,
        mk_and not_map1 not_map2,
        SMap.union xts1 xts2
      )

  (* !test *)
  | loc, Unary { Unary.operator = Unary.Not; argument; _ } ->
      let (t, map, not_map, xts) = predicate_of_condition cx argument in
      (BoolT.at loc, not_map, map, xts)

  (* fallthrough case: evaluate test expr, no refinements *)
  | e ->
      empty_result (expression cx e)
))

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
        Flow_js.flow cx (spec, GetT(reason, "value", tvar));
      )
    ) in
    Flow_js.mk_object_with_map_proto cx reason map proto

  | ("getPrototypeOf", [ Expression e ]) ->
    let o = expression cx e in
    Flow_js.mk_tvar_where cx reason (fun tvar ->
      Flow_js.flow cx (o, GetT(reason, "__proto__", tvar));
    )

  | (("getOwnPropertyNames" | "keys"), [ Expression e ]) ->
    let o = expression cx e in
    ArrT (reason,
      Flow_js.mk_tvar_where cx reason (fun tvar ->
        let reason = prefix_reason "element of " reason in
        Flow_js.flow cx (o, KeyT(reason, tvar));
      ),
          [])

  | ("defineProperty", [ Expression e;
                         Expression (_, Literal
                           { Ast.Literal.value = Ast.Literal.String x; _ });
                         Expression config ]) ->
    let o = expression cx e in
    let spec = expression cx config in
    let tvar = Flow_js.mk_tvar cx reason in
    Flow_js.flow cx (spec, GetT(reason, "value", tvar));
    Flow_js.flow cx (o, SetT (reason, x, tvar));
    o

  | ("defineProperties", [ Expression e;
                         Expression (_, Object { Object.properties }) ]) ->
    let o = expression cx e in
    let pmap, _ = object_ cx properties in
    pmap |> SMap.iter (fun x spec ->
      let reason = prefix_reason (spf ".%s of " x) reason in
      let tvar = Flow_js.mk_tvar cx reason in
      Flow_js.flow cx (spec, GetT(reason, "value", tvar));
      Flow_js.flow cx (o, SetT (reason, x, tvar));
    );
    o

  | ("assign", (Expression e)::others) ->
    let this = expression cx e in
    let those = List.map (expression_or_spread cx) others in
    chain_objects cx reason this those

  (* Freezing an object literal is supported since there's no way it could
     have been mutated elsewhere *)
  | ("freeze", [Expression ((_, Object _) as e)]) ->
    let t = Flow_js.mk_tvar_where cx reason (fun tvar ->
      Flow_js.flow cx (expression cx e, ObjFreezeT (reason, tvar));
    ) in
    Flow_js.static_method_call cx "Object" reason m [t]

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
    Flow_js.flow cx (cls, MethodT(reason, m, cls, argts, tvar, 0));
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
      Flow_js.mk_instance cx reason c

and body_loc = Ast.Statement.FunctionDeclaration.(function
  | BodyBlock (loc, _) -> loc
  | BodyExpression (loc, _) -> loc
)

(* Makes signatures for fields and methods in a class. *)
and mk_signature cx reason_c c_type_params_map body = Ast.Statement.Class.(
  let _, { Body.body = elements } = body in

  (* In case there is no constructor, we create one. *)
  let default_methods =
    SMap.singleton "constructor"
      (replace_reason "default constructor" reason_c, [], SMap.empty,
       ([], [], VoidT.t, SMap.empty, SMap.empty))
  in
  (* NOTE: We used to mine field declarations from field assignments in a
     constructor as a convenience, but it was not worth it: often, all that did
     was exchange a complaint about a missing field for a complaint about a
     missing annotation. Moreover, it caused fields declared in the super class
     to be redeclared if they were assigned in the constructor. So we don't do
     it. In the future, we could do it again, but only for private fields. *)

  List.fold_left (fun (sfields, smethods, fields, methods) -> function

    (* instance and static methods *)
    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Identifier (_,
          { Ast.Identifier.name; _ });
        value = _, { Ast.Expression.Function.params; defaults; rest;
          returnType; typeParameters; body; _ };
        kind = Ast.Expression.Object.Property.Init;
        static;
      }) ->

      let typeparams, f_type_params_map =
        mk_type_param_declarations cx ~map:c_type_params_map typeParameters in

      let map = SMap.fold SMap.add f_type_params_map c_type_params_map in

      let params_ret = mk_params_ret cx map
        (params, defaults, rest) (body_loc body, returnType) in
      let params_ret = if not static && name = "constructor"
        then (
          let params, pnames, ret, params_map, params_loc = params_ret in
          let return_void = VoidT (mk_reason "return undefined" loc) in
          Flow_js.flow cx (ret, return_void);
          params, pnames, return_void, params_map, params_loc
        )
        else params_ret
      in
      let reason_m = mk_reason (spf "method %s" name) loc in
      let method_sig = reason_m, typeparams, f_type_params_map, params_ret in
      if static
      then
        sfields,
        SMap.add name method_sig smethods,
        fields,
        methods
      else
        sfields,
        smethods,
        fields,
        SMap.add name method_sig methods

    (* fields *)
    | Body.Property (loc, {
        Property.key = Ast.Expression.Object.Property.Identifier
          (_, { Ast.Identifier.name; _ });
        typeAnnotation = (_, typeAnnotation);
        static;
      }) ->
        let t = convert cx c_type_params_map typeAnnotation in
        if static
        then
          SMap.add name t sfields,
          smethods,
          fields,
          methods
        else
          sfields,
          smethods,
          SMap.add name t fields,
          methods

    (* get/set *)
    | Body.Method (loc, {
        Method.kind = Ast.Expression.Object.Property.Get
                    | Ast.Expression.Object.Property.Set;
        _
      }) ->
        let msg = "get/set properties not yet supported" in
        Flow_js.add_error cx [mk_reason "" loc, msg];
        sfields, smethods, fields, methods

    (* literal LHS *)
    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Literal _;
        Method.kind = Ast.Expression.Object.Property.Init;
        _
      })
    | Body.Property (loc, {
        Property.key = Ast.Expression.Object.Property.Literal _;
        _
      }) ->
        let msg = "literal properties not yet supported" in
        Flow_js.add_error cx [mk_reason "" loc, msg];
        sfields, smethods, fields, methods

    (* computed LHS *)
    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Computed _;
        Method.kind = Ast.Expression.Object.Property.Init;
        _
      })
    | Body.Property (loc, {
        Property.key = Ast.Expression.Object.Property.Computed _;
        _
      }) ->
        let msg = "computed property keys not supported" in
        Flow_js.add_error cx [mk_reason "" loc, msg];
        sfields, smethods, fields, methods

  ) (SMap.empty, SMap.empty, SMap.empty, default_methods) elements
)

(* Processes the bodies of instance and static methods. *)
and mk_class_elements cx instance_info static_info body = Ast.Statement.Class.(
  let _, { Body.body = elements } = body in
  List.iter (function

    | Body.Method (loc, {
        Method.key = Ast.Expression.Object.Property.Identifier (_,
          { Ast.Identifier.name; _ });
        value = _, { Ast.Expression.Function.params; defaults; rest;
          returnType; typeParameters; body; _ };
        kind = Ast.Expression.Object.Property.Init;
        static;
      }) ->
      let this, super, method_sigs =
        if static then static_info else instance_info
      in

      let reason, typeparams, type_params_map,
           (_, _, ret, param_types_map, param_loc_map) =
        SMap.find_unsafe name method_sigs in

      let save_return_exn = Abnormal.swap Abnormal.Return false in
      let save_throw_exn = Abnormal.swap Abnormal.Throw false in
      generate_tests cx reason typeparams (fun map_ ->
        let param_types_map =
          param_types_map |> SMap.map (Flow_js.subst cx map_) in
        let ret = Flow_js.subst cx map_ ret in

        mk_body None cx param_types_map param_loc_map ret body this super;
      );
      ignore (Abnormal.swap Abnormal.Return save_return_exn);
      ignore (Abnormal.swap Abnormal.Throw save_throw_exn)

    | _ -> ()
  ) elements
)

and mk_methodtype (reason_m, typeparams,_,(params,pnames,ret,_,_)) =
  let ft = FunT (
    reason_m, Flow_js.dummy_static, Flow_js.dummy_prototype,
    Flow_js.mk_functiontype2 params pnames ret 0
  ) in
  if (typeparams = [])
  then ft
  else PolyT (typeparams, ft)

(* Process a class definition, returning a (polymorphic) class type. A class
   type is a wrapper around an instance type, which contains types of instance
   members, a pointer to the super instance type, and a container for types of
   static members. The static members can be thought of as instance members of a
   "metaclass": thus, the static type is itself implemented as an instance
   type. *)
and mk_class cx reason_c type_params extends body =
  (* As a running example, let's say the class declaration is:
     class C<X> extends D<X> { f: X; m<Y: X>(x: Y): X { ... } }
  *)

  (* type parameters: <X> *)
  let typeparams, type_params_map =
    mk_type_param_declarations cx type_params in

  (* fields: { f: X }, methods_: { m<Y: X>(x: Y): X } *)
  let sfields, smethods_, fields, methods_ =
    mk_signature cx reason_c type_params_map body
  in

  let id = Flow_js.mk_nominal cx in

  (* super: D<X> *)
  let super = mk_extends cx reason_c type_params_map extends in
  let super_static = ClassT (super) in

  let super_reason = prefix_reason "super of " reason_c in
  let static_reason = prefix_reason "statics of " reason_c in

  generate_tests cx reason_c typeparams (fun map_ ->
    (* map_: [X := T] where T = mixed, _|_ *)

    (* super: D<T> *)
    let super = Flow_js.subst cx map_ super in
    let super_static = Flow_js.subst cx map_ super_static in

    (* fields: { f: T } *)
    let fields = fields |> SMap.map (Flow_js.subst cx map_) in
    let sfields = sfields |> SMap.map (Flow_js.subst cx map_) in

    (* methods: { m<Y: X>(x: Y): T } *)
    let subst_method_sig cx map_
        (reason_m, typeparams,type_params_map,
         (params,pnames,ret,param_types_map,param_loc_map)) =

      (* typeparams = <Y: X> *)
      let typeparams = List.map (fun typeparam ->
        { typeparam with bound = Flow_js.subst cx map_ typeparam.bound }
      ) typeparams in
      let type_params_map = SMap.map (Flow_js.subst cx map_) type_params_map in

      (* params = (x: Y), ret = T *)
      let params = List.map (Flow_js.subst cx map_) params in
      let ret = Flow_js.subst cx map_ ret in
      let param_types_map =
        SMap.map (Flow_js.subst cx map_) param_types_map in

      (reason_m, typeparams,type_params_map,
       (params, Some pnames, ret, param_types_map, param_loc_map))
    in

    let methods_ = methods_ |> SMap.map (subst_method_sig cx map_) in
    let methods = methods_ |> SMap.map mk_methodtype in
    let smethods_ = smethods_ |> SMap.map (subst_method_sig cx map_) in
    let smethods = smethods_ |> SMap.map mk_methodtype in

    let static_instance = {
      class_id = 0;
      type_args = type_params_map |> SMap.map (Flow_js.subst cx map_);
      fields_tmap = Flow_js.mk_propmap cx sfields;
      methods_tmap = Flow_js.mk_propmap cx smethods;
      mixins = false;
      structural = false;
    } in
    Flow_js.flow cx (super_static, SuperT(super_reason, static_instance));
    let static = InstanceT (
      static_reason,
      MixedT.t,
      super_static,
      static_instance
    ) in

    let instance = {
      class_id = id;
      type_args = type_params_map |> SMap.map (Flow_js.subst cx map_);
      fields_tmap = Flow_js.mk_propmap cx fields;
      methods_tmap = Flow_js.mk_propmap cx methods;
      mixins = false;
      structural = false;
    } in
    Flow_js.flow cx (super, SuperT(super_reason, instance));
    let this = InstanceT (reason_c,static,super,instance) in

    mk_class_elements cx
      (this, super, methods_)
      (static, super_static, smethods_)
      body;
  );

  let enforce_void_return
      (reason_m, typeparams, type_params_map,
       (params,pnames,ret,params_map,params_loc)) =
    let ret =
      if (is_void cx ret)
      then (VoidT.at (loc_of_t ret))
      else ret
    in
    mk_methodtype
      (reason_m, typeparams, type_params_map,
       (params,Some pnames,ret,params_map,params_loc))
  in

  let methods = methods_ |> SMap.map enforce_void_return in
  let smethods = smethods_ |> SMap.map enforce_void_return in

  let static_instance = {
    class_id = 0;
    type_args = type_params_map;
    fields_tmap = Flow_js.mk_propmap cx sfields;
    methods_tmap = Flow_js.mk_propmap cx smethods;
    mixins = false;
    structural = false;
  } in
  let static = InstanceT (
    static_reason,
    MixedT.t,
    super_static,
    static_instance
  ) in

  let instance = {
    class_id = id;
    type_args = type_params_map;
    fields_tmap = Flow_js.mk_propmap cx fields;
    methods_tmap = Flow_js.mk_propmap cx methods;
    mixins = false;
    structural = false;
  } in
  let this = InstanceT (reason_c, static, super, instance) in

  if (typeparams = [])
  then
    ClassT this
  else
    PolyT(typeparams, ClassT this)

(* Processes a declare class. The fact that we process an interface the same way
   as a declare class is legacy, and might change when we have proper support
   for interfaces. One difference between declare class and interfaces is that
   the the A ~> B check is structural if B is an interface and nominal if B is
   a declare class. If you set the structural flag to true, then this interface
   will be checked structurally *)
and mk_interface cx reason typeparams map (sfmap, smmap, fmap, mmap) extends structural =
  let id = Flow_js.mk_nominal cx in
  let extends =
    match extends with
    | [] -> (None,None)
    | (loc,{Ast.Statement.Interface.Extends.id; typeParameters})::_ ->
        (* TODO: multiple extends *)
        Some (loc,Ast.Expression.Identifier id), typeParameters
  in
  let super_reason = prefix_reason "super of " reason in
  let static_reason = prefix_reason "statics of " reason in

  let super = mk_extends cx super_reason map extends in
  let super_static = ClassT(super) in

  let (imixins,fmap) =
    match SMap.get "mixins" fmap with
    | None -> ([], fmap)
    | Some (ArrT(_,_,ts)) -> (ts, SMap.remove "mixins" fmap)
    | _ -> assert false
  in

  let static_instance = {
    class_id = 0;
    type_args = map;
    fields_tmap = Flow_js.mk_propmap cx sfmap;
    methods_tmap = Flow_js.mk_propmap cx smmap;
    mixins = imixins <> [];
    structural;
  } in
  Flow_js.flow cx (super_static, SuperT(static_reason, static_instance));
  let static = InstanceT (
    static_reason,
    MixedT.t,
    super_static,
    static_instance
  ) in

  let instance = {
    class_id = id;
    type_args = map;
    fields_tmap = Flow_js.mk_propmap cx fmap;
    methods_tmap = Flow_js.mk_propmap cx mmap;
    mixins = imixins <> [];
    structural;
  } in
  Flow_js.flow cx (super, SuperT(super_reason, instance));
  let this = InstanceT (reason, static, super, instance) in

  (* TODO: Mixins are handled quite superficially. *)
  (* mixins must be consistent with instance and static properties *)
  imixins |> List.iter (fun imixin ->
    Flow_js.flow cx (imixin, SuperT(super_reason, instance));
    Flow_js.flow cx (
      ClassT(imixin),
      SuperT(static_reason, static_instance)
    );
  );

  if typeparams = []
  then ClassT(this)
  else PolyT (typeparams, ClassT(this))

and function_decl id cx (reason:reason) type_params params ret body this super =
  let typeparams, type_params_map = mk_type_param_declarations cx type_params in

  let (params, pnames, ret, param_types_map, param_types_loc) =
    mk_params_ret cx type_params_map params (body_loc body, ret) in

  let save_return_exn = Abnormal.swap Abnormal.Return false in
  let save_throw_exn = Abnormal.swap Abnormal.Throw false in
  generate_tests cx reason typeparams (fun map_ ->
    let param_types_map =
      param_types_map |> SMap.map (Flow_js.subst cx map_) in
    let ret = Flow_js.subst cx map_ ret in

    mk_body id cx param_types_map param_types_loc ret body this super;
  );

  ignore (Abnormal.swap Abnormal.Return save_return_exn);
  ignore (Abnormal.swap Abnormal.Throw save_throw_exn);

  let ret =
    if (is_void cx ret)
    then (VoidT.at (loc_of_t ret))
    else ret
  in

  (typeparams,params,pnames,ret)

and is_void cx = function
  | OpenT(_,id) ->
      Flow_js.check_types cx id (function | VoidT _ -> true | _ -> false)
  | _ -> false

and mk_upper_bound cx locs name t =
  create_env_entry t t (SMap.get name locs)

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
  let scope = ref (
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
  Env_js.push_env cx scope;
  let set = Env_js.swap_changeset (fun _ -> SSet.empty) in

  let stmts = Ast.Statement.(match body with
    | FunctionDeclaration.BodyBlock (_, { Block.body }) ->
        body
    | FunctionDeclaration.BodyExpression expr ->
        [ fst expr, Return { Return.argument = Some expr } ]
  ) in

  List.iter (statement_decl cx) stmts;

  if not (has_return_exception_handler (fun () -> toplevels cx stmts))
  then (
    let phantom_return_loc = before_pos (body_loc body) in
    Flow_js.flow cx
      (VoidT (mk_reason "return undefined" phantom_return_loc), ret)
  );

  Env_js.pop_env();
  Env_js.changeset := set;

  Env_js.update_frame cx ctx

and before_pos loc =
  Ast.Loc.(
    let line = loc.start.line in
    let column = loc.start.column in
    let offset = loc.start.offset in
    { loc with
        start = { line = line; column = column - 1; offset = offset - 1; };
        _end = { line = line; column = column; offset = offset; }
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
                  let t =
                    if optional
                    then OptionalT t
                    else t
                  in
                  t :: tlist,
                  name :: pnames,
                  SMap.add name t tmap,
                  SMap.add name loc lmap
              | Some expr ->
                  (* TODO: assert (not optional) *)
                  let te = expression cx expr in
                  Flow_js.flow cx (te, t);
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
   parameter with its bound and Bottom. Run a closure that takes these
   instantiations, each one in turn, and does something with it. We modify the
   salt for every instantiation so that re-analyzing the same AST with different
   instantiations causes different reasons to be generated. *)

and generate_tests cx reason typeparams each =
  typeparams
  |> List.fold_left (fun list {name; bound; _ } ->
    let xreason = replace_reason name reason in
    let bot = UndefT (
      prefix_reason "some incompatible instantiation of " xreason
    ) in
    List.rev_append
      (list |> List.map (fun map ->
        SMap.add name (Flow_js.subst cx map bound) map)
      )
      (list |> List.map (SMap.add name bot))
  ) [SMap.empty]
  |> List.iteri (fun i map_ ->
       each map_;
     )

(* take a list of types appearing in AST as type params,
   do semantic checking and create tvars for them. *)
and mk_type_params cx ?(map=SMap.empty) (types: Ast.Identifier.t list) =
  let mk_type_param (typeparams, smap) (loc, t) =
    let name = t.Ast.Identifier.name in
    let reason = mk_reason name loc in
    let bound = match t.Ast.Identifier.typeAnnotation with
      | None -> MixedT reason
      | Some (_, u) -> mk_type_ cx (SMap.union smap map) reason (Some u)
    in

    let typeparam = { reason; name; bound } in
    (typeparam :: typeparams,
     SMap.add name (BoundT typeparam) smap)
  in
  let typeparams, smap =
    List.fold_left mk_type_param ([], SMap.empty) types
  in
  List.rev typeparams, smap

and mk_type_param_declarations cx ?(map=SMap.empty) typeParameters =
  mk_type_params cx ~map (extract_type_param_declarations typeParameters)

and extract_type_param_declarations = function
  | None -> []
  | Some (_, typeParameters) -> typeParameters.Ast.Type.ParameterDeclaration.params

and extract_type_param_instantiations = function
  | None -> []
  | Some (_, typeParameters) -> typeParameters.Ast.Type.ParameterInstantiation.params

(* Process a function definition, returning a (polymorphic) class type. *)
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
    closure_t = Env_js.peek_frame ()
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
          params (Some pnames) ret (Env_js.peek_frame ()))

(* scrape top-level, unconditional field assignments from constructor code *)
(** TODO: use a visitor **)
(** NOTE: dead code **)
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
  let constraints = Flow_js.find_graph cx id in
  let before = Errors_js.ErrorSet.cardinal cx.errors in
  Flow_js.enforce_strict cx id constraints;
  let after = Errors_js.ErrorSet.cardinal cx.errors in
  let ground_node = new_unresolved_root () in
  let ground_bounds = bounds_of_unresolved_root ground_node in
  if (after = before)
  then (
    match constraints with
    | Unresolved bounds ->
        ground_bounds.lower <- bounds.lower;
        ground_bounds.lowertvars <- bounds.lowertvars
    | _ -> ()
  );
  cx.graph <- IMap.add id ground_node cx.graph

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
  cx.weak <- weak;

  Env_js.init cx;

  let reason_exports_module = reason_of_string (spf "exports of module %s" m) in
  let local_exports = Flow_js.mk_tvar cx reason_exports_module in
  let scope = ref (
    SMap.singleton "exports"
      (create_env_entry
        local_exports
        local_exports
        None
      )
    |> SMap.add (internal_name "exports")
      (create_env_entry
        (UndefT (reason_of_string "undefined exports"))
        (AnyT reason_exports_module)
        None
      )
  ) in
  Env_js.push_env cx scope;
  Env_js.changeset := SSet.empty;

  let reason = new_reason "exports" (Pos.make_from
    (Relative_path.create Relative_path.Dummy cx.file)) in

  if check then (
    let init_exports = mk_object cx reason in
    set_module_exports cx reason init_exports;
    Flow_js.flow cx (
      init_exports,
      Env_js.get_var_in_scope cx "exports" reason
    );
    infer_core cx statements;
  );

  cx.checked <- check;
  Flow_js.flow cx (
    get_module_exports cx reason,
    exports cx m);

  let ins = (Flow_js.builtins)::(
    SSet.fold (fun r list -> (lookup_module cx r)::list) cx.required []
  ) in
  let outs = [lookup_module cx m] in
  Flow_js.do_gc cx ins outs;

  (* insist that whatever type flows into exports is fully annotated *)
  (if modes.strict then force_annotations cx);

  cx

(* return all comments preceding the first executable statement *)
let get_comment_header (_, stmts, comments) =
  match stmts with
  | [] -> comments
  | stmt :: _ ->
    let stmtloc = fst stmt in
    let rec loop acc comments =
      match comments with
      | c :: cs when fst c < stmtloc ->
        loop (c :: acc) cs
      | _ -> acc
    in
    List.rev (loop [] comments)

(* Given a filename, retrieve the parsed AST, derive a module name,
   and invoke the local (infer) pass. This will build and return a
   fresh context object for the module. *)
let infer_module file =
  let ast = Parsing_service_js.get_ast_unsafe file in
  let comments = get_comment_header ast in
  let module_name = Module_js.exported_module file comments in
  infer_ast ast file module_name modes.all

(* Map.union: which is faster, union M N or union N M when M > N?
   union X Y = fold add X Y which means iterate over X, adding to Y
   So running time is roughly X * log Y.

   Now, when M > N, we have M * log N > N * log M.
   So do union N M as long as N may override M for overlapping keys.
*)

(* aggregate module context data with other module context data *)
let aggregate_context_data cx cx_other =
  cx.closures <-
    IMap.union cx_other.closures cx.closures;
  cx.property_maps <-
    IMap.union cx_other.property_maps cx.property_maps;
  cx.globals <-
    SSet.union cx_other.globals cx.globals

(* update module graph with other module graph *)
let update_graph cx cx_other =
  let _, builtin_id = open_tvar Flow_js.builtins in
  let master_node = IMap.find_unsafe builtin_id cx.graph in
  let master_bounds = bounds_of_unresolved_root master_node in
  cx_other.graph |> IMap.iter (fun id module_node ->
    if (id = builtin_id) then (
      let module_bounds = bounds_of_unresolved_root module_node in
      master_bounds.uppertvars <- IMap.union
        module_bounds.uppertvars
        master_bounds.uppertvars;
      master_bounds.upper <- TypeMap.union
        module_bounds.upper
        master_bounds.upper
    )
    else
      cx.graph <- cx.graph |> IMap.add id module_node
  )

type direction = Out | In

(* make sure a module typed in the given context also has a type
 * in the master context, and create a flow between the two types
 * in the direction specified *)
let link_module_types dir cx m =
  let glo = exports Flow_js.master_cx m in
  let loc = lookup_module cx m in
  let edge = match dir with Out -> (glo, loc) | In -> (loc, glo) in
  Flow_js.flow Flow_js.master_cx edge

(* map an exported module type from context to master *)
let export_to_master cx m =
  link_module_types In cx m

(* map a required module type from master to context *)
let require_from_master cx m =
  link_module_types Out cx m

(* Copy context from cx_other to cx *)
let copy_context cx cx_other =
  aggregate_context_data cx cx_other;
  update_graph cx cx_other

let copy_context_master cx =
  copy_context Flow_js.master_cx cx

(* Connect the builtins object in master_cx to the builtins reference in some
   arbitrary cx. *)
let implicit_require_strict cx master_cx =
  let _, builtin_id = open_tvar Flow_js.builtins in
  let types = Flow_js.possible_types master_cx builtin_id in
  types |> List.iter (fun t ->
    Flow_js.flow cx (t, Flow_js.builtins)
  )

(* Connect the export of cx_from to its import in cx_to. This happens in some
   arbitrary cx, so cx_from and cx_to should have already been copied to cx. *)
let explicit_impl_require_strict cx (cx_from, cx_to) =
  let m = cx_from._module in
  let from_t = lookup_module cx_from m in
  let to_t =
    try lookup_module cx_to m
    with _ ->
      (* The module exported by cx_from may be imported by path in cx_to *)
      lookup_module cx_to cx_from.file
  in
  Flow_js.flow cx (from_t, to_t)

(* Connect a export of a declared module to its imports in cxs_to. This happens
   in some arbitrary cx, so all cxs_to should have already been copied to cx. *)
let explicit_decl_require_strict cx m cxs_to =
  let reason = reason_of_string m in
  let from_t = Flow_js.mk_tvar cx reason in
  Flow_js.lookup_builtin cx (internal_module_name m) reason None from_t;
  cxs_to |> List.iter (fun cx_to ->
    let to_t = lookup_module cx_to m in
    Flow_js.flow cx (from_t, to_t)
  )

(* Merge context of module with contexts of its implicit requires and explicit
   requires. The implicit requires are those defined in lib.js (master_cx). For
   the explicit requires, we need to merge the entire dependency graph: this
   includes "nodes" (cxs) and edges (implementations and
   declarations). Intuitively, the operation we really need is "substitution" of
   known types for unknown type variables. This operation is simulated by the
   more general procedure of copying and linking graphs. *)
let merge_module_strict cx cxs implementations declarations master_cx =
  Flow_js.Cache.clear();

  (* First, copy cxs and master_cx to the host cx. *)
  cxs |> List.iter (copy_context cx);
  copy_context cx master_cx;

  (* Connect links between implementations and requires. Since the host contains
     copies of all graphs, the links should already be available. *)
  implementations |> List.iter (explicit_impl_require_strict cx);

  (* Connect links between declarations and requires. Since the host contains
     copies of all graphs, the links should already be available *)
  declarations |> SMap.iter (explicit_decl_require_strict cx);

  (* Connect the builtins object to the builtins reference. Since the builtins
     reference is shared, this connects the definitions of builtins to their
     uses in all contexts. *)
  implicit_require_strict cx master_cx

(* variation of infer + merge for lib definitions *)
let init file statements save_errors =
  Flow_js.Cache.clear();

  let cx = new_context file Files_js.lib_module in

  Env_js.init cx;

  let scope = ref SMap.empty in
  Env_js.push_env cx scope;
  Env_js.changeset := SSet.empty;

  infer_core cx statements;

  !scope |> SMap.iter (fun x {specific=t;_} ->
    Flow_js.set_builtin cx x t
  );

  copy_context_master cx;

  let errs = cx.errors in
  cx.errors <- Errors_js.ErrorSet.empty;
  save_errors errs

(* Legacy functions for managing non-strict merges. *)
(* !!!!!!!!!!! TODO: out of date !!!!!!!!!!!!!!! *)

(* merge module context into master context *)
let merge_module cx =
  copy_context_master cx;
  (* link local and global types for our export and our required's *)
  export_to_master cx cx._module;
  SSet.iter (require_from_master cx) cx.required;
  ()

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
