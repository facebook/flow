(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Constraint
open Type
open Utils_js

let header =
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"
  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
  xmlns:y=\"http://www.yworks.com/xml/graphml\"
  xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">
  <key id=\"d0\" for=\"node\" yfiles.type=\"nodegraphics\"/>
  <key id=\"d1\" for=\"edge\" yfiles.type=\"edgegraphics\"/>
  <graph id=\"G\" edgedefault=\"directed\">
"

let footer =
  "</graph>
</graphml>"

let xml_escape =
  let tab = ["<", "&lt;"; ">", "&gt;"; "&", "&amp;"] in
  let subs = List.fold_left (fun l (a, b) -> (Str.regexp a, b) :: l) [] tab in
  fun s -> List.fold_left (fun s (a, b) -> Str.global_replace a b s) s subs

let format_node ?(kids=[]) ?(attrs=[]) ?(shape="roundrectangle") id label =
  spf
"<node id=\"%d\" %s>
  <data key=\"d0\">
    <y:ShapeNode>
      <y:NodeLabel>%s</y:NodeLabel>
      <y:Shape type=\"%s\"/>
      <y:Fill color=\"#CCCCFF\" transparent=\"false\"/>
      <y:BorderStyle type=\"line\" width=\"1.0\" color=\"#000000\"/>
    </y:ShapeNode>
  </data>%s
</node>
" id
  (String.concat " " (List.map (fun (n,v) -> spf "%s=%S" n v) attrs))
  (xml_escape label)
  shape
  (String.concat "\n" kids)

let format_tvar ?(kids=[]) ?(attrs=[]) id label =
  format_node ~kids ~attrs ~shape:"hexagon" id label

let format_edge ?(attrs=[]) ?(label="") ?(source="none") ?(target="standard")
  s t =
  spf
"<edge source=\"%d\" target=\"%d\" %s>
  <data key=\"d1\">
    <y:PolyLineEdge>
      <y:EdgeLabel>%s</y:EdgeLabel>
      <y:Arrows source=\"%s\" target=\"%s\"/>
    </y:PolyLineEdge>
  </data>
</edge>
" s t
  (String.concat " " (List.map (fun (n,v) -> spf "%s=%S" n v) attrs))
  (xml_escape label)
  source
  target

let format_flow_edge = format_edge

let format_contain_edge ?(label="") = format_edge ~target:"diamond" ~label

let format_port = spf "<port name=\"%s\"/>"

let tvar_ports =
  List.map format_port ["lower";"upper";"lowertvars";"uppertvars"]

let pos_only_reason r =
  let open Reason in
  let open Loc in
  repos_reason { (loc_of_reason r) with source = Some (SourceFile "") } r
(*  spf "%s: %S" (string_of_loc_pos (loc_of_reason r)) (desc_of_reason r) *)

let t_label cx t =
  let t = mod_reason_of_t pos_only_reason t in
  Debug_js.dump_t ~depth:1 cx t

let use_t_label cx u =
  let u = mod_reason_of_use_t pos_only_reason u in
  Debug_js.dump_use_t ~depth:1 cx u

let has_id id (iset, _, _) = ISet.mem id iset
let put_id id (iset, tmap, umap) = ISet.add id iset, tmap, umap

module TypeHT = Hashtbl.Make(struct
  type t = Type.t
  let equal i j = i == j
  let hash = Hashtbl.hash
end)

module UseTypeHT = Hashtbl.Make(struct
  type t = Type.use_t
  let equal i j = i == j
  let hash = Hashtbl.hash
end)

let newstate n = ISet.empty, TypeHT.create n, UseTypeHT.create n

let get_t t (_, ttbl, _) = try Some (TypeHT.find ttbl t) with _ -> None
let put_t t id (iset, ttbl, utbl) = iset, (TypeHT.replace ttbl t id; ttbl), utbl

let get_u u (_, _,utbl) = try Some (UseTypeHT.find utbl u) with _ -> None
let put_u u id (iset, ttbl, utbl) =
  iset, ttbl, (UseTypeHT.replace utbl u id; utbl)

type tnode = Def of Type.t | Use of Type.use_t

let list_parts = List.mapi (fun i t -> spf "#%d" i, Def t)
let map_parts m = List.map (fun (n, t) -> n, Def t) (SMap.bindings m)

let rec add_t cx t (ts, nodes, edges) =
  match t with
  | OpenT (_, id) ->
    let c = IMap.find_unsafe id (Context.graph cx) in
    id, add_constraint cx id c (ts, nodes, edges)
  | _ ->
    match get_t t ts with
    | Some id -> id, (ts, nodes, edges)
    | None ->
      let id = Reason.mk_id () in
      let ts = put_t t id ts in
      let nodes = format_node id (t_label cx t) :: nodes in
      id, add_parts cx id (parts_of_t cx t) (ts, nodes, edges)

and add_parts cx id parts state =
  List.fold_left (fun state (label, tnode) ->
    let pid, (ts, nodes, edges) = match tnode with
      | Def t -> add_t cx t state
      | Use u -> add_use_t cx u state in
    let edge = format_contain_edge id pid ~label in
    ts, nodes, edge :: edges
  ) state parts

and parts_of_t cx = function
| OpenT _ -> assert false
| NumT _ | StrT _ | BoolT  _ -> []
| FunT (_, _, _, funtype) -> parts_of_funtype funtype
| EmptyT _ | MixedT _ | AnyT _ | NullT _ | VoidT _ -> []
| FunProtoT _ | FunProtoApplyT _ | FunProtoBindT _ | FunProtoCallT _ -> []
| PolyT (_, t) -> ["t", Def t]
| ThisClassT t -> ["this", Def t]
| BoundT _
| ExistsT _ -> []
| ObjT (_, { props_tmap; dict_t; proto_t; _ }) ->
  ("proto", Def proto_t) ::
  map_parts (Context.find_props cx props_tmap) @
  begin match dict_t with
  | None -> []
  | Some { key; value; _ } -> ["#key#", Def key; "#val#", Def value]
  end
| ArrT (_, elem, tup) -> ("elem", Def elem) :: list_parts tup
| ClassT t -> ["class", Def t]
| InstanceT (_, static, super,
  { type_args; fields_tmap; methods_tmap; _ }) ->
  map_parts type_args @
  map_parts (Context.find_props cx fields_tmap) @
  map_parts (Context.find_props cx methods_tmap) @
  ["static", Def static; "super", Def super]
| TypeT (_, t) -> ["t", Def t]
| AnnotT (sink, source) -> ["sink", Def sink; "source", Def source]
| OptionalT t
| RestT t
| AbstractT t
| EvalT (t, _, _) -> ["t", Def t]
| TypeAppT (t, args) -> ("t", Def t) :: list_parts args
| ThisTypeAppT (t, this, args) ->
  ("t", Def t) :: ("this", Def this) :: list_parts args
| ExactT (_, t)
| MaybeT t -> ["t", Def t]
| TaintT _ -> []
| IntersectionT (_, rep) -> list_parts (InterRep.members rep)
| UnionT (_, rep) -> list_parts (UnionRep.members rep)
| AnyWithLowerBoundT t | AnyWithUpperBoundT t -> ["t", Def t]
| AnyObjT _ | AnyFunT _ -> []
| ShapeT t -> ["t", Def t]
| DiffT (l, r) -> ["left", Def l; "right", Def r]
| KeysT (_, t) -> ["t", Def t]
| SingletonStrT _ | SingletonNumT _ | SingletonBoolT _ -> []
| ModuleT (_, exporttypes) -> parts_of_exporttypes cx exporttypes
| ExtendsT (nexts, l, u) -> ("l", Def l) :: ("u", Def u) :: list_parts nexts
| CustomFunT _ | ChoiceKitT _ -> []
| IdxWrapper (_, inner) -> ["inner", Def inner]
| OpenPredT (_, base, _, _) -> ["base", Def base]

and parts_of_funtype { params_tlist; params_names; return_t; _ } =
  (* OMITTED: static, prototype, this_t *)
  ("return_t", Def return_t) ::
  match params_names with
  | Some ns -> List.map2 (fun n t -> n, Def t) ns params_tlist
  | None -> list_parts params_tlist

and parts_of_exporttypes cx { exports_tmap; cjs_export; _ } =
  map_parts (Context.find_props cx exports_tmap) @
  match cjs_export with
  | Some t -> ["cjs_export", Def t]
  | None -> []

and add_use_t cx u (ts, nodes, edges) =
  match get_u u ts with
  | Some id -> id, (ts, nodes, edges)
  | None ->
    let id = Reason.mk_id () in
    let ts = put_u u id ts in
    let nodes = format_node id (use_t_label cx u) :: nodes in
    id, add_parts cx id (parts_of_use_t cx u)
      (ts, nodes, edges)

and parts_of_use_t cx = function
| UseT (_, t)
| BecomeT (_, t)
| SummarizeT (_, t) -> ["t", Def t]
| SuperT _ -> []
| MixinT (_, t) -> ["t", Def t]
| ApplyT (_, f, funtype) -> ("f", Def f) :: parts_of_funtype funtype
| BindT (_, funtype)
| CallT (_, funtype)
| MethodT (_, _, _, funtype) -> parts_of_funtype funtype
| SetPropT (_, _, t) -> ["t", Def t]
| GetPropT (_, _, out)
| TestPropT (_, _, out) -> ["out", Def out]
| SetElemT (_, ix, t) -> ["ix", Def ix; "t", Def t]
| GetElemT (_, ix, out) -> ["ix", Def ix; "out", Def out]
| ConstructorT (_, args, out) -> ("out", Def out) :: list_parts args
| AdderT (_, r, out) -> ["right", Def r; "out", Def out]
| ComparatorT (_, arg) -> ["arg", Def arg]
| ReposLowerT (_, u) -> ["upper", Use u]
| ReposUseT (_, _, l) ->  ["lower", Def l]
| PredicateT (_, out) -> ["out", Def out]
| GuardT (_, t, out) -> ["iftrue", Def t; "out", Def out]
| EqT (_, arg) -> ["arg", Def arg]
| AndT (_, r, out)
| OrT (_, r, out) -> ["right", Def r; "out", Def out]
| NotT (_, out) -> ["out", Def out]
| SpecializeT (_, _, _, args, out) -> ("out", Def out) :: list_parts args
| ThisSpecializeT (_, t, out) -> ["t", Def t; "out", Def out]
| VarianceCheckT (_, args, _) -> list_parts args
| LookupT (_, _, nexts, _, out) -> ("out", Def out) :: list_parts nexts
| UnifyT (x, y) -> ["x", Def x; "y", Def y]
| ObjAssignT (_, p, t, _, _) -> ["proto", Def p; "t", Def t]
| ObjFreezeT (_, out)
| ObjRestT (_, _, out)
| ObjSealT (_, out) -> ["out", Def out]
| ObjTestT (_, d, t) -> ["default", Def d; "out", Def t]
| ArrRestT (_, _, out) -> ["out", Def out]
| UnaryMinusT (_, out)
| GetKeysT (_, out) -> ["out", Def out]
| HasOwnPropT _ | HasPropT _ -> []
| ElemT (_, l, t, _) -> ["l", Def l; "t", Def t]
| MakeExactT (_, x) -> ["x", match x with Lower t -> Def t | Upper u -> Use u]
| ImportModuleNsT (_, out)
| ImportDefaultT (_, _, _, out)
| ImportNamedT (_, _, _, out)
| ImportTypeT (_, _, out)
| ImportTypeofT (_, _, out) -> ["out", Def out]
| AssertImportIsValueT _ -> []
| CJSRequireT (_, out) -> ["out", Def out]
| CJSExtractNamedExportsT (_, (_, exporttypes), out) ->
  ("out", Def out) :: parts_of_exporttypes cx exporttypes
| CopyNamedExportsT (_, target, out) -> ["target", Def target; "out", Def out]
| ExportNamedT (_, map, out) -> ("out", Def out) :: map_parts map
| DebugPrintT _ -> []
| TupleMapT (_, t, out)
| ReactCreateElementT (_, t, out)
| SentinelPropTestT (t, _, _, out) -> ["t", Def t; "out", Def out]
| IntersectionPreprocessKitT (_, ipt) -> parts_of_inter_preprocess_tool ipt
| ChoiceKitUseT _-> []
| IdxUnwrap (_, out)
| IdxUnMaybeifyT (_, out) -> ["out", Def out]
| CallLatentPredT (_, _, _, t, out)
| CallOpenPredT (_, _, _, t, out) -> ["t", Def t; "out", Def out]
| SubstOnPredT (_, _, t)
| RefineT (_, _, t) -> ["t", Def t]

and parts_of_inter_preprocess_tool = function
| ConcretizeTypes (unresolved, resolved, it, u) ->
  ("it", Def it) :: ("u", Use u) :: list_parts unresolved @ list_parts resolved
| SentinelPropTest (_, _, t, it, out) ->
  ["t", Def t; "it", Def it; "out", Def out]
| PropExistsTest (_, _, it, out) ->
  ["it", Def it; "out", Def out]

and add_bounds cx id { lower; upper; lowertvars; uppertvars } (ts, ns, es) =
  (* NOTE: filtering out non-immediate LB/UBs for readability, but this
     can drastically lie about graph connectedness and tvar bulk *)
  let lower = IMap.fold (fun lowertvar _ lower ->
    match Flow_js.find_graph cx lowertvar with
    | Resolved _ -> lower
    | Unresolved { lower = xlower; _ } ->
      TypeMap.fold (fun k _ acc -> TypeMap.remove k acc) xlower lower
  ) lowertvars lower in
  let ts, ns, es = TypeMap.fold (fun t _ (ts, nodes, edges) ->
    let tid, (ts, nodes, edges) = add_t cx t (ts, nodes, edges) in
    let edge = format_flow_edge ~attrs:["targetport","lower"] tid id in
    ts, nodes, edge :: edges
  ) lower (ts, ns, es) in
  let upper = IMap.fold (fun uppertvar _ upper ->
    match Flow_js.find_graph cx uppertvar with
    | Resolved _ -> upper
    | Unresolved { upper = xupper; _ } ->
      UseTypeMap.fold (fun k _ acc -> UseTypeMap.remove k acc) xupper upper
  ) uppertvars upper in
  let ts, ns, es = UseTypeMap.fold (fun t _ (ts, nodes, edges) ->
    let tid, (ts, nodes, edges) = add_use_t cx t (ts, nodes, edges) in
    let edge = format_flow_edge ~attrs:["sourceport","upper"] id tid in
    ts, nodes, edge :: edges
  ) upper (ts, ns, es) in
  (* CAUTION: edges from lowertvars to us are redundant (each duplicates
     an edge in that lowertvar, with us appearing in their uppertvars)
     *only as long as* all our lowertvars are roots. We should probably
     keep a list of generated edges instead of just blindly omitting these. *)
  ignore lowertvars;
  let ts, ns, es = IMap.fold (fun uid _ (ts, nodes, edges) ->
    let edge = format_flow_edge
      ~attrs:["sourceport", "uppertvars"; "targetport","lowertvars"] id uid in
    ts, nodes, edge :: edges
  ) uppertvars (ts, ns, es) in
  put_id id ts, ns, es

and add_root cx id _rank constraints (ts, nodes, edges) =
  let reason = pos_only_reason (Context.find_tvar_reason cx id) in
  let rstr = Reason.string_of_reason reason in
  match constraints with
  | Resolved t ->
    let label = spf "[%d] Resolved %s" id rstr in
    let node = format_tvar id label in
    let tid, (ts, nodes, edges) = add_t cx t (ts, nodes, edges) in
    let edge = format_contain_edge id tid ~label:"out" in
    put_id id ts, node :: nodes, edge :: edges
  | Unresolved b ->
    let label = spf "[%d] %s" id rstr in
    let node = format_tvar ~kids:tvar_ports id label in
    let nodes = node :: nodes in
    add_bounds cx id b (ts, nodes, edges)

and add_constraint cx id c (ts, nodes, edges) =
  match has_id id ts with
  | true -> ts, nodes, edges
  | false ->
    match c with
    | Goto gid ->
      let label = spf "[%d] Goto: %d" id gid in
      let node = format_tvar ~kids:tvar_ports id label in
      let edge = format_contain_edge id gid in
      put_id id ts, node :: nodes, edge :: edges
    | Root { rank; constraints } ->
      add_root cx id rank constraints (ts, nodes, edges)

let format cx =
  let g = Context.graph cx in
  let n = IMap.cardinal g in
  let _, nodes, edges = IMap.fold (add_constraint cx) g
    (newstate n, [], []) in
  header :: nodes @ edges @ [footer]
