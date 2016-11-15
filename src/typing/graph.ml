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

let node_of_cont = function
| Lower t -> Def t
| Upper u -> Use u

let list_parts = List.mapi (fun i t -> spf "#%d" i, Def t)
let map_parts m = List.map (fun (n, t) -> n, Def t) (SMap.bindings m)

let prop (n, p) =
  match p with
  | Field (t, Neutral) -> [n, Def t]
  | Field (t, Positive) -> ["+"^n, Def t]
  | Field (t, Negative) -> ["-"^n, Def t]
  | Get t -> ["get "^n, Def t]
  | Set t -> ["set "^n, Def t]
  | GetSet (t1, t2) -> ["get "^n, Def t1; "set "^n, Def t2]

let map_props m = SMap.bindings m |> List.map prop |> List.flatten

let lookup_action_parts = function
  | RWProp (t, Read) -> [("read", Def t)]
  | RWProp (t, Write) -> [("write", Def t)]
  | LookupProp p -> prop ("lookup", p)
  | SuperProp p -> prop ("super", p)

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
| AbstractT t -> ["t", Def t]
| AnnotT (sink, source) -> ["sink", Def sink; "source", Def source]
| AnyObjT _ | AnyFunT _ -> []
| AnyT _ -> []
| AnyWithLowerBoundT t | AnyWithUpperBoundT t -> ["t", Def t]
| ArrT (_, elem, tup) -> ("elem", Def elem) :: list_parts tup
| BoolT  _ -> []
| BoundT _ -> []
| ClassT t -> ["class", Def t]
| CustomFunT _ | ChoiceKitT _ -> []
| DiffT (l, r) -> ["left", Def l; "right", Def r]
| EmptyT _ -> []
| EvalT (t, _, _) -> ["t", Def t]
| ExactT (_, t) -> ["t", Def t]
| ExistsT _ -> []
| ExtendsT (nexts, l, u) -> ("l", Def l) :: ("u", Def u) :: list_parts nexts
| FunProtoApplyT _ -> []
| FunProtoBindT _ -> []
| FunProtoCallT _ -> []
| FunProtoT _ -> []
| FunT (_, _, _, funtype) -> parts_of_funtype funtype
| IdxWrapper (_, inner) -> ["inner", Def inner]
| InstanceT (_, static, super,
  { type_args; fields_tmap; methods_tmap; _ }) ->
  map_parts type_args @
  map_props (Context.find_props cx fields_tmap) @
  map_props (Context.find_props cx methods_tmap) @
  ["static", Def static; "super", Def super]
| IntersectionT (_, rep) -> list_parts (InterRep.members rep)
| KeysT (_, t) -> ["t", Def t]
| MaybeT t -> ["t", Def t]
| MixedT _ -> []
| ModuleT (_, exporttypes) -> parts_of_exporttypes cx exporttypes
| NullT _ -> []
| NumT _ -> []
| ObjProtoT _ -> []
| ObjT (_, { props_tmap; dict_t; proto_t; _ }) ->
  ("proto", Def proto_t) ::
  map_props (Context.find_props cx props_tmap) @
  begin match dict_t with
  | None -> []
  | Some { key; value; _ } -> ["#key#", Def key; "#val#", Def value]
  end
| OpenPredT (_, base, _, _) -> ["base", Def base]
| OptionalT t
| PolyT (_, t) -> ["t", Def t]
| RestT t
| ShapeT t -> ["t", Def t]
| SingletonBoolT _ -> []
| SingletonNumT _ -> []
| SingletonStrT _ -> []
| StrT _ -> []
| TaintT _ -> []
| ThisClassT t -> ["this", Def t]
| ThisTypeAppT (t, this, args) ->
  ("t", Def t) :: ("this", Def this) :: list_parts args
| TypeAppT (t, args) -> ("t", Def t) :: list_parts args
| TypeMapT (_, _, t1, t2) -> ["t", Def t1; "mapfn", Def t2]
| TypeT (_, t) -> ["t", Def t]
| UnionT (_, rep) -> list_parts (UnionRep.members rep)
| VoidT _ -> []

and parts_of_funtype { params_tlist; params_names; return_t; _ } =
  (* OMITTED: static, prototype, this_t *)
  ("return_t", Def return_t) ::
  match params_names with
  | Some ns -> List.map2 (fun n t -> n, Def t) ns params_tlist
  | None -> list_parts params_tlist

and parts_of_exporttypes cx { exports_tmap; cjs_export; _ } =
  map_parts (Context.find_exports cx exports_tmap) @
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
| UseT (_, t) -> ["t", Def t]
| AdderT (_, r, out) -> ["right", Def r; "out", Def out]
| AndT (_, r, out) -> ["right", Def r; "out", Def out]
| ApplyT (_, f, funtype) -> ("f", Def f) :: parts_of_funtype funtype
| ArrRestT (_, _, out) -> ["out", Def out]
| AssertArithmeticOperandT _ -> []
| AssertBinaryInLHST _ -> []
| AssertBinaryInRHST _ -> []
| AssertForInRHST _ -> []
| AssertImportIsValueT _ -> []
| BecomeT (_, t) -> ["t", Def t]
| BindT (_, funtype) -> parts_of_funtype funtype
| CallElemT (_, _, ix, ft) -> ("ix", Def ix) :: parts_of_funtype ft
| CallLatentPredT (_, _, _, t, out) -> ["t", Def t; "out", Def out]
| CallOpenPredT (_, _, _, t, out) -> ["t", Def t; "out", Def out]
| CallT (_, funtype) -> parts_of_funtype funtype
| ChoiceKitUseT _-> []
| CJSExtractNamedExportsT (_, (_, exporttypes), out) ->
    ("out", Def out) :: parts_of_exporttypes cx exporttypes
| CJSRequireT (_, out) -> ["out", Def out]
| ComparatorT (_, arg) -> ["arg", Def arg]
| ConstructorT (_, args, out) -> ("out", Def out) :: list_parts args
| CopyNamedExportsT (_, target, out) -> ["target", Def target; "out", Def out]
| DebugPrintT _ -> []
| ElemT (_, l, ReadElem t) -> ["l", Def l; "read", Def t]
| ElemT (_, l, WriteElem t) -> ["l", Def l; "write", Def t]
| ElemT (_, l, CallElem (_, ft)) -> ("l", Def l) :: parts_of_funtype ft
| EqT (_, arg) -> ["arg", Def arg]
| ExportNamedT (_, map, out) -> ("out", Def out) :: map_parts map
| GetElemT (_, ix, out) -> ["ix", Def ix; "out", Def out]
| GetKeysT (_, out) -> ["out", Def out]
| GetPropT (_, _, out) -> ["out", Def out]
| GetStaticsT (_, out) -> ["out", Def out]
| GuardT (_, t, out) -> ["iftrue", Def t; "out", Def out]
| HasOwnPropT _ | HasPropT _ -> []
| IdxUnMaybeifyT (_, out) -> ["out", Def out]
| IdxUnwrap (_, out) -> ["out", Def out]
| ImportDefaultT (_, _, _, out) -> ["out", Def out]
| ImportModuleNsT (_, out) -> ["out", Def out]
| ImportNamedT (_, _, _, out) -> ["out", Def out]
| ImportTypeofT (_, _, out) -> ["out", Def out]
| ImportTypeT (_, _, out) -> ["out", Def out]
| IntersectionPreprocessKitT (_, ipt) -> parts_of_inter_preprocess_tool ipt
| LookupT (_, _, nexts, _, action) ->
    lookup_action_parts action @ list_parts nexts
| MakeExactT (_, k) -> ["cont", node_of_cont k]
| MapTypeT (_, _, t, k) -> ["t", Def t; "cont", node_of_cont k]
| MethodT (_, _, _, funtype) -> parts_of_funtype funtype
| MixinT (_, t) -> ["t", Def t]
| NotT (_, out) -> ["out", Def out]
| ObjAssignT (_, p, t, _, _) -> ["proto", Def p; "t", Def t]
| ObjFreezeT (_, out) -> ["out", Def out]
| ObjRestT (_, _, out) -> ["out", Def out]
| ObjSealT (_, out) -> ["out", Def out]
| ObjTestT (_, d, t) -> ["default", Def d; "out", Def t]
| OrT (_, r, out) -> ["right", Def r; "out", Def out]
| PredicateT (_, out) -> ["out", Def out]
| ReactCreateElementT (_, t, out) -> ["t", Def t; "out", Def out]
| RefineT (_, _, t) -> ["t", Def t]
| ReposLowerT (_, u) -> ["upper", Use u]
| ReposUseT (_, _, l) ->  ["lower", Def l]
| SentinelPropTestT (t, _, _, out) -> ["t", Def t; "out", Def out]
| SetElemT (_, ix, t) -> ["ix", Def ix; "t", Def t]
| SetPropT (_, _, t) -> ["t", Def t]
| SpecializeT (_, _, _, args, out) -> ("out", Def out) :: list_parts args
| SubstOnPredT (_, _, t) -> ["t", Def t]
| SummarizeT (_, t) -> ["t", Def t]
| SuperT _ -> []
| TestPropT (_, _, out) -> ["out", Def out]
| ThisSpecializeT (_, t, out) -> ["t", Def t; "out", Def out]
| UnaryMinusT (_, out) -> ["out", Def out]
| UnifyT (x, y) -> ["x", Def x; "y", Def y]
| VarianceCheckT (_, args, _) -> list_parts args
| TypeAppVarianceCheckT _ -> []

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
