(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Layout
open Utils_js
open Ty

(***********)
(* Utils   *)
(***********)

let varname n = spf "V$%d" n

let crop_symbol = "..."

let crop_atom = Atom crop_symbol

(* from Js_layout_generator *)
let utf8_escape = Js_layout_generator.utf8_escape

let better_quote = Js_layout_generator.better_quote

let wrap_in_parens = Js_layout_generator.wrap_in_parens

let with_semicolon = Js_layout_generator.with_semicolon

let in_quotes s =
  let quote = better_quote s in
  let a = utf8_escape ~quote s in
  [Atom quote; Atom a; Atom quote]

let option ~f = Option.value_map ~default:Empty ~f

let property_key_quotes_needed x =
  let regexp = Str.regexp "^[a-zA-Z\\$_][a-zA-Z0-9\\$_]*$" in
  not (Str.string_match regexp x 0)

(*************************)
(* Main Transformation   *)
(*************************)

let type_ ?(size = 5000) ?(with_comments = true) t =
  let env_map : Layout.layout_node IMap.t ref = ref IMap.empty in
  let size = ref size in
  (* util to limit the number of calls to a (usually recursive) function *)
  let counted_map f xs =
    let rec type_list_aux acc xs_ =
      if !size = 0 then
        crop_atom :: acc
      else
        match xs_ with
        | [] -> acc
        | y :: ys -> type_list_aux (f y :: acc) ys
    in
    type_list_aux [] xs |> List.rev
  in
  let builtin_value = function
    | FunProto -> Atom "Function.prototype"
    | ObjProto -> Atom "Object.prototype"
    | FunProtoApply -> Atom "Function.prototype.apply"
    | FunProtoBind -> Atom "Function.prototype.bind"
    | FunProtoCall -> Atom "Function.prototype.call"
  in
  (* The depth parameter is useful for formatting unions: Top-level does not
     get parentheses.
  *)
  let rec type_ ~depth t =
    let depth = depth + 1 in
    count_calls ~counter:size ~default:crop_atom (fun () -> type_impl ~depth t)
  and type_impl ~depth (t : Ty.t) =
    match t with
    | TVar (v, targs) -> type_reference ~depth (type_var v) targs
    | Bound (_, name) -> Atom name
    | Any k -> any ~depth k
    | Top -> Atom "mixed"
    | Bot _ -> Atom "empty"
    | Void -> Atom "void"
    | Null -> Atom "null"
    | Num _ -> Atom "number"
    | Str _ -> Atom "string"
    | Bool _ -> Atom "boolean"
    | Symbol -> Atom "symbol"
    | Fun func -> type_function ~depth ~sep:(fuse [pretty_space; Atom "=>"]) func
    | Obj obj -> type_object ~depth obj
    | Arr arr -> type_array ~depth arr
    | Generic g -> type_generic ~depth g
    | Union (t1, t2, ts) -> type_union ~depth (t1 :: t2 :: ts)
    | Inter (t1, t2, ts) -> type_intersection ~depth (t1 :: t2 :: ts)
    | ClassDecl (n, ps) -> class_decl ~depth n ps
    | InterfaceDecl (n, ps) -> interface_decl ~depth n ps
    | EnumDecl n -> enum_decl n
    | Utility s -> utility ~depth s
    | Tup ts ->
      list
        ~wrap:(Atom "[", Atom "]")
        ~sep:(Atom ",")
        ~trailing:false
        (counted_map (type_ ~depth) ts)
    | StrLit raw -> fuse (in_quotes raw)
    | NumLit raw -> Atom raw
    | BoolLit value ->
      Atom
        ( if value then
          "true"
        else
          "false" )
    | TypeAlias ta -> type_alias ta
    | InlineInterface { if_extends; if_body } -> type_interface ~depth if_extends if_body
    | TypeOf pv -> fuse [Atom "typeof"; space; builtin_value pv]
    | Module (sym, { cjs_export; exports }) -> module_t ~depth sym exports cjs_export
    | Mu (i, t) ->
      let t = type_ ~depth:0 t in
      env_map := IMap.add i t !env_map;
      Atom (varname i)
  and export ~depth (name, t) = fuse [identifier name; Atom ":"; space; type_ ~depth t]
  and module_t ~depth sym exports cjs_export =
    let name =
      match sym with
      | Some { name; _ } -> fuse [space; identifier name]
      | None -> Empty
    in
    let cjs_name = "exports" in
    let exports =
      Option.value_map ~f:(fun cjs -> (cjs_name, cjs) :: exports) ~default:exports cjs_export
    in
    fuse
      [
        Atom "module";
        name;
        Atom ":";
        space;
        list ~wrap:(Atom "{", Atom "}") ~sep:(Atom ",") (counted_map (export ~depth) exports);
      ]
  and type_var (RVar i) = Atom (varname i)
  and type_generic ~depth g =
    let ({ name; _ }, _, targs) = g in
    let name = identifier name in
    type_reference ~depth name targs
  and type_reference ~depth name targs =
    let targs = option (type_args ~depth) targs in
    fuse [name; targs]
  and type_args ~depth targs =
    list ~wrap:(Atom "<", Atom ">") ~sep:(Atom ",") (counted_map (type_ ~depth) targs)
  and identifier name = Atom name
  and any ~depth kind =
    let kind =
      match kind with
      | Annotated -> "explicit"
      | _ -> "implicit"
    in
    fuse
      [
        Atom "any";
        ( if depth = 1 && with_comments then
          fuse [pretty_space; Atom kind |> wrap_in_parens]
        else
          Empty );
      ]
  and type_alias { ta_name = { name; _ }; ta_tparams; ta_type } =
    fuse
      ( [Atom "type"; space; identifier name; option (type_parameter ~depth:0) ta_tparams]
      @ Option.value_map ta_type ~default:[] ~f:(fun t ->
            [pretty_space; Atom "="; pretty_space; type_ ~depth:0 t]) )
  and type_interface ~depth extends body =
    let extends =
      match extends with
      | [] -> Empty
      | _ ->
        fuse_with_space
          [Atom "extends"; list ~sep:(Atom ",") (Base.List.map ~f:(type_generic ~depth) extends)]
    in
    let body = type_object ~depth body in
    fuse_with_space [Atom "interface"; extends; body]
  and type_function ~depth ~sep { fun_params; fun_rest_param; fun_return; fun_type_params } =
    let params = counted_map (type_function_param ~depth) fun_params in
    let params =
      match fun_rest_param with
      | Some (name, t) ->
        params @ [fuse [Atom "..."; type_function_param ~depth (name, t, { prm_optional = false })]]
      | None -> params
    in
    fuse
      [
        option (type_parameter ~depth) fun_type_params;
        list ~wrap:(Atom "(", Atom ")") ~sep:(Atom ",") ~trailing:false params;
        sep;
        pretty_space;
        type_ ~depth fun_return;
      ]
  and type_function_param ~depth (name, annot, { prm_optional }) =
    fuse
      [
        begin
          match name with
          | Some id ->
            fuse
              [
                identifier id;
                ( if prm_optional then
                  Atom "?"
                else
                  Empty );
                Atom ":";
                pretty_space;
              ]
          | None -> Empty
        end;
        type_ ~depth annot;
      ]
  and type_object_property =
    let to_key x =
      if property_key_quotes_needed x then
        let quote = better_quote x in
        fuse [Atom quote; Atom (utf8_escape ~quote x); Atom quote]
      else
        identifier x
    in
    Ty.(
      fun ~depth prop ->
        match prop with
        | NamedProp (key, named_prop) ->
          begin
            match named_prop with
            | Field (t, { fld_polarity; fld_optional }) ->
              fuse
                [
                  variance_ fld_polarity;
                  to_key key;
                  ( if fld_optional then
                    Atom "?"
                  else
                    Empty );
                  Atom ":";
                  pretty_space;
                  type_ ~depth t;
                ]
            | Method func -> fuse [to_key key; type_function ~depth ~sep:(Atom ":") func]
            | Get t ->
              group
                [
                  Atom "get";
                  space;
                  to_key key;
                  Atom "(";
                  softline;
                  Atom ")";
                  Atom ":";
                  pretty_space;
                  type_ ~depth t;
                ]
            | Set t ->
              group
                [
                  Atom "set";
                  space;
                  to_key key;
                  wrap_and_indent (Atom "(", Atom ")") [type_ ~depth t];
                  Atom ":";
                  pretty_space;
                  type_ ~depth Void;
                ]
          end
        | IndexProp { dict_polarity; dict_name; dict_key; dict_value } ->
          fuse
            [
              variance_ dict_polarity;
              Atom "[";
              begin
                match dict_name with
                | Some id -> fuse [identifier id; Atom ":"; pretty_space]
                | None -> Empty
              end;
              type_ ~depth dict_key;
              Atom "]";
              Atom ":";
              pretty_space;
              type_ ~depth dict_value;
            ]
        | CallProp func -> fuse [type_function ~depth ~sep:(Atom ":") func]
        | SpreadProp t -> fuse [Atom "..."; type_ ~depth t])
  and type_array ~depth { arr_readonly; arr_literal = _; arr_elt_t } =
    fuse
      [
        Atom
          ( if arr_readonly then
            "$ReadOnlyArray"
          else
            "Array" );
        Atom "<";
        type_ ~depth arr_elt_t;
        Atom ">";
      ]
  and type_object ~depth ?(sep = Atom ",") { obj_exact; obj_props; _ } =
    let s_exact =
      if obj_exact then
        Atom "|"
      else
        Empty
    in
    list
      ~wrap:(fuse [Atom "{"; s_exact], fuse [s_exact; Atom "}"])
      ~sep
      ~trailing:false
      (counted_map (type_object_property ~depth) obj_props)
  and type_union ~depth ts =
    let (prefix, ts) =
      if List.mem Null ts && List.mem Void ts then
        let ts = List.filter (fun t -> t <> Null && t <> Void) ts in
        let ts =
          match ts with
          | [] -> [Bot EmptyType]
          | _ -> ts
        in
        (Atom "?", ts)
      else
        (Empty, ts)
    in
    let wrap =
      if depth > 1 && List.length ts > 1 then
        Some (Atom "(", Atom ")")
      else
        None
    in
    let elts = Base.List.intersperse (counted_map (type_with_parens ~depth) ts) ~sep:(Atom "|") in
    fuse [prefix; list ?wrap ~inline:(false, true) elts]
  and type_intersection ~depth ts =
    let wrap =
      if depth > 1 then
        Some (Atom "(", Atom ")")
      else
        None
    in
    let elts = Base.List.intersperse (counted_map (type_with_parens ~depth) ts) ~sep:(Atom "&") in
    list ?wrap ~inline:(false, true) elts
  and type_with_parens ~depth t =
    match t with
    | Fun _
    | Union _
    | Inter _ ->
      wrap_in_parens (type_ ~depth t)
    | _ -> type_ ~depth t
  and class_decl ~depth { name; _ } typeParameters =
    fuse [Atom "class"; space; identifier name; option (type_parameter ~depth) typeParameters]
  and interface_decl ~depth { name; _ } typeParameters =
    fuse [Atom "interface"; space; identifier name; option (type_parameter ~depth) typeParameters]
  and enum_decl { name; _ } = fuse [Atom "enum"; space; identifier name]
  and utility ~depth u =
    let ctor = Ty.string_of_utility_ctor u in
    let ts = Ty.types_of_utility u in
    type_reference ~depth (identifier ctor) ts
  and type_parameter ~depth params =
    list
      ~wrap:(Atom "<", Atom ">")
      ~sep:(Atom ",")
      ~trailing:false
      (counted_map (type_param ~depth) params)
  and type_param ~depth { tp_name; tp_bound; tp_polarity; tp_default } =
    fuse
      [
        variance_ tp_polarity;
        Atom tp_name;
        option (type_annotation ~depth) tp_bound;
        begin
          match tp_default with
          | Some t -> fuse [pretty_space; Atom "="; pretty_space; type_ ~depth t]
          | None -> Empty
        end;
      ]
  and type_annotation ~depth t = fuse [Atom ":"; pretty_space; type_ ~depth t]
  and variance_ = function
    | Positive -> Atom "+"
    | Negative -> Atom "-"
    | Neutral -> Empty
  in
  let env_ (i, layout) =
    with_semicolon
      (fuse [Atom "type"; space; Atom (varname i); pretty_space; Atom "="; pretty_space; layout])
  in
  (* Main call *)
  let type_layout = type_ ~depth:0 t in
  (* Run type_ first so that env_map has been populated *)
  let env_layout = Base.List.map ~f:env_ (IMap.bindings !env_map) in
  Layout.(join Newline (env_layout @ [type_layout]))

(* Same as Compact_printer with the exception of:
   - IfPretty to allow spaces after punctuation.
   We still maintain a single line format.
*)
let print ~force_single_line ~source_maps node =
  let rec print_node src = function
    (* this printer does not output locations *)
    | SourceLocation _ -> src
    | Newline ->
      if force_single_line then
        Source.add_space 1 src
      else
        Source.add_newline src
    | Indent node -> print_node src node
    | IfPretty (node, _) -> print_node src node
    | Concat nodes
    | Group nodes
    | Sequence (_, nodes) ->
      List.fold_left print_node src nodes
    | Atom s -> Source.add_string s src
    | Identifier (loc, s) -> Source.add_identifier loc s src
    | IfBreak (_, no_break) -> print_node src no_break
    | Empty -> src
  in
  print_node (Source.create ~source_maps ()) node

let string_of_t ?(force_single_line = false) ?(with_comments = true) (ty : Ty.t) : string =
  print ~force_single_line ~source_maps:None (type_ ~with_comments ty) |> Source.contents
