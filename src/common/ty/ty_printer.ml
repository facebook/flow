(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

let in_quotes ~prefer_single_quotes s =
  let quote = better_quote ~prefer_single_quotes s in
  let a = utf8_escape ~quote s in
  [Atom quote; Atom a; Atom quote]

let option ~f = Base.Option.value_map ~default:Empty ~f

let property_key_quotes_needed x =
  let regexp = Str.regexp "^[a-zA-Z\\$_][a-zA-Z0-9\\$_]*$" in
  not (Str.string_match regexp x 0)

let variance_ = function
  | Positive -> Atom "+"
  | Negative -> Atom "-"
  | Neutral -> Empty

(*************************)
(* Main Transformation   *)
(*************************)

let layout_of_elt ~prefer_single_quotes ?(size = 5000) ?(with_comments = true) ~exact_by_default elt
    =
  let size = ref size in
  let local_name_of_symbol symbol =
    let { sym_name = name; sym_provenance = provenance; _ } = symbol in
    match provenance with
    | Remote { imported_as = Some (_loc, name, _mode); _ } ->
      (* If the type is imported use the local name *)
      Reason.OrdinaryName name
    | _ -> name
  in
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
    | TSymbol s -> Atom (Reason.display_string_of_name (local_name_of_symbol s))
  in
  (* The depth parameter is useful for formatting unions: Top-level does not
     get parentheses.
  *)
  let rec type_ ~depth t =
    let depth = depth + 1 in
    count_calls ~counter:size ~default:crop_atom (fun () -> type_impl ~depth t)
  and type_impl ~depth (t : Ty.t) =
    match t with
    | Bound (_, name) -> Atom name
    | Any k -> any ~depth k
    | Top -> Atom "mixed"
    | Bot _ -> Atom "empty"
    | Void -> Atom "void"
    | Null -> Atom "null"
    | Num _ -> Atom "number"
    | Str _ -> Atom "string"
    | Bool _ -> Atom "boolean"
    | BigInt _ -> Atom "bigint"
    | Symbol -> Atom "symbol"
    | Fun func -> type_function ~depth ~sep:(fuse [pretty_space; Atom "=>"]) func
    | Obj obj -> type_object ~depth obj
    | Arr arr -> type_array ~depth arr
    | Generic g -> type_generic ~depth g
    | Union (_, t1, t2, ts) -> type_union ~depth (t1 :: t2 :: ts)
    | Inter (t1, t2, ts) -> type_intersection ~depth (t1 :: t2 :: ts)
    | Utility s -> utility ~depth s
    | IndexedAccess { _object; index; optional } ->
      let left_delim =
        if optional then
          Atom "?.["
        else
          Atom "["
      in
      fuse [type_ ~depth _object; left_delim; type_ ~depth index; Atom "]"]
    | Tup elements ->
      let tuple_element ~depth = function
        | TupleElement { name; t; polarity; optional } ->
          fuse
            [
              variance_ polarity;
              (match name with
              | Some id ->
                fuse
                  [
                    identifier (Reason.OrdinaryName id);
                    ( if optional then
                      Atom "?"
                    else
                      Empty
                    );
                    Atom ":";
                    pretty_space;
                  ]
              | None -> Empty);
              type_ ~depth t;
            ]
        | TupleSpread { name; t } ->
          fuse
            [
              Atom "...";
              (match name with
              | Some name -> fuse [identifier (Reason.OrdinaryName name); Atom ":"; pretty_space]
              | None -> Empty);
              type_ ~depth t;
            ]
      in
      list
        ~wrap:(Atom "[", Atom "]")
        ~sep:(Atom ",")
        ~trailing:false
        (counted_map (tuple_element ~depth) elements)
    | StrLit raw -> fuse (in_quotes ~prefer_single_quotes (Reason.display_string_of_name raw))
    | NumLit raw -> Atom raw
    | BoolLit value ->
      Atom
        ( if value then
          "true"
        else
          "false"
        )
    | BigIntLit raw -> Atom raw
    | InlineInterface { if_extends; if_props; if_dict } ->
      type_interface ~depth if_extends if_props if_dict
    | TypeOf (pv, targs) ->
      fuse [Atom "typeof"; space; builtin_value pv; option ~f:(type_args ~depth) targs]
    | CharSet s ->
      fuse [Atom "$CharSet"; Atom "<"; fuse (in_quotes ~prefer_single_quotes s); Atom ">"]
    | Conditional { check_type; extends_type; true_type; false_type } ->
      group
        [
          fuse
            [
              type_with_parens ~depth check_type;
              space;
              Atom "extends";
              space;
              type_with_parens ~depth extends_type;
            ];
          Indent
            (fuse
               [
                 pretty_line;
                 Atom "?";
                 pretty_space;
                 type_ ~depth true_type;
                 pretty_line;
                 Atom ":";
                 pretty_space;
                 type_ ~depth false_type;
               ]
            );
        ]
    | Infer ({ sym_name = name; _ }, b) ->
      fuse
        [
          Atom "infer";
          space;
          fuse
            [
              identifier name;
              option ~f:(fun t -> fuse [space; Atom "extends"; space; type_ ~depth t]) b;
            ];
        ]
    | Renders (t, variant) ->
      let renders_str =
        match variant with
        | RendersNormal -> "renders"
        | RendersMaybe -> "renders?"
        | RendersStar -> "renders*"
      in
      fuse [Atom renders_str; space; type_ ~depth t]
  and type_generic ~depth (s, _, targs) =
    let name = identifier (local_name_of_symbol s) in
    type_reference ~depth name targs
  and type_reference ~depth name targs =
    let targs = option ~f:(type_args ~depth) targs in
    fuse [name; targs]
  and type_args ~depth targs =
    list ~wrap:(Atom "<", Atom ">") ~sep:(Atom ",") (counted_map (type_ ~depth) targs)
  and identifier name = Atom (Reason.display_string_of_name name)
  and any ~depth kind =
    let kind =
      match kind with
      | Annotated _ -> "explicit"
      | Recursive -> "recursive"
      | Placeholder -> "placeholder"
      | AnyError _
      | Unsound _
      | Untyped ->
        "implicit"
    in
    fuse
      [
        Atom "any";
        ( if depth = 1 && with_comments then
          fuse [pretty_space; Atom kind |> wrap_in_parens]
        else
          Empty
        );
      ]
  and type_interface ~depth extends props dict =
    let extends =
      match extends with
      | [] -> Empty
      | _ ->
        fuse_with_space
          [Atom "extends"; list ~sep:(Atom ",") (Base.List.map ~f:(type_generic ~depth) extends)]
    in
    let properties = counted_map (type_object_property ~depth) props in
    let properties =
      match dict with
      | Some d -> type_dict ~depth d :: properties
      | _ -> properties
    in
    let body = list ~wrap:(Atom "{", Atom "}") ~sep:(Atom ";") ~trailing:false properties in
    fuse_with_space [Atom "interface"; extends; body]
  and type_function
      ~depth
      ~sep
      { fun_params; fun_rest_param; fun_return; fun_type_params; fun_static = _; fun_hook } =
    let params = counted_map (type_function_param ~depth) fun_params in
    let params =
      match fun_rest_param with
      | Some (name, t) ->
        params @ [fuse [Atom "..."; type_function_param ~depth (name, t, { prm_optional = false })]]
      | None -> params
    in
    fuse
      (( if fun_hook then
         [Atom "hook"; pretty_space]
       else
         []
       )
      @ [
          option ~f:(type_parameter ~depth) fun_type_params;
          list ~wrap:(Atom "(", Atom ")") ~sep:(Atom ",") ~trailing:false params;
          sep;
          pretty_space;
          return_t ~depth fun_return;
        ]
      )
  and type_function_param ~depth (name, annot, { prm_optional }) =
    fuse
      [
        begin
          match name with
          | Some id ->
            fuse
              [
                identifier (Reason.OrdinaryName id);
                ( if prm_optional then
                  Atom "?"
                else
                  Empty
                );
                Atom ":";
                pretty_space;
              ]
          | None -> Empty
        end;
        type_ ~depth annot;
      ]
  and return_t ~depth t =
    match t with
    | ReturnType t -> type_ ~depth t
    | TypeGuard (x, t) -> fuse_with_space [Atom x; Atom "is"; type_ ~depth t]
  and type_object_property =
    let to_key x =
      if property_key_quotes_needed x then
        let quote = better_quote ~prefer_single_quotes x in
        fuse [Atom quote; Atom (utf8_escape ~quote x); Atom quote]
      else
        identifier (Reason.OrdinaryName x)
    in
    Ty.(
      fun ~depth prop ->
        match prop with
        | NamedProp { name = key; prop = named_prop; _ } -> begin
          match named_prop with
          | Field { t; polarity; optional } ->
            fuse
              [
                variance_ polarity;
                to_key (Reason.display_string_of_name key);
                ( if optional then
                  Atom "?"
                else
                  Empty
                );
                Atom ":";
                pretty_space;
                type_ ~depth t;
              ]
          | Method func ->
            fuse
              [
                to_key (Reason.display_string_of_name key); type_function ~depth ~sep:(Atom ":") func;
              ]
          | Get t ->
            group
              [
                Atom "get";
                space;
                to_key (Reason.display_string_of_name key);
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
                to_key (Reason.display_string_of_name key);
                wrap_and_indent (Atom "(", Atom ")") [type_ ~depth t];
                Atom ":";
                pretty_space;
                type_ ~depth Void;
              ]
        end
        | CallProp func -> fuse [type_function ~depth ~sep:(Atom ":") func]
        | SpreadProp t -> fuse [Atom "..."; type_ ~depth t]
        | MappedTypeProp
            {
              key_tparam = { tp_name; _ };
              source;
              prop;
              flags = { polarity; optional };
              homomorphic;
            } ->
          let optional_modifier =
            match optional with
            | KeepOptionality -> Empty
            | RemoveOptional -> Atom "-?"
            | MakeOptional -> Atom "?"
          in
          fuse
            [
              variance_ polarity;
              Atom "[";
              Atom tp_name;
              Atom " in ";
              (match homomorphic with
              | Homomorphic -> Atom "keyof "
              | SemiHomomorphic _
              | Unspecialized ->
                Empty);
              type_ ~depth source;
              Atom "]";
              optional_modifier;
              Atom ":";
              pretty_space;
              type_ ~depth prop;
            ]
    )
  and type_array ~depth { arr_readonly; arr_literal; arr_elt_t } =
    let arr =
      if arr_readonly then
        "$ReadOnlyArray"
      else
        match arr_literal with
        | Some true -> "$TEMPORARY$array"
        | Some false
        | None ->
          "Array"
    in
    fuse [Atom arr; Atom "<"; type_ ~depth arr_elt_t; Atom ">"]
  and type_dict ~depth { dict_polarity; dict_name; dict_key; dict_value } =
    fuse
      [
        variance_ dict_polarity;
        Atom "[";
        begin
          match dict_name with
          | Some id -> fuse [identifier (Reason.OrdinaryName id); Atom ":"; pretty_space]
          | None -> Empty
        end;
        type_ ~depth dict_key;
        Atom "]";
        Atom ":";
        pretty_space;
        type_ ~depth dict_value;
      ]
  and type_object ~depth ?(sep = Atom ",") obj =
    let { obj_kind; obj_props; obj_literal; _ } = obj in
    let s_exact =
      if obj_kind = ExactObj && not exact_by_default then
        Atom "|"
      else
        Empty
    in
    let props = counted_map (type_object_property ~depth) obj_props in
    let props =
      match obj_kind with
      | IndexedObj d -> type_dict ~depth d :: props
      | InexactObj -> props @ [Atom "..."]
      | ExactObj -> props
      | MappedTypeObj -> props
    in
    let o =
      list ~wrap:(fuse [Atom "{"; s_exact], fuse [s_exact; Atom "}"]) ~sep ~trailing:false props
    in
    match obj_literal with
    | Some true -> fuse [Atom "$TEMPORARY$object"; Atom "<"; o; Atom ">"]
    | Some false
    | None ->
      o
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
    | Inter _
    | Conditional _ ->
      wrap_in_parens (type_ ~depth t)
    | _ -> type_ ~depth t
  and enum_decl s = fuse [Atom "enum"; space; identifier (local_name_of_symbol s)]
  and utility ~depth u =
    let ctor = Ty.string_of_utility_ctor u in
    let ts = Ty.types_of_utility u in
    type_reference ~depth (identifier (Reason.OrdinaryName ctor)) ts
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
        option ~f:(type_annotation ~depth) tp_bound;
        begin
          match tp_default with
          | Some t -> fuse [pretty_space; Atom "="; pretty_space; type_ ~depth t]
          | None -> Empty
        end;
      ]
  and type_annotation ~depth t = fuse [Atom ":"; pretty_space; type_ ~depth t] in

  let class_decl ~depth s typeParameters =
    fuse
      [
        Atom "class";
        space;
        identifier (local_name_of_symbol s);
        option ~f:(type_parameter ~depth) typeParameters;
      ]
  in
  let interface_decl ~depth s typeParameters =
    fuse
      [
        Atom "interface";
        space;
        identifier (local_name_of_symbol s);
        option ~f:(type_parameter ~depth) typeParameters;
      ]
  in
  let nominal_component_decl ~depth s typeParameters is_type =
    let base =
      [
        Atom "component";
        space;
        identifier (local_name_of_symbol s);
        option ~f:(type_parameter ~depth) typeParameters;
      ]
    in
    if is_type then
      fuse (Atom "element" :: space :: Atom "of" :: space :: base)
    else
      fuse base
  in
  let type_alias ~depth name tparams t_opt =
    let { sym_name = name; _ } = name in
    let tparams = option ~f:(type_parameter ~depth) tparams in
    let body =
      match t_opt with
      | Some t -> fuse [pretty_space; Atom "="; pretty_space; type_ ~depth t]
      | None -> Empty
    in
    fuse [Atom "type"; space; identifier name; tparams; body]
  in
  let variable_decl ~depth name t =
    fuse
      [Atom "declare"; space; Atom "var"; space; identifier name; Atom ":"; space; type_ ~depth t]
  in
  let module_ ~depth:_ name =
    let name =
      match name with
      | Some name ->
        fuse
          (space :: in_quotes ~prefer_single_quotes (Reason.display_string_of_name name.Ty.sym_name))
      | None -> Empty
    in
    fuse [Atom "module"; name]
  in
  let decl ~depth = function
    | VariableDecl (name, t) -> variable_decl ~depth name t
    | TypeAliasDecl { name; tparams; type_; _ } -> type_alias ~depth name tparams type_
    | ClassDecl (s, ps) -> class_decl ~depth s ps
    | InterfaceDecl (s, ps) -> interface_decl ~depth s ps
    | EnumDecl n -> enum_decl n
    | NominalComponentDecl { name; tparams; is_type } ->
      nominal_component_decl ~depth name tparams is_type
    | ModuleDecl { name; exports = _; default = _ } -> module_ ~depth name
  in
  match elt with
  | Type t -> type_ ~depth:0 t
  | Decl d -> decl ~depth:0 d

let layout_of_type_at_pos_result
    ~prefer_single_quotes ?size ?with_comments ~exact_by_default { Ty.unevaluated; evaluated } =
  let layout_unevaluated =
    layout_of_elt ~prefer_single_quotes ?size ?with_comments ~exact_by_default unevaluated
  in
  match (unevaluated, evaluated) with
  | (_, None) -> layout_unevaluated
  | (unevaluated, Some evaluated) when Ty_utils.elt_equal unevaluated evaluated ->
    layout_unevaluated
  | (_, Some evaluated) ->
    let layout_evaluated =
      let evaluated =
        match evaluated with
        | Decl (TypeAliasDecl { type_ = Some t; _ }) -> Type t
        | x -> x
      in
      layout_of_elt ~prefer_single_quotes ?size ?with_comments ~exact_by_default evaluated
    in
    fuse [layout_unevaluated; hardline; Atom "="; space; layout_evaluated]

(* Same as Compact_printer with the exception of locations *)
let print_single_line ~source_maps node =
  let rec print_node src = function
    (* this printer does not output locations *)
    | SourceLocation _ -> src
    | Newline -> Source.add_space 1 src
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

let print_pretty ~source_maps node = Pretty_printer.print ~source_maps ~skip_endline:true node

let string_of_elt
    ?(prefer_single_quotes = false) ?(with_comments = true) (elt : Ty.elt) ~exact_by_default :
    string =
  layout_of_elt ~prefer_single_quotes ~with_comments ~exact_by_default elt
  |> print_pretty ~source_maps:None
  |> Source.contents

let string_of_elt_single_line
    ?(prefer_single_quotes = false) ?(with_comments = true) ?(exact_by_default = true) (elt : Ty.elt)
    =
  layout_of_elt ~prefer_single_quotes ~with_comments ~exact_by_default elt
  |> print_single_line ~source_maps:None
  |> Source.contents

let string_of_t
    ?(prefer_single_quotes = false) ?(with_comments = true) ?(exact_by_default = true) (ty : Ty.t) =
  string_of_elt ~prefer_single_quotes ~with_comments ~exact_by_default (Ty.Type ty)

let string_of_t_single_line
    ?(prefer_single_quotes = false) ?(with_comments = true) ?(exact_by_default = true) (ty : Ty.t) =
  string_of_elt_single_line ~prefer_single_quotes ~with_comments ~exact_by_default (Ty.Type ty)

let string_of_decl_single_line
    ?(prefer_single_quotes = false) ?(with_comments = true) ?(exact_by_default = true) (d : Ty.decl)
    =
  string_of_elt_single_line ~prefer_single_quotes ~with_comments ~exact_by_default (Ty.Decl d)

let string_of_type_at_pos_result
    ?(prefer_single_quotes = false) ?(with_comments = true) ?(exact_by_default = true) result =
  layout_of_type_at_pos_result ~prefer_single_quotes ~with_comments ~exact_by_default result
  |> print_pretty ~source_maps:None
  |> Source.contents
