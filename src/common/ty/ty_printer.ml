(**
 * Copyright (c) 2013-present, Facebook, Inc.
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

(*************************)
(* Main Transformation   *)
(*************************)

let type_ ?(size=5000) t =

  let env_map: (Layout.layout_node IMap.t) ref = ref IMap.empty in
  let size = ref size in


  (* util to limit the number of calls to a (usually recursive) function *)
  let counted_map f xs =
    let rec type_list_aux acc xs_ =
      if !size = 0 then
        crop_atom::acc
      else begin
        match xs_ with
        | [] -> acc
        | y::ys -> type_list_aux (f y :: acc) ys
      end
    in
    type_list_aux [] xs |> List.rev
  in

  (* The depth parameter is useful for formatting unions: Top-level does not
     get parentheses.
  *)
  let rec type_ ~depth t =
    let depth = depth + 1 in
    count_calls ~counter:size ~default:crop_atom (fun () ->
      type_impl ~depth t
    )

  and type_impl ~depth (t: Ty.t) =
    match t with
    | TVar v -> type_var v
    | Any -> Atom "any"
    | AnyObj -> Atom "Object"
    | AnyFun -> Atom "Function"
    | Top -> Atom "mixed"
    | Bot -> Atom "empty"
    | Void -> Atom "void"
    | Null -> Atom "null"
    | Num -> Atom "number"
    | Str -> Atom "string"
    | Bool -> Atom "boolean"
    | Fun func ->
      type_function ~depth
        ~sep:(fuse [pretty_space; Atom "=>"])
        func
    | Obj obj -> type_object ~depth obj
    | Arr t -> fuse [type_ ~depth t; Atom "[]"]
    | Generic (n, _, ts) -> type_generic ~depth n ts
    | Union (t1, t2, ts) ->
      type_union ~depth  (t1::t2::ts)
    | Inter (t1, t2, ts) ->
      type_intersection ~depth (t1::t2::ts)
    | Class (n, s, ps) -> type_class ~depth n s ps
    | Tup ts ->
      list
        ~wrap:(Atom "[", Atom "]")
        ~sep:(Atom ",")
        ~trailing:false
        (counted_map (type_ ~depth) ts)
    | StrLit raw -> fuse (in_quotes raw)
    | NumLit raw -> Atom raw
    | BoolLit value -> Atom (if value then "true" else "false")
    | Exists -> Atom "*"
    | This -> Atom "this"
    | TypeAlias ta -> type_alias ta
    | TypeOf n -> fuse [Atom "typeof"; space; identifier n]
    | Mu (i, t) ->
      let t = type_ ~depth:0 t in
      env_map := IMap.add i t !env_map;
      Atom (varname i)

  and type_var = function
    | RVar i -> Atom (varname i)
    | TParam s -> Atom s

  and type_generic ~depth id typeParameters =
    fuse [
      identifier id;
      option (type_parameter_instantiation ~depth) typeParameters;
    ]

  and type_parameter_instantiation ~depth params =
    list
      ~wrap:(Atom "<", Atom ">")
      ~sep:(Atom ",")
      (counted_map (type_ ~depth) params)

  and identifier name = Atom name

  and type_alias { ta_name; ta_imported; ta_tparams; ta_type } =
    match ta_imported with
    | Some file -> fuse  @@ [
        Atom "imported"; space;
        identifier ta_name; space;
        Atom "from"; space;
      ]
      @ in_quotes file

    | None -> fuse @@ [
        Atom "type"; space;
        identifier ta_name;
        option (type_parameter ~depth:0) ta_tparams;
      ]
      @ Option.value_map ta_type ~default:[] ~f:(fun t -> [
          pretty_space; Atom "="; pretty_space; type_ ~depth:0 t
        ])

  and type_function ~depth ~sep
    { fun_params; fun_rest_param; fun_return; fun_type_params } =
    let params = counted_map (type_function_param ~depth) fun_params in
    let params = match fun_rest_param with
    | Some (name, t) -> params @ [ fuse [
          Atom "...";
          type_function_param ~depth (name, t, { prm_optional = false })
        ]
      ]
    | None -> params
    in
    fuse [
      option (type_parameter ~depth) fun_type_params;
      list
        ~wrap:(Atom "(", Atom ")")
        ~sep:(Atom ",")
        ~trailing:false
        params;
      sep;
      pretty_space;
      type_ ~depth fun_return;
    ]

  and type_function_param ~depth (name, typeAnnotation, { prm_optional }) =
    fuse [
      begin match name with
      | Some id -> fuse [
          identifier id;
          if prm_optional then Atom "?" else Empty;
          Atom ":";
          pretty_space;
        ]
      | None -> Empty
      end;
      type_ ~depth typeAnnotation;
    ]

  and type_object_property ~depth =
    let open Ty in
    function
    | NamedProp (key, named_prop) -> begin
      match named_prop with
      | Field (t, { fld_polarity; fld_optional }) ->
        fuse [
          variance_ fld_polarity;
          identifier key;
          if fld_optional then Atom "?" else Empty;
          Atom ":";
          pretty_space;
          type_ ~depth t
        ]

      | Method func -> fuse [
          identifier key;
          type_function ~depth ~sep:(Atom ":") func;
        ]

      | Get t -> fuse [
          Atom "get"; space;
          identifier key;
          type_ ~depth t;
        ]

      | Set t -> fuse [
          Atom "set"; space;
          identifier key;
          type_ ~depth t;
        ]
      end

    | IndexProp { dict_polarity; dict_name; dict_key; dict_value } ->
      fuse [
        variance_ dict_polarity;
        Atom "[";
        begin match dict_name with
        | Some id -> fuse [
            identifier id; Atom ":"; pretty_space;
          ]
        | None -> Empty
        end;
        type_ ~depth dict_key;
        Atom "]"; Atom ":"; pretty_space;
        type_ ~depth dict_value;
      ]
    | CallProp func -> fuse [
        type_function ~depth ~sep:(Atom ":") func
      ]

  and type_object ~depth ?(sep=(Atom ",")) { obj_exact; obj_props } =
    let s_exact = if obj_exact then Atom "|" else Empty in
    list
      ~wrap:(fuse [Atom "{"; s_exact], fuse [s_exact; Atom "}"])
      ~sep
      ~trailing:false
      (counted_map (type_object_property ~depth) obj_props)

  and type_union ~depth ts =
    let prefix, ts =
      if List.mem Null ts && List.mem Void ts then
        let ts = List.filter (fun t -> t <> Null && t <> Void) ts in
        (Atom "?", ts)
      else
        (Empty, ts)
    in
    let wrap =
      if depth > 1 && List.length ts > 1
        then Some (Atom "(", Atom ")")
        else None
    in
    let elts = Core_list.intersperse (
      counted_map (type_with_parens ~depth) ts
    ) ~sep:(Atom "|")
    in
    fuse [prefix; list ?wrap ~inline:(false, true) elts]

  and type_intersection ~depth ts =
    let wrap = if depth > 1 then Some (Atom "(", Atom ")") else None in
    let elts = Core_list.intersperse (
      counted_map (type_with_parens ~depth) ts
    ) ~sep:(Atom "&")
    in
    list ?wrap ~inline:(false, true) elts

  and type_with_parens ~depth t =
    match t with
    | Fun _
    | Union _
    | Inter _ -> wrap_in_parens (type_ ~depth t)
    | _ -> type_ ~depth t

  and type_class ~depth id structural typeParameters = fuse [
    Atom (if structural then "interface" else "class");
    space;
    identifier id;
    option (type_parameter ~depth) typeParameters;
  ]

  and type_parameter ~depth params =
    list
      ~wrap:(Atom "<", Atom ">")
      ~sep:(Atom ",")
      ~trailing:false
      (counted_map (type_param ~depth) params)

  and type_param ~depth { tp_name; tp_bound; tp_polarity; tp_default } =
    fuse [
      variance_ tp_polarity;
      Atom tp_name;
      option (type_annotation ~depth) tp_bound;
      begin match tp_default with
      | Some t -> fuse [
          pretty_space;
          Atom "=";
          pretty_space;
          type_ ~depth t;
        ]
      | None -> Empty
      end;
    ]

  and type_annotation ~depth t =
    fuse [
      Atom ":";
      pretty_space;
      type_ ~depth t;
    ]

  and variance_ = function
    | Positive -> Atom "+"
    | Negative -> Atom "-"
    | Neutral -> Empty

  in
  let env_ (i, layout) =
    with_semicolon (fuse [
      Atom "type"; space;
      Atom (varname i);
      pretty_space; Atom "="; pretty_space;
      layout
    ])
  in

  (* Main call *)
  let type_layout = type_ ~depth:0 t in
  (* Run type_ first so that env_map has been populated *)
  let env_layout = List.map env_ (IMap.bindings !env_map) in
  Sequence (
    { break=Break_always; inline=(true, true); indent=0 },
    env_layout @ [type_layout]
  )

(* Same as Compact_printer with the exception of:
   - "Sequence Break_always" to allow breaks after every type alias
   - IfPretty to allow spaces after punctuation.
   We still maintain a single line format.
*)
let print ~force_single_line ~source_maps node =
  let rec print_node src = function
    (* this printer does not output locations *)
    | SourceLocation _ -> src
    | Sequence ({ break=Break_always; _ }, nodes) ->
      let rec go acc = function
      | [] -> acc
      | [n] -> print_node acc n
      | n::ns ->
        if force_single_line
          then go (Source.add_space 1 (print_node acc n)) ns
          else go (Source.add_newline (print_node acc n)) ns
      in
      go src nodes
    | IfPretty (node, _) -> print_node src node
    | Concat nodes
    | Sequence (_, nodes) -> List.fold_left print_node src nodes
    | Atom s -> Source.add_string s src
    | Identifier (loc, s) -> Source.add_identifier loc s src
    | IfBreak (_, no_break) -> print_node src no_break
    | Empty -> src
    in
  print_node (Source.create ~source_maps ()) node

let string_of_t ?(force_single_line=false) (ty: Ty.t) : string =
  print ~force_single_line ~source_maps:None (type_ ty) |> Source.contents
