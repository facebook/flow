(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Coverage_level
open Utils
open Hh_json

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let () = Ide.is_ide_mode := true

let (files: (Relative_path.t, string) Hashtbl.t) = Hashtbl.create 23

let globals = Hashtbl.create 23
let parse_errors = Hashtbl.create 23

(*****************************************************************************)
(* helpers *)
(*****************************************************************************)

let rec to_js_object json =
  match json with
  | JSON_Array l ->
      let l = List.map l to_js_object in
      let l = Array.of_list l in
      Js.Unsafe.inject (Js.array l)
  | JSON_Object l ->
      let l = List.map l begin fun (k, v) ->
        k, to_js_object v
      end in
      let l = Array.of_list l in
      Js.Unsafe.obj l
  | JSON_Bool b -> Js.Unsafe.inject (Js.bool b)
  | JSON_String s -> Js.Unsafe.inject (Js.string s)
  | JSON_Null -> Js.Unsafe.inject Js.null
  | JSON_Number i -> Js.Unsafe.inject (Js.number_of_float (float_of_string i))


let error el =
  let res =
    if el = [] then
      JSON_Object [ "passed",         JSON_Bool true;
               "errors",         JSON_Array [];
               "internal_error", JSON_Bool false;
             ]
    else
      let errors_json = List.map el (compose Errors.to_json Errors.to_absolute)
      in JSON_Object [ "passed",         JSON_Bool false;
                  "errors",         JSON_Array errors_json;
                  "internal_error", JSON_Bool false;
                ]
  in
  to_js_object res

(*****************************************************************************)

let type_fun tcopt x fn =
  try
    let tenv = Typing_env.empty tcopt fn in
    let fun_ = Naming_heap.FunHeap.find_unsafe x in
    Typing.fun_def tenv x fun_;
  with Not_found ->
    ()

let type_class tcopt x fn =
  try
    let tenv = Typing_env.empty tcopt fn in
    let class_ = Naming_heap.ClassHeap.find_unsafe x in
    Typing.class_def tenv x class_
  with Not_found ->
    ()

let make_funs_classes ast =
  List.fold_left ast ~f:begin fun (funs, classes, typedefs, consts) def ->
    match def with
    | Ast.Fun f -> f.Ast.f_name :: funs, classes, typedefs, consts
    | Ast.Class c -> funs, c.Ast.c_name :: classes, typedefs, consts
    | Ast.Typedef td -> funs, classes, td.Ast.t_id :: typedefs, consts
    | Ast.Constant cst -> funs, classes, typedefs, cst.Ast.cst_name :: consts
    | _ -> funs, classes, typedefs, consts
  end ~init:([], [], [], [])

let rec get_sub_classes classes =
  let sub_classes = SMap.fold (fun x _ acc -> SSet.add x acc) classes SSet.empty in
  SSet.fold get_sub_class sub_classes sub_classes

and get_sub_class cname acc =
  let sub_classes = try
    Hashtbl.find Typing_deps.extends_igraph cname
  with Not_found -> SSet.empty in
  SSet.fold begin fun sub_cname acc ->
    if SSet.mem sub_cname acc
    then acc
    else
      let acc = SSet.add sub_cname acc in
      get_sub_class sub_cname acc
  end sub_classes acc

let declare_file fn content =
  let _, old_funs, old_classes =
    try Hashtbl.find globals fn
    with Not_found -> true, [], []
  in
  List.iter old_funs begin fun (_, fname) ->
    Naming_heap.FunPosHeap.remove fname;
    Naming_heap.FunCanonHeap.remove (NamingGlobal.canon_key fname);
    Naming_heap.FunHeap.remove fname;
    Typing_env.Funs.remove fname;
  end;
  List.iter old_classes begin fun (_, cname) ->
    Naming_heap.ClassPosHeap.remove cname;
    Naming_heap.ClassCanonHeap.remove (NamingGlobal.canon_key cname);
    Naming_heap.ClassHeap.remove cname;
    Typing_env.Classes.remove cname;
  end;
  try
    Autocomplete.auto_complete := false;
    let {Parser_hack.file_mode; comments; ast} =
      Parser_hack.program fn content
    in
    let is_php = file_mode = None in
    Parser_heap.ParserHeap.add fn ast;
    if not is_php
    then begin
      let funs, classes, typedefs, consts = make_funs_classes ast in
      Hashtbl.replace globals fn (is_php, funs, classes);
      let tcopt = TypecheckerOptions.permissive in
      NamingGlobal.make_env ~funs ~classes ~typedefs ~consts;
      let all_classes = List.fold_right classes ~f:begin fun (_, cname) acc ->
        SMap.add cname (Relative_path.Set.singleton fn) acc
      end ~init:SMap.empty in
      Typing_decl.make_env tcopt fn;
      let sub_classes = get_sub_classes all_classes in
      SSet.iter begin fun cname ->
        match Naming_heap.ClassHeap.get cname with
        | None -> ()
        | Some c -> Typing_decl.class_decl tcopt c
      end sub_classes
    end
    else Hashtbl.replace globals fn (false, [], [])
  with _ ->
    Hashtbl.replace globals fn (true, [], []);
    ()

let rec last_error errors =
  match errors with
    | [] -> None
    | [e] -> Some e
    | _ :: tail -> last_error tail

let hh_add_file fn content =
  let fn = Relative_path.create Relative_path.Root fn in
  Hashtbl.replace files fn content;
  try
    let errors, _ = Errors.do_ begin fun () ->
      declare_file fn content
    end in
    Hashtbl.replace parse_errors fn (last_error errors)
  with e ->
    ()

let hh_add_dep fn content =
  let fn = Relative_path.create Relative_path.Root fn in
  Typing_deps.is_dep := true;
  (try
    Errors.ignore_ begin fun () ->
      declare_file fn content;
      Parser_heap.ParserHeap.remove fn
    end
  with e ->
    ()
  );
  Typing_deps.is_dep := false

let hh_check fn =
  let fn = Relative_path.create Relative_path.Root fn in
  match Hashtbl.find parse_errors fn with
    | Some e -> error [e]
    | None ->
      Autocomplete.auto_complete := false;
      Errors.try_
        begin fun () ->
        let ast = Parser_heap.ParserHeap.find_unsafe fn in
        let funs, classes, typedefs, consts = make_funs_classes ast in
        let tcopt = TypecheckerOptions.permissive in
        NamingGlobal.make_env ~funs ~classes ~typedefs ~consts;
        Typing_decl.make_env tcopt fn;
        List.iter funs (fun (_, fname) -> type_fun tcopt fname fn);
        List.iter classes (fun (_, cname) -> type_class tcopt cname fn);
        error []
        end
        begin fun l ->
          error [l]
        end

let hh_check_syntax fn content =
  let fn = Relative_path.create Relative_path.Root fn in
  let errors, _ = Errors.do_ begin fun () ->
    Parser_hack.program fn content
  end in
  error errors


let permissive_empty_envs fn =
  let tcopt = TypecheckerOptions.permissive in
  tcopt, Typing_env.empty tcopt fn

let hh_auto_complete fn =
  let fn = Relative_path.create Relative_path.Root fn in
  AutocompleteService.attach_hooks();
  try
    let ast = Parser_heap.ParserHeap.find_unsafe fn in
    Errors.ignore_ begin fun () ->
      List.iter ast begin fun def ->
        match def with
        | Ast.Fun f ->
            let tcopt, tenv = permissive_empty_envs fn in
            let f = Naming.fun_ tcopt f in
            Typing.fun_def tenv (snd f.Nast.f_name) f
        | Ast.Class c ->
            let tcopt, tenv = permissive_empty_envs fn in
            let c = Naming.class_ tcopt c in
            Typing_decl.class_decl tcopt c;
            let res = Typing.class_def tenv (snd c.Nast.c_name) c in
            res
        | _ -> ()
      end;
    end;
    let completion_type_str =
      match !Autocomplete.argument_global_type with
      | Some Autocomplete.Acid -> "id"
      | Some Autocomplete.Acnew -> "new"
      | Some Autocomplete.Actype -> "type"
      | Some Autocomplete.Acclass_get -> "class_get"
      | Some Autocomplete.Acprop -> "var"
      | None -> "none" in
    let result = AutocompleteService.get_results [] [] in
    let result =
      List.map result AutocompleteService.autocomplete_result_to_json
    in
    AutocompleteService.detach_hooks();
    to_js_object (JSON_Object [ "completions",    JSON_Array result;
                          "completion_type", JSON_String completion_type_str;
                          "internal_error",  JSON_Bool false;
                        ])
  with _ ->
    AutocompleteService.detach_hooks();
    to_js_object (JSON_Object [ "internal_error", JSON_Bool true;
                        ])

let hh_get_method_at_position fn line char =
  Autocomplete.auto_complete := false;
  let fn = Relative_path.create Relative_path.Root fn in
  let result = ref None in
  IdentifySymbolService.attach_hooks result line char;
  try
    let ast = Parser_heap.ParserHeap.find_unsafe fn in
    Errors.ignore_ begin fun () ->
      List.iter ast begin fun def ->
        match def with
        | Ast.Fun f ->
            let tcopt, tenv = permissive_empty_envs fn in
            let f = Naming.fun_ tcopt f in
            Typing.fun_def tenv (snd f.Nast.f_name) f
        | Ast.Class c ->
            let tcopt, tenv = permissive_empty_envs fn in
            let c = Naming.class_ tcopt c in
            let res = Typing.class_def tenv (snd c.Nast.c_name) c in
            res
        | _ -> ()
      end;
    end;
    IdentifySymbolService.detach_hooks ();
    let result =
      match !result with
      | Some res ->
          let result_type =
            match res.IdentifySymbolService.type_ with
            | IdentifySymbolService.Class -> "class"
            | IdentifySymbolService.Method -> "method"
            | IdentifySymbolService.Function -> "function"
            | IdentifySymbolService.LocalVar -> "local" in
          JSON_Object [ "name",           JSON_String res.IdentifySymbolService.name;
                   "result_type",    JSON_String result_type;
                   "pos",            Pos.json (Pos.to_absolute res.IdentifySymbolService.pos);
                   "internal_error", JSON_Bool false;
                 ]
      | _ -> JSON_Object [ "internal_error", JSON_Bool false;
                    ] in
    to_js_object result
  with _ ->
    IdentifySymbolService.detach_hooks ();
    to_js_object (JSON_Object [ "internal_error", JSON_Bool true;
                        ])

let hh_get_deps =
  let already_sent = ref Typing_deps.DSet.empty in
  fun () ->
    let result = ref [] in
    let deps = !(Typing_deps.deps) in
    Typing_deps.deps := Typing_deps.DSet.empty;
    Typing_deps.DSet.iter begin fun dep ->
      if Typing_deps.DSet.mem dep !already_sent
      then ()
      else begin
        already_sent := Typing_deps.DSet.add dep !already_sent;
        result :=
          (match dep with
          | Typing_deps.Dep.Class s
            when Typing_env.Classes.get s = None ->
              (JSON_Object [ "name", JSON_String s;
                        "type", JSON_String "class";
                      ]) :: !result
          | Typing_deps.Dep.Fun s
            when Typing_env.Funs.get s = None ->
              (JSON_Object [ "name", JSON_String s;
                        "type", JSON_String "fun";
                      ]) :: !result
          | _ -> !result
          )
      end
    end deps;
    to_js_object (JSON_Object [ "deps",          JSON_Array !result;
                          "internal_error", JSON_Bool false;
                        ])

let infer_at_pos file line char =
  try
    let get_result = InferAtPosService.attach_hooks line char in
    ignore (hh_check file);
    InferAtPosService.detach_hooks ();
    get_result ()
  with _ ->
    InferAtPosService.detach_hooks ();
    None, None

let hh_find_lvar_refs file line char =
  try
    let get_result = FindLocalsService.attach_hooks line char in
    Errors.ignore_ begin fun () ->
      ignore (hh_check file);
    end;
    FindLocalsService.detach_hooks ();
    let res_list =
      List.map (get_result ()) (compose Pos.json Pos.to_absolute) in
    to_js_object (JSON_Object [ "positions",      JSON_Array res_list;
                           "internal_error", JSON_Bool false;
                        ])
  with _ ->
    FindLocalsService.detach_hooks ();
    to_js_object (JSON_Object [ "internal_error", JSON_Bool true;
                        ])

let hh_infer_type file line char =
  let _, ty = infer_at_pos file line char in
  let output = match ty with
  | Some ty -> JSON_Object [ "type",           JSON_String ty;
                        "internal_error", JSON_Bool false;
                      ]
  | None -> JSON_Object [ "internal_error", JSON_Bool false;
                   ]
  in
  to_js_object output

let hh_infer_pos file line char =
  let pos, _ = infer_at_pos file line char in
  let output = match pos with
  | Some pos -> JSON_Object [ "pos",            Pos.json (Pos.to_absolute pos);
                         "internal_error", JSON_Bool false;
                       ]
  | None -> JSON_Object [ "internal_error", JSON_Bool false;
                   ]
  in
  to_js_object output

let hh_file_summary fn =
  let fn = Relative_path.create Relative_path.Root fn in
  try
    let ast = Parser_heap.ParserHeap.find_unsafe fn in
    let outline = FileOutline.outline_ast ast in
    let res_list = List.map outline begin fun (pos, name, type_) ->
      JSON_Object [ "name", JSON_String name;
               "type", JSON_String type_;
               "pos",  Pos.json pos;
             ]
      end in
    to_js_object (JSON_Object [ "summary",         JSON_Array res_list;
                          "internal_error",   JSON_Bool false;
                        ])
  with _ ->
    to_js_object (JSON_Object [ "internal_error", JSON_Bool true;
                        ])

let hh_hack_coloring fn =
  let type_acc = Hashtbl.create 0 in
  Typing.with_expr_hook
    (fun (p, _) ty -> Hashtbl.replace type_acc p ty)
    (fun () -> ignore (hh_check fn));
  let fn = Relative_path.create Relative_path.Root fn in
  let level_of_type x = snd (Coverage_level.level_of_type_mapper fn x) in
  let result = Hashtbl.fold (fun p ty xs ->
    (Pos.info_raw p, level_of_type (p, ty)) :: xs) type_acc [] in
  let result = ColorFile.go (Hashtbl.find files fn) result in
  let result = List.map result begin fun input ->
    match input with
    | (Some lvl, str) -> (string_of_level lvl, str)
    | (None, str) -> ("default", str)
  end in
  let result = List.map result begin fun (checked, text) ->
    JSON_Object [ "checked", JSON_String checked;
             "text",    JSON_String text;
           ]
  end in
  to_js_object (JSON_Object [ "coloring",      JSON_Array result;
                         "internal_error", JSON_Bool false;
                      ])

let hh_get_method_calls fn =
  Typing_defs.accumulate_method_calls := true;
  Typing_defs.accumulate_method_calls_result := [];
  ignore (hh_check fn);
  let results = !Typing_defs.accumulate_method_calls_result in
  let results = List.map results begin fun (p, name) ->
    JSON_Object [ "method_name", JSON_String name;
             "pos",         Pos.json (Pos.to_absolute p);
           ]
  end in
  Typing_defs.accumulate_method_calls := false;
  Typing_defs.accumulate_method_calls_result := [];
  to_js_object (JSON_Object [ "method_calls",  JSON_Array results;
                        "internal_error", JSON_Bool false;
                      ])

let hh_arg_info fn line char =
  (* all the hooks for arg info happen in typing,
   * so we only need to run typing*)
  ArgumentInfoService.attach_hooks (line, char);
  let fn = Relative_path.create Relative_path.Root fn in
  let _, funs, classes = Hashtbl.find globals fn in
  Errors.ignore_ begin fun () ->
    List.iter funs begin fun (_, f_name) ->
      let _tcopt, tenv = permissive_empty_envs fn in
      let f = Naming_heap.FunHeap.find_unsafe f_name in
      Typing.fun_def tenv f_name f
    end;
    List.iter classes begin fun (_, c_name) ->
      let _tcopt, tenv = permissive_empty_envs fn in
      let c = Naming_heap.ClassHeap.find_unsafe c_name in
      Typing.class_def tenv c_name c
    end;
  end;
  let result = ArgumentInfoService.get_result() in
  let result = match result with
    | Some result -> result
    | None -> (-1), []
  in
  ArgumentInfoService.detach_hooks();
  let json_res =
    ("internal_error", JSON_Bool false) :: ArgumentInfoService.to_json result
  in
  to_js_object (JSON_Object json_res)

let hh_format contents start end_ =
  let modes = [Some FileInfo.Mstrict; Some FileInfo.Mpartial] in
  let result =
    Format_hack.region modes Path.dummy_path start end_ contents in
  let error, result, internal_error = match result with
    | Format_hack.Disabled_mode -> "Php_or_decl", "", false
    | Format_hack.Parsing_error _ -> "Parsing_error", "", false
    | Format_hack.Internal_error -> "", "", true
    | Format_hack.Success s -> "", s, false
  in
  to_js_object (JSON_Object [ "error_message", JSON_String error;
                        "result", JSON_String result;
                        "internal_error",   JSON_Bool internal_error;
                      ])

(* Helpers to turn JavaScript strings into OCaml strings *)
let js_wrap_string_1 func =
  let f str = begin
    let str = Js.to_string str in
    func str
  end in
  Js.wrap_callback f

let js_wrap_string_2 func =
  let f str1 str2 = begin
    let str1 = Js.to_string str1 in
    let str2 = Js.to_string str2 in
    func str1 str2
  end in
  Js.wrap_callback f

let () =
  Relative_path.set_path_prefix Relative_path.Root (Path.make "/");
  Js.Unsafe.set Js.Unsafe.global "hh_check_file" (js_wrap_string_1 hh_check);
  Js.Unsafe.set Js.Unsafe.global "hh_check_syntax" (js_wrap_string_2 hh_check_syntax);
  Js.Unsafe.set Js.Unsafe.global "hh_add_file" (js_wrap_string_2 hh_add_file);
  Js.Unsafe.set Js.Unsafe.global "hh_add_dep" (js_wrap_string_2 hh_add_dep);
  Js.Unsafe.set Js.Unsafe.global "hh_auto_complete" (js_wrap_string_1 hh_auto_complete);
  Js.Unsafe.set Js.Unsafe.global "hh_get_deps" (Js.wrap_callback hh_get_deps);
  Js.Unsafe.set Js.Unsafe.global "hh_infer_type" (js_wrap_string_1 hh_infer_type);
  Js.Unsafe.set Js.Unsafe.global "hh_infer_pos" (js_wrap_string_1 hh_infer_pos);
  Js.Unsafe.set Js.Unsafe.global "hh_file_summary" (js_wrap_string_1 hh_file_summary);
  Js.Unsafe.set Js.Unsafe.global "hh_hack_coloring" (js_wrap_string_1 hh_hack_coloring);
  Js.Unsafe.set Js.Unsafe.global "hh_find_lvar_refs" (js_wrap_string_1 hh_find_lvar_refs);
  Js.Unsafe.set Js.Unsafe.global "hh_get_method_calls" (js_wrap_string_1 hh_get_method_calls);
  Js.Unsafe.set Js.Unsafe.global "hh_get_method_name" (js_wrap_string_1 hh_get_method_at_position);
  Js.Unsafe.set Js.Unsafe.global "hh_arg_info" (js_wrap_string_1 hh_arg_info);
  Js.Unsafe.set Js.Unsafe.global "hh_format" (js_wrap_string_1 hh_format)
