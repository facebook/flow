(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


open Utils
open Nast

class output((o: string -> unit)) = object(self)
  val mutable margin = 0
  val mutable cclass = ""
  val mutable statics = ISet.empty

  method spaces() =
    for i = 0 to margin - 1 do
      self#out " "
    done;
    ()

  method out s =
    o s

  method sout s =
    self#spaces();
    self#out s

  method outnl s =
    self#out s;
    self#nl()

  method soutnl s =
    self#spaces();
    self#outnl s

  method nl() =
    self#out "\n"

  method scope f =
    margin <- margin + 2;
    let () = f() in
    margin <- margin - 2;

  method set_class s =
    cclass <- s

  method get_class() =
    cclass

  method set_statics s =
    statics <- s

  method statics() = statics

end

module Header = struct

  let make o =
    let o = o#out in
    o "hhclone = function (obj) {
        res = new Object();
        for(i in obj) {
          res[i] = obj[i];
        }
        if (res.hhdynamic && res.hhdynamic.hhf___clone) {
          res.hhdynamic.hhf___clone(res);
        }
        return res;
      }\n";

    o "hhfatal = function(s) { throw ('Fatal: '+s); } \n";
    o "hhstring = function(obj) {\n";
    o "  if(obj && obj.hhdynamic && obj.hhdynamic.hhf___toString) {\n";
    o "    return obj.hhdynamic.hhf___toString(obj);\n";
    o "  }\n";
    o "  else { return ''+obj }\n";
    o "}\n";

    o "hhf_echo = function() {\n";
    o "  s = '';\n";
    o "  for(i in arguments) { s += hhstring(arguments[i]); };\n";
    o "  console.log(s);\n";
    o "}\n";

    o "hhf_log = function(s) {\n";
    o "  console.log(s);\n";
    o "}\n";

    o "hhforeach = function (obj, fun)\n";
    o "{\n";
    o "  if(obj.hhdynamic && obj.hhdynamic.hhf_next) {\n";
    o "    obj.hhdynamic.hhf_rewind(obj);\n";
    o "    obj.hhdynamic.hhf_next(obj);\n";
    o "    while (obj.hhdynamic.hhf_valid(obj)) {\n";
    o "      fun.call(fun, obj.hhdynamic.hhf_key(obj), obj.hhdynamic.hhf_current(obj));\n";
    o "      obj.hhdynamic.hhf_next(obj);\n";
    o "    }\n";
    o "  } else if(obj instanceof Array) {\n";
    o "    for(var key = 0; key < obj.length; key++) \n";
    o "    { \n";
    o "        fun(key, obj[key]);\n";
    o "    };\n";
    o "  }\n";
    o "  else if((keys = Object.keys(obj)) && keys[0] >= '0' && keys[0] <= '9') {\n";
    o "    for(var key in obj) \n";
    o "    { key = parseInt(key); \n";
    o "      fun(key, obj[key]);\n";
    o "    };\n";
    o "  }\n";
    o "  else {\n";
    o "    for(var key in obj) \n";
    o "    { \n";
    o "      fun(key, obj[key]);\n";
    o "    };\n";
    o "  }\n";
    o "}\n";

    o "hhit_of_array = function(arr) {\n";
    o "  return {\n";
    o "    idx: 0,\n";
    o "    current: null,\n";
    o "    hhf_valid: function(obj) { return this.idx < arr.length; },\n";
    o "    hhf_key: function(obj) { return this.idx; },\n";
    o "    hhf_current: function(obj) { return this.current; },\n";
    o "    hhf_rewind: function(obj) { this.idx = -1; this.current = null; },\n";
    o "    hhf_next: function(obj) {\n";
    o "      if(this.idx >= arr.length) {\n";
    o "        throw new_hhc_Exception('Continuation already finished');\n";
    o "      }\n";
    o "      this.idx++;\n";
    o "      this.current = arr[this.idx];\n";
    o "    }\n";
    o "  }\n";
    o "}\n";

    o "hhit_of_map = function(map) {\n";
    o "  var arr = new Array();\n";
    o "  var i = 0;\n";
    o "  for(k in map) {\n";
    o "    arr[i] = {key:k, value: map[k]};\n";
    o "    i++;\n";
    o "  }\n";
    o "  return {\n";
    o "    idx: 0,\n";
    o "    current: null,\n";
    o "    hhf_valid: function(obj) { return this.idx < arr.length; },\n";
    o "    hhf_key: function(obj) { return this.current.key; },\n";
    o "    hhf_current: function(obj) { return this.current.value; },\n";
    o "    hhf_rewind: function(obj) { this.idx = -1; this.current = null; },\n";
    o "    hhf_next: function(obj) {\n";
    o "      if(this.idx >= arr.length) {\n";
    o "        throw new_hhc_Exception('Continuation already finished');\n";
    o "      }\n";
    o "      this.idx++;\n";
    o "      this.current = arr[this.idx];\n";
    o "    }\n";
    o "  }\n";
    o "}\n";

    o "hhmake_iterator = function (obj)\n";
    o "{\n";
    o "  if(obj.hhdynamic && obj.hhdynamic.hhf_next) {\n";
    o "    return obj;\n";
    o "  } else if(obj instanceof Array) {\n";
    o "    return {hhdynamic: hhit_of_array(obj)}; \n";
    o "  }\n";
    o "  else {\n";
    o "    return {hhdynamic: hhit_of_map(obj)}; \n";
    o "  }\n";
    o "}\n";

    o "hhforeachv = function (obj, fun)\n";
    o "{\n";
    o "  if(obj.hhdynamic && obj.hhdynamic.hhf_next) {\n";
    o "    obj.hhdynamic.hhf_rewind(obj);\n";
    o "    obj.hhdynamic.hhf_next(obj);\n";
    o "    while (obj.hhdynamic.hhf_valid(obj)) {\n";
    o "      fun.call(fun, obj.hhdynamic.hhf_current(obj));\n";
    o "      obj.hhdynamic.hhf_next(obj);\n";
    o "    }\n";
    o "  } else if(obj instanceof Array) {\n";
    o "    for(var key = 0; key < obj.length; key++) \n";
    o "    { \n";
    o "        fun(obj[key]);\n";
    o "    };\n";
    o "  }\n";
    o "  else {\n";
    o "    for(var key in obj) \n";
    o "    { \n";
    o "      fun(obj[key]);\n";
    o "    };\n";
    o "  }\n";
    o "}\n";

    o "function hhf_is_int(value) { return typeof(value) === 'number' && value === parseInt(value); }\n";
    o "function hhf_is_bool(value) { return typeof(value) === 'bool'; }\n";
    o "function hhf_is_string(value) { return typeof(value) === 'string'; }\n";
    o "function hhf_is_float(value) { return typeof(value) === 'number' && value === parseFloat(value); }\n";

    o "hhinstanceof = function(obj, s) {\n";
    o " return obj && obj.hhdynamic && obj.hhdynamic.hhimplements && !!obj.hhdynamic.hhimplements[s];\n";
    o "}\n";

end

module IsGenerator = struct

  let rec is_gen stl =
    block false stl

  and block acc stl =
    List.fold_left stmt acc stl

  and stmt acc = function
    | Expr (_, Binop (_, _, (_, (Yield _))))
    | Expr (_, (Yield _)) -> true
    | Return _
    | Throw _
    | Expr _
    | Break
    | Noop
    | Static_var _
    | Continue -> acc
    | If (_, b1, b2) ->
        let acc = block acc b1 in
        let acc = block acc b2 in
        acc
    | While (_, b)
    | Foreach (_, _, b)
    | Do (b, _) -> block acc b
    | For (_, _, _, b) ->
        block acc b
    | Switch (_, cl) ->
        List.fold_left case acc cl
    | Try (b, cl) ->
        let acc = block acc b in
        List.fold_left catch acc cl

  and case acc = function
    | Default b
    | Case (_, b) -> block acc b

  and catch acc (_, _, b) = block acc b

end

module GetLocals = struct

  let rec fun_param acc param =
    ISet.remove (snd param.param_id) acc

  and fun_ params body =
    let acc = ISet.empty in
    let acc = block acc body in
    let acc = List.fold_left fun_param acc params in
    acc

  and block acc stl =
    List.fold_left stmt acc stl

  and stmt acc = function
    | Return (_, Some e)
    | Throw (_, e)
    | Expr e -> expr acc e
    | Break
    | Return _
    | Noop
    | Continue -> acc
    | Static_var el ->
        List.fold_left expr acc el
    | If (e, b1, b2) ->
        let acc = expr acc e in
        let acc = block acc b1 in
        let acc = block acc b2 in
        acc
    | While (e, b)
    | Do (b, e) -> block (expr acc e) b
    | For (e1, e2, e3, b) ->
        let acc = expr acc e1 in
        let acc = expr acc e2 in
        let acc = expr acc e3 in
        block acc b
    | Switch (e, cl) ->
        let acc = expr acc e in
        List.fold_left case acc cl
    | Foreach (e, ae, b) ->
        let acc = expr acc e in
        let acc = as_expr acc ae in
        let acc = block acc b in
        acc
    | Try (b, cl) ->
        let acc = block acc b in
        List.fold_left catch acc cl

  and as_expr acc = function
    | As_v e -> expr acc e
    | As_kv (e1, e2) -> expr (expr acc e1) e2

  and expr acc (_, e) =
    match e with
    | Array _
    | Id _
    | Fun_id _
    | Class_get _
    | Class_const _
    | True
    | False
    | Int _
    | Float _
    | Null
    | String _
    | Efun _
    | Yield_break
    | This -> acc
    | ValCollection (_, el) -> List.fold_left expr acc el
    | KeyValCollection (_, fl) -> List.fold_left field acc fl
    | Lvar (_, x) -> ISet.add x acc
    | Array_get (e, None)
    | Assert (AE_assert e)
    | Clone e
    | Yield e
    | Special_func (Result e)
    | Special_func (Wait_for e)
    | Special_func (Wait_forv e)
    | Special_func (Wait_forvr e)
    | Special_func (Wait_for_result e)
    | Special_func (Wait_forv_result e)
    | Special_func (Wait_forvr_result e)
    | Cast (_, e)
    | Unop (_, e)
    | InstanceOf (e, _)
    | Obj_get (e, _) -> expr acc e
    | Array_get (e1, Some e2) -> expr (expr acc e1) e2
    | Assert (AE_invariant_violation (e, el))
    | Call (_, e, el) ->
        List.fold_left expr (expr acc e) el
    | Special_func (Wait_forva el)
    | Special_func (Wait_forvar el)
    | Special_func (Wait_forva_result el)
    | Special_func (Wait_forvar_result el)
    | List el
    | Expr_list el
    | New (_, el)
    | String2 (el, _) ->
        List.fold_left expr acc el
    | Pair (e1, e2) ->
        expr (expr acc e1) e2
    | Assert (AE_invariant (e1, e2, el)) ->
        List.fold_left expr (expr (expr acc e1) e2) el
    | Binop (_, e1, e2) ->
        expr (expr acc e1) e2
    | Eif (e1, None, e3) ->
        expr (expr acc e1) e3
    | Eif (e1, Some e2, e3) ->
        expr (expr (expr acc e1) e2) e3
    | Xml (_, sel, el) ->
        List.fold_left expr acc (List.map snd sel @ el)

  and case acc = function
    | Default b -> block acc b
    | Case (e, b) -> block (expr acc e) b

  and catch acc (_, _, b) = block acc b

  and field acc (e1, e2) = expr (expr acc e1) e2

end

module GetStatics = struct

  let rec fun_ args body =
    let acc = IMap.empty in
    let acc = block acc body in
    acc

  and block acc stl =
    List.fold_left stmt acc stl

  and stmt acc = function
    | Throw _
    | Expr _
    | Break
    | Return _
    | Noop
    | Continue -> acc
    | Static_var el ->
        List.fold_left expr acc el
    | If (e, b1, b2) ->
        let acc = block acc b1 in
        let acc = block acc b2 in
        acc
    | While (_, b)
    | Do (b, _)
    | Foreach (_, _, b)
    | For (_, _, _, b) -> block acc b
    | Switch (_, cl) ->
        List.fold_left case acc cl
    | Try (b, cl) ->
        let acc = block acc b in
        List.fold_left catch acc cl

  and expr acc (_, e) =
    match e with
    | Binop (Ast.Eq None, (_, Lvar (_, lid)), expr) ->
        IMap.add lid expr acc
    | Lvar (_, lid) ->
        IMap.add lid (Pos.none, Null) acc
    | _ -> assert false

  and case acc = function
    | Default b -> block acc b
    | Case (_, b) -> block acc b

  and catch acc (_, _, b) = block acc b

end


let rec list f o l =
  match l with
  | [] -> ()
  | [x] -> f o x
  | x :: rl -> f o x; o#out ", "; list f o rl

let fun_id o fid =
  o#out ("hhf_"^ fid)

let local_id o x =
  o#out ("l"^string_of_int x)

let class_id o x =
  o#out ("hhc_"^x)

let sclass_id o x =
  o#out ("hhclass_init_"^x)

let class_global_id o x =
  o#out ("hhclass_"^x)

let const_id o x =
  o#out ("hhcst_"^x)

let class_member_id o x =
  o#out ("hhm_"^x)

let is_local_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_' -> true
  | _ -> false

let rec string2 args s i =
  let size = String.length s in
  if i >= size
  then []
  else if s.[i] = '$'
  then begin
    match args with
    | [] -> assert false
    | x :: rl ->
        let i = ref (i+1) in
        while !i < size && is_local_char s.[!i] do
          incr i;
        done;
        x :: string2 rl s !i
  end
  else begin
    let j = ref (i+1) in
    while !j < size && s.[!j] != '$' do
      incr j;
    done;
    (Pos.none, String (Pos.none, String.sub s i (!j - i)))
    :: string2 args s !j
  end


let rec program o { funs = funs; classes = classes } =
  let o = new output(o) in
  Header.make o;
  SMap.iter (fun_ o) funs;
  SMap.iter (declare_static_class o) classes;
  SMap.iter (class_ o) classes;
  SMap.iter (static_class o) classes;
  SMap.iter (class_global o) classes;
  SMap.iter (init_class o) classes;
  o#outnl "hhf_main();";
  ()

and class_ o cid c =
  if c.c_kind = Ast.Cinterface then () else begin
  o#set_class cid;
  class_id o cid;
  o#outnl " = function(obj) {";
  o#scope begin fun () ->
    List.iter (class_var o) c.c_vars;
  end;
  o#outnl "}";

  (* The constructor *)
  class_id o cid; o#out "_cstr = ";
  (match c.c_constructor with
  | None -> o#outnl "function(obj) { return function(arg) { }; }"
  | Some m ->
      o#out "function(obj) { return ";
      fun_def ~is_method:false o m.m_params m.m_body;
      o#outnl "}"
  );

(* *)
  o#out "new_"; class_id o cid; o#out " = ";
  o#outnl "function () {";
  o#scope begin fun () ->
    o#spaces();
    o#outnl "var obj = new Object();";
    o#spaces();
    o#out "obj.hhstatic = "; class_global_id o (cid^"_static");
    o#outnl ";";
    o#spaces();
    o#out "obj.hhdynamic = "; class_global_id o (cid^"_dynamic");
    o#outnl ";";
    o#spaces();
    class_id o cid; o#outnl "(obj);";
    (match c.c_constructor with
    | None ->
        (match c.c_extends with
        | (_, Happly ((_, cid), _)) :: _ ->
            o#spaces();
            class_id o cid;
            o#outnl "_cstr(obj).apply(obj, arguments);";
        | _ -> ())
    | Some cstr ->
        o#spaces();
        class_id o cid;
        o#outnl "_cstr(obj).apply(obj, arguments);";
    );
    o#spaces();
    o#outnl "return obj;"
  end;
  o#outnl "};"
 end;


and static_class o cid c =
  if c.c_kind = Ast.Cinterface then () else begin
  o#set_class cid;
  sclass_id o (cid^"_vars");
  o#outnl " = function(obj) {";
  o#scope begin fun () ->
    List.iter (class_const o) c.c_consts;
    List.iter (class_var o) c.c_static_vars;
  end;
  o#outnl "}";
  sclass_id o (cid^"_static");
  o#outnl " = function(obj) {";
  o#scope begin fun () ->
    List.iter (method_ o) c.c_static_methods;
  end;
  o#outnl "}";
  sclass_id o (cid^"_dynamic");
  o#outnl " = function(obj) {";
  o#scope begin fun () ->
    List.iter (extends o) c.c_extends;
    List.iter (extends o) c.c_uses;
    List.iter (method_ o) c.c_methods;
    o#spaces();
    o#outnl ("obj.hhf_get_class = function() { return '"^cid^"';}");
    (match c.c_extends with
    | (_, Happly ((_, pid), _)) :: _ ->
        o#spaces();
        o#outnl ("obj.hhf_get_parent_class = function() { return '"^pid^"';}");
    | _ ->
        o#spaces();
        o#outnl ("obj.hhf_get_parent_class = function() { return null;}");
    );
    o#spaces();
    o#outnl "if(!obj.hhimplements) { obj.hhimplements = new Object(); }";
    o#spaces();
    o#outnl ("obj.hhimplements['"^cid^"'] = true;");
    List.iter begin function
      | _, Happly ((_, cid), _) ->
          o#spaces();
          o#outnl ("obj.hhimplements['"^cid^"'] = true;");
      | _ -> ()
    end (c.c_extends @ c.c_implements)
  end;
  o#outnl "}";
 end

and class_global o cid c =
  if c.c_kind = Ast.Cinterface then () else begin
  o#set_class cid;
    match c.c_extends with
    | (_, Happly ((_, parent), _)) :: _ ->
        class_global_id o (cid^"_vars");
        o#out " = ";
        class_global_id o (parent^"_vars");
        o#outnl ";";
    | _ ->
        class_global_id o (cid^"_vars");
        o#outnl " = new Object();";
  end

and declare_static_class o cid _ =
  o#out "var "; class_global_id o (cid^"_vars"); o#outnl " = new Object();";
  o#out "var "; class_global_id o cid; o#outnl " = new Object();";
  o#out "var "; class_global_id o (cid^"_static"); o#outnl " = new Object();";
  o#out "var "; class_global_id o (cid^"_dynamic"); o#outnl " = new Object();";

and init_class o cid c =
  if c.c_kind = Ast.Cinterface then () else begin
  o#set_class cid;
  sclass_id o (cid^"_vars");
  (match c.c_extends with
  | (_, Happly ((_, parent), _)) :: _ ->
      o#out "(";
      class_global_id o (parent^"_vars");
      o#outnl ");";
  | _ ->
      o#out "(";
      class_global_id o (cid^"_vars");
      o#outnl ");";
  );
  class_global_id o (cid^"_static"); o#outnl " = new Object();";
  class_global_id o (cid^"_static"); o#out ".parent = ";
  class_global_id o (cid^"_dynamic"); o#out ".parent = ";
  (match c.c_extends with
  | (_, Happly ((_, parent), _)) :: _ ->
      class_global_id o parent;
      o#outnl ";"
  | _ -> o#outnl "null;"
  );
  class_global_id o (cid^"_dynamic"); o#outnl " = new Object();";
  List.iter begin fun h ->
    match h with
    | _, Happly ((_, x), _) ->
        call_init o x cid
    | _ -> ()
  end (c.c_extends @ c.c_uses);
  call_init o cid cid;
  class_global_id o cid; o#outnl " = new Object();";
  class_global_id o cid; o#out ".hhstatic = "; class_global_id o (cid^"_static");
  o#outnl ";";
  class_global_id o cid; o#out ".hhdynamic = "; class_global_id o (cid^"_dynamic");
  o#outnl ";"
  end

and call_init o x cid =
  sclass_id o (x^"_static");
  o#out "("; class_global_id o (cid^"_static");
  o#outnl ");";
  sclass_id o (x^"_dynamic");
  o#out "("; class_global_id o (cid^"_dynamic");
  o#outnl ");";
  ()

and extends o = function
  | _, Happly ((_, x), _) ->
      o#spaces();
      class_id o x;
      o#out "(obj);";
      o#nl();
      ()
  | _ -> ()

and class_var o v =
  o#spaces();
  o#out "obj.";
  class_member_id o (snd v.cv_id);
  o#out " = ";
  match v.cv_expr with
  | None -> o#outnl "null;";
  | Some e -> expr o e; o#outnl ";"

and class_const o (_, (_, s), e) =
  o#spaces();
  o#out "obj."; const_id o s; o#out " = ";
  expr o e;
  o#nl();
  ()

and method_ o m =
  assert (not m.m_ddd);
  o#spaces();
  o#out "obj.";
  fun_or_method ~is_method:true
    o (snd m.m_name) m.m_params m.m_body

and fun_ o id f =
  fun_or_method o id f.f_params f.f_body

and fun_or_method ?(is_method=false) o id params body =
  fun_id o id;
  o#out " = ";
  fun_def ~is_method o params body

and fun_def ~is_method o params body =
  o#out "function(";
  if is_method
  then begin
    o#out "obj";
    if params <> []
    then o#out ", "
    else ()
  end;
  list fun_param o params;
  o#out ") ";
  let locals = ISet.elements (GetLocals.fun_ params body) in
  let statics = GetStatics.fun_ params body in
  let decl () =
    begin match locals with
    | [] -> ()
    | [x] ->
        o#spaces();
        o#out "var ";
        local_id o x;
        o#outnl ";";
        o#nl();
    | x :: rl ->
        o#spaces();
        o#out "var ";
        local_id o x;
        List.iter begin fun v ->
          o#out ", ";
          local_id o v;
        end rl;
        o#outnl ";";
        o#nl();
    end;
    if not (IMap.is_empty statics) then begin
      o#soutnl "if(!(this.hhinit)) {";
      o#scope begin fun () ->
        o#soutnl "this.hhinit = true;";
        IMap.iter begin fun k v ->
          o#soutnl "statics = new Object();";
          o#spaces();
          o#out "statics.";
          local_id o k; o#out " = "; expr o v; o#outnl ";"
        end statics
      end;
      o#soutnl "}";
    end
  in
  o#set_statics (IMap.fold (fun x _ acc -> ISet.add x acc) statics ISet.empty);
  let is_gen = IsGenerator.is_gen body in
  if is_gen
  then begin
    o#spaces();
    o#outnl "{ var gen = new Object();";
    o#spaces(); o#outnl "gen.hhf_current = function (obj) { return gen.value; };";
    o#spaces(); o#outnl "gen.hhf_key = function (obj) { return gen.key; };";
    o#spaces(); o#outnl "gen.hhf_valid = function (obj) { return gen.valid; };";
    o#spaces();
    o#out "var cont = function () {"
  end;
  block_f o decl body;
  if is_gen
  then begin
    o#outnl "};";
    o#spaces(); o#outnl "gen.hhf_next = cont";
    o#spaces();
    o#outnl "gen.hhf_rewind = function (obj) { gen.value = null; gen.key = -1; gen.valid = true; gen.hhf_next = cont; };";
    o#outnl "return {hhdynamic: gen}}"
  end;
  ()

and block_f o decl b =
  o#outnl "{";
  o#scope begin fun () ->
    decl();
    stmtl o b
  end;
  o#spaces();
  o#outnl "}"

and block o b =
  block_f o (fun () -> ()) b

and fun_param o param =
  local_id o (snd param.param_id);
  match (param.param_expr) with
  | None -> ()
  | Some e ->
      o#out " = "; expr o e

and stmtl o stl =
  if IsGenerator.is_gen stl
  then stmtgen (retlast o) o stl
  else List.iter (stmt o) stl

and retlast o () =
  o#spaces();
  o#out "if (gen.valid) { gen.valid = false; } else
     { throw new_hhc_Exception('Continuation already finished'); }\n"

and retvoid o () =
  o#spaces();
  o#outnl "throw 'Eol';"

and stmtgen last o = function
  | [] -> last()
  | Expr (_, Yield e) :: rl ->
      o#spaces();
      o#outnl "gen.hhf_next = function () {";
      stmtgen last o rl;
      o#spaces();
      o#outnl "};";
      o#spaces();
      o#out "gen.key++; gen.value = "; expr o e;
      o#outnl ";";
  | Expr (
    _, Binop (
      bop, lv, (
      _, (
      Yield e
     )))) :: rl ->
      let lid = Ident.tmp() in
      let n = Pos.none in
      o#spaces();
      o#out "return {value: ";
      expr o begin match e with
      | _, Special_func (Result e) -> n, Call(Cnormal,(n, Id (n, "result")), [e])
      | _, Special_func (Wait_for e) -> n, Call(Cnormal,(n, Id (n, "wait_for")), [e])
      | _, Special_func (Wait_forv e) -> n, Call(Cnormal,(n, Id (n, "wait_forv")), [e])
      | _, Special_func (Wait_forvr e) -> n, Call(Cnormal,(n, Id (n, "wait_forvr")), [e])
      | _, Special_func (Wait_forva el) -> n, Call(Cnormal,(n, Id (n, "wait_forva")), el)
      | _, Special_func (Wait_forvar el) -> n, Call(Cnormal,(n, Id (n, "wait_forvar")), el)
      | _, Special_func (Wait_for_result e) -> n, Call(Cnormal,(n, Id (n, "wait_for_result")), [e])
      | _, Special_func (Wait_forv_result e) -> n, Call(Cnormal,(n, Id (n, "wait_forv_result")), [e])
      | _, Special_func (Wait_forvr_result e) -> n, Call(Cnormal,(n, Id (n, "wait_forvr_result")), [e])
      | _, Special_func (Wait_forva_result el) -> n, Call(Cnormal,(n, Id (n, "wait_forva_result")), el)
      | _, Special_func (Wait_forvar_result el) -> n, Call(Cnormal,(n, Id (n, "wait_forvar_result")), el)
      | _ -> e
      end;
      o#out ", next: function ("; local_id o lid;
      o#outnl ") {";
      stmt o (Expr (
              Pos.none,
              Binop (
              bop, lv, (
              Pos.none,
              Lvar (Pos.none, lid)))));
      stmtgen last o rl;
      o#spaces();
      o#outnl "}};"
  | If (c, stl1, stl2) :: rl
    when IsGenerator.is_gen stl1 || IsGenerator.is_gen stl2 ->
      let new_last = make_tail last o rl in
      o#spaces();
      o#out "if("; expr o c; o#outnl ") {";
      stmtgen new_last o stl1;
      o#outnl "}";
      o#spaces();
      o#outnl "else {";
      stmtgen new_last o stl2;
      o#outnl "}";
  | While (c, b) :: rl when IsGenerator.is_gen b ->
      let self = Ident.tmp() in
      let loop_last() =
        o#spaces();
        local_id o self;
        o#outnl "();";
      in

      o#spaces();
      o#out "var ";
      local_id o self;
      o#outnl " = function () { ";
      o#scope begin fun () ->
        o#spaces();
        o#out "if ("; expr o c; o#outnl ") {";
        o#scope begin fun () ->
          stmtgen loop_last o b;
        end;
        o#spaces();
        o#outnl "}";
        o#spaces();
        o#outnl "else {";
        stmtgen last o rl;
        o#spaces();
        o#outnl "}";
      end;
      o#spaces();
      o#outnl "}; ";
      o#spaces();
      local_id o self; o#outnl "();";

  | Do (b, c) :: rl when IsGenerator.is_gen b ->

      let self = Ident.tmp() in
      let loop_last() =
        o#spaces();
        local_id o self;
        o#outnl "();";
      in

      o#spaces();
      o#out "var ";
      local_id o self;
      o#outnl " = function () { ";
      o#scope begin fun () ->
        stmtgen loop_last o b;
        o#spaces();
        o#out "if (!("; expr o c; o#outnl ")) {";
        o#scope begin fun () ->
          stmtgen last o rl;
        end;
        o#spaces();
        o#outnl "}";
      end;
      o#spaces();
      o#outnl "}; ";
      o#spaces();
      local_id o self; o#outnl "();";

  | For (e1, e2, e3, b) :: rl when IsGenerator.is_gen b ->
      expr o e1;
      stmtgen last o
        (While (e2, b @ [Expr e3]) :: rl)

  | Foreach (e, ae, b) :: rl when IsGenerator.is_gen b ->
      let obj = Ident.tmp() in
      o#spaces();
      o#out "var ";
      local_id o obj;
      o#out " = hhmake_iterator("; expr o e; o#outnl ");";
      o#spaces();
      local_id o obj;
      o#out ".hhdynamic.hhf_rewind("; local_id o obj; o#outnl ");";
      o#spaces();
      let self = Ident.tmp() in
      let loop_last() =
        o#spaces();
        local_id o self;
        o#outnl "();";
      in

      o#spaces();
      o#out "var ";
      local_id o self;
      o#outnl " = function () { ";
      o#scope begin fun () ->
        local_id o obj;
        o#out ".hhdynamic.hhf_next("; local_id o obj; o#outnl ");";
        o#spaces();
        o#out "if ("; local_id o obj; o#out ".hhdynamic.hhf_valid(";
        local_id o obj; o#outnl ")) { ";
        o#scope begin fun () ->
          (match ae with
          | As_v (_, Lvar (_, v)) ->
              o#spaces();
              o#out "var ";
              local_id o v;
              o#out " = "; local_id o obj;
              o#out ".hhdynamic.hhf_current("; local_id o obj; o#outnl ");";
          | As_kv ((_, Lvar (_, k)), (_, Lvar (_, v))) ->
              o#spaces();
              o#out "var ";
              local_id o k;
              o#out " = "; local_id o obj;
              o#out ".hhdynamic.hhf_key("; local_id o obj; o#outnl ");";
              o#spaces();
              o#out "var ";
              local_id o v;
              o#out " = "; local_id o obj;
              o#out ".hhdynamic.hhf_current("; local_id o obj; o#outnl ");";
          | _ -> failwith "TODO Javascript jsGen.ml: foreach not finished"
          );
          stmtgen loop_last o b;
        end;
        o#spaces();
        o#outnl "}";
        o#spaces();
        o#outnl "else {";
        o#scope begin fun () ->
          stmtgen last o rl;
        end;
        o#spaces();
        o#outnl "}";
      end;
      o#spaces();
      o#outnl "}; ";
      o#spaces();
      local_id o self; o#outnl "();";
  | st :: rl ->
      stmt o st;
      stmtgen last o rl

and named_expr o e =
  let id = Ident.tmp() in
  o#spaces();
  o#out "var ";
  local_id o id;
  o#out " = function () { return ";
  expr o e;
  o#outnl "; };";
  id

and loop last o id b =
  o#spaces();
  o#out "var ";
  local_id o id;
  o#outnl " = function () { ";
  o#scope begin fun () ->
  stmtgen last o b;
  end;
  o#spaces();
  o#outnl "};";
  id

and named_block last o b =
  let id = Ident.tmp() in
  o#spaces();
  o#out "var ";
  local_id o id;
  o#outnl " = function () { ";
  stmtgen last o b;
  o#spaces();
  o#outnl "};";
  id

and make_tail last o b =
  let last_id = named_block last o b in
  let new_last () =
    o#spaces();
    local_id o last_id;
    o#outnl "();"
  in
  new_last

and stmt o st =
  o#spaces();
  stmt_ o st;
  o#nl()

and assign o op e1 e2 =
  match op with
  | None -> assign_ o e1 e2
  | Some bop -> assign_ o e1 (Pos.none, Binop (bop, e1, e2))

and assign_ o e1 e2 =
  match snd e1 with
  | Lvar _
  | Obj_get _ ->
      expr o e1; o#out " = "; expr o e2;
  | Array_get (e1, Some k) ->
      expr o e1; o#out "["; expr o k; o#out "] = ";
      expr o e2;
  | Array_get (e, None) ->
      o#out "("; expr o e; o#out ").push("; expr o e2; o#out ")"
  | List el ->
      let lid = Ident.tmp() in
      o#out "(("; local_id o lid; o#out " = ("; expr o e2; o#out "))";
      let i = ref 0 in
      List.iter begin fun e ->
        o#out ", (";
        let p = Pos.none in
        let n = p, Int (p, soi !i) in
        assign_ o e (p, Array_get ((p, Lvar (p, lid)), Some n));
        o#out ")";
        incr i
      end el;
      o#out ", "; local_id o lid; o#out ")"
  | _ -> failwith "Rest of lvalue"

and stmt_ ?(semi = true) o = function
  | Expr (_, Binop (Ast.Eq op, e1, e2)) ->
      assign o op e1 e2;
      if semi then
        o#out ";"
  | Expr (_, Binop (bop, e1, e2)) ->
      expr o e1;
      binop o bop;
      expr o e2;
      if semi then
        o#out ";"
  | Expr e ->
      expr o e;
      if semi then
        o#out ";"
  | Break ->
      o#out "break";
      if semi
      then o#out ";"
  | Continue ->
      o#out "continue";
      if semi
      then o#out ";"
  | Throw (_, e) ->
      o#out "throw ";
      expr o e;
      o#out ";";
      ()
  | Return (_, None) ->
      o#out "return";
      if semi
      then o#out ";"
  | Return (_, Some e) ->
      o#out "return ";
      expr o e;
      if semi
      then o#out ";"
  | If (e, b1, []) ->
      o#out "if("; expr o e;
      o#out ") ";
      block o b1;
  | If (e, b1, b2) ->
      o#out "if("; expr o e;
      o#out ") ";
      block o b1;
      o#spaces();
      o#out "else ";
      block o b2;
  | Static_var _ -> ()
  | Do (b, e) ->
      o#out "do ";
      block o b;
      o#spaces();
      o#out "while (";
      expr o e;
      o#out ")";
      if semi
      then o#out ";"
  | While (e, b) ->
      o#out "while ("; expr o e; o#out ") ";
      block o b
  | For (e1, e2,e3, b) ->
      o#out "for (";
      expr o e1;
      expr o e2; o#out "; ";
      expr o e3;
      o#out ") ";
      block o b
  | Switch (e, cl) ->
      o#out "switch ("; expr o e;
      o#out ") ";
      casel o cl
  | Foreach (e, ae, b) ->
      let obj =
        match e with
        | (_, Lvar (_, obj)) -> obj
        | _ -> failwith "TODO Javascript jsGen.ml: foreach not finished"
      in
      (match ae with
      | As_v (_, Lvar (_, v)) ->
          o#out "hhforeachv("; local_id o obj; o#out ", function(";
          local_id o v;
      | As_kv ((_, Lvar (_, k)), (_, Lvar (_, v))) ->
          o#out "hhforeach("; local_id o obj; o#out ", function(";
          list local_id o [k;v];
      | _ -> failwith "TODO Javascript jsGen.ml: foreach not finished"
      );
      o#out ") ";
      block o b;
      o#spaces();
      o#outnl ");";

  | Try (b, cl) ->
      o#out "try ";
      block o b;
      o#spaces();
      o#out "catch (";
      let err = Ident.tmp() in
      local_id o err;
      o#outnl ") {";
      o#scope begin fun () ->
        if cl <> [] then
          catchl err o cl;
      end;
      o#spaces();
      o#outnl "}"
  | Noop -> o#out ";"

(*
and block = stmt list
*)

and expr ?(is_lvalue=None) o (_, e) =
  expr_ ~is_lvalue o e

and expr_ ~is_lvalue o e =
  let expr = expr ~is_lvalue in
  match e with
  | Lvar (_, x) ->
      if (ISet.mem x (o#statics()))
      then o#out "statics.";
      local_id o x
  | Binop (Ast.Dot, e1, e2) ->
      o#out "(hhstring("; expr o e1;
      o#out ") + hhstring("; expr o e2;
      o#out "))"
  | Binop (Ast.Eq op, e1, e2) ->
      o#out "(";
      assign o op e1 e2;
      o#out ")";
  | Binop (bop, e1, e2) ->
      o#out "(";
      expr o e1;
      binop o bop;
      expr o e2;
      o#out ")";
  | Id (_, s)
  | Fun_id (_, s) ->
      fun_id o s
  | True -> o#out "true"
  | False -> o#out "false"
  | Int (_, s) -> o#out s
  | Float (_, s) -> o#out s
  | Null -> o#out "null"
  | String (_, s) ->
      o#out "'"; o#out s; o#out "'"
  | String2 (el, s) ->
      o#out "(''";
      List.iter begin fun x ->
        o#out "+hhstring(";
        expr o x;
        o#out ")"
      end (string2 el s 0);
      o#out ")"
  | Array fdl ->
      make_array o true fdl
  | ValCollection (_, el) ->
      let el = List.map (fun x -> AFvalue x) el in
      make_array o false el
  | KeyValCollection (_, fdl) ->
      let el = List.map (fun (x, y) -> AFkvalue (x, y)) fdl in
      make_array o false el
  | List el
  | Expr_list el
  | Call (Cnormal,(_, Id (_, "tuple")), el) ->
      let el = List.map (fun x -> AFvalue x) el in
      make_array o true el
  | Pair (e1, e2) ->
      let el = [AFvalue e1; AFvalue e2] in
      make_array o true el
  | Array_get (e, Some k) ->
      expr o e; o#out "["; expr o k; o#out "]";
  | This -> o#out "obj"
  | Call (Cnormal,(_, Id (_, "is_null")), [e]) ->
    expr o (Pos.none, Binop(Ast.EQeqeq, (Pos.none, Null), e))
  | Call (Cnormal, (_, Class_const (cid, (_, s))), el) ->
      (match cid with
      | CIparent -> o#out "obj.hhstatic.parent.hhdynamic."
      | CIself ->
          class_global_id o (o#get_class());
          o#out ".hhstatic.";
      | CIstatic -> o#out "obj.hhstatic."
      | CI (_, sid) ->
          class_global_id o sid;
          o#out ".hhstatic.";
      );
      fun_id o s;
      o#out "(";
      (match cid with
      | CIparent
      | CIstatic -> o#out "obj"
      | CIself -> class_global_id o (o#get_class())
      | CI (_, sid) -> class_global_id o sid
      );
      if el <> [] then o#out ", ";
      list expr o el;
      o#out ")"
  | Call (Cnormal, (_, Obj_get (e, (_, Id (_, s)))), el) ->
      let obj = Ident.tmp() in
      o#out "("; local_id o obj; o#out " = ";
      expr o e;
      o#out ")";
      o#out ".hhdynamic.";
      fun_id o s;
      o#out "(";
      local_id o obj;
      if el <> []
      then o#out ", ";
      list expr o el;
      o#out ")"
  | Call (_, e, el) ->
      expr o e;
      o#out "(";
      list expr o el;
      o#out ")"
  | Class_get (cid, (_, s)) ->
      let s = String.sub s 1 (String.length s - 1) in
      (match cid with
      | CIparent
      | CIself
      | CIstatic ->
          class_global_id o (o#get_class()^"_vars")
      | CI (_, sid) -> class_global_id o (sid^"_vars")
      );
      o#out ".";
      class_member_id o s
  | Class_const (cid, (_, s)) ->
      (match cid with
      | CIparent
      | CIself
      | CIstatic ->
          class_global_id o (o#get_class()^"_vars")
      | CI (_, sid) -> class_global_id o (sid^"_vars")
      );
      o#out ".";
      const_id o s;
  | Obj_get (e, (_, Id (_, s))) ->
      expr o e;
      o#out ".";
      class_member_id o s
  | Obj_get _ -> failwith "TODO Javascript jsGen.ml: Obj_get not finished"
  | Array_get _ -> assert false
  | New (CI (_, cid), el) ->
      o#out "new_"; class_id o cid; o#out "(";
      list expr o el;
      o#out ")";
  | New _ -> failwith "TODO"
  | Unop (Ast.Utild, _) -> failwith "TODO Javascript jsGen.ml: implement operator ~"
  | Unop (Ast.Unot, e) -> o#out "(!"; expr o e; o#out ")"
  | Unop (Ast.Uminus, e) -> o#out "(-"; expr o e; o#out ")"
  | Unop (Ast.Uincr, e) -> o#out "(++"; expr o e; o#out ")"
  | Unop (Ast.Udecr, e) -> o#out "(--"; expr o e; o#out ")"
  | Unop (Ast.Upincr, e) -> o#out "("; expr o e; o#out "++)"
  | Unop (Ast.Updecr, e) -> o#out "("; expr o e; o#out "--)"
  | Eif (e1, None, e3) ->
      o#out "("; expr o e1; o#out "?"; expr o e1; o#out ":";
      expr o e3; o#out ")";
  | Eif (e1, Some e2, e3) ->
      o#out "("; expr o e1; o#out "?"; expr o e2; o#out ":";
      expr o e3; o#out ")";
  | Efun (f, _) -> fun_def ~is_method:false o f.f_params f.f_body
  | Yield_break -> failwith "yield break not supported"
  | Special_func (Result _) -> failwith "result() not supported"
  | Special_func (Wait_for _) -> failwith "wait_for() not supported"
  | Special_func (Wait_forv _) -> failwith "wait_forv() not supported"
  | Special_func (Wait_forvr _) -> failwith "wait_forvr() not supported"
  | Special_func (Wait_forva _) -> failwith "wait_forva() not supported"
  | Special_func (Wait_forvar _) -> failwith "wait_forvar() not supported"
  | Special_func (Wait_for_result _) -> failwith "wait_for_result() not supported"
  | Special_func (Wait_forv_result _) -> failwith "wait_forv_result() not supported"
  | Special_func (Wait_forvr_result _) -> failwith "wait_forvr_result() not supported"
  | Special_func (Wait_forva_result _) -> failwith "wait_forva_result() not supported"
  | Special_func (Wait_forvar_result _) -> failwith "wait_forvar_result() not supported"
  | Yield e -> o#out "yield "; expr o e
  | Assert _ -> o#out "true"
  | InstanceOf (e, (_, Id s)) ->
      o#out "hhinstanceof("; expr o e; o#out (", '"^snd s^"')")
  | InstanceOf _ -> failwith "TODO Javascript jsGen.ml: implement dynamic instanceof"
  | Cast ((_, (Hany | Hmixed | Habstr _ | Harray _ | Hprim Tvoid)), e) -> expr o e
  | Cast ((_, Hprim (Tint)), e) ->
      o#out "parseInt(hhstring("; expr o e ; o#out "))"
  | Cast ((_, Hprim (Tfloat)), e) ->
      o#out "parseFloat(hhstring("; expr o e ; o#out "))"
  | Cast ((_, Hprim (Tbool)), e) -> o#out "!!("; expr o e; o#out ")"
  | Cast ((_, Hprim (Tstring)), e) ->
      o#out "(hhstring("; expr o e ; o#out "))"
  | Cast ((_, (Hoption _ | Hfun _ | Htuple _ | Hpair _)), _) ->
      assert false (* no syntax for that *)
  | Cast ((_, Happly (s, _)), e) ->
      let lid = Ident.tmp() in
      o#out "((";
      local_id o lid;
      o#out " = "; expr o e;
      o#out "), hhinstanceof("; local_id o lid; o#out (", '"^snd s^"')");
      o#out "? "; local_id o lid; o#out (": hhfatal('cannot convert to '+'"^ snd s ^"'))");
  | Cast ((_, Hprim (Tnum)), _) -> assert false
  | Xml _ -> failwith "TODO Javascript jsGen.ml: implement XHP"
  | Clone e ->
      o#out "hhclone("; expr o e; o#out ")"
(*
  | Xml of sid * (pstring * expr) list * expr list
*)

and make_array o is_array fdl =
  let id = Ident.tmp() in
  let i = ref 0 in
  o#out "(";
  local_id o id;
  o#out (" = new "^(if is_array then "Array" else "Object")^"(),");
  List.iter begin function
    | AFvalue e ->
        o#out "(";
        local_id o id;
        o#out "[";
        o#out (string_of_int !i);
        o#out "] = ";
        expr o e;
        o#out "), ";
        incr i
    | AFkvalue (k, e) ->
        o#out "(";
        local_id o id;
        o#out "[";
        expr o k;
        o#out "] = ";
        expr o e;
        o#out "), "
  end fdl;
  local_id o id;
  o#out ")"


and gclass_id o = function
  | CIparent
  | CIself -> failwith "TODO javascript jsGen.ml: implement parent and self"
  | CIstatic ->
      o#out "obj.hhstatic"
  | CI (_, sid) -> class_global_id o sid


and casel o cl =
  o#outnl "{";
  o#scope begin fun () ->
    List.iter (case o) cl
  end;
  o#spaces();
  o#outnl "}"

and case o = function
  | Default b ->
      o#spaces();
      o#out "default:";
      o#scope begin fun () ->
        stmtl o b
      end
  | Case (e, b) ->
      o#spaces();
      o#out "case ";
      expr o e;
      o#out ":";
      o#scope begin fun () ->
        stmtl o b
      end


and catchl err o = function
  | [] ->
      o#outnl "{";
      o#scope begin fun () ->
        o#spaces();
        o#out "throw("; local_id o err; o#outnl ");"
      end;
      o#spaces();
      o#outnl "}"
  | c :: rl ->
      catch err o c;
      o#spaces();
      o#out "else ";
      catchl err o rl

and catch err o (sid, (p, x), b) =
  o#spaces();
  o#out "if (";
  expr o (p, InstanceOf ((p, Lvar (p, err)), (fst sid, Id sid)));
  o#out ") ";
  block_f o begin fun () ->
    o#spaces();
    o#out "var "; local_id o x;
    o#out " = "; local_id o err;
    o#outnl ";"
  end b;

and binop o = function
  | Ast.Plus    -> o#out "+"
  | Ast.Minus   -> o#out "-"
  | Ast.Star    -> o#out "*"
  | Ast.Slash   -> o#out "/"
  | Ast.Eqeq    -> o#out "=="
  | Ast.EQeqeq  -> o#out "==="
  | Ast.Diff    -> o#out "!="
  | Ast.Diff2   -> o#out "!=="
  | Ast.AMpamp  -> o#out "&&"
  | Ast.BArbar  -> o#out "||"
  | Ast.Lt      -> o#out "<"
  | Ast.Lte     -> o#out "<="
  | Ast.Gt      -> o#out ">"
  | Ast.Gte     -> o#out ">="
  | Ast.Dot     -> assert false
  | Ast.Amp     -> o#out "&"
  | Ast.Bar     -> o#out "|"
  | Ast.Ltlt    -> o#out "<<"
  | Ast.Gtgt    -> o#out ">>"
  | Ast.Percent -> o#out "%"
  | Ast.Xor     -> o#out "^"
  | Ast.Eq None -> o#out " = "
  | Ast.Eq (Some Ast.Dot) -> assert false
  | Ast.Eq (Some bop) ->
      o#out " "; binop o bop; o#out "= "
