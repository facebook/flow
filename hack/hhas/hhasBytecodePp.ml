open BufferedPrinter
open HhasBytecodeIst
open SharedCollections

let map_illegals str =
  String.map begin function
    | '!' -> '$'
    | '@' -> '.'
    | c -> c
  end str

let variable str = "$" ^ map_illegals str

let qual_name s =
  if s <> "" && s.[0] = '\\'
  then map_illegals (String.sub s 1 (String.length s - 1))
  else failwith ("Missing namespace for name " ^ s)

let ident (Id s) = qual_name s

let string o s =
  o#out_char '"';
  String.iter (function
      | '"' -> o#out "\\\""
      | '\\' -> o#out "\\\\"
      | c -> o#out_char c
    ) s;
  o#out_char '"'

let attributes o attrs =
  let attr o = function
    | AttrStatic -> o#out "static"
    | AttrAbstract -> o#out "abstract"
    | AttrPublic -> o#out "public"
    | AttrPrivate -> o#out "private"
    | AttrMayusevv -> o#out "mayusevv"
  in
  let attrs = List.sort_uniq compare attrs in
  o#out "["; list ~sep:" " attr o attrs; o#out "]"

let rec to_files dir { class_defs; fun_defs; } =
  let o = new_out () in
  UMap.iter (fun _ cd ->
      let o = new_out () in
      pseudo_main o 1;
      class_def o cd;
      out_to_file o (dir ^ "/" ^ ident cd.cla_name ^ ".hhas")
    ) class_defs;
  pseudo_main o 0;
  UMap.iter (fun _ fd -> fun_def o fd) fun_defs;
  out_to_file o (dir ^ "/rx.hhas")

and pseudo_main o nclasses =
  o#out ".main {";
  o#newline;
  o#margin begin fun () ->
    for i = 1 to nclasses do
      o#out (Printf.sprintf "DefCls %d" (i-1));
      o#newline
    done;
    o#out "Int 1";
    o#newline;
    o#out "RetC";
    o#newline
  end;
  o#out "}";
  o#newline

and class_def o cd =
  o#out ".class ";
  begin match cd.cla_kind with
    | KClass -> ()
    | KInterface -> o#out "[interface] "
    | KFinalClass -> o#out "[final] "
    | KTrait -> o#out "[final trait] "
    | KAbstractFinal -> o#out "[abstract final] " end;
  o#out (ident cd.cla_name);
  if cd.cla_extends <> [] then begin
    o#out " extends ";
    list ~sep:" " (fun o c -> o#out (ident c)) o cd.cla_extends;
  end;
  if cd.cla_implements <> [] then begin
    o#out " implements (";
    list ~sep:" " (fun o c -> o#out (ident c)) o cd.cla_implements;
    o#out ")";
  end;
  o#out " {";
  o#newline;
  o#margin begin fun () ->
    uses o cd.cla_uses cd.cla_met_use;
    type_defs o cd.cla_types;
    (* TODO require extends is not supported by the
       HHAS parser yet, c.f. task 12483869 *)
    UMap.iter (fun _ md -> method_def o md) cd.cla_methods;
    method_def o cd.cla_constructor;
    List.iter (property o) cd.cla_properties
  end;
  o#out "}";
  o#newline;
  lambdas o cd.cla_lambdas

and uses o traits conflicts =
  let traits = UMap.key_values traits in
  if traits <> [] then begin
    o#out  ".use ";
    list ~sep:" "
      (fun o (n, _) -> o#out (qual_name n))
      o traits;
    if UMap.for_all (fun _ (_, c) -> c = []) conflicts
    then o#out ";"
    else begin
      o#out " {";
      o#newline;
      o#margin begin fun () ->
        UMap.iter (fun met_name (cla_name, conflicted) ->
            if conflicted <> [] then begin
              o#out (qual_name cla_name);
              o#out "::";
              o#out (map_illegals met_name);
              o#out " insteadof ";
              list ~sep:" "
                (fun o n -> o#out (qual_name n))
                o conflicted;
              o#out ";";
              o#newline;
            end
          ) conflicts;
      end;
      o#out "}";
    end;
    o#newline;
  end

and type_defs o types =
  (* TODO
     make sure type parameters can indeed be ignored
     make sure the abstract type constants can be serialized
  *)
  let type_kind = function
    (* see hphp/runtime/base/type-structure.h *)
    | "void" -> Some 0
    | "HH\\int" -> Some 1
    | "HH\\bool" -> Some 2
    | "HH\\float" -> Some 3
    | "HH\\string" -> Some 4
    | "HH\\arraykey" -> Some 7
    | "HH\\array" -> Some 12
    | "HH\\dict" -> Some 19
    | "HH\\vec" -> Some 20
    | "HH\\keyset" -> Some 21
    | s ->
      assert (String.length s < 3 || String.sub s 0 3 <> "HH\\");
      None
  in
  UMap.iter (fun _ (name, tother, _typ) ->
      let tother = ident tother in
      o#out ".const ";
      o#out (map_illegals name);
      o#out " isType = ";
      (match type_kind tother with
      | Some n ->
        o#out (Printf.sprintf "\"\"\"a:1:{s:4:\\\"kind\\\";i:%d;}\"\"\"" n)
      | None ->
        let len = String.length tother in
        o#out "\"\"\"a:2:{s:4:\\\"kind\\\";i:101;s:9:\\\"classname\\\";s:";
        o#out (Printf.sprintf "%d:\\\"%s\\\";}\"\"\"" len tother);
      );
      o#out ";";
      o#newline
    ) types

and type_user ty =
  match ty with
  | Tany -> "HH\\mixed"
  | Tvoid -> "HH\\void"
  | Tapply (n, []) -> qual_name n
  | Tapply (n, tyl) ->
    let tyl = List.map type_user tyl in
    Printf.sprintf "%s<%s>"
      (qual_name n)
      (String.concat ", " tyl)
  | Ttuple tyl ->
    let tyl = List.map type_user tyl in
    Printf.sprintf "(%s)"
      (String.concat ", " tyl)
  | Tfun (fty_params, fty_return) ->
    let fty_params = List.map type_user fty_params in
    Printf.sprintf "(function (%s): %s)"
      (String.concat ", " fty_params)
      (type_user fty_return)
  | Tparam (n, _) -> map_illegals n
  | Tinst _ -> "@inst"
  | Tthis _ -> "HH\\this"
  | Tinter _ -> assert false

and type_hint o tyh =
  o#out "< ";
  let t = type_user tyh.tyh_user in
  string o t;
  o#space;
  (match tyh.tyh_hhvm with
  | Some (t, fl) ->
    let type_flag o = function
      | HintVar -> o#out "type_var"
      | HintExtended -> o#out "extended_hint"
      | HintSoft -> o#out "soft"
    in
    string o (ident t);
    o#out " hh_type";
    let fl = List.sort_uniq compare fl in
    if fl <> [] then o#space;
    list ~sep:" " type_flag o fl
  | None -> o#out "N"
  );
  o#out " >"

and lambdas o ls =
  UMap.iter (fun _ -> lambda o) ls
and lambda o lam =
  o#out ".class [no_override unique] ";
  o#out (ident lam.lam_name);
  o#out " extends Closure {";
  o#newline;
  o#margin begin fun () ->
    SSet.iter (fun p ->
        property o ~uninit:true (p, [AttrPublic])
      ) lam.lam_uses;
    o#out ".method ";
    attributes o begin
      if lam.lam_kind = LamAsyncBlock
      then [AttrPublic; AttrStatic; AttrMayusevv]
      else [AttrPublic; AttrMayusevv]
    end;
    o#out " < \"\" N > __invoke(";
    list (fun o p -> o#out (variable p)) o lam.lam_params;
    o#out ")";
    if lam.lam_kind = LamNormal true
    || lam.lam_kind = LamAsyncBlock
    then o#out " isAsync";
    o#out " isClosureBody {";
    o#newline;
    o#margin begin fun () ->
      declvars o ~closure:lam.lam_uses lam.lam_locals;
      blocks o lam.lam_blocks
    end;
    o#out "}";
    o#newline
  end;
  o#out "}";
  o#newline

and property o ?(uninit = false) (pname, pattrs) =
  o#out ".property ";
  attributes o pattrs;
  o#space;
  o#out (map_illegals pname);
  if uninit
  then o#out " = uninit;"
  else o#out " = \"\"\"N;\"\"\";";
  o#newline

and method_def o md =
  o#out ".method ";
  attributes o md.met_attrs;
  o#space;
  type_hint o md.met_return;
  o#space;
  o#out (map_illegals md.met_name);
  fun_params o md.met_params;
  if md.met_async then o#out " isAsync";
  o#out " {";
  o#newline;
  o#margin begin fun () ->
    declvars o md.met_locals;
    blocks o md.met_blocks
  end;
  o#out "}";
  o#newline

and fun_def o fd =
  o#out ".function ";
  attributes o fd.fun_attrs;
  o#space;
  type_hint o fd.fun_return;
  o#space;
  o#out (ident fd.fun_name);
  fun_params o fd.fun_params;
  if fd.fun_async then o#out " isAsync";
  o#out " {";
  o#newline;
  o#margin begin fun () ->
    declvars o fd.fun_locals;
    blocks o fd.fun_blocks
  end;
  o#out "}";
  o#newline;
  lambdas o fd.fun_lambdas

and fun_params o params =
  let parameter o (p, tyh) =
    type_hint o tyh;
    o#space;
    o#out (variable p)
  in
  o#out "(";
  list parameter o params;
  o#out ")"

and declvars o ?closure locals =
  if closure <> None || not (SSet.is_empty locals) then
  begin
    let pset s = SSet.iter (fun n -> o#space; o#out (variable n)) s in
    o#out ".declvars";
    (match closure with
    | None -> ()
    | Some use -> o#out " $0Closure"; pset use
    );
    pset locals;
    o#out_char ';';
    o#newline
  end

and blocks o = List.iter (block o)
and block o = function
  | BSimple (lbl, instrs, jmp) ->
    (match lbl with
    | None -> ()
    | Some l ->
      o#newline;
      o#out l;
      o#out_char ':';
      o#newline
    );
    List.iter (instr o) instrs;
    jump o jmp
  | BTryCatch (btry, bcatches) ->
    o#out ".try_catch ";
    List.iter (fun (ty, lbl, _) ->
        o#out ("(" ^ ident ty ^ " " ^ lbl ^ ") ")
      ) bcatches;
    o#out "{";
    o#newline;
    o#margin begin fun () ->
      blocks o btry
    end;
    o#out "}";
    o#newline;
    List.iter (fun (_, _, cb) -> blocks o cb) bcatches

and jump o = function
  | JNext -> ()
  | JJmp lbl -> o#out ("Jmp " ^ lbl); o#newline
  | JJmpZ lbl -> o#out ("JmpZ " ^ lbl); o#newline
  | JJmpNZ lbl -> o#out ("JmpNZ " ^ lbl); o#newline
  | JRetC -> o#out "RetC"; o#newline
  | JThrow -> o#out "Throw"; o#newline
  | JFatal FatalRuntimeOmitFrame ->
    o#out "Fatal RuntimeOmitFrame"; o#newline

and instr o i =
  instr_ o i;
  o#newline
and instr_ o i =
  let f = Printf.sprintf in
  match i with
  | IAGetC -> o#out "AGetC"
  | IAdd -> o#out "Add"
  | IAwait -> o#out "Await"
  | IBareThis -> o#out "BareThis Notice"
  | IBaseC slot -> o#out (f "BaseC %d" slot)
  | IBaseH -> o#out "BaseH"
  | IBaseL var -> o#out (f "BaseL %s Warn" (variable var))
  | ICGetL var -> o#out (f "CGetL %s" (variable var))
  | ICUGetL var -> o#out (f "CUGetL %s" (variable var))
  | ICatch -> o#out "Catch"
  | ICheckThis -> o#out "CheckThis"
  | IClsCns cns -> o#out "ClsCns "; string o cns
  | IConcat -> o#out "Concat"
  | ICreateCl (n, id) -> o#out (f "CreateCl %d " n); string o (ident id)
  | IDim mem -> o#out "Dim Warn "; member o mem
  | IDiv -> o#out "Div"
  | IDouble flt -> o#out "Double "; o#out (string_of_float flt)
  | IDup -> o#out "Dup"
  | IEq -> o#out "Eq"
  | IFCall n -> o#out (f "FCall %d" n)
  | IFPassCE n -> o#out (f "FPassCE %d" n)
  | IFPushClsMethod n -> o#out (f "FPushClsMethod %d" n)
  | IFPushClsMethodD (n, met, cls) ->
    o#out (f "FPushClsMethodD %d " n);
    string o (map_illegals met);
    o#space;
    string o (ident cls)
  | IFPushCtor n -> o#out (f "FPushCtor %d" n)
  | IFPushCtorD (n, cls) -> o#out (f "FPushCtorD %d " n); string o (ident cls)
  | IFPushFunc n -> o#out (f "FPushFunc %d" n)
  | IFPushFuncD (n, fn) -> o#out (f "FPushFuncD %d " n); string o (ident fn)
  | IFPushObjMethodD (n, met) ->
    o#out (f "FPushObjMethodD %d " n);
    string o (map_illegals met);
    o#out " NullThrows"
  | IFalse -> o#out "False"
  | IGt -> o#out "Gt"
  | IGte -> o#out "Gte"
  | IInstanceOfD cls -> o#out "InstanceOfD "; string o (ident cls)
  | IInt n -> o#out "Int "; o#out (Int64.to_string n)
  | IIsTypeCNull -> o#out "IsTypeC Null"
  | ILateBoundCls -> o#out "LateBoundCls"
  | ILt -> o#out "Lt"
  | ILte -> o#out "Lte"
  | IMod -> o#out "Mod"
  | IMul -> o#out "Mul"
  | INameA -> o#out "NameA"
  | INeq -> o#out "Neq"
  | INewPackedArray n -> o#out (f "NewPackedArray %d" n)
  | INot -> o#out "Not"
  | INull -> o#out "Null"
  | IPopC -> o#out "PopC"
  | IPopR -> o#out "PopR"
  | IQueryM (n, mem) -> o#out (f "QueryM %d CGet " n); member o mem
  | ISetL var -> o#out (f "SetL %s" (variable var))
  | ISetM (n, mem) -> o#out (f "SetM %d " n); member o mem
  | IString str -> o#out "String "; string o str
  | IStringId id -> o#out "String "; string o (ident id)
  | ISub -> o#out "Sub"
  | ITrue -> o#out "True"
  | IUnboxR -> o#out "UnboxR"

and member o = function
  | MembField s -> o#out "PT:"; string o (map_illegals s)
  | MembIndex i -> o#out "EI:"; o#out (string_of_int i)
